#include "buildtest.h"
#include "compare.h"
#include "regress.h"

#include <fstream>
#include <iostream>

namespace {

struct Model {
  const char* id;
  bool order_n;
};

// Three models, each with its .sd stored next to the golden output so a
// later edit under examples/ does not move the baseline. Kane is the
// generator default. Order(N) fbar locks -n as well.
const Model kModels[] = {
    {"fbar", false},
    {"pend", false},
    {"humpage", false},
    {"fbar", true},
};

std::string read_file(const std::filesystem::path& path) {
  std::ifstream in(path, std::ios::binary);
  if (!in)
    return {};
  std::ostringstream buf;
  buf << in.rdbuf();
  return buf.str();
}

bool write_file(const std::filesystem::path& path, const std::string& text) {
  std::filesystem::create_directories(path.parent_path());
  std::ofstream out(path, std::ios::binary);
  if (!out)
    return false;
  out << text;
  return static_cast<bool>(out);
}

std::vector<std::string> path_spellings(const std::filesystem::path& sd) {
  std::vector<std::string> out;
  std::error_code ec;
  std::filesystem::path abs = std::filesystem::absolute(sd, ec);
  if (!ec)
    out.push_back(abs.string());
  out.push_back(sd.string());
  std::string generic = sd.generic_string();
  if (generic != sd.string())
    out.push_back(generic);
  if (!ec) {
    std::string abs_generic = abs.generic_string();
    if (abs_generic != abs.string())
      out.push_back(abs_generic);
  }
  return out;
}

int run_sdfast(const std::filesystem::path& bin, const std::string& lang,
               bool order_n, const std::filesystem::path& sd,
               const std::string& base, const std::filesystem::path& workdir) {
  std::vector<std::string> args;
  // -lc is explicit. -lfortran is the generator's default language.
  if (lang == "c")
    args.push_back("-lc");
  else if (lang == "fortran")
    args.push_back("-lfortran");
  else
    args.push_back("-l" + lang);
  if (order_n)
    args.push_back("-n");
  args.push_back("-ge");
  args.push_back(sd.string());
  args.push_back(base);
  return run_program(bin.string(), args, workdir.string());
}

int check_one(bool update, const std::filesystem::path& source,
              const std::filesystem::path& sdfast, const std::string& lang,
              const Model& model) {
  const char* formulation = model.order_n ? "ordern" : "kane";
  std::filesystem::path golden_dir =
      source / "tests" / "golden" / lang / formulation / model.id;
  // The input that produced this directory lives beside the outputs.
  std::filesystem::path sd = golden_dir / (std::string(model.id) + ".sd");
  std::string base = sd.stem().string();
  std::filesystem::path work =
      std::filesystem::path("generated/golden") / lang / formulation / model.id;
  std::filesystem::remove_all(work);
  std::filesystem::create_directories(work);

  std::string label = std::string(lang) + "/" + formulation + "/" + model.id;
  if (!std::filesystem::exists(sd)) {
    std::cerr << "[FAIL] " << label << " missing input " << sd << "\n";
    return 1;
  }
  if (run_sdfast(sdfast, lang, model.order_n, sd, base, work) != 0) {
    std::cerr << "[FAIL] " << label << " sdfast\n";
    return 1;
  }

  SdOutputNames names = sd_output_names(lang, base);
  std::vector<std::string> produced = {names.dyn, names.sar, names.info,
                                       names.lib};
  auto spellings = path_spellings(sd);

  int rc = 0;
  for (const auto& filename : produced) {
    std::filesystem::path actual_path = work / filename;
    std::string canon = canonical_output_name(filename);
    std::filesystem::path golden_path = golden_dir / canon;
    if (!std::filesystem::exists(actual_path)) {
      std::cerr << "[FAIL] " << label << " did not write " << filename << "\n";
      rc = 1;
      continue;
    }
    std::string actual = normalize_generated(read_file(actual_path), spellings);
    if (update) {
      if (!write_file(golden_path, actual)) {
        std::cerr << "[FAIL] cannot write " << golden_path << "\n";
        rc = 1;
      } else {
        std::cout << "[WROTE] " << golden_path.lexically_relative(source) << "\n";
      }
      continue;
    }
    if (!std::filesystem::exists(golden_path)) {
      std::cerr << "[FAIL] " << label << " missing golden " << golden_path
                << "\n        run: cmake --build <build> --target update-golden\n";
      rc = 1;
      continue;
    }
    // Goldens are stored already normalized. Running the same pass again
    // is idempotent and still catches a clock stamp if one was missed.
    std::string golden = normalize_generated(read_file(golden_path), {});
    std::string report;
    if (!compare_texts(actual, golden, {}, report)) {
      std::cerr << "[FAIL] " << label << " " << canon << "\n" << report;
      rc = 1;
    }
  }
  if (rc == 0 && !update)
    std::cout << "[PASS] " << label << "\n";
  return rc;
}

bool compile_and_run_fbar(const std::string& lang, const std::filesystem::path& source,
                          const std::filesystem::path& sdfast,
                          const std::filesystem::path& work,
                          const std::filesystem::path& stdout_path) {
  // Same locked input as the fbar golden, not examples/c/fbar.sd.
  std::filesystem::path sd =
      source / "tests" / "golden" / "c" / "kane" / "fbar" / "fbar.sd";
  std::filesystem::remove_all(work);
  std::filesystem::create_directories(work);
  if (run_sdfast(sdfast, lang, false, sd, "fbar", work) != 0)
    return false;

  SdOutputNames names = sd_output_names(lang, "fbar");
  std::filesystem::path driver =
      lang == "fortran" ? source / "examples/fortran/fbar.f"
                        : source / "examples/c/fbarc.c";

  std::vector<std::string> args;
  std::string compiler;
  if (lang == "c") {
    compiler = SDFAST_C_COMPILER;
    if (compiler_is_msvc(compiler)) {
      args = {"/nologo", "/TC", "/Fe:fbar.exe", driver.string(), names.dyn,
              names.sar, names.lib};
    } else {
      args = {"-std=gnu89", driver.string(), names.dyn, names.sar, names.lib,
              "-lm", "-o", "fbar"};
    }
  } else {
    compiler = SDFAST_FORTRAN_COMPILER;
    if (compiler.empty()) {
      std::cout << "[SKIP] fbar fortran numeric: no Fortran compiler\n";
      return true;
    }
    args = {"-std=legacy",
            "-ffixed-form",
            "-ffixed-line-length-none",
            "-fallow-argument-mismatch",
            "-fno-automatic",
            driver.string(),
            names.dyn,
            names.sar,
            names.lib,
            "-o",
            "fbar"};
  }

  if (run_program(compiler, args, work.string()) != 0) {
    std::cerr << "[FAIL] compile fbar (" << lang << ")\n";
    return false;
  }

  std::filesystem::path exe = std::filesystem::absolute(work / "fbar");
#ifdef _WIN32
  if (!std::filesystem::exists(exe))
    exe = std::filesystem::absolute(work / "fbar.exe");
#endif
  // Absolute path: run_program chdirs to the work directory first.
  if (run_program(exe.string(), {}, work.string(), stdout_path.string()) != 0) {
    std::cerr << "[FAIL] run fbar (" << lang << ")\n";
    return false;
  }
  return true;
}

} // namespace

int golden_test(bool update) {
  std::filesystem::path source = SDFAST_SOURCE_DIR;
  std::filesystem::path sdfast = SDFAST_BIN_PATH;
  int rc = 0;
  for (const char* lang : {"c", "fortran"}) {
    for (const Model& model : kModels)
      rc |= check_one(update, source, sdfast, lang, model);
  }
  return rc;
}

int numeric_test(bool update) {
  std::filesystem::path source = SDFAST_SOURCE_DIR;
  std::filesystem::path sdfast = SDFAST_BIN_PATH;
  std::filesystem::path stdout_c = std::filesystem::absolute("generated/numeric/fbar_c.out");
  std::filesystem::path stdout_f = std::filesystem::absolute("generated/numeric/fbar_f.out");
  std::filesystem::create_directories(stdout_c.parent_path());

  if (!compile_and_run_fbar("c", source, sdfast, "generated/numeric/c", stdout_c))
    return 1;

  std::string c_text = read_file(stdout_c);
  std::filesystem::path reference =
      source / "tests/golden/numeric/fbar_c_stdout.txt";

  if (update) {
    if (!write_file(reference, c_text)) {
      std::cerr << "[FAIL] cannot write " << reference << "\n";
      return 1;
    }
    std::cout << "[WROTE] " << reference.lexically_relative(source) << "\n";
  } else if (!std::filesystem::exists(reference)) {
    std::cerr << "[FAIL] missing " << reference << "\n";
    return 1;
  }

  bool have_fortran = std::string(SDFAST_FORTRAN_COMPILER).size() > 0;
  bool fortran_ok = true;
  if (have_fortran) {
    fortran_ok = compile_and_run_fbar("fortran", source, sdfast,
                                      "generated/numeric/fortran", stdout_f);
    if (!fortran_ok)
      return 1;
  }

  if (update)
    return 0;

  CompareOptions traj;
  traj.rel_tol = kTrajectoryRelTol;
  traj.abs_tol = kTrajectoryAbsTol;

  auto c_nums = extract_numbers(c_text);
  auto ref_nums = extract_numbers(read_file(reference));
  std::string report;
  if (!compare_number_lists(c_nums, ref_nums, traj, report)) {
    std::cerr << "[FAIL] fbar C stdout vs reference\n" << report;
    return 1;
  }
  std::cout << "[PASS] fbar C trajectory (" << c_nums.size() << " values)\n";

  if (have_fortran) {
    auto f_nums = extract_numbers(read_file(stdout_f));
    if (!compare_number_lists(f_nums, ref_nums, traj, report)) {
      std::cerr << "[FAIL] fbar Fortran stdout vs reference\n" << report;
      return 1;
    }
    if (!compare_number_lists(c_nums, f_nums, traj, report)) {
      std::cerr << "[FAIL] fbar C vs Fortran\n" << report;
      return 1;
    }
    std::cout << "[PASS] fbar Fortran trajectory (" << f_nums.size()
              << " values)\n";
  }
  return 0;
}
