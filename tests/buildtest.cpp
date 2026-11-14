#include "buildtest.h"

#include <cctype>
#include <fstream>
#include <map>

#ifdef _WIN32
// WINDOWS
#include <windows.h>

int run_program(const std::string& executable,
                const std::vector<std::string>& args,
                const std::string& workdir,
                const std::string& stdout_path) {
  std::ostringstream cmd_stream;
  cmd_stream << "\"" << executable << "\"";

  for (const auto& a : args)
    cmd_stream << " \"" << a << "\""; // safer quoting

  std::string cmd = cmd_stream.str();

  STARTUPINFOA si{};
  PROCESS_INFORMATION pi{};
  si.cb = sizeof(si);

  HANDLE hOut = INVALID_HANDLE_VALUE;
  if (!stdout_path.empty()) {
    SECURITY_ATTRIBUTES sa{};
    sa.nLength = sizeof(sa);
    sa.bInheritHandle = TRUE;
    hOut = CreateFileA(stdout_path.c_str(), GENERIC_WRITE, FILE_SHARE_READ, &sa,
                       CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
    if (hOut == INVALID_HANDLE_VALUE) {
      std::cerr << "[ERROR] cannot open stdout file " << stdout_path << "\n";
      return EXIT_FAILURE;
    }
    si.dwFlags |= STARTF_USESTDHANDLES;
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    si.hStdOutput = hOut;
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
  }

  std::vector<char> buffer(cmd.begin(), cmd.end());
  buffer.push_back('\0');

  BOOL success = CreateProcessA(
      nullptr, buffer.data(), nullptr, nullptr, hOut != INVALID_HANDLE_VALUE,
      0, nullptr, workdir.empty() ? nullptr : workdir.c_str(), &si, &pi);
  if (hOut != INVALID_HANDLE_VALUE)
    CloseHandle(hOut);

  if (!success) {
    DWORD err = GetLastError();
    std::cerr << "[ERROR] CreateProcess failed (code " << err << ")\n";
    return EXIT_FAILURE;
  }

  DWORD wait_result = WaitForSingleObject(pi.hProcess, INFINITE);
  if (wait_result != WAIT_OBJECT_0) {
    std::cerr << "[ERROR] WaitForSingleObject failed\n";
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    return EXIT_FAILURE;
  }

  DWORD exit_code = 0;
  if (!GetExitCodeProcess(pi.hProcess, &exit_code)) {
    std::cerr << "[ERROR] GetExitCodeProcess failed\n";
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    return EXIT_FAILURE;
  }

  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);

  return static_cast<int>(exit_code);
}

#else
// POSIX
#include <fcntl.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

int run_program(const std::string& executable,
                const std::vector<std::string>& args,
                const std::string& workdir,
                const std::string& stdout_path) {

  // Build full command string (for logging)
  std::ostringstream cmd_stream;
  cmd_stream << executable;
  for (const auto& a : args)
    cmd_stream << " " << a;

  std::string full_cmd = cmd_stream.str();

  std::vector<char*> argv;
  argv.push_back(const_cast<char*>(executable.c_str()));

  for (const auto& arg : args)
    argv.push_back(const_cast<char*>(arg.c_str()));

  argv.push_back(nullptr);

  pid_t pid = fork();

  if (pid < 0) {
    std::cerr << "[ERROR] fork() failed for command:\n" << full_cmd << "\n";
    perror("fork");
    return EXIT_FAILURE;
  }

  if (pid == 0) {
    // Child

    if (!stdout_path.empty()) {
      // Open before chdir so a relative path is the parent's cwd, matching
      // the Windows CreateFile path. Callers pass an absolute path.
      int fd = open(stdout_path.c_str(), O_WRONLY | O_CREAT | O_TRUNC, 0644);
      if (fd < 0) {
        perror(stdout_path.c_str());
        _exit(EXIT_FAILURE);
      }
      if (dup2(fd, STDOUT_FILENO) < 0) {
        perror("dup2");
        _exit(EXIT_FAILURE);
      }
      close(fd);
    }

    if (!workdir.empty()) {
      if (chdir(workdir.c_str()) != 0) {
        std::cerr << "[ERROR] chdir failed: " << workdir << "\n";
        perror("chdir");
        _exit(EXIT_FAILURE);
      }
    }

    execvp(executable.c_str(), argv.data());
    std::cerr << "[ERROR] execvp failed for command:\n" << full_cmd << "\n";
    perror(executable.c_str());
    _exit(EXIT_FAILURE);
  }

  // Parent
  int status = EXIT_SUCCESS;
  if (waitpid(pid, &status, 0) < 0) {
    std::cerr << "[ERROR] waitpid failed for command:\n" << full_cmd << "\n";
    perror("waitpid");
    return EXIT_FAILURE;
  }

  if (WIFEXITED(status)) {
    int exit_code = WEXITSTATUS(status);
    if (exit_code != 0) {
      std::cerr << "[ERROR] command exited with code " << exit_code << ":\n"
                << full_cmd << "\n";
    }
    return exit_code;
  }

  if (WIFSIGNALED(status)) {
    std::cerr << "[ERROR] command killed by signal " << WTERMSIG(status)
              << ":\n"
              << full_cmd << "\n";
  }
  return EXIT_FAILURE;
}

#endif

namespace {

std::string read_text(const std::filesystem::path& path) {
  std::ifstream in(path, std::ios::binary);
  if (!in)
    return {};
  std::ostringstream buf;
  buf << in.rdbuf();
  return buf.str();
}

std::string upper_ident(std::string text) {
  for (char& c : text) {
    unsigned char u = static_cast<unsigned char>(c);
    if (u >= 'a' && u <= 'z')
      c = static_cast<char>(u - 'a' + 'A');
    else if (!((u >= 'A' && u <= 'Z') || (u >= '0' && u <= '9')))
      c = '_';
  }
  if (text.empty())
    text = "SD";
  return text;
}

bool take_int(const std::string& text, size_t& i, int& value) {
  while (i < text.size() &&
         std::isspace(static_cast<unsigned char>(text[i])))
    ++i;
  if (i >= text.size())
    return false;
  size_t end = i;
  if (text[end] == '-' || text[end] == '+')
    ++end;
  size_t digits = end;
  while (end < text.size() &&
         std::isdigit(static_cast<unsigned char>(text[end])))
    ++end;
  if (end == digits)
    return false;
  value = std::stoi(text.substr(i, end - i));
  i = end;
  return true;
}

std::map<std::string, int> parse_defines(const std::string& text,
                                        const std::string& prefix) {
  std::map<std::string, int> out;
  const std::string key = "#define " + prefix + "_";
  for (size_t at = 0; (at = text.find(key, at)) != std::string::npos;) {
    size_t name = at + key.size();
    size_t end = name;
    while (end < text.size() &&
           (std::isalnum(static_cast<unsigned char>(text[end])) ||
            text[end] == '_'))
      ++end;
    size_t i = end;
    int value = 0;
    if (end > name && take_int(text, i, value))
      out[text.substr(name, end - name)] = value;
    at = end + 1;
  }
  return out;
}

std::map<std::string, int> parse_info_params(const std::string& text) {
  std::map<std::string, int> out;
  size_t i = 0;
  while (i < text.size()) {
    size_t eol = text.find('\n', i);
    if (eol == std::string::npos)
      eol = text.size();
    std::string line = text.substr(i, eol - i);
    i = eol < text.size() ? eol + 1 : eol;
    size_t name = 0;
    while (name < line.size() &&
           std::isspace(static_cast<unsigned char>(line[name])))
      ++name;
    size_t end = name;
    while (end < line.size() &&
           std::islower(static_cast<unsigned char>(line[end])))
      ++end;
    if (end == name)
      continue;
    size_t num = end;
    int value = 0;
    if (!take_int(line, num, value))
      continue;
    out[line.substr(name, end - name)] = value;
  }
  return out;
}

std::map<std::string, int> parse_gtopo_scalars(const std::string& text) {
  std::map<std::string, int> out;
  const std::string mark = "/* ";
  for (size_t at = 0; (at = text.find(mark, at)) != std::string::npos;) {
    size_t name = at + mark.size();
    size_t end = name;
    while (end < text.size() &&
           std::islower(static_cast<unsigned char>(text[end])))
      ++end;
    if (end == name || text.compare(end, 3, " */") != 0) {
      at = name;
      continue;
    }
    size_t i = end + 3;
    int value = 0;
    if (take_int(text, i, value) && !out.count(text.substr(name, end - name)))
      out[text.substr(name, end - name)] = value;
    at = i;
  }
  return out;
}

bool expect_eq(const std::string& what, int got, int want) {
  if (got == want)
    return true;
  std::cerr << "[FAIL] " << what << " is " << got << ", expected " << want
            << "\n";
  return false;
}

}  // namespace

int check_model_defs(const std::filesystem::path& defs,
                     const std::filesystem::path& info,
                     const std::filesystem::path& dyn,
                     const std::string& prefix) {
  const std::string pfx = upper_ident(prefix);
  const std::string header = read_text(defs);
  const std::string info_text = read_text(info);
  const std::string dyn_text = read_text(dyn);
  if (header.empty() || info_text.empty() || dyn_text.empty()) {
    std::cerr << "[FAIL] missing defs, info, or dyn for size check\n";
    return 1;
  }
  const auto macros = parse_defines(header, pfx);
  const auto reported = parse_info_params(info_text);
  const auto topo = parse_gtopo_scalars(dyn_text);

  const std::pair<const char*, const char*> info_map[] = {
      {"nbod", "NBOD"},   {"njnt", "NJNT"},   {"ndof", "NDOF"},
      {"nloop", "NLOOP"}, {"nldof", "NLDOF"}, {"nq", "NQ"},
      {"nu", "NU"},       {"nlq", "NLQ"},     {"nlu", "NLU"},
      {"nc", "NC"},       {"nlc", "NLC"},     {"npresc", "NPRESC"},
      {"nuserc", "NUSERC"},
  };
  const std::pair<const char*, const char*> topo_map[] = {
      {"nbod", "NBOD"},     {"ndof", "NDOF"},   {"ncons", "NC"},
      {"nloop", "NLOOP"},   {"nldof", "NLDOF"}, {"nloopc", "NLC"},
      {"nball", "NBALL"},   {"nlball", "NLBALL"}, {"npres", "NPRESC"},
      {"nuser", "NUSERC"},
  };

  int rc = 0;
  auto macro_of = [&](const char* name, int& value) -> bool {
    auto it = macros.find(name);
    if (it == macros.end()) {
      std::cerr << "[FAIL] missing #" << pfx << "_" << name << "\n";
      rc = 1;
      return false;
    }
    value = it->second;
    return true;
  };

  for (const auto& item : info_map) {
    auto it = reported.find(item.first);
    int value = 0;
    if (it == reported.end()) {
      std::cerr << "[FAIL] sdinfo has no " << item.first << "\n";
      rc = 1;
      continue;
    }
    if (!macro_of(item.second, value))
      continue;
    if (!expect_eq(std::string(pfx) + "_" + item.second + " vs sdinfo " +
                       item.first,
                   value, it->second))
      rc = 1;
  }
  for (const auto& item : topo_map) {
    auto it = topo.find(item.first);
    int value = 0;
    if (it == topo.end()) {
      std::cerr << "[FAIL] sdgtopo has no " << item.first << "\n";
      rc = 1;
      continue;
    }
    if (!macro_of(item.second, value))
      continue;
    if (!expect_eq(std::string(pfx) + "_" + item.second + " vs sdgtopo " +
                       item.first,
                   value, it->second))
      rc = 1;
  }

  int nq = 0, nu = 0, ndof = 0, nball = 0, nlq = 0, nldof = 0, nlball = 0;
  int njnt = 0, nbod = 0, nloop = 0, nc = 0, npresc = 0, nlc = 0, nxc = 0,
      nuserc = 0;
  if (macro_of("NQ", nq) && macro_of("NDOF", ndof) && macro_of("NBALL", nball))
    if (!expect_eq(pfx + "_NQ vs ndof+nball", nq, ndof + nball))
      rc = 1;
  if (macro_of("NU", nu) && macro_of("NDOF", ndof))
    if (!expect_eq(pfx + "_NU vs ndof", nu, ndof))
      rc = 1;
  if (macro_of("NLQ", nlq) && macro_of("NLDOF", nldof) &&
      macro_of("NLBALL", nlball))
    if (!expect_eq(pfx + "_NLQ vs nldof+nlball", nlq, nldof + nlball))
      rc = 1;
  if (macro_of("NJNT", njnt) && macro_of("NBOD", nbod) &&
      macro_of("NLOOP", nloop))
    if (!expect_eq(pfx + "_NJNT vs nbod+nloop", njnt, nbod + nloop))
      rc = 1;
  if (macro_of("NC", nc) && macro_of("NPRESC", npresc) && macro_of("NLC", nlc) &&
      macro_of("NXC", nxc) && macro_of("NUSERC", nuserc))
    if (!expect_eq(pfx + "_NC vs presc+loop+explicit+user", nc,
                   npresc + nlc + nxc + nuserc))
      rc = 1;
  return rc;
}

int test_sdfast_example(const test_program& t,
                        const std::filesystem::path& sdfast_examples,
                        const std::filesystem::path& sdfast_bin) {

  std::filesystem::path src_path = sdfast_examples / t.lang;

  std::string base = t.base;

  std::filesystem::path sd_file = src_path / (base + ".sd");

  std::filesystem::path build_dir =
      std::filesystem::path("generated/" + t.lang + "/" + base);
  std::filesystem::path driver_path = src_path / t.driver;

  SdOutputNames names = sd_output_names(t.lang, base);
  std::string dyn = names.dyn;
  std::string sar = names.sar;
  std::string sdlib = names.lib;

  std::filesystem::create_directories(build_dir);

  // run sdfast
  {

    std::vector<std::string> args = {"-l" + t.lang, "-ge", sd_file.string(),
                                     base};

    if (run_program(sdfast_bin.string(), args, build_dir.string()) !=
        EXIT_SUCCESS) {
      std::cerr << "sdfast: " << base << "\n";
      return EXIT_FAILURE;
    }
  }

  // A user translation unit that includes only the model header must be
  // able to size arrays from the prefix macros and call library routines.
  if (t.lang == "c" && base == "sphere") {
    if (check_model_defs(build_dir / names.defs, build_dir / names.info,
                         build_dir / names.dyn, "sd") != 0)
      return EXIT_FAILURE;
    const std::filesystem::path user_tu = build_dir / "defs_user.c";
    {
      std::ofstream out(user_tu);
      if (!out)
        return EXIT_FAILURE;
      out << "#include \"" << names.defs << "\"\n"
          << "static void step(double t, double st[], double dst[],\n"
          << "                 double param[], int* status) {\n"
          << "    (void)t; (void)st; (void)dst; (void)param; (void)status;\n"
          << "}\n"
          << "static void residual(double vars[], double param[], double fval[]) {\n"
          << "    (void)vars; (void)param; (void)fval;\n"
          << "}\n"
          << "int main(void) {\n"
          << "    double q[SD_NQ];\n"
          << "    double u[SD_NU];\n"
          << "    double st[1] = {0}, dst[1] = {0}, param[1] = {0}, work[8] = {0};\n"
          << "    double time = 0, step_h = 0.1, tol = 1e-8;\n"
          << "    double vars[1] = {0}, jw[4] = {0}, dw[16] = {0}, rw[32] = {0};\n"
          << "    double fret[1] = {0}, ld[1] = {0}, sum[1] = {0};\n"
          << "    double m[1] = {0}, l[1] = {0}, d[1] = {0};\n"
          << "    int lock[1] = {0}, iw[16] = {0}, map[1] = {0};\n"
          << "    int err = 0, which = 0, fcnt = 0;\n"
          << "    (void)q;\n"
          << "    (void)u;\n"
          << "    sdvinteg(step, &time, st, dst, param, 0.1, &step_h, 1, tol,\n"
          << "             work, &err, &which);\n"
          << "    sdroot(residual, vars, param, 1, 1, 0, lock, 1e-6, 1e-6, 1,\n"
          << "           jw, dw, rw, iw, fret, &fcnt, &err);\n"
          << "    sdldudcomp(1, 1, map, 1e-12, ld, sum, m, l, d);\n"
          << "    return 0;\n"
          << "}\n";
    }
    const std::string cc = SDFAST_C_COMPILER;
    std::vector<std::string> uargs;
    /* cl is started with cwd = build_dir, so the source name is the
     * leaf. A path relative to the test binary's directory is not found. */
    if (compiler_is_msvc(cc))
      uargs = {"/nologo", "/std:c17", "/W4", "/WX", "/TC", "/c",
               user_tu.filename().string()};
    else
      uargs = {"-std=c17", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-c",
               user_tu.filename().string(), "-o", "defs_user.o"};
    if (run_program(cc, uargs, build_dir.string()) != EXIT_SUCCESS) {
      std::cerr << "defs header user TU failed to compile\n";
      return EXIT_FAILURE;
    }
  }

  // Compile + Link

  std::vector<std::string> args;
  if (t.lang == "c" && base == "sphere") {
    // sphere.c is C17 and is held to the same warnings as the generator.
    // Generated files still use implicit declarations, so they stay gnu89.
    const std::string cc = SDFAST_C_COMPILER;
    if (compiler_is_msvc(cc)) {
      args = {"/nologo", "/std:c17", "/W4", "/WX", "/TC", "/c",
              "/Fosphere.obj", driver_path.string()};
      if (run_program(cc, args, build_dir.string()) != EXIT_SUCCESS)
        return EXIT_FAILURE;
      for (const std::string& src : {dyn, sar, sdlib}) {
        args = {"/nologo", "/TC", "/c", src};
        if (run_program(cc, args, build_dir.string()) != EXIT_SUCCESS)
          return EXIT_FAILURE;
      }
      std::string dyn_obj = std::filesystem::path(dyn).stem().string() + ".obj";
      std::string sar_obj = std::filesystem::path(sar).stem().string() + ".obj";
      args = {"/nologo", "/Fe:sphere.exe", "sphere.obj", dyn_obj, sar_obj,
              "sdlib.obj"};
      if (run_program(cc, args, build_dir.string()) != EXIT_SUCCESS)
        return EXIT_FAILURE;
    } else {
      args = {"-std=c17", "-Wall", "-Wextra", "-Wpedantic", "-Werror",
              "-c", driver_path.string(), "-o", "sphere.o"};
      if (run_program(cc, args, build_dir.string()) != EXIT_SUCCESS)
        return EXIT_FAILURE;
      args = {"-std=gnu89", "-c", dyn, "-o", "sphere_dyn.o"};
      if (run_program(cc, args, build_dir.string()) != EXIT_SUCCESS)
        return EXIT_FAILURE;
      args = {"-std=gnu89", "-c", sar, "-o", "sphere_sar.o"};
      if (run_program(cc, args, build_dir.string()) != EXIT_SUCCESS)
        return EXIT_FAILURE;
      args = {"-std=gnu89", "-c", sdlib, "-o", "sdlib.o"};
      if (run_program(cc, args, build_dir.string()) != EXIT_SUCCESS)
        return EXIT_FAILURE;
      args = {"-o", base, "sphere.o", "sphere_dyn.o", "sphere_sar.o", "sdlib.o",
              "-lm"};
      if (run_program(cc, args, build_dir.string()) != EXIT_SUCCESS)
        return EXIT_FAILURE;
    }
  } else if (t.lang == "c") {
    args = {"-std=gnu89", driver_path.string(), dyn, sar, sdlib, "-lm", "-o",
            base};

    if (run_program(SDFAST_C_COMPILER, args, build_dir.string()) !=
        EXIT_SUCCESS)
      return EXIT_FAILURE;
  }

  else if (t.lang == "cpp") {

    args = {"-std=c++17", driver_path.string(), dyn, sar, sdlib, "-lm", "-o",
            base};

    if (run_program(SDFAST_CXX_COMPILER, args, build_dir.string()) !=
        EXIT_SUCCESS)
      return EXIT_FAILURE;
  }

  else if (t.lang == "fortran") {

    // Ensure the Fortran compiler is configured before attempting to use it.
    std::string fortran_compiler = SDFAST_FORTRAN_COMPILER;
    if (fortran_compiler.empty()) {
      std::cerr << "Fortran compiler is not configured "
                << "skipping Fortran test : " << base << "\n";
      std::cout << "[SKIP] " << base << "\n";
      return EXIT_SUCCESS;
    }

    if (base == "quickret") {
      // quickret.f calls SD2ROOT. A library-only run with no input file
      // leaves system dimensions at 0; malloc(0) is non-null on glibc and
      // null on macOS, and that path segfaults there. Pass the model so
      // the sd2-prefixed library is generated the same way as sdlib.
      std::vector<std::string> gen = {"-l" + t.lang, "-gl", "-psd2",
                                     sd_file.string(), base};
      if (run_program(sdfast_bin.string(), gen, build_dir.string()) !=
          EXIT_SUCCESS) {
        std::cerr << "sdfast preprocessing failed\n";
        return EXIT_FAILURE;
      }
    }

    args = {"-std=legacy",
            "-ffixed-form",
            "-ffixed-line-length-none",
            "-fallow-argument-mismatch",
            "-fno-automatic",
            driver_path.string(),
            dyn};

    if (std::filesystem::exists(build_dir / sar) && base != "slew")
      args.push_back(sar);
    args.push_back(sdlib);

    std::string sd2lib = "sd2lib" + names.ext;
    if (std::filesystem::exists(build_dir / sd2lib))
      args.push_back(sd2lib);

    args.push_back("-o");
    args.push_back(base);
    if (run_program(fortran_compiler, args, build_dir.string()) !=
        EXIT_SUCCESS) {
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASS] " << base << "\n";
  return EXIT_SUCCESS;
}

int build_test() {

  std::filesystem::path project_root = SDFAST_SOURCE_DIR;

  std::filesystem::path sdfast_bin = SDFAST_BIN_PATH;
  std::filesystem::path sdfast_examples = project_root / "examples";

  // cl.exe does not accept the gcc/gfortran flags the other examples use.
  // sphere.c is C17 and is compiled with /W4 /WX. Golden comparison still
  // runs on MSVC; the fbar numeric test invokes cl itself.
  if (compiler_is_msvc(SDFAST_C_COMPILER)) {
    test_program sphere = {"c", "sphere", "sphere.c"};
    int rc = test_sdfast_example(sphere, sdfast_examples, sdfast_bin);
    std::cout << "[SKIP] other example compile tests on MSVC "
                 "(gcc-style flags).\n";
    return rc;
  }

  std::vector<test_program> test_programs = {

      {"c", "fbar", "fbarc.c"},
      {"c", "sphere", "sphere.c"},

      {"fortran", "fbar", "fbar.f"},
      {"fortran", "orbit", "orbit.f"},
      {"fortran", "pend", "pend.f"},
      {"fortran", "quickret", "quickret.f"},
      {"fortran", "humpage", "humpage.f"},
      {"fortran", "slew", "slew.f"}

  };

  bool all_passed = true;
  for (const auto& t : test_programs) {
    if (test_sdfast_example(t, sdfast_examples, sdfast_bin) != EXIT_SUCCESS) {
      all_passed = false;
    }
  }

  return all_passed ? EXIT_SUCCESS : EXIT_FAILURE;
}
