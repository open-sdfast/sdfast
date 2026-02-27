#include "buildtest.h"

#ifdef _WIN32
// WINDOWS
#include <windows.h>

int run_program(const std::string &executable,
                const std::vector<std::string> &args,
                const std::string &workdir = "") {
  std::ostringstream cmd_stream;
  cmd_stream << "\"" << executable << "\"";

  for (const auto &a : args)
    cmd_stream << " \"" << a << "\""; // safer quoting

  std::string cmd = cmd_stream.str();

  STARTUPINFOA si{};
  PROCESS_INFORMATION pi{};
  si.cb = sizeof(si);

  std::vector<char> buffer(cmd.begin(), cmd.end());
  buffer.push_back('\0');

  BOOL success = CreateProcessA(
      nullptr, buffer.data(), nullptr, nullptr, FALSE, 0, nullptr,
      workdir.empty() ? nullptr : workdir.c_str(), &si, &pi);

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
#include <sys/wait.h>
#include <unistd.h>

int run_program(const std::string &executable,
                const std::vector<std::string> &args,
                const std::string &workdir = "") {

  // Build full command string (for logging)
  std::ostringstream cmd_stream;
  cmd_stream << executable;
  for (const auto &a : args)
    cmd_stream << " " << a;

  std::string full_cmd = cmd_stream.str();

  std::vector<char *> argv;
  argv.push_back(const_cast<char *>(executable.c_str()));

  for (const auto &arg : args)
    argv.push_back(const_cast<char *>(arg.c_str()));

  argv.push_back(nullptr);

  pid_t pid = fork();

  if (pid < 0) {
    std::cerr << "[ERROR] fork() failed for command:\n" << full_cmd << "\n";
    perror("fork");
    _exit(EXIT_FAILURE);
  }

  if (pid == 0) {
    // Child

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

int test_sdfast_example(const test_program &t,
                        const std::filesystem::path &sdfast_examples,
                        const std::filesystem::path &sdfast_bin) {

  std::filesystem::path src_path = sdfast_examples / t.lang;

  std::string base = t.base;

  std::filesystem::path sd_file = src_path / (base + ".sd");

  std::filesystem::path build_dir =
      std::filesystem::path("generated/" + t.lang + "/" + base);
  std::filesystem::path driver_path = src_path / t.driver;

  std::string dyn =
      (t.lang == "fortran") ? base + "_dyn.f" : base + "_dyn." + t.lang;
  std::string sar =
      (t.lang == "fortran") ? base + "_sar.f" : base + "_sar." + t.lang;
  std::string sdlib = (t.lang == "fortran") ? "sdlib.f" : "sdlib." + t.lang;

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

  // Compile + Link

  std::vector<std::string> args;
  if (t.lang == "c") {
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

    if (base == "quickret") {

      std::vector<std::string> gen = {"-gl", "-psd2"};
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

    if (std::filesystem::exists(build_dir / "sd2lib.f"))
      args.push_back("sd2lib.f");

    args.push_back("-o");
    args.push_back(base);
    if (run_program(SDFAST_FORTRAN_COMPILER, args, build_dir.string()) !=
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
  for (const auto &t : test_programs) {
    if (test_sdfast_example(t, sdfast_examples, sdfast_bin) != EXIT_SUCCESS) {
      all_passed = false;
    }
  }

  return all_passed ? EXIT_SUCCESS : EXIT_FAILURE;
}
