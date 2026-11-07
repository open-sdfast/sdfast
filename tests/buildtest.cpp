#include "buildtest.h"

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

  // cl.exe does not accept the gcc/gfortran flags this driver uses.
  // Golden comparison still runs on MSVC; the fbar numeric test invokes cl
  // with its own command line.
  if (compiler_is_msvc(SDFAST_C_COMPILER)) {
    std::cout << "[SKIP] example compile test on MSVC "
                 "(gcc-style flags). Golden and fbar numeric tests still run.\n";
    return EXIT_SUCCESS;
  }

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
  for (const auto& t : test_programs) {
    if (test_sdfast_example(t, sdfast_examples, sdfast_bin) != EXIT_SUCCESS) {
      all_passed = false;
    }
  }

  return all_passed ? EXIT_SUCCESS : EXIT_FAILURE;
}
