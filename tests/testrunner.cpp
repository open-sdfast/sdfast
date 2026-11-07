#include "buildtest.h"
#include "compare.hpp"
#include "regress.hpp"

#include <clocale>
#include <cstdlib>
#include <iostream>
#include <string>

namespace {

bool update_requested() {
  const char *env = std::getenv("SDFAST_UPDATE_GOLDEN");
  return env != nullptr && env[0] != '\0' && env[0] != '0';
}

void force_c_locale() {
  // Generated literals and fbar's printf both use sprintf, which follows
  // LC_NUMERIC. Pin the C locale so a developer machine with a comma
  // decimal does not rewrite the physics constants.
#ifndef _WIN32
  setenv("LC_ALL", "C", 1);
#else
  // _putenv keeps the pointer; it must outlive the process.
  static char lc_all[] = "LC_ALL=C";
  _putenv(lc_all);
#endif
  std::setlocale(LC_ALL, "C");
}

void usage() {
  std::cerr
      << "usage: sdfast_tests [selfcheck|golden|numeric|build|update|all]\n"
      << "       sdfast_tests compare <actual> <golden>\n"
      << "update rewrites tests/golden (also set SDFAST_UPDATE_GOLDEN=1).\n";
}

} // namespace

int main(int argc, char **argv) {
  force_c_locale();

  std::string cmd = argc > 1 ? argv[1] : "all";
  bool update = update_requested() || cmd == "update";

  if (cmd == "compare") {
    if (argc != 4) {
      usage();
      return 2;
    }
    return compare_path_files(argv[2], argv[3]);
  }
  if (cmd == "selfcheck")
    return comparator_self_check();
  if (cmd == "golden")
    return golden_test(update);
  if (cmd == "numeric")
    return numeric_test(update);
  if (cmd == "build")
    return build_test();
  if (cmd == "update") {
    int rc = comparator_self_check();
    rc |= golden_test(true);
    rc |= numeric_test(true);
    return rc == 0 ? 0 : 1;
  }
  if (cmd == "all" || cmd == "") {
    int rc = comparator_self_check();
    rc |= golden_test(update);
    rc |= numeric_test(update);
    rc |= build_test();
    return rc == 0 ? 0 : 1;
  }
  usage();
  return 2;
}
