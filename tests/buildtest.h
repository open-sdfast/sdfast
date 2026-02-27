#ifndef SDFAST_BUILDTEST_H
#define SDFAST_BUILDTEST_H

#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <string>
#include <vector>

struct test_program {
  std::string lang;
  std::string base;
  std::string driver;
};

int build_test();
#endif // !SDFAST_BUILDTEST_H
