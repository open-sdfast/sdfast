#ifndef SDFAST_BUILDTEST_H
#define SDFAST_BUILDTEST_H

#include <cctype>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

struct test_program {
  std::string lang;
  std::string base;
  std::string driver;
};

// Names the generator actually writes. Windows (_WIN32) still uses the
// short suffixes and .for; Unix and macOS use _dyn/_info/_sar and .f.
// Tests follow that. canonical_output_name maps either spelling onto the
// Unix names stored under tests/golden.
struct SdOutputNames {
  std::string dyn;
  std::string sar;
  std::string info;
  std::string lib;
  std::string ext;
};

inline SdOutputNames sd_output_names(const std::string& lang,
                                     const std::string& base) {
#ifdef _WIN32
  const bool win = true;
#else
  const bool win = false;
#endif
  SdOutputNames n;
  n.ext = (lang == "fortran") ? (win ? ".for" : ".f")
                              : (lang == "cpp" ? ".cpp" : ".c");
  n.dyn = base + (win ? "_d" : "_dyn") + n.ext;
  n.sar = base + (win ? "_s" : "_sar") + n.ext;
  n.info = base + (win ? "_i" : "_info");
  n.lib = "sdlib" + n.ext;
  return n;
}

inline bool stem_ends_with(const std::string& stem, const std::string& suf) {
  return stem.size() >= suf.size() &&
         stem.compare(stem.size() - suf.size(), suf.size(), suf) == 0;
}

inline std::string canonical_output_name(std::string name) {
  if (name.size() >= 4 && name.compare(name.size() - 4, 4, ".for") == 0)
    name.replace(name.size() - 4, 4, ".f");
  auto dot = name.find_last_of('.');
  std::string stem = dot == std::string::npos ? name : name.substr(0, dot);
  std::string ext = dot == std::string::npos ? std::string() : name.substr(dot);
  if (stem_ends_with(stem, "_dyn") || stem_ends_with(stem, "_sar") ||
      stem_ends_with(stem, "_info")) {
    return stem + ext;
  }
  if (stem_ends_with(stem, "_d"))
    stem = stem.substr(0, stem.size() - 2) + "_dyn";
  else if (stem_ends_with(stem, "_s"))
    stem = stem.substr(0, stem.size() - 2) + "_sar";
  else if (stem_ends_with(stem, "_i"))
    stem = stem.substr(0, stem.size() - 2) + "_info";
  return stem + ext;
}

inline bool compiler_is_msvc(const std::string& compiler) {
#ifdef _MSC_VER
  (void)compiler;
  return true;
#else
  std::string name = std::filesystem::path(compiler).stem().string();
  for (char& c : name)
    c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
  return name == "cl";
#endif
}

// stdout_path, when non-empty, is a file that receives the child stdout.
// The path is opened by the parent, so it is relative to the parent's cwd.
int run_program(const std::string& executable,
                const std::vector<std::string>& args,
                const std::string& workdir = "",
                const std::string& stdout_path = "");

int build_test();
#endif // !SDFAST_BUILDTEST_H
