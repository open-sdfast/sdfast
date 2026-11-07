#ifndef SDFAST_COMPARE_HPP
#define SDFAST_COMPARE_HPP

#include <filesystem>
#include <string>
#include <vector>

// Source literals are printed with 15 significant digits (%.15g / D-format).
// One unit in the last printed digit is about 1e-15 relative. 1e-10 is far
// above glibc/macOS/MSVC printf disagreement and far below a 1e-6 relative
// edit, which is treated as a real change. The absolute floor covers values
// near zero, where a relative test is unstable.
inline constexpr double kSourceRelTol = 1e-10;
inline constexpr double kSourceAbsTol = 1e-12;

// fbar's drivers print with %g (~6 significant digits) and Fortran f12.5
// (5 digits after the decimal). On one Linux machine the C and Fortran
// trajectories differed by at most ~5e-6 absolute, from that rounding.
// 1e-4 absolute / 1e-3 relative accepts print rounding and modest libm
// drift, and rejects a trajectory that moves by a tenth of a percent.
inline constexpr double kTrajectoryRelTol = 1e-3;
inline constexpr double kTrajectoryAbsTol = 1e-4;

struct CompareOptions {
  double rel_tol = kSourceRelTol;
  double abs_tol = kSourceAbsTol;
  int max_reports = 12;
};

// Replace generation timestamps, the sdgentime clock, the CPU/memory
// trailer, and any of abs_paths with a stable placeholder. Newlines
// become '\n'. This is applied to both sides before token compare, and
// is what gets written into tests/golden.
std::string normalize_generated(std::string text,
                                const std::vector<std::string>& abs_paths);

// Whitespace separates tokens and is not compared, so a CRT that prints
// a slightly wider literal (and therefore wraps a line differently) does
// not fail unless it adds or drops a real token. Non-numeric tokens must
// match exactly. Numeric literals compare by value.
bool compare_texts(const std::string& actual, const std::string& golden,
                   const CompareOptions& opt, std::string& report);

bool numbers_close(double actual, double golden, double rel_tol,
                   double abs_tol);

// Extract numeric literals in order. Used for fbar stdout, where the C
// and Fortran drivers format the same trajectory differently.
std::vector<double> extract_numbers(const std::string& text);

bool compare_number_lists(const std::vector<double>& actual,
                          const std::vector<double>& golden,
                          const CompareOptions& opt, std::string& report);

int comparator_self_check();

// Compare two already-normalized files. Exit status 0 on match.
int compare_path_files(const std::filesystem::path& actual,
                       const std::filesystem::path& golden);

#endif
