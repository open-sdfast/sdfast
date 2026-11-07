#include "compare.hpp"

#include <algorithm>
#include <cctype>
#include <clocale>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

namespace {

bool is_ident(unsigned char c) {
  return std::isalnum(c) || c == '_';
}

bool is_exp(unsigned char c) {
  return c == 'e' || c == 'E' || c == 'd' || c == 'D';
}

bool is_space(unsigned char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' ||
         c == '\v';
}

// Parse a numeric literal at i. Accepts C and Fortran forms:
//   1  1.  1.5  .5  -9.8  0D0  -9.8D0  1.25e-10  1.D0
// A sign is part of the literal only when it is not glued to an
// identifier or a previous digit. Returns false without consuming.
bool parse_number(const std::string &s, size_t i, size_t &end, double &value) {
  if (i > 0 && is_ident(static_cast<unsigned char>(s[i - 1])))
    return false;

  size_t j = i;
  if (j < s.size() && (s[j] == '+' || s[j] == '-')) {
    if (j + 1 >= s.size())
      return false;
    ++j;
  }

  bool any = false;
  while (j < s.size() &&
         std::isdigit(static_cast<unsigned char>(s[j]))) {
    any = true;
    ++j;
  }

  if (j < s.size() && s[j] == '.') {
    size_t k = j + 1;
    bool frac = false;
    while (k < s.size() &&
           std::isdigit(static_cast<unsigned char>(s[k]))) {
      frac = true;
      ++k;
    }
    if (frac) {
      any = true;
      j = k;
    } else if (any) {
      unsigned char next = (j + 1 < s.size())
                               ? static_cast<unsigned char>(s[j + 1])
                               : 0;
      // "1.and." is the integer 1 followed by the Fortran operator.
      // "1.D0" and "1." are numeric literals.
      if (next == 0 || !std::isalpha(next) || is_exp(next))
        ++j;
    }
  }

  if (!any)
    return false;

  if (j < s.size() && is_exp(static_cast<unsigned char>(s[j]))) {
    size_t k = j + 1;
    if (k < s.size() && (s[k] == '+' || s[k] == '-'))
      ++k;
    size_t exp_digits = k;
    while (k < s.size() &&
           std::isdigit(static_cast<unsigned char>(s[k])))
      ++k;
    if (k > exp_digits)
      j = k;
  }

  std::string token = s.substr(i, j - i);
  for (char &c : token) {
    if (c == 'd' || c == 'D')
      c = 'e';
  }

  char *stop = nullptr;
  value = std::strtod(token.c_str(), &stop);
  if (stop != token.c_str() + token.size())
    return false;

  end = j;
  return true;
}

struct Tok {
  bool is_num = false;
  std::string text;
  double value = 0;
  int line = 1;
};

std::vector<Tok> tokenize(const std::string &text) {
  std::vector<Tok> out;
  int line = 1;
  size_t i = 0;
  while (i < text.size()) {
    unsigned char c = static_cast<unsigned char>(text[i]);
    if (c == '\n') {
      ++line;
      ++i;
      continue;
    }
    if (is_space(c)) {
      ++i;
      continue;
    }
    size_t end = i;
    double value = 0;
    if (parse_number(text, i, end, value)) {
      Tok t;
      t.is_num = true;
      t.text = text.substr(i, end - i);
      t.value = value;
      t.line = line;
      out.push_back(std::move(t));
      i = end;
      continue;
    }
    // Identifiers stay whole (`s1`, `sdstate`). Every other character is
    // its own token so spaces around operators do not matter.
    size_t j = i + 1;
    if (is_ident(c)) {
      j = i;
      while (j < text.size() &&
             is_ident(static_cast<unsigned char>(text[j])))
        ++j;
    }
    Tok t;
    t.text = text.substr(i, j - i);
    t.line = line;
    out.push_back(std::move(t));
    i = j;
  }
  return out;
}

std::string snippet(const std::vector<Tok> &toks, size_t at) {
  std::ostringstream os;
  size_t begin = at > 3 ? at - 3 : 0;
  size_t end = std::min(toks.size(), at + 4);
  for (size_t i = begin; i < end; ++i) {
    if (i)
      os << ' ';
    if (i == at)
      os << ">>>";
    os << toks[i].text;
    if (i == at)
      os << "<<<";
  }
  return os.str();
}

void replace_all(std::string &text, const std::string &from,
                 const std::string &to) {
  if (from.empty())
    return;
  size_t pos = 0;
  while ((pos = text.find(from, pos)) != std::string::npos) {
    text.replace(pos, from.size(), to);
    pos += to.size();
  }
}

// "Generated <date time> by SD/FAST" -> "Generated TIMESTAMP by SD/FAST".
void strip_generated_dates(std::string &text) {
  const std::string key = "Generated";
  const std::string tail = "by SD/FAST";
  size_t pos = 0;
  while ((pos = text.find(key, pos)) != std::string::npos) {
    size_t by = text.find(tail, pos + key.size());
    size_t nl = text.find('\n', pos);
    if (by != std::string::npos && (nl == std::string::npos || by < nl) &&
        by > pos + key.size()) {
      text.replace(pos + key.size(), by - (pos + key.size()), " TIMESTAMP ");
      pos += key.size();
    } else {
      pos += key.size();
    }
  }
}

// sdgentime stores HHMMSS. It changes every second and is not physics.
void strip_gentime(std::string &text) {
  size_t pos = 0;
  while (pos < text.size()) {
    size_t found = std::string::npos;
    for (size_t i = pos; i + 5 <= text.size(); ++i) {
      char c0 = text[i];
      if ((c0 == 'g' || c0 == 'G') &&
          (text[i + 1] == 'e' || text[i + 1] == 'E') &&
          (text[i + 2] == 'n' || text[i + 2] == 'N') &&
          (text[i + 3] == 't' || text[i + 3] == 'T') &&
          (text[i + 4] == 'm' || text[i + 4] == 'M')) {
        found = i;
        break;
      }
    }
    if (found == std::string::npos)
      return;
    size_t j = found + 5;
    while (j < text.size() &&
           (text[j] == ' ' || text[j] == '\t'))
      ++j;
    if (j >= text.size() || text[j] != '=') {
      pos = found + 5;
      continue;
    }
    ++j;
    while (j < text.size() &&
           (text[j] == ' ' || text[j] == '\t'))
      ++j;
    size_t digits = j;
    while (j < text.size() &&
           std::isdigit(static_cast<unsigned char>(text[j])))
      ++j;
    if (j == digits) {
      pos = found + 5;
      continue;
    }
    text.replace(digits, j - digits, "TIMESTAMP");
    pos = digits + 9;
  }
}

// sdanal.c emits sdgentime(&i); if (i != HHMMSS) so the simplified
// analysis file can tell it was built in the same second as the
// dynamics file. That integer changes every run. The serial check
// (sdserialno / i != 10001) is stable and is left alone.
void strip_gentime_checks(std::string &text) {
  size_t pos = 0;
  while (pos < text.size()) {
    size_t found = std::string::npos;
    for (size_t i = pos; i + 7 <= text.size(); ++i) {
      bool match = true;
      const char *word = "gentime";
      for (int k = 0; k < 7; ++k) {
        char c = text[i + k];
        if (c >= 'A' && c <= 'Z')
          c = static_cast<char>(c - 'A' + 'a');
        if (c != word[k]) {
          match = false;
          break;
        }
      }
      if (match) {
        found = i;
        break;
      }
    }
    if (found == std::string::npos)
      return;
    size_t after = found + 7;
    size_t window = std::min(text.size(), after + 160);
    size_t op = std::string::npos;
    size_t op_len = 0;
    for (size_t i = after; i + 2 <= window; ++i) {
      if (text[i] == '!' && text[i + 1] == '=') {
        op = i;
        op_len = 2;
        break;
      }
      if (i + 4 <= window && text[i] == '.' &&
          (text[i + 1] == 'N' || text[i + 1] == 'n') &&
          (text[i + 2] == 'E' || text[i + 2] == 'e') && text[i + 3] == '.') {
        op = i;
        op_len = 4;
        break;
      }
    }
    if (op == std::string::npos) {
      pos = after;
      continue;
    }
    size_t j = op + op_len;
    while (j < text.size() && (text[j] == ' ' || text[j] == '\t'))
      ++j;
    size_t digits = j;
    while (j < text.size() &&
           std::isdigit(static_cast<unsigned char>(text[j])))
      ++j;
    if (j == digits) {
      pos = after;
      continue;
    }
    text.replace(digits, j - digits, "TIMESTAMP");
    pos = digits + 9;
  }
}

// END_COMPSUB prints "Used N seconds CPU time" and "N additional bytes
// of memory" from CPU_SECONDS() and sbrk(). The byte count is whichever
// malloc happened to grow the break during that routine, so the same run
// can report 0 or 135168, and two machines swap which routine gets the
// bump. The add/multiply counts that follow are the real lock and stay.
void replace_number_before(std::string &text, const std::string &tail) {
  size_t pos = 0;
  while ((pos = text.find(tail, pos)) != std::string::npos) {
    size_t j = pos;
    while (j > 0 && (text[j - 1] == ' ' || text[j - 1] == '\t'))
      --j;
    size_t end = j;
    while (j > 0 &&
           (std::isdigit(static_cast<unsigned char>(text[j - 1])) ||
            text[j - 1] == '.'))
      --j;
    if (j < end)
      text.replace(j, end - j, "TIMESTAMP");
    pos += tail.size();
  }
}

void strip_cpu_trailer(std::string &text) {
  const std::string key = "CPU seconds used:";
  size_t pos = 0;
  while ((pos = text.find(key, pos)) != std::string::npos) {
    size_t nl = text.find('\n', pos);
    size_t end = nl == std::string::npos ? text.size() : nl;
    text.replace(pos, end - pos,
                 "CPU seconds used: TIMESTAMP  Memory used: TIMESTAMP bytes.");
    pos += key.size();
  }
}

} // namespace

bool numbers_close(double actual, double golden, double rel_tol,
                   double abs_tol) {
  if (std::isnan(actual) || std::isnan(golden))
    return std::isnan(actual) && std::isnan(golden);
  double diff = std::fabs(actual - golden);
  if (diff <= abs_tol)
    return true;
  double scale = std::max(std::fabs(actual), std::fabs(golden));
  return diff <= rel_tol * scale;
}

std::string basename_any(const std::string &path) {
  auto slash = path.find_last_of("/\\");
  if (slash == std::string::npos || slash + 1 >= path.size())
    return path;
  return path.substr(slash + 1);
}

std::string with_slashes(std::string path, char slash) {
  for (char &c : path) {
    if (c == '/' || c == '\\')
      c = slash;
  }
  return path;
}

std::string normalize_generated(std::string text,
                                const std::vector<std::string> &abs_paths) {
  std::string nl;
  nl.reserve(text.size());
  for (size_t i = 0; i < text.size(); ++i) {
    if (text[i] == '\r') {
      if (i + 1 < text.size() && text[i + 1] == '\n')
        ++i;
      nl.push_back('\n');
    } else {
      nl.push_back(text[i]);
    }
  }
  text.swap(nl);

  // Replace longer paths first so a directory prefix cannot shadow a file.
  std::vector<std::string> paths = abs_paths;
  std::sort(paths.begin(), paths.end(),
            [](const std::string &a, const std::string &b) {
              return a.size() > b.size();
            });
  for (const auto &path : paths) {
    if (path.empty())
      continue;
    std::string base = basename_any(path);
    if (base.empty())
      base = "INPUT";
    replace_all(text, path, base);
    replace_all(text, with_slashes(path, '/'), base);
    replace_all(text, with_slashes(path, '\\'), base);
  }

  strip_generated_dates(text);
  strip_gentime(text);
  strip_gentime_checks(text);
  strip_cpu_trailer(text);
  replace_number_before(text, " seconds CPU time");
  replace_number_before(text, " additional bytes of memory");
  return text;
}

bool compare_texts(const std::string &actual, const std::string &golden,
                   const CompareOptions &opt, std::string &report) {
  // strtod honors LC_NUMERIC. Generated text always uses '.'.
  std::string previous = "C";
  if (const char *cur = std::setlocale(LC_NUMERIC, nullptr))
    previous = cur;
  std::setlocale(LC_NUMERIC, "C");

  auto actual_toks = tokenize(actual);
  auto golden_toks = tokenize(golden);

  std::setlocale(LC_NUMERIC, previous.c_str());

  std::ostringstream out;
  int reports = 0;
  bool ok = true;
  size_t n = std::min(actual_toks.size(), golden_toks.size());
  for (size_t i = 0; i < n; ++i) {
    const Tok &a = actual_toks[i];
    const Tok &g = golden_toks[i];
    bool match = false;
    if (a.is_num && g.is_num)
      match = numbers_close(a.value, g.value, opt.rel_tol, opt.abs_tol);
    else if (!a.is_num && !g.is_num)
      match = a.text == g.text;
    if (match)
      continue;
    ok = false;
    if (reports < opt.max_reports) {
      out << "token " << (i + 1) << " golden line " << g.line << " actual line "
          << a.line << "\n";
      out << "  golden: " << g.text << "\n";
      out << "  actual: " << a.text << "\n";
      if (a.is_num && g.is_num) {
        double diff = std::fabs(a.value - g.value);
        double scale = std::max(std::fabs(a.value), std::fabs(g.value));
        out << "  abs " << diff << " rel "
            << (scale == 0 ? 0 : diff / scale) << "\n";
      }
      out << "  golden context: " << snippet(golden_toks, i) << "\n";
      out << "  actual context: " << snippet(actual_toks, i) << "\n";
      ++reports;
    }
  }
  if (actual_toks.size() != golden_toks.size()) {
    ok = false;
    if (reports < opt.max_reports) {
      out << "token count golden " << golden_toks.size() << " actual "
          << actual_toks.size() << "\n";
      ++reports;
    }
  }
  if (!ok && reports >= opt.max_reports)
    out << "(further mismatches omitted)\n";
  report = out.str();
  return ok;
}

std::vector<double> extract_numbers(const std::string &text) {
  std::string previous = "C";
  if (const char *cur = std::setlocale(LC_NUMERIC, nullptr))
    previous = cur;
  std::setlocale(LC_NUMERIC, "C");
  std::vector<double> values;
  for (const auto &t : tokenize(text)) {
    if (t.is_num)
      values.push_back(t.value);
  }
  std::setlocale(LC_NUMERIC, previous.c_str());
  return values;
}

bool compare_number_lists(const std::vector<double> &actual,
                          const std::vector<double> &golden,
                          const CompareOptions &opt, std::string &report) {
  std::ostringstream out;
  bool ok = true;
  int reports = 0;
  size_t n = std::min(actual.size(), golden.size());
  for (size_t i = 0; i < n; ++i) {
    if (numbers_close(actual[i], golden[i], opt.rel_tol, opt.abs_tol))
      continue;
    ok = false;
    if (reports < opt.max_reports) {
      double diff = std::fabs(actual[i] - golden[i]);
      double scale = std::max(std::fabs(actual[i]), std::fabs(golden[i]));
      out << "value " << (i + 1) << " golden " << golden[i] << " actual "
          << actual[i] << " abs " << diff << " rel "
          << (scale == 0 ? 0 : diff / scale) << "\n";
      ++reports;
    }
  }
  if (actual.size() != golden.size()) {
    ok = false;
    out << "value count golden " << golden.size() << " actual " << actual.size()
        << "\n";
  }
  if (!ok && reports >= opt.max_reports)
    out << "(further mismatches omitted)\n";
  report = out.str();
  return ok;
}

namespace {

int expect_match(const std::string &name, const std::string &actual,
                 const std::string &golden, bool should_pass,
                 const CompareOptions &opt = {}) {
  std::string report;
  bool pass = compare_texts(actual, golden, opt, report);
  if (pass == should_pass)
    return 0;
  std::cerr << "[FAIL] selfcheck " << name << " expected "
            << (should_pass ? "pass" : "fail") << "\n"
            << report;
  return 1;
}

} // namespace

int comparator_self_check() {
  int rc = 0;
  rc |= expect_match("identical", "a = 1.25;\n", "a = 1.25;\n", true);
  rc |= expect_match("last digit", "v = 1.23456789012345;\n",
                     "v = 1.23456789012346;\n", true);
  rc |= expect_match("rel 1e-6", "v = 1.00000000000000;\n",
                     "v = 1.00000100000000;\n", false);
  rc |= expect_match("token", "call sdstate(t,q,u)\n", "call sdstatx(t,q,u)\n",
                     false);
  rc |= expect_match("D vs E", "DATA X/-9.8D0/\n", "DATA X/-9.8E0/\n", true);
  rc |= expect_match("0D0 vs 0", "DATA X/0D0/\n", "DATA X/0.0/\n", true);
  rc |= expect_match("whitespace", "a = 1.0;\n", "a=1.0;\n", true);
  rc |= expect_match("near zero pass", "x=0;\n", "x=1e-15;\n", true);
  rc |= expect_match("near zero fail", "x=0;\n", "x=1e-6;\n", false);
  std::string gentime_a = "sdgentime(&i);\nif (i != 111213) {\n";
  std::string gentime_b = "sdgentime(&i);\nif (i != 235959) {\n";
  rc |= expect_match("gentime check", normalize_generated(gentime_a, {}),
                     normalize_generated(gentime_b, {}), true);
  std::string serial_a = "sdserialno(&i);\nif (i != 10001) {\n";
  std::string serial_b = "sdserialno(&i);\nif (i != 10002) {\n";
  rc |= expect_match("serial stays", normalize_generated(serial_a, {}),
                     normalize_generated(serial_b, {}), false);
  std::string mem_a =
      "Used 0.00 seconds CPU time,\n 0 additional bytes of memory.\n"
      " Equations contain   48 adds/subtracts/negates\n";
  std::string mem_b =
      "Used 0.02 seconds CPU time,\n 135168 additional bytes of memory.\n"
      " Equations contain   48 adds/subtracts/negates\n";
  rc |= expect_match("sbrk footer", normalize_generated(mem_a, {}),
                     normalize_generated(mem_b, {}), true);
  std::string ops_b =
      "Used 0.02 seconds CPU time,\n 135168 additional bytes of memory.\n"
      " Equations contain   49 adds/subtracts/negates\n";
  rc |= expect_match("op count stays", normalize_generated(mem_a, {}),
                     normalize_generated(ops_b, {}), false);

  std::string stamp1 =
      "Generated  7-Nov-2026 19:44:06 by SD/FAST, Kane's formulation\n"
      "ROADMAP (/tmp/one/fbar.sd)\n"
      "    *gentm = 194406;\n"
      "Done. CPU seconds used: 0.01  Memory used: 135168 bytes.\n"
      "gk = -9.8;\n";
  std::string stamp2 =
      "Generated  1-Jan-1998 00:00:01 by SD/FAST, Kane's formulation\n"
      "ROADMAP (C:\\work\\models\\fbar.sd)\n"
      "    *gentm = 1;\n"
      "Done. CPU seconds used: 12.50  Memory used: 999 bytes.\n"
      "gk = -9.8;\n";
  std::string n1 = normalize_generated(stamp1, {"/tmp/one/fbar.sd"});
  std::string n2 = normalize_generated(stamp2, {"C:\\work\\models\\fbar.sd"});
  rc |= expect_match("stamps and paths", n1, n2, true);

  if (n1.find("194406") != std::string::npos ||
      n1.find("19:44:06") != std::string::npos ||
      n1.find("/tmp/one/") != std::string::npos) {
    std::cerr << "[FAIL] selfcheck normalize left a volatile fragment\n"
              << n1;
    rc |= 1;
  }

  if (rc == 0)
    std::cout << "[PASS] comparator self-check\n";
  return rc == 0 ? 0 : 1;
}

int compare_path_files(const std::filesystem::path &actual,
                       const std::filesystem::path &golden) {
  std::ifstream ain(actual, std::ios::binary);
  std::ifstream gin(golden, std::ios::binary);
  if (!ain || !gin) {
    std::cerr << "[FAIL] cannot read " << actual << " or " << golden << "\n";
    return 1;
  }
  std::ostringstream abuf, gbuf;
  abuf << ain.rdbuf();
  gbuf << gin.rdbuf();
  std::string report;
  if (compare_texts(abuf.str(), gbuf.str(), {}, report)) {
    std::cout << "[PASS] " << actual << "\n";
    return 0;
  }
  std::cerr << "[FAIL] " << actual << " vs " << golden << "\n" << report;
  return 1;
}
