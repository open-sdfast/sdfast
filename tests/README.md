# Regression tests

These tests lock what `sdfast` emits and what the four-bar example does,
without changing the generator.

CMake 3.21 or newer is required. The generator (`calc/`, `libs/`, `sdfast`) is built as ISO C17.

```
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
ctest --test-dir build --output-on-failure
```

`SDFAST_WARNINGS_AS_ERRORS` defaults on, so that configure line builds the generator and `sdfast_tests` with `-Werror` or `/WX`. Generated `sdlib` and dynamics files are compiled by the tests and are not under that flag. Pass `-DSDFAST_WARNINGS_AS_ERRORS=OFF` to turn it off.

`ctest` runs four tests:

| Test | What it checks |
| --- | --- |
| `sdfast_selfcheck` | The comparator itself (last-digit pass, 1e-6 fail, token fail) |
| `sdfast_golden` | C (`-lc`) and Fortran (`-lfortran`) for pend, fbar, and humpage, plus Order(N) fbar |
| `sdfast_numeric` | Build and run fbar, C and Fortran, and compare the trajectory |
| `sdfast_build` | The older compile-the-examples driver |

The example-compile test still builds every driver under `examples/`. Sphere is not executed: its driver reads stdin. The golden and numeric tests do not read those `.sd` files.

## Golden comparison

Each baseline directory holds the `.sd` that produced it (`fbar.sd`, `pend.sd`, or `humpage.sd`). `sdfast` is run on that copy, not on `examples/`, so a later edit of the examples does not move the baseline. C uses `-lc`. Fortran uses `-lfortran`, the generator's default language. The files it writes (`_dyn`, `_sar`, `_info`, or the Windows `_d` / `_s` / `_i` names) are compared with `tests/golden/<lang>/<kane|ordern>/<model>/`. `sdlib` is not stored per model. Across these models the library is byte-identical after timestamp stripping; Kane versus Order(N) differs only by the banner word `Kane's formulation` versus `Order(N) formulation`. One copy per language lives in `tests/golden/lib/` (`sdlib.c`, `sdlib.f`), taken from Kane fbar (`-lc` / `-lfortran` `-ge` on that directory's `fbar.sd`). Every model's generated library is compared to that copy after the banner is folded to `FORMULATION`. Windows `.for` is mapped to `sdlib.f` before the lookup. The generator's suffixes are unchanged.

Models: Kane pend, fbar, and humpage, plus Order(N) fbar. The fbar numeric reference is `tests/golden/numeric/fbar_c_stdout.txt`.

Before compare, both sides are normalized:

- newlines become `\n`
- the absolute input path becomes the file name
- `Generated <date time> by SD/FAST` becomes `Generated TIMESTAMP by SD/FAST`
- the `sdgentime` value, and the `if (i != HHMMSS)` / `IF (I .NE. HHMMSS)` checks that compare the analysis file to that clock, become `TIMESTAMP`. The serial check (`sdserialno`, currently `10001`) is not a clock and is compared exactly
- the trailer `CPU seconds used` / `Memory used`, and each routine footer `Used N seconds CPU time` / `N additional bytes of memory`. Those byte counts are a host statistic (`BYTES_USED`, now always 0; it used to be an `sbrk` delta). The add/multiply/divide/assignment counts in that footer stay exact

The comparator then tokenizes the text. Whitespace only separates tokens, so a line wrap caused by a slightly wider literal does not fail by itself. Every other token must match exactly. Numeric literals (`1.5`, `.5`, `0D0`, `-9.8D0`, `1.25e-10`) compare by value:

- relative tolerance `1e-10`
- absolute floor `1e-12`

Fifteen-digit `printf` noise is about `1e-15` relative. `1e-10` is above glibc / macOS libc / MSVC CRT disagreement and below a `1e-6` relative edit, which fails. `1.25D0` and `1.25E0` are the same value. A renamed identifier fails.

The test forces `LC_ALL=C` so a comma decimal locale cannot rewrite constants.

## Numeric fbar run

`examples/c/fbarc.c` and `examples/fortran/fbar.f` are built against a fresh generate of the stored `tests/golden/c/kane/fbar/fbar.sd` and run with no stdin. Fortran is skipped when no Fortran compiler was found (MSVC). The C stdout is stored at `tests/golden/numeric/fbar_c_stdout.txt`. C, Fortran, and that reference are compared as sequences of numbers:

- relative tolerance `1e-3`
- absolute floor `1e-4`

The drivers only print about five or six digits (`%g` and `f12.5`). On Linux the C and Fortran listings differ by at most about `5e-6` absolute because of that rounding. This check is not a 15-digit lock; the golden source test is.

## Regenerating the baselines

Do this on Linux with glibc and commit the result. It reruns `sdfast` on the `.sd` files already stored under `tests/golden/` and rewrites the outputs. It does not copy a new input from `examples/`. Other platforms should match within the tolerances, not rewrite the files.

```
cmake --build build --target update-golden
```

That runs `SDFAST_UPDATE_GOLDEN=1 sdfast_tests update`.

## MSVC

The example-compile test is skipped there because it passes gcc flags (`-std=gnu89`, `-lm`). Golden generation still runs, including Fortran source, which does not need a Fortran compiler. The fbar C trajectory is compiled with `cl /TC`. Generated C still calls library routines without prototypes; if `cl` rejects that, the numeric test fails and the golden test is still a separate result.
