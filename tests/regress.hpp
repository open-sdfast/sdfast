#ifndef SDFAST_REGRESS_HPP
#define SDFAST_REGRESS_HPP

// Golden source comparison and the fbar numeric trajectory.
// When SDFAST_UPDATE_GOLDEN is set (not "0"), rewrite tests/golden
// instead of comparing. The CMake target update-golden sets that.
int golden_test(bool update);
int numeric_test(bool update);

#endif
