add_executable(sdfast_tests
    buildtest.cpp
    compare.cpp
    goldentest.cpp
    testrunner.cpp
)

target_compile_features(sdfast_tests PRIVATE cxx_std_17)

target_link_libraries(sdfast_tests
    PRIVATE
        calc
        libs
)

# Each piece is its own ctest so a Fortran skip or an MSVC compile
# failure does not hide a golden mismatch.
add_test(NAME sdfast_selfcheck COMMAND sdfast_tests selfcheck)
add_test(NAME sdfast_golden COMMAND sdfast_tests golden)
add_test(NAME sdfast_numeric COMMAND sdfast_tests numeric)
add_test(NAME sdfast_build COMMAND sdfast_tests build)

# Rewrite tests/golden from the sdfast just built. Linux/glibc is the
# reference checked in; other platforms compare with tolerance.
add_custom_target(update-golden
    COMMAND ${CMAKE_COMMAND} -E env SDFAST_UPDATE_GOLDEN=1
            $<TARGET_FILE:sdfast_tests> update
    DEPENDS sdfast sdfast_tests
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Regenerate tests/golden (timestamps and input paths stripped)"
)
