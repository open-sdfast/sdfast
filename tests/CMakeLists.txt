add_executable(sdfast_tests
    buildtest.cpp
    testrunner.cpp
)

target_link_libraries(sdfast_tests
    PRIVATE
        calc
        libs
)

add_test(NAME SDFAST_RUN COMMAND sdfast_tests)
