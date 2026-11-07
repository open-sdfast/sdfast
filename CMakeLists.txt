cmake_minimum_required(VERSION 3.21)
project(SDFAST C CXX)

# Single-config generators (Makefiles, Ninja, MinGW) otherwise leave
# CMAKE_BUILD_TYPE empty. Multi-config generators (Visual Studio, Xcode)
# set CMAKE_CONFIGURATION_TYPES and pick the config at build time, so
# they are left alone. An explicit -DCMAKE_BUILD_TYPE= still wins.
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING
      "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
               "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Collect sources for calc library
file(GLOB CALC_SOURCES calc/*.c)
add_library(calc STATIC ${CALC_SOURCES})
target_include_directories(calc PUBLIC calc)

# Collect sources for libs library
file(GLOB LIBS_SOURCES libs/*.c)
add_library(libs STATIC ${LIBS_SOURCES})
target_include_directories(libs PUBLIC libs)

# Collect sources for sdfast executable (including main.c and any other .c files)
file(GLOB SDFAST_SOURCES sdfast_main/*.c)
add_executable(sdfast ${SDFAST_SOURCES})
target_include_directories(sdfast PRIVATE sdfast_main)

# Generator sources are ISO C17. Extensions stay off so a GNU-mode
# `unix` macro cannot sneak back in.
set_target_properties(calc libs sdfast PROPERTIES
    C_STANDARD 17
    C_STANDARD_REQUIRED ON
    C_EXTENSIONS OFF
)

# Off by default so a local compiler that warns on something the CI
# compilers do not still produces a binary. CI passes
# -DSDFAST_WARNINGS_AS_ERRORS=ON. Generated sdlib/dyn/sar files are
# compiled by the test driver, not by these targets, and are not clean
# under -Wall (implicit declarations, unused parameters).
option(SDFAST_WARNINGS_AS_ERRORS
    "Treat warnings as errors for the generator and the test harness" OFF)

# -Wall -Wextra -Wpedantic is the strict set that is still practical:
# -Wconversion and -Weverything are too noisy for this code, and -Wpedantic
# did not add warnings beyond -Wall -Wextra once the sources were clean.
# MSVC /W4 matches that level. /Wall warns inside system headers.
# C4061 reports every unlisted enumerator even when a default arm handles
# it; those defaults are the intentional "not this joint or node" path.
function(sdfast_set_warnings target)
    if(MSVC)
        target_compile_options(${target} PRIVATE /W4 /wd4061)
        if(SDFAST_WARNINGS_AS_ERRORS)
            target_compile_options(${target} PRIVATE /WX)
        endif()
    else()
        target_compile_options(${target} PRIVATE -Wall -Wextra -Wpedantic)
        if(SDFAST_WARNINGS_AS_ERRORS)
            target_compile_options(${target} PRIVATE -Werror)
        endif()
    endif()
endfunction()

sdfast_set_warnings(calc)
sdfast_set_warnings(libs)
sdfast_set_warnings(sdfast)

# Link libraries - libm is only needed on Unix-like systems
if(UNIX)
    target_link_libraries(sdfast PRIVATE calc libs m)
else()
    target_link_libraries(sdfast PRIVATE calc libs)
endif()

# Detect compiler command paths
set(SDFAST_C_COMPILER "${CMAKE_C_COMPILER}")
set(SDFAST_CXX_COMPILER "${CMAKE_CXX_COMPILER}")

# Fortran is optional. check_language is the CMake 3.21 way to skip a
# missing compiler; enable_language(OPTIONAL) needs 3.25.
include(CheckLanguage)
check_language(Fortran)
if(CMAKE_Fortran_COMPILER)
    enable_language(Fortran)
    set(SDFAST_FORTRAN_COMPILER "${CMAKE_Fortran_COMPILER}")
else()
    set(SDFAST_FORTRAN_COMPILER "")
endif()

# Build and run tests
enable_testing()
add_subdirectory(tests)
sdfast_set_warnings(sdfast_tests)

target_compile_definitions(sdfast_tests
    PRIVATE 
    SDFAST_BIN_PATH="$<TARGET_FILE:sdfast>"
    SDFAST_SOURCE_DIR="${CMAKE_SOURCE_DIR}"
    SDFAST_C_COMPILER="${SDFAST_C_COMPILER}"
    SDFAST_CXX_COMPILER="${SDFAST_CXX_COMPILER}"
    SDFAST_FORTRAN_COMPILER="${SDFAST_FORTRAN_COMPILER}"
)
