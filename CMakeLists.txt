cmake_minimum_required(VERSION 3.10)
project(SDFAST C CXX Fortran)

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

# Link libraries - libm is only needed on Unix-like systems
if(UNIX)
    target_link_libraries(sdfast PRIVATE calc libs m)
else()
    target_link_libraries(sdfast PRIVATE calc libs)
endif()

# Detect compiler command paths
set(SDFAST_C_COMPILER "${CMAKE_C_COMPILER}")
set(SDFAST_CXX_COMPILER "${CMAKE_CXX_COMPILER}")
if(CMAKE_Fortran_COMPILER)
    set(SDFAST_FORTRAN_COMPILER "${CMAKE_Fortran_COMPILER}")
endif()

enable_testing()
add_subdirectory(tests)
target_compile_definitions(sdfast_tests
    PRIVATE 
    SDFAST_BIN_PATH="$<TARGET_FILE:sdfast>"
    SDFAST_SOURCE_DIR="${CMAKE_SOURCE_DIR}")

target_compile_definitions(sdfast_tests PRIVATE
    SDFAST_C_COMPILER="${SDFAST_C_COMPILER}"
    SDFAST_CXX_COMPILER="${SDFAST_CXX_COMPILER}"
    SDFAST_FORTRAN_COMPILER="${SDFAST_FORTRAN_COMPILER}"
)

