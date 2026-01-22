cmake_minimum_required(VERSION 3.10)
project(InheritedCProject C)

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
