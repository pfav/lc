cmake_minimum_required(VERSION 3.5.0)

project(lc VERSION 0.0.0 LANGUAGES C CXX)

set(CMAKE_VERBOSE_MAKEFILE OFF)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)


set(CMAKE_CXX_FLAGS "-Wall -Wextra -Werror -pedantic")
include_directories(${PROJECT_SOURCE_DIR})

add_subdirectory(tests)

add_executable(lc main.cpp lc.h)
target_link_libraries(lc -lreadline)
