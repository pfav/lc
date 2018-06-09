include_directories("${CMAKE_SOURCE_DIR}/test")

add_executable(lcTest test.cpp catch.hpp tests.cpp)

enable_testing()
add_test(lcTest lcTest)

