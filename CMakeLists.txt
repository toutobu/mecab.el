cmake_minimum_required(VERSION 3.10.0)

project(mecabel VERSION 0.1)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(BOOST_VERSION 1.73.0)

set(mecabel_include_dirs
  ${CMAKE_CURRENT_SOURCE_DIR}/include)

########################################################################
#
# Boost

configure_file(cmake/CMakeLists.boost.txt.in
  boost-download/CMakeLists.txt @ONLY)

execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/boost-download)
execute_process(COMMAND ${CMAKE_COMMAND} --build .
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/boost-download)

set(BOOST_ROOT
  ${CMAKE_CURRENT_BINARY_DIR}/boost-download/boost-${BOOST_VERSION}/src/Boost)

find_package(Boost ${BOOST_VERSION} REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

########################################################################
#
# MeCab

exec_program(mecab-config ARGS --libs OUTPUT_VARIABLE mecab_flags)
set(mecab_flags "${mecab_flags}" CACHE STRING "MeCab Flags")

########################################################################
#
# googletest

configure_file(cmake/CMakeLists.gtest.txt.in googletest-download/CMakeLists.txt)

execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googletest-download)
execute_process(COMMAND ${CMAKE_COMMAND} --build .
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googletest-download )

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

add_subdirectory(
  ${CMAKE_CURRENT_BINARY_DIR}/googletest-src
  ${CMAKE_CURRENT_BINARY_DIR}/googletest-build
  EXCLUDE_FROM_ALL)

add_executable(test_mecabel test/test_mecabel.cc
  ${mecabel_include_dirs}/tagger.h)
target_link_libraries(test_mecabel gtest_main)
target_include_directories(test_mecabel PUBLIC
  ${mecabel_include_dirs})
add_test(NAME test_mecabel COMMAND test_mecabel)

########################################################################
#
# Define the mecabel-impl library.

add_library(mecabel_impl SHARED src/mecabel_impl.cc
  ${mecabel_include_dirs}/tagger.h)

target_include_directories(mecabel_impl PUBLIC
  ${mecabel_include_dirs})

set_target_properties(mecabel_impl PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    PREFIX ""
    LINK_FLAGS ${mecab_flags}
    OUTPUT_NAME "mecabel-impl")
