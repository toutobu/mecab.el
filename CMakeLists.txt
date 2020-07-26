cmake_minimum_required(VERSION 3.10.0)

project(mecabel VERSION 0.1)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(BOOST_VERSION 1.73.0)

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
# Define the mecabel-impl library,

set(mecabel_include_dirs
  ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_library(mecabel_impl SHARED src/mecabel_impl.cc
  ${mecabel_include_dirs}/tagger.h)

target_include_directories(mecabel_impl PUBLIC
  ${mecabel_include_dirs})

set_target_properties(mecabel_impl PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    PREFIX ""
    OUTPUT_NAME "mecabel-impl")
