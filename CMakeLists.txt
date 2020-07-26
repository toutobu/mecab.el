cmake_minimum_required(VERSION 3.10.0)

project(mecabel VERSION 0.1)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(mecabel_include_dirs
  ${CMAKE_CURRENT_SOURCE_DIR}/include)

message(debug " ${mecabel_include_dirs}/tagger.h")

add_library(mecabel_impl SHARED src/mecabel_impl.cc
  ${mecabel_include_dirs}/tagger.h)

target_include_directories(mecabel_impl PUBLIC
  ${mecabel_include_dirs})

set_target_properties(mecabel_impl PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    PREFIX ""
    OUTPUT_NAME "mecabel-impl")
