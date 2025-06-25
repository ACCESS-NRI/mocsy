cmake_minimum_required(VERSION 3.22)
project(MOCSY LANGUAGES Fortran VERSION 2024.08.1)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Relwithdebinfo' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Relwithdebinfo"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
else()
  message(STATUS "Setting build type to ${CMAKE_BUILD_TYPE}")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(
    WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

set(MOCSY_PRECISION 2 CACHE STRING "Precision to use (1 or 2)")
set(CUSTOM_COMPILE_FLAGS "" CACHE STRING "Extra compile flags for the MOCSY target (e.g., -O3 -fno-alias)")
add_library(MOCSY STATIC)
option(MOCSY_PRECISION "Precision to use (1 or 2)" 2)


set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(CheckFortranCompilerFlag)
include(set_mocsy_compile_flags)

configure_fortran_flags()

set_target_properties(MOCSY PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod
  OUTPUT_NAME mocsy
)

target_compile_definitions(MOCSY PRIVATE USE_PRECISION=${MOCSY_PRECISION}) 

# Apply custom flags if provided
if(CUSTOM_COMPILE_FLAGS)
  separate_arguments(CUSTOM_COMPILE_FLAGS)
  message(STATUS "Using ${CUSTOM_COMPILE_FLAGS} to compile !")
  target_compile_options(MOCSY PRIVATE ${CUSTOM_COMPILE_FLAGS})
endif()


target_include_directories(MOCSY PUBLIC
  $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>
  $<INSTALL_INTERFACE:include>
)

add_subdirectory(src)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/mocsy.pc.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mocsy.pc"
  @ONLY
)


install(TARGETS MOCSY
  EXPORT mocsyTargets
  ARCHIVE DESTINATION lib
)

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/mocsy.pc"
  DESTINATION lib/pkgconfig
)


install(DIRECTORY ${CMAKE_BINARY_DIR}/mod/
  DESTINATION include
  FILES_MATCHING PATTERN "*.mod"
)

install(EXPORT mocsyTargets
  FILE mocsyTargets.cmake
  NAMESPACE MOCSY::
  DESTINATION lib/cmake/mocsy
)

include(CMakePackageConfigHelpers)

configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/MOCSYConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/MOCSYConfig.cmake"
  INSTALL_DESTINATION lib/cmake/mocsy
)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/MOCSYConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion
)


install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/MOCSYConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/MOCSYConfigVersion.cmake"
  DESTINATION lib/cmake/mocsy
)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/MOCSYConfig.cmake"
  DESTINATION lib/cmake/mocsy
  RENAME MOCSY-config.cmake
)

