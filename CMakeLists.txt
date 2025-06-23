cmake_minimum_required(VERSION 3.22)
project(MOCSY LANGUAGES Fortran VERSION 2024.08.1)

set(MOCSY_PRECISION 2 CACHE STRING "Precision to use (1 or 2)")
# Custom user-defined compile flags
set(CUSTOM_COMPILE_FLAGS "" CACHE STRING "Extra compile flags for the MOCSY target (e.g., -O3 -fno-alias)")
# Create the target first, so subdirs can modify it
add_library(MOCSY STATIC)
option(MOCSY_PRECISION "Precision to use (1 or 2)" 2)

# Set module output directory for Fortran `.mod` files
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


# Include the generated mod path during build
target_include_directories(MOCSY PUBLIC
  $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>
  $<INSTALL_INTERFACE:include>
)

# Add subdirectory where sources and compile options are added
add_subdirectory(src)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/mocsy.pc.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mocsy.pc"
  @ONLY
)


# Install the library target and mod files
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

# Export the CMake target information
install(EXPORT mocsyTargets
  FILE mocsyTargets.cmake
  NAMESPACE MOCSY::
  DESTINATION lib/cmake/mocsy
)

# Create and install Config and Version files
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

# Optional lowercase alias for compatibility (e.g., Spack, older tooling)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/MOCSYConfig.cmake"
  DESTINATION lib/cmake/mocsy
  RENAME MOCSY-config.cmake
)

