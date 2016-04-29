cmake_minimum_required (VERSION 3.0)

# About BQC Project
project (BQC)
set (BQC_VERSION_MAJOR   "1")
set (BQC_VERSION_MINOR   "0")
set (BQC_VERSION_PATCH   "0")
set (BQC_VERSION "${BQC_VERSION_MAJOR}.${BQC_VERSION_MINOR}.${BQC_VERSION_PATCH}")

set (CMAKE_COLOR_MAKEFILE ON)

# CXX flags
set(CMAKE_CXX_FLAGS "-g -Wall")

# Requirements
set( EIGEN3_INCLUDE_DIR "$ENV{EIGEN3_INCLUDE_DIR}" )
if( NOT EIGEN3_INCLUDE_DIR )
    message( FATAL_ERROR "
 Please point the environment variable EIGEN3_INCLUDE_DIR to the include directory of your Eigen3 installation.
     Type:
          export EIGEN3_INCLUDE_DIR=<path to Eigen3>
")
endif()
#find_package(PkgConfig)
#pkg_search_module(Eigen3 REQUIRED eigen3)
include_directories ("${EIGEN3_INCLUDE_DIR}")

# Options
option (USE_EIGEN   "Use high-performance mathematical library system"  ON )  # recommended
option (USE_LIBINT  "Use 2-electron integral package by E. Valeev"      OFF)  # not interfaced yet

# Setting the environment
include_directories ("${PROJECT_SOURCE_DIR}")
IF (NO)
  link_directories ("${PROJECT_SOURCE_DIR}/external/lib")
ENDIF(NO)


# LibInt
if (USE_LIBINT)
  include_directories ("${PROJECT_SOURCE_DIR}/LibInt")
  add_subdirectory (LibInt)
  set (EXTRA_LIBS ${EXTRA_LIBS} LibInt)
endif (USE_LIBINT)

# Eigen
message ("BQC Binary Tree Path: ${PROJECT_BINARY_DIR}")

# Add subfolders
add_subdirectory (src)
#add_subdirectory (eigen)

# configure header file
configure_file (
  "${PROJECT_SOURCE_DIR}/BqcConfig.h.in"
  "${PROJECT_BINARY_DIR}/BqcConfig.h"
)

# Add the executable
add_executable (bqc bqc.cc)

# Link the libraries
target_link_libraries (bqc BQC-Basis)
target_link_libraries (bqc BQC-Math)
target_link_libraries (bqc BQC-IO)
target_link_libraries (bqc BQC-SCF)
target_link_libraries (bqc BQC-Ints)
target_link_libraries (bqc BQC-Molecule)

target_link_libraries (bqc ${EXTRA_LIBS})
