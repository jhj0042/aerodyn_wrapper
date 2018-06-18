#=============================================================================
#
# CMake configuration file for the Aerodyn_ECN project
#
#=============================================================================


#-----------------------------------------------------------------------------
# Check backward compatibility
#-----------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.1)

#-----------------------------------------------------------------------------
# Prevent in-source builds
#-----------------------------------------------------------------------------
if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
    message( FATAL_ERROR  "In-source build is not possible and not recommended. Choose an empty directory for build output.")
endif(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})

#-----------------------------------------------------------------------------
# Define project name and specify version information
#-----------------------------------------------------------------------------
project(Aerodyn_ECN Fortran)

set(Aerodyn_ECN_DICE_VERSION_MAJOR 0)
set(Aerodyn_ECN_DICE_VERSION_MINOR 1)
set(Aerodyn_ECN_DICE_VERSION_PATCH 0)

set(Aerodyn_ECN_DICE_VERSION
        "${Aerodyn_ECN_DICE_VERSION_MAJOR}.${Aerodyn_ECN_DICE_VERSION_MINOR}.${Aerodyn_ECN_DICE_VERSION_PATCH}")


#-----------------------------------------------------------------------------
# Setting the configuration of the build (Debug, Release...)
#-----------------------------------------------------------------------------
#set(CMAKE_BUILD_TYPE Debug)

#-----------------------------------------------------------------------------
# C++ dialect
#-----------------------------------------------------------------------------
set(CMAKE_CXX_STANDARD 14)


# Access to local cmake scripts
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

include(FastFortranOptions)
set_fast_fortran()


# Setup dependencies
find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)

# Adding libs
add_subdirectory(src/nwtc-library)
add_subdirectory(src/aerodyn)
add_subdirectory(src/aerodyn_wrapper)


# Adding tests
add_subdirectory(tests)
