#
# Build file for the GP2 system
#
# Copyright 2013 Alex Elliott <alex@alex-elliott.co.uk>
#
# ------------------------------------------------------------------------------
#
# This file sets up the environment for building the GP2 toolchain and GP
# Developer - the new GP Integrated Development Environment. Initially shared
# libraries are located, and then the CMakeLists.txt file for each of the tools
# is included in order to build those components.
#
# The components included are:
#   src/developer/CMakeLists.txt    - GP Developer
#   src/libgp/CMakeLists.txt        - GP2 utility library
#   src/gprc/CMakeLists.txt         - GP2 rule compiler
#   src/gpxc/CMakeLists.txt         - GP2 program compiler
#   src/gpmake/CMakeLists.txt       - GP2 build tool
#

# Note: On office computer, set the prefix path before calling by
# export CMAKE_PREFIX_PATH=/local/sda5/ish503/root:/local/sda5/ish503/root/qt4:/local/sda5/ish503/root/boost_1_55_0/:/local/sda5/ish503/root/OGDF-source/:/local/sda5/ish503/root/OGDF-source/include/

CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

PROJECT(GP2)

# Note: Set the location of the various components manually here
SET(GP2_COMPILER_LOCATION "~/github/GP2/Compiler")  # Change this 

SET(BOOST_INCLUDE_DIR /local/sda5/ish503/root)  # Change this
FIND_PACKAGE(Boost 1.49.0 REQUIRED)

SET(OGDF_INCLUDE_DIR /local/sda5/ish503/root/OGDF-source) # Change this 

# Find OGDF
FIND_PATH(OGDF_INCLUDE_DIR ogdf/basic/basic.h)
IF(WIN32)
    FIND_LIBRARY(OGDF_LIBRARY libOGDF)
ELSE()
    FIND_LIBRARY(OGDF_LIBRARY OGDF /local/sda5/ish503/root/OGDF-build/lib64) # Change this
ENDIF()


SET(GP2_VERSION_MAJOR 1)
SET(GP2_VERSION_MINOR 0)

INCLUDE_DIRECTORIES(
    ${CMAKE_SOURCE_DIR}
    ${GP2_SOURCE_DIR}/src/developer/translate   #defines libTranslate
    ${GP2_SOURCE_DIR}/src/developer
    ${GP2_SOURCE_DIR}/src/libgp
    ${GP2_SOURCE_DIR}/src/gprc
    ${GP2_SOURCE_DIR}/src/gpxc
    ${GP2_SOURCE_DIR}/src/gpmake
    ${GP2_BINARY_DIR}
)

add_subdirectory(${GP2_SOURCE_DIR}/src/developer/translate)

ADD_DEFINITIONS( -DCOMPILER_LOCATION="${GP2_COMPILER_LOCATION}" )

INCLUDE(CheckIncludeFiles)

SET(QT_MIN_VERSION "4.8.0")
FIND_PACKAGE(Qt4 REQUIRED)


SET(QT_USE_QTMAIN true)
SET(QT_USE_QTXML true)
SET(QT_USE_QTSVG true)

INCLUDE(${QT_USE_FILE})

# Enable unit testing
ENABLE_TESTING()

# Set CPack variables
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "GP2 Tools")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
SET(CPACK_PACKAGE_FILE_NAME "GP Developer")
SET(CPACK_PACKAGE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/src/developer/images/graph.png")
SET(CPACK_BUNDLE_NAME "GP Developer")
SET(CPACK_BUNDLE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/src/developer/images/icon.icns")
SET(CPACK_BUNDLE_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist")
SET(CPACK_BUNDLE_STARTUP_COMMAND gpdeveloper)
SET(CPACK_PACKAGE_EXECUTABLES gpdeveloper "GP Developer")
SET(CPACK_PACKAGE_NAME "GP2")
SET(CPACK_PACKAGE_VERSION_MAJOR ${GP2_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${GP2_VERSION_MINOR})
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "GP2")

# Unix platform add a "lib" prefix, MSVC doesn't
SET(libGP_NAME GP)
IF(CMAKE_COMPILER_IS_MSVC)
    SET(libGP_NAME libGP)
ENDIF(CMAKE_COMPILER_IS_MSVC)

# Build libGP
INCLUDE("src/libgp/CMakeLists.txt")

# We want to be able to link against libGP, so make sure that the directory is checked when linking
LINK_DIRECTORIES(${GP2_BINARY_DIR})

# Build GP Developer
INCLUDE("src/developer/CMakeLists.txt")

# Build gprc
#INCLUDE("src/gprc/CMakeLists.txt")

# Build gpxc
#INCLUDE("src/gpxc/CMakeLists.txt")

# Build gpmake
#INCLUDE("src/gpmake/CMakeLists.txt")

# Bring in CPack
#INCLUDE(CPack)
