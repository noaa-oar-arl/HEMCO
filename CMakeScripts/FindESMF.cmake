# FindESMF.cmake
#
# Finds the ESMF library and includes
#
# This module sets the following variables:
#  ESMF_FOUND - set to true if ESMF is found
#  ESMF_INCLUDE_DIRS - paths to ESMF include directories
#  ESMF_LIBRARIES - paths to ESMF libraries
#  ESMF_VERSION - ESMF version if available
#  NUOPC_FOUND - set to true if NUOPC is found
#  NUOPC_LIBRARIES - paths to NUOPC libraries
#
# If ESMF is not found, this module will attempt to find it using:
# 1. The ESMFMKFILE environment variable (best method)
# 2. The ESMF_ROOT or ESMF_DIR environment variable
# 3. Pkg-config if available

# Function to extract information from esmf.mk file
function(parse_esmfmkfile MKFILE OUTPUT_INCLUDES OUTPUT_LIBS OUTPUT_VERSION)
  file(STRINGS "${MKFILE}" esmf_mk_contents)

  set(includes "")
  set(libs "")
  set(version "")

  foreach(line ${esmf_mk_contents})
    # Extract include paths
    if(line MATCHES "^ESMF_F90COMPILEPATHS=(.*)$")
      string(REGEX REPLACE "^ESMF_F90COMPILEPATHS=(.*)" "\\1" inc "${line}")
      string(REPLACE "-I" "" inc "${inc}")
      string(REPLACE " " ";" inc "${inc}")
      set(includes ${inc})
    endif()

    # Extract library paths and names
    if(line MATCHES "^ESMF_F90LINKPATHS=(.*)$")
      string(REGEX REPLACE "^ESMF_F90LINKPATHS=(.*)" "\\1" lib_paths "${line}")
      string(REPLACE "-L" "" lib_paths "${lib_paths}")
      string(REPLACE " " ";" lib_paths "${lib_paths}")
    endif()

    if(line MATCHES "^ESMF_F90LINKRPATHS=(.*)$")
      string(REGEX REPLACE "^ESMF_F90LINKRPATHS=(.*)" "\\1" lib_rpaths "${line}")
      string(REPLACE "-Wl,-rpath," "" lib_rpaths "${lib_rpaths}")
      string(REPLACE " " ";" lib_rpaths "${lib_rpaths}")
    endif()

    if(line MATCHES "^ESMF_F90ESMFLINKLIBS=(.*)$")
      string(REGEX REPLACE "^ESMF_F90ESMFLINKLIBS=(.*)" "\\1" lib_names "${line}")
      string(REPLACE " " ";" lib_names "${lib_names}")
    endif()

    # Extract version
    if(line MATCHES "^ESMF_VERSION_STRING=(.*)")
      string(REGEX REPLACE "^ESMF_VERSION_STRING=(.*)" "\\1" version "${line}")
    endif()
  endforeach()

  # Construct full library paths
  set(libraries "")
  foreach(path ${lib_paths})
    foreach(name ${lib_names})
      if(EXISTS "${path}/lib${name}.so" OR EXISTS "${path}/lib${name}.a")
        list(APPEND libraries "-L${path}" "-l${name}")
      endif()
    endforeach()

    # Explicitly check for NUOPC library in the same path
    if(EXISTS "${path}/libnuopc.so" OR EXISTS "${path}/libnuopc.a")
      # Add NUOPC library explicitly
      set(NUOPC_FOUND TRUE PARENT_SCOPE)
      list(APPEND libraries "-L${path}" "-lnuopc")
      message(STATUS "Found NUOPC library at ${path}")
    endif()
  endforeach()

  # If no libraries found but we have lib_names, use them directly
  if(NOT libraries AND lib_names)
    set(libraries ${lib_names})
    # Also add NUOPC if we're in default library mode
    list(APPEND libraries "-lnuopc")
    set(NUOPC_FOUND TRUE PARENT_SCOPE)
  endif()

  # Set return values
  set(${OUTPUT_INCLUDES} ${includes} PARENT_SCOPE)
  set(${OUTPUT_LIBS} ${libraries} PARENT_SCOPE)
  set(${OUTPUT_VERSION} ${version} PARENT_SCOPE)
endfunction()

# First try: look for ESMFMKFILE environment variable (preferred method)
if(NOT DEFINED ESMFMKFILE)
  set(ESMFMKFILE $ENV{ESMFMKFILE})
endif()

# Initialize NUOPC_FOUND to FALSE
set(NUOPC_FOUND FALSE)

if(ESMFMKFILE)
  if(EXISTS ${ESMFMKFILE})
    message(STATUS "Found ESMF mkfile: ${ESMFMKFILE}")
    parse_esmfmkfile(${ESMFMKFILE} ESMF_INCLUDE_DIRS ESMF_LIBRARIES ESMF_VERSION)
  else()
    message(WARNING "ESMFMKFILE specified but file does not exist: ${ESMFMKFILE}")
  endif()
endif()

# Second try: look for ESMF_ROOT or ESMF_DIR environment variable
if(NOT ESMF_INCLUDE_DIRS OR NOT ESMF_LIBRARIES)
  set(ESMF_ROOT $ENV{ESMF_ROOT})
  if(NOT ESMF_ROOT)
    set(ESMF_ROOT $ENV{ESMF_DIR})
  endif()

  if(ESMF_ROOT)
    # Try to find esmf.mk in the ESMF installation
    file(GLOB_RECURSE ESMF_MKFILE_PATHS "${ESMF_ROOT}/*esmf.mk")

    if(ESMF_MKFILE_PATHS)
      list(GET ESMF_MKFILE_PATHS 0 ESMF_MKFILE)
      message(STATUS "Found ESMF mkfile: ${ESMF_MKFILE}")
      parse_esmfmkfile(${ESMF_MKFILE} ESMF_INCLUDE_DIRS ESMF_LIBRARIES ESMF_VERSION)
    else()
      # Manually try to find the components
      find_path(ESMF_INCLUDE_DIR
                NAMES ESMF.h ESMF_Init.h
                PATHS ${ESMF_ROOT}/include ${ESMF_ROOT}/include/esmf
                NO_DEFAULT_PATH)

      find_library(ESMF_LIBRARY
                   NAMES esmf
                   PATHS ${ESMF_ROOT}/lib ${ESMF_ROOT}/lib64
                   NO_DEFAULT_PATH)

      # Explicitly look for NUOPC library
      find_library(NUOPC_LIBRARY
                   NAMES nuopc
                   PATHS ${ESMF_ROOT}/lib ${ESMF_ROOT}/lib64
                   NO_DEFAULT_PATH)

      if(ESMF_INCLUDE_DIR AND ESMF_LIBRARY)
        set(ESMF_INCLUDE_DIRS ${ESMF_INCLUDE_DIR})
        set(ESMF_LIBRARIES ${ESMF_LIBRARY})

        # Add NUOPC if found
        if(NUOPC_LIBRARY)
          set(NUOPC_FOUND TRUE)
          set(NUOPC_LIBRARIES ${NUOPC_LIBRARY})
          list(APPEND ESMF_LIBRARIES ${NUOPC_LIBRARY})
          message(STATUS "Found NUOPC: ${NUOPC_LIBRARY}")
        endif()
      endif()
    endif()
  endif()
endif()

# Third try: use pkg-config
if(NOT ESMF_INCLUDE_DIRS OR NOT ESMF_LIBRARIES)
  find_package(PkgConfig QUIET)
  if(PKG_CONFIG_FOUND)
    pkg_check_modules(PC_ESMF QUIET esmf)
    if(PC_ESMF_FOUND)
      set(ESMF_INCLUDE_DIRS ${PC_ESMF_INCLUDE_DIRS})
      set(ESMF_LIBRARIES ${PC_ESMF_LIBRARIES})
      set(ESMF_VERSION ${PC_ESMF_VERSION})

      # Look for NUOPC using pkg-config
      pkg_check_modules(PC_NUOPC QUIET nuopc)
      if(PC_NUOPC_FOUND)
        set(NUOPC_FOUND TRUE)
        set(NUOPC_LIBRARIES ${PC_NUOPC_LIBRARIES})
        list(APPEND ESMF_LIBRARIES ${PC_NUOPC_LIBRARIES})
        message(STATUS "Found NUOPC via pkg-config")
      endif()
    endif()
  endif()
endif()

# Last try: system paths
if(NOT ESMF_INCLUDE_DIRS OR NOT ESMF_LIBRARIES)
  find_path(ESMF_INCLUDE_DIR
            NAMES ESMF.h ESMF_Init.h
            PATHS /usr/include /usr/local/include /opt/include
            PATH_SUFFIXES esmf)

  find_library(ESMF_LIBRARY
               NAMES esmf
               PATHS /usr/lib /usr/lib64 /usr/local/lib /usr/local/lib64 /opt/lib /opt/lib64)

  # # Look for NUOPC library in system paths
  # find_library(NUOPC_LIBRARY
  #              NAMES nuopc
  #              PATHS /usr/lib /usr/lib64 /usr/local/lib /usr/local/lib64 /opt/lib /opt/lib64)

  if(ESMF_INCLUDE_DIR AND ESMF_LIBRARY)
    set(ESMF_INCLUDE_DIRS ${ESMF_INCLUDE_DIR})
    set(ESMF_LIBRARIES ${ESMF_LIBRARY})

    # # Add NUOPC if found
    # if(NUOPC_LIBRARY)
    #   set(NUOPC_FOUND TRUE)
    #   set(NUOPC_LIBRARIES ${NUOPC_LIBRARY})
    #   list(APPEND ESMF_LIBRARIES ${NUOPC_LIBRARY})
    #   message(STATUS "Found NUOPC: ${NUOPC_LIBRARY}")
    # endif()
  endif()
endif()

# If NUOPC wasn't found but we have ESMF, try to locate it by inserting -lnuopc
# if(NOT NUOPC_FOUND AND ESMF_FOUND)
#   foreach(lib_path ${ESMF_LIBRARIES})
#     if(lib_path MATCHES "^-L(.*)")
#       string(REGEX REPLACE "^-L(.*)" "\\1" path "${lib_path}")
#       if(EXISTS "${path}/libnuopc.so" OR EXISTS "${path}/libnuopc.a")
#         set(NUOPC_FOUND TRUE)
#         set(NUOPC_LIBRARIES "-lnuopc")
#         list(APPEND ESMF_LIBRARIES "-lnuopc")
#         message(STATUS "Found NUOPC library in ESMF path: ${path}")
#         break()
#       endif()
#     endif()
#   endforeach()

#   # If we still didn't find NUOPC, just add -lnuopc and hope it works
#   if(NOT NUOPC_FOUND)
#     set(NUOPC_FOUND TRUE)
#     set(NUOPC_LIBRARIES "-lnuopc")
#     list(APPEND ESMF_LIBRARIES "-lnuopc")
#     message(STATUS "Adding NUOPC library to link line (assuming it exists with ESMF)")
#   endif()
# endif()

# Handle the QUIETLY and REQUIRED arguments and set ESMF_FOUND
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ESMF
                                  REQUIRED_VARS ESMF_LIBRARIES ESMF_INCLUDE_DIRS
                                  VERSION_VAR ESMF_VERSION)

if(ESMF_FOUND)
  # Create an imported target for ESMF
  if(NOT TARGET ESMF::ESMF)
    add_library(ESMF::ESMF INTERFACE IMPORTED)
    set_target_properties(ESMF::ESMF PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${ESMF_INCLUDE_DIRS}"
      INTERFACE_LINK_LIBRARIES "${ESMF_LIBRARIES}")
  endif()

  # Alias the target to match what the code is looking for
  if(NOT TARGET ESMF)
    add_library(ESMF ALIAS ESMF::ESMF)
  endif()

  # Create NUOPC target if found
  if(NUOPC_FOUND AND NOT TARGET ESMF::NUOPC)
    add_library(ESMF::NUOPC INTERFACE IMPORTED)
    set_target_properties(ESMF::NUOPC PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${ESMF_INCLUDE_DIRS}"
      INTERFACE_LINK_LIBRARIES "${NUOPC_LIBRARIES}")

    # Alias the target
    if(NOT TARGET NUOPC)
      add_library(NUOPC ALIAS ESMF::NUOPC)
    endif()
  endif()

  message(STATUS "Found ESMF: ${ESMF_LIBRARIES}")
  if(ESMF_VERSION)
    message(STATUS "ESMF version: ${ESMF_VERSION}")
  endif()

  if(NUOPC_FOUND)
    message(STATUS "Found NUOPC: ${NUOPC_LIBRARIES}")
  else()
    message(WARNING "NUOPC library not found. This may cause linking errors for NUOPC interfaces.")
  endif()
endif()