# FindESMF.cmake -- Modern CMake module for ESMF
#
# This module finds ESMF using either CMake targets or classic Makefile-based detection.
# It creates a modern imported target 'ESMF::esmf' for use with target_link_libraries.

find_package(PkgConfig)

# Prefer CMake-based ESMF targets if available
if(TARGET ESMF::esmf)
  message(STATUS "Using existing ESMF::esmf CMake target.")
  set(ESMF_FOUND TRUE)
  return()
elseif(TARGET esmf)
  message(STATUS "Using existing esmf CMake target.")
  set(ESMF_FOUND TRUE)
  # Create the canonical target
  if(NOT TARGET ESMF::esmf)
    add_library(ESMF::esmf ALIAS esmf)
  endif()
  return()
endif()

# Classic Makefile-based ESMF detection
if(NOT DEFINED ENV{ESMFMKFILE})
  message(FATAL_ERROR "ESMFMKFILE env variable is not defined")
endif()
set(ESMFMKFILE $ENV{ESMFMKFILE})
message(STATUS "ESMFMKFILE: ${ESMFMKFILE}")

# Parse esmf.mk for ESMF variables
file(STRINGS ${ESMFMKFILE} esmf_mk_text)
foreach(line ${esmf_mk_text})
  string(REGEX REPLACE "^[ ]+" "" line ${line})
  if(line MATCHES "^ESMF_.*=")
    string(REGEX MATCH "^ESMF_[^=]+" esmf_name ${line})
    string(REPLACE "${esmf_name}=" "" esmf_value ${line})
    set(${esmf_name} "${esmf_value}" CACHE INTERNAL "ESMF var from esmf.mk")
  endif()
endforeach()

# Clean up and convert ESMF variables
string(REPLACE "-I" "" ESMF_F90COMPILEPATHS ${ESMF_F90COMPILEPATHS})
string(REPLACE " " ";" ESMF_F90COMPILEPATHS ${ESMF_F90COMPILEPATHS})

# Set ESMF_VERSION_STRING for find_package_handle_standard_args
if(ESMF_VERSION_MAJOR AND ESMF_VERSION_MINOR AND ESMF_VERSION_PATCH)
  set(ESMF_VERSION_STRING "${ESMF_VERSION_MAJOR}.${ESMF_VERSION_MINOR}.${ESMF_VERSION_PATCH}")
endif()

# Ensure all required ESMF variables are set
if(ESMF_VERSION_MAJOR AND ESMF_F90COMPILEPATHS AND ESMF_F90ESMFLINKRPATHS AND ESMF_F90ESMFLINKLIBS)
  message(STATUS "Found ESMF:")
  message(STATUS "  ESMF_VERSION_MAJOR:     ${ESMF_VERSION_MAJOR}")
  message(STATUS "  ESMF_F90COMPILEPATHS:   ${ESMF_F90COMPILEPATHS}")
  message(STATUS "  ESMF_F90ESMFLINKRPATHS: ${ESMF_F90ESMFLINKRPATHS}")
  message(STATUS "  ESMF_F90ESMFLINKLIBS:   ${ESMF_F90ESMFLINKLIBS}")
else()
  message(FATAL_ERROR "One of the required ESMF_ variables is not defined.\nESMF_VERSION_MAJOR=${ESMF_VERSION_MAJOR}\nESMF_F90COMPILEPATHS=${ESMF_F90COMPILEPATHS}\nESMF_F90ESMFLINKRPATHS=${ESMF_F90ESMFLINKRPATHS}\nESMF_F90ESMFLINKLIBS=${ESMF_F90ESMFLINKLIBS}")
endif()

# Set ESMF_INCLUDE_DIRS and ESMF_LIBRARY_DIRS for modern CMake usage
set(ESMF_INCLUDE_DIRS ${ESMF_F90COMPILEPATHS})
set(ESMF_LIBRARY_DIRS "")

# Parse ESMF_F90LINKPATHS to get all library directories
string(REPLACE " " ";" ESMF_LINKPATH_LIST "${ESMF_F90LINKPATHS}")
foreach(path ${ESMF_LINKPATH_LIST})
  string(REGEX MATCH "^-L(.+)" _ "${path}")
  if(CMAKE_MATCH_1 AND EXISTS "${CMAKE_MATCH_1}")
    list(APPEND ESMF_LIBRARY_DIRS "${CMAKE_MATCH_1}")
  endif()
endforeach()

# Also include directories from ESMF_F90ESMFLINKRPATHS for backwards compatibility
foreach(rpath ${ESMF_F90ESMFLINKRPATHS})
  string(REPLACE "-Wl,-rpath," "" rpath_cleaned "${rpath}")
  if(EXISTS "${rpath_cleaned}")
    list(APPEND ESMF_LIBRARY_DIRS "${rpath_cleaned}")
  endif()
endforeach()

# Remove duplicates
list(REMOVE_DUPLICATES ESMF_LIBRARY_DIRS)

# Parse library paths and dependencies from ESMF_F90ESMFLINKLIBS
string(REPLACE " " ";" ESMF_LINK_FLAGS_LIST "${ESMF_F90ESMFLINKLIBS}")

# Find the actual ESMF library file
set(ESMF_LIBRARY_FILE "")
foreach(lib_dir ${ESMF_LIBRARY_DIRS})
  find_library(ESMF_LIB_FOUND esmf PATHS ${lib_dir} NO_DEFAULT_PATH)
  if(ESMF_LIB_FOUND)
    set(ESMF_LIBRARY_FILE ${ESMF_LIB_FOUND})
    message(STATUS "Found ESMF library: ${ESMF_LIBRARY_FILE}")
    break()
  endif()
  unset(ESMF_LIB_FOUND CACHE)
endforeach()

if(NOT ESMF_LIBRARY_FILE)
  message(FATAL_ERROR "Could not find libesmf in any of the ESMF library directories: ${ESMF_LIBRARY_DIRS}")
endif()

# Extract additional library dependencies (exclude ESMF itself and problematic libraries)
set(ESMF_INTERFACE_LIBS "")
foreach(flag ${ESMF_LINK_FLAGS_LIST})
  # Skip ESMF library itself and NetCDF libraries (they should come from NetCDF targets)
  if(flag MATCHES "^-l" AND NOT flag MATCHES "^-lesmf$" AND NOT flag MATCHES "^-lnetcdf")
    list(APPEND ESMF_INTERFACE_LIBS ${flag})
  elseif(flag MATCHES "\\.(a|so|dylib)$" AND NOT flag MATCHES "esmf" AND NOT flag MATCHES "netcdf")
    # Include full path libraries (but not ESMF or NetCDF)
    if(EXISTS ${flag})
      list(APPEND ESMF_INTERFACE_LIBS ${flag})
    endif()
  endif()
endforeach()

# Create the ESMF imported target
if(NOT TARGET ESMF::esmf)
  add_library(ESMF::esmf UNKNOWN IMPORTED)

  # Set the main ESMF library
  set_target_properties(ESMF::esmf PROPERTIES
    IMPORTED_LOCATION "${ESMF_LIBRARY_FILE}"
    INTERFACE_INCLUDE_DIRECTORIES "${ESMF_INCLUDE_DIRS}"
  )

  # Add interface link libraries if any (excluding NetCDF dependencies)
  if(ESMF_INTERFACE_LIBS)
    set_target_properties(ESMF::esmf PROPERTIES
      INTERFACE_LINK_LIBRARIES "${ESMF_INTERFACE_LIBS}"
    )
  endif()

  # Add library directories to the target's interface
  if(ESMF_LIBRARY_DIRS)
    set_target_properties(ESMF::esmf PROPERTIES
      INTERFACE_LINK_DIRECTORIES "${ESMF_LIBRARY_DIRS}"
    )
  endif()
endif()

# Create esmf alias for backwards compatibility
if(NOT TARGET esmf)
  add_library(esmf ALIAS ESMF::esmf)
endif()

set(ESMF_FOUND TRUE)

# Print summary for diagnostics
message(STATUS "==== ESMF CMake Summary ====")
message(STATUS "ESMF_FOUND:           ${ESMF_FOUND}")
message(STATUS "ESMF_VERSION_STRING:  ${ESMF_VERSION_STRING}")
message(STATUS "ESMF_INCLUDE_DIRS:    ${ESMF_INCLUDE_DIRS}")
message(STATUS "ESMF_LIBRARY_DIRS:    ${ESMF_LIBRARY_DIRS}")
message(STATUS "ESMF_LIBRARY_FILE:    ${ESMF_LIBRARY_FILE}")
message(STATUS "ESMF_INTERFACE_LIBS:  ${ESMF_INTERFACE_LIBS}")
message(STATUS "=============================")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ESMF
  FOUND_VAR ESMF_FOUND
  REQUIRED_VARS
    ESMF_LIBRARY_FILE
    ESMF_F90COMPILEPATHS
    ESMF_F90ESMFLINKRPATHS
    ESMF_F90ESMFLINKLIBS
  VERSION_VAR ESMF_VERSION_STRING
)