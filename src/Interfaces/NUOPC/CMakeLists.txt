# Only build the NUOPC interface if it's enabled
if(BUILD_NUOPC_INTERFACE)
  # List of files to compile into the HEMCO NUOPC interface library
  set (HCOI_NUOPC_SRCS
    HCOI_NUOPCGridCompMod.F90
    HCOI_NUOPCStandaloneDriverMod.F90
  )

  # Standalone NUOPC driver executable source
  set (HCOI_NUOPC_STANDALONE_SRCS
    hemco_nuopc_standalone.F90
  )

  # Check for ESMFMKFILE environment variable
  if(DEFINED ENV{ESMFMKFILE})
    message(STATUS "ESMFMKFILE found: $ENV{ESMFMKFILE}")

    # Extract ESMF settings from esmf.mk file
    file(READ $ENV{ESMFMKFILE} esmf_mk_contents)

    # Extract ESMF include flags
    if(esmf_mk_contents MATCHES "ESMF_F90COMPILEPATHS=([^\n]*)")
      string(STRIP "${CMAKE_MATCH_1}" ESMF_INCLUDE_FLAGS)
      # Convert to a list and clean up -I flags
      string(REGEX REPLACE "-I" ";" ESMF_INCLUDE_FLAGS_LIST "${ESMF_INCLUDE_FLAGS}")
      # Create a clean list with trimmed paths
      set(ESMF_INCLUDE_DIRS "")
      foreach(dir ${ESMF_INCLUDE_FLAGS_LIST})
        string(STRIP "${dir}" dir_trimmed)
        if(NOT "${dir_trimmed}" STREQUAL "")
          list(APPEND ESMF_INCLUDE_DIRS "${dir_trimmed}")
        endif()
      endforeach()
      message(STATUS "ESMF include directories: ${ESMF_INCLUDE_DIRS}")
    endif()

    # Extract ESMF library flags
    if(esmf_mk_contents MATCHES "ESMF_F90LINKPATHS=([^\n]*)")
      string(STRIP "${CMAKE_MATCH_1}" ESMF_LINK_FLAGS)
      # Extract library paths
      string(REGEX REPLACE "-L" ";" ESMF_LINK_FLAGS_LIST "${ESMF_LINK_FLAGS}")
      # Create a clean list with trimmed paths
      set(ESMF_LIBRARY_DIRS "")
      foreach(dir ${ESMF_LINK_FLAGS_LIST})
        string(STRIP "${dir}" dir_trimmed)
        if(NOT "${dir_trimmed}" STREQUAL "")
          list(APPEND ESMF_LIBRARY_DIRS "${dir_trimmed}")
        endif()
      endforeach()
      message(STATUS "ESMF library directories: ${ESMF_LIBRARY_DIRS}")
    endif()

    # Extract ESMF runtime paths (rpath flags)
    if(esmf_mk_contents MATCHES "ESMF_F90LINKRPATHS=([^\n]*)")
      string(STRIP "${CMAKE_MATCH_1}" ESMF_RPATH_FLAGS)
      message(STATUS "ESMF runtime paths: ${ESMF_RPATH_FLAGS}")
    endif()

    # Extract ESMF libraries
    if(esmf_mk_contents MATCHES "ESMF_F90ESMFLINKLIBS=([^\n]*)")
      string(STRIP "${CMAKE_MATCH_1}" ESMF_LIBRARIES_RAW)
      # Convert to a list and clean up -l flags
      string(REGEX REPLACE "-l" "" ESMF_LIBRARIES_RAW_LIST "${ESMF_LIBRARIES_RAW}")
      # Create a clean list with trimmed library names
      set(ESMF_LIBRARIES_LIST "")
      foreach(lib ${ESMF_LIBRARIES_RAW_LIST})
        string(STRIP "${lib}" lib_trimmed)
        if(NOT "${lib_trimmed}" STREQUAL "")
          list(APPEND ESMF_LIBRARIES_LIST "${lib_trimmed}")
        endif()
      endforeach()
      message(STATUS "ESMF libraries: ${ESMF_LIBRARIES_LIST}")
    endif()

    # Check for NUOPC availability by looking for NUOPC modules
    set(NUOPC_CHECK_FILE "${CMAKE_CURRENT_BINARY_DIR}/check_nuopc.F90")
    file(WRITE ${NUOPC_CHECK_FILE} "
      program check_nuopc
        use ESMF
        use NUOPC
        use NUOPC_Model
        implicit none
        print *, 'NUOPC is available'
      end program
    ")

    try_compile(NUOPC_FOUND ${CMAKE_CURRENT_BINARY_DIR} ${NUOPC_CHECK_FILE}
      CMAKE_FLAGS "-DINCLUDE_DIRECTORIES=${ESMF_INCLUDE_DIRS}")

    if(NUOPC_FOUND)
      message(STATUS "NUOPC found in ESMF installation")
      set(ESMF_FOUND TRUE)
    else()
      message(WARNING "NUOPC not found in ESMF installation. NUOPC interface will not be built.")
      set(ESMF_FOUND FALSE)
    endif()
  else()
    message(WARNING "ESMFMKFILE environment variable not set. ESMF/NUOPC features may not work correctly.")
    set(ESMF_FOUND FALSE)
  endif()

  # Only continue if ESMF and NUOPC are found
  if(ESMF_FOUND)
    # Create the HEMCO NUOPC interface library
    add_library(HCOI_NUOPC STATIC ${HCOI_NUOPC_SRCS})

    # Define dependencies for this library
    target_link_libraries(HCOI_NUOPC
      PUBLIC HCO
      PUBLIC HCOI_Standalone
    )

    # Add ESMF dependencies if ESMF was found
    # Add include directories
    target_include_directories(HCOI_NUOPC
      PRIVATE ${ESMF_INCLUDE_DIRS}
    )

    # Add library directories to link flags
    foreach(lib_dir ${ESMF_LIBRARY_DIRS})
      target_link_libraries(HCOI_NUOPC PRIVATE "-L${lib_dir}")
    endforeach()

    # Add ESMF libraries
    foreach(lib ${ESMF_LIBRARIES_LIST})
      target_link_libraries(HCOI_NUOPC PRIVATE "${lib}")
    endforeach()

    # Add runtime paths
    if(DEFINED ESMF_RPATH_FLAGS AND NOT "${ESMF_RPATH_FLAGS}" STREQUAL "")
      target_link_libraries(HCOI_NUOPC PRIVATE ${ESMF_RPATH_FLAGS})
    endif()

    # Add ESMF and NUOPC as compile definitions
    target_compile_definitions(HCOI_NUOPC PRIVATE ESMF_ NUOPC_)

    # Specify include directories for the NUOPC interface library
    target_include_directories(HCOI_NUOPC
      INTERFACE ${HEMCO_BINARY_DIR}/mod
    )

    # Specify compilation options for the NUOPC interface library
    target_compile_options(HCOI_NUOPC
      PRIVATE
        ""
        $<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-ffree-line-length-none>
    )

    # Define preprocessor flags for the NUOPC interface
    target_compile_definitions(HCOI_NUOPC
      PRIVATE
      NUOPC_HCO_READ
      NUOPC_HCO_WRITE
    )

    # Create the standalone NUOPC driver executable
    add_executable(hemco_nuopc_standalone ${HCOI_NUOPC_STANDALONE_SRCS})

    # Link the standalone executable with the NUOPC interface library and its dependencies
    target_link_libraries(hemco_nuopc_standalone
      PRIVATE HCOI_NUOPC
      PRIVATE ${ESMF_LIBRARIES_LIST}
    )

    # Add include directories for the standalone executable
    target_include_directories(hemco_nuopc_standalone
      PRIVATE ${ESMF_INCLUDE_DIRS}
    )

    # Add library directories to link flags for the standalone executable
    foreach(lib_dir ${ESMF_LIBRARY_DIRS})
      target_link_libraries(hemco_nuopc_standalone PRIVATE "-L${lib_dir}")
    endforeach()

    # Add runtime paths for the standalone executable
    if(DEFINED ESMF_RPATH_FLAGS AND NOT "${ESMF_RPATH_FLAGS}" STREQUAL "")
      target_link_libraries(hemco_nuopc_standalone PRIVATE ${ESMF_RPATH_FLAGS})
    endif()

    # Add ESMF and NUOPC as compile definitions for the standalone executable
    target_compile_definitions(hemco_nuopc_standalone PRIVATE ESMF_ NUOPC_)

    # Specify compilation options for the standalone executable
    target_compile_options(hemco_nuopc_standalone
      PRIVATE
        ""
        $<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-ffree-line-length-none>
    )
  else()
    message(STATUS "Skipping NUOPC interface build due to missing ESMF/NUOPC")
    set(BUILD_NUOPC_INTERFACE OFF CACHE BOOL "Build HEMCO NUOPC interface" FORCE)
  endif()
endif()
