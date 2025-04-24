# HEMCO/src/Interfaces/ESMX/CMakeLists.txt

# Only build the ESMX interface if it's enabled
if(BUILD_ESMX_INTERFACE)
  # List of files to compile into the HEMCO ESMX interface library
  set(HCOI_ESMX_SRCS
    HCOI_ESMxInterfaceMod.F90
    HCOI_ESMxGridCompMod.F90
  )

  # Create the HEMCO ESMX interface library
  add_library(HCOI_ESMX STATIC ${HCOI_ESMX_SRCS})

  # Define dependencies for this library
  target_link_libraries(HCOI_ESMX
    PUBLIC HCO
    PUBLIC HCOI_NUOPC
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

  # Use the ESMF settings from the NUOPC interface if available
  if(ESMF_FOUND)
    # Add include directories
    target_include_directories(HCOI_ESMX
      PRIVATE ${ESMF_INCLUDE_DIRS}
    )

    # Add library directories to link flags
    foreach(lib_dir ${ESMF_LIBRARY_DIRS})
      target_link_libraries(HCOI_ESMX PRIVATE "-L${lib_dir}")
    endforeach()

    # Add ESMF libraries
    foreach(lib ${ESMF_LIBRARIES_LIST})
      target_link_libraries(HCOI_ESMX PRIVATE "${lib}")
    endforeach()

    # Add runtime paths
    if(DEFINED ESMF_RPATH_FLAGS AND NOT "${ESMF_RPATH_FLAGS}" STREQUAL "")
      target_link_libraries(HCOI_ESMX PRIVATE ${ESMF_RPATH_FLAGS})
    endif()

    # Add ESMF as compile definition
    target_compile_definitions(HCOI_ESMX PRIVATE ESMF_)
  endif()

  # Specify include directories for the ESMX interface library
  target_include_directories(HCOI_ESMX
    INTERFACE ${HEMCO_BINARY_DIR}/mod
  )

  # Specify compilation options for the ESMX interface library
  target_compile_options(HCOI_ESMX
    PRIVATE
    ""
    $<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-ffree-line-length-none>
  )

  # Define preprocessor flags for the ESMX interface
  target_compile_definitions(HCOI_ESMX
    PRIVATE
    ESMX_HCO_READ
    ESMX_HCO_WRITE
  )
endif()