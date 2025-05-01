# HEMCO/src/Interfaces/NUOPC/CMakeLists.txt

#=============================================================================
# NUOPC Interface for HEMCO
#=============================================================================

# List of source files needed for the NUOPC interface module
# This will be compiled into the HEMCO library.
add_library(HCOI_ESMF_NUOPC STATIC
    hcoi_esmf_nuopc_mod.F90
)

# Add dependencies
target_link_libraries(HCOI_ESMF_NUOPC
    PUBLIC HCOI_Shared
    PUBLIC HEMCO_Core
)

# Add include directories
target_include_directories(HCOI_ESMF_NUOPC
    PUBLIC ${HEMCO_BINARY_DIR}/mod
    PUBLIC ${ESMF_INCLUDE_DIRS}
)

# Add ESMF libraries
if(ESMF_FOUND)
    target_link_libraries(HCOI_ESMF_NUOPC
        PUBLIC ${ESMF_LIBRARIES}
    )
endif()

# Add compiler options
target_compile_options(HCOI_ESMF_NUOPC
    PRIVATE
        ""
        $<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-ffree-line-length-none>
)

#=============================================================================
# NUOPC Main Program (standalone executable)
#=============================================================================
if(NOT HEMCO_EXTERNAL_CONFIG)
    # Standalone NUOPC executable that uses the ESMF_NUOPC interface
    add_executable(hemco_nuopc
        hemco_nuopc_main.F90
    )

    # Link to the interface library and HEMCO core
    target_link_libraries(hemco_nuopc
        PRIVATE HCOI_ESMF_NUOPC
        PRIVATE HCOI_Shared
        PRIVATE HEMCO_Core
    )

    # Set properties for the executable
    set_target_properties(hemco_nuopc PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
    )

    # Make sure the build system knows about this executable
    # but only include it in 'all' target if explicitly requested
    if(NOT BUILD_WITHOUT_RUNDIR)
        set_target_properties(hemco_nuopc PROPERTIES
            EXCLUDE_FROM_ALL FALSE
        )
    else()
        set_target_properties(hemco_nuopc PROPERTIES
            EXCLUDE_FROM_ALL TRUE
        )
    endif()
endif()