# HEMCO/src/Interfaces/Shared/CMakeLists.txt

add_library(HCOI_Shared STATIC EXCLUDE_FROM_ALL
        hco_interface_common.F90
)

target_link_libraries(HCOI_Shared
	PUBLIC HCOX
)

# Use generator expressions to avoid build directory paths
target_include_directories(HCOI_Shared
	INTERFACE $<BUILD_INTERFACE:${HEMCO_BINARY_DIR}/mod>
)

# Add ESMF include directories when NUOPC interface is enabled
if(BUILD_NUOPC_INTERFACE AND ESMF_FOUND)
    target_include_directories(HCOI_Shared
        PUBLIC ${ESMF_INCLUDE_DIRS}
    )
    target_link_libraries(HCOI_Shared
        PUBLIC ${ESMF_LIBRARIES}
    )
endif()

target_compile_options(HCOI_Shared
	PRIVATE
		""
		$<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-ffree-line-length-none>
)
