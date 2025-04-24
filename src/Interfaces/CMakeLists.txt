# HEMCO/src/Interfaces/CMakeLists.txt

# Always build shared interface files
add_subdirectory(Shared)

# For now, always build standalone interface files
add_subdirectory(Standalone)

# if GCHP CTM:
if(HEMCO_EXTERNAL_CONFIG AND MAPL_ESMF)
    add_subdirectory(MAPL_ESMF)
endif()

# if org GEOS:
# option (BUILD_GEOS_INTERFACE FALSE)

if (BUILD_GEOS_INTERFACE)
    add_subdirectory(GEOS)
endif()

# Add NUOPC Interface
if (BUILD_NUOPC_INTERFACE)
    add_subdirectory(NUOPC)
endif()

# Add ESMx Interface
if (BUILD_ESMX_INTERFACE)
    add_subdirectory(ESMX)
endif()
