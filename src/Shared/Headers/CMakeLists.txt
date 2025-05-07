add_library(HeadersHco STATIC EXCLUDE_FROM_ALL
	hco_charpak_mod.F90
	hco_inquireMod.F90
	hco_precision_mod.F90
)
target_link_libraries(HeadersHco
	PUBLIC HEMCOBuildProperties
)
# Fix include directories to use generator expressions
target_include_directories(HeadersHco
	INTERFACE $<BUILD_INTERFACE:${HEMCO_BINARY_DIR}/mod>
)
