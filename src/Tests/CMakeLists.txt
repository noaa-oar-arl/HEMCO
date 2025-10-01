# HEMCO Tests CMakeLists.txt

# Define the test executables
add_executable(hco_esmf_read_test
  hco_esmf_read_test.F90
)

add_executable(hco_nexus_test
  hco_nexus_test.F90
)

# Link against HEMCO libraries
target_link_libraries(hco_esmf_read_test
  hemco_core
  hemco_interfaces
)

target_link_libraries(hco_nexus_test
  hemco_core
  hemco_interfaces
)

# Install the test executables
install(TARGETS hco_esmf_read_test hco_nexus_test DESTINATION bin)