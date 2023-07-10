if(MEDFILE_BUILD_DOC)
  #-- Add a custom target to run Doxygen when ever the project is built
  add_custom_target (Docs ALL
     COMMAND ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/doc/dox/Doxyfile.cfg
     SOURCES ${PROJECT_BINARY_DIR}/doc/dox/Doxyfile.cfg)
  # IF you do NOT want the documentation to be generated EVERY time you build the project
  # then leave out the 'ALL' keyword from the above command.
endif()
