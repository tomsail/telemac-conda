INCLUDE(${SWIG_USE_FILE})

INCLUDE_DIRECTORIES(
  ${PYTHON_INCLUDE_DIRS}
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
  ${HDF5_INCLUDE_DIRS}
  ${MPI_INCLUDE_DIRS}
  )

SET(_swig_files 
  medenum_module.i
  medenumtest_module.i
  medequivalence_module.i
  medfamily_module.i
  medfield_module.i
  medfile_module.i
  medfilter_module.i
  medinterp_module.i
  medlibrary_module.i
  medlink_module.i
  medlocalization_module.i
  medmesh_module.i
  medparameter_module.i
  medprofile_module.i
  medsubdomain_module.i
  medstructelement_module.i
)

IF(APPLE)
  SET(PYTHON_LIBRARIES "-undefined dynamic_lookup")
ENDIF(APPLE)

SET(_link_libs
  medC
  ${PYTHON_LIBRARIES}
  )

SET(_install_dir lib/python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}/site-packages/med)

# Ensure we will be able to run the Python tests from the build tree by outputing
# files into the package folder:
SET(CMAKE_SWIG_OUTDIR med)
FILE(COPY "__init__.py" DESTINATION ${PROJECT_BINARY_DIR}/python/med)

FOREACH(f ${_swig_files})
  SET_SOURCE_FILES_PROPERTIES(${f} PROPERTIES CPLUSPLUS ON SWIG_FLAGS "-shadow")

  # Get module name
  STRING(REPLACE "_module.i" "" _module_name ${f})

  # SWIG source file and module name differ - the following needs to be
  # set to avoid unnecessary rebuilds:
  SET_SOURCE_FILES_PROPERTIES(${f} PROPERTIES SWIG_MODULE_NAME ${_module_name})

  SET(SWIG_MODULE_libbatch_EXTRA_DEPS ${SWIG_LB_DPYS})
  SWIG_ADD_MODULE(${_module_name} python ${f})
  SWIG_LINK_LIBRARIES(${_module_name} ${_link_libs})

  INSTALL(TARGETS ${SWIG_MODULE_${_module_name}_REAL_NAME} DESTINATION ${_install_dir})
  INSTALL_AND_COMPILE_PYTHON_FILE("${CMAKE_CURRENT_BINARY_DIR}/med/${_module_name}.py" ${_install_dir})
  
ENDFOREACH()

INSTALL_AND_COMPILE_PYTHON_FILE("${CMAKE_CURRENT_SOURCE_DIR}/__init__.py" ${_install_dir})

