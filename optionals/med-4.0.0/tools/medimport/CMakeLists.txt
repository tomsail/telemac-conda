INCLUDE_DIRECTORIES(
  ${HDF5_INCLUDE_DIRS}
  ${MPI_INCLUDE_DIRS}
)

# Needed by all targets:
ADD_DEFINITIONS(-DMED_MESGERR_FOR_TOOLS)
INCLUDE_DIRECTORIES(include)

# Those two need specific includes:
ADD_SUBDIRECTORY("2.1.x")
ADD_SUBDIRECTORY("2.3.1")

# For the rest:
INCLUDE_DIRECTORIES("${PROJECT_BINARY_DIR}/include"
    "${PROJECT_SOURCE_DIR}/include")

ADD_SUBDIRECTORY("2.3.6")
ADD_SUBDIRECTORY("3.0.0")
ADD_SUBDIRECTORY("3.1.0")
ADD_SUBDIRECTORY("3.2.0")

SET(medimporttop_SOURCES
  libmedimport.c
  MAJ_version.c
)

SET(medimport_SOURCES
  $<TARGET_OBJECTS:_medimport21x>
  $<TARGET_OBJECTS:_medimport231>
  $<TARGET_OBJECTS:_medimport236>
  $<TARGET_OBJECTS:_medimport300>
  $<TARGET_OBJECTS:_medimport310>
  $<TARGET_OBJECTS:_medimport320>
  ${medimporttop_SOURCES}
)

SET(medimport_static_SOURCES
  $<TARGET_OBJECTS:_medimport21x_static>
  $<TARGET_OBJECTS:_medimport231_static>
  $<TARGET_OBJECTS:_medimport236_static>
  $<TARGET_OBJECTS:_medimport300_static>
  $<TARGET_OBJECTS:_medimport310_static>
  $<TARGET_OBJECTS:_medimport320_static>
  ${medimporttop_SOURCES}
)

# Shared version
IF(MEDFILE_BUILD_SHARED_LIBS)
  ADD_LIBRARY(medimportengine SHARED ${medimport_SOURCES})
  IF(WIN32)
    MED_SET_DEFINITIONS(medimportengine medimport_EXPORTS MEDIMPORTCXX_DLL_EXPORTS)
  ENDIF()
  SET_TARGET_PROPERTIES(medimportengine PROPERTIES
    VERSION   0.3.0
    SOVERSION 0.3.0)
  TARGET_LINK_LIBRARIES(medimportengine medC)
  SET_TARGET_PROPERTIES(medimportengine PROPERTIES OUTPUT_NAME medimport)
  INSTALL(TARGETS medimportengine DESTINATION lib)
  SET(_lib_to_link "medimportengine")
ENDIF()

# Static version
IF(MEDFILE_BUILD_STATIC_LIBS)
  ADD_LIBRARY(medimportengine_static STATIC ${medimport_static_SOURCES})
  TARGET_LINK_LIBRARIES(medimportengine_static medC_static)
  MED_SET_DEFINITIONS(medimportengine_static MED3_USESTATIC)
  SET_TARGET_PROPERTIES(medimportengine_static PROPERTIES OUTPUT_NAME medimport)
  INSTALL(TARGETS medimportengine_static DESTINATION lib)
  # Give precendence to the shared object for the linking of 
  # the executable medimport:
  IF(NOT _lib_to_link)
    SET(_lib_to_link "medimportengine_static")
  ENDIF()
ENDIF()

# Only build the medimport executable on certain
# conditions to avoid a nasty name conflict:
IF(NOT (WIN32 AND MEDFILE_BUILD_STATIC_LIBS))
  ADD_EXECUTABLE(medimport medimportcxx.cxx)
  IF(MEDFILE_BUILD_STATIC_LIBS)
    MED_SET_DEFINITIONS(medimport MED3_USESTATIC)
  ENDIF()
  TARGET_LINK_LIBRARIES(medimport ${_lib_to_link})
    
  # Installation
  INSTALL(TARGETS medimport DESTINATION bin )  
ENDIF()

INSTALL(FILES include/medimport_win_dll.h include/MEDimport.h include/MEDimport.hxx include/medimportcxx_win_dll.h DESTINATION include)
