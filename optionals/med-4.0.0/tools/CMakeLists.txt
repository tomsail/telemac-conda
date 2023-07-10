# Decide if the tools will use dynamic or static linking:
IF(MEDFILE_BUILD_SHARED_LIBS)
  SET(MEDFILE_ENGINE_LIB medC)
ELSE()
  ADD_DEFINITIONS(-DMED3_USESTATIC)
  SET(MEDFILE_ENGINE_LIB medC_static)
ENDIF()

# Definitions used in tools:
ADD_DEFINITIONS(-DMED_MESGERR_FOR_TOOLS)

ADD_SUBDIRECTORY(mdump)
ADD_SUBDIRECTORY(medconforme)
ADD_SUBDIRECTORY(medimport)
