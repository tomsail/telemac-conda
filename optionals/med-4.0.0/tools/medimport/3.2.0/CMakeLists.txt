SET(medimport320_SOURCES
  MAJ_320_330_champs.c
)

# On Win platform, we need to make sure symbols are properly exported in
# the intermediary OBJECT targets
# The definition "-D<target>_EXPORTS" is normally added automatically by CMake
# when building Win DLL,
# but this doesn't work when using intermediary targets (ADD_LIBRARY(... OBJECT ...)
IF(WIN32)
  ADD_DEFINITIONS(-Dmedimport_EXPORTS)
ENDIF()

ADD_LIBRARY(_medimport320 OBJECT ${medimport320_SOURCES})

ADD_LIBRARY(_medimport320_static OBJECT ${medimport320_SOURCES})
MED_SET_DEFINITIONS(_medimport320_static MED3_USESTATIC)
