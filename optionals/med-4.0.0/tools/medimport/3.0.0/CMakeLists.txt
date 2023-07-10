SET(medimport300_SOURCES
  MAJ_300_310_champs.c
)

# On Win platform, we need to make sure symbols are properly exported in
# the intermediary OBJECT targets
# The definition "-D<target>_EXPORTS" is normally added automatically by CMake
# when building Win DLL,
# but this doesn't work when using intermediary targets (ADD_LIBRARY(... OBJECT ...)
IF(WIN32)
  ADD_DEFINITIONS(-Dmedimport_EXPORTS)
ENDIF()

ADD_LIBRARY(_medimport300 OBJECT ${medimport300_SOURCES})

ADD_LIBRARY(_medimport300_static OBJECT ${medimport300_SOURCES})
MED_SET_DEFINITIONS(_medimport300_static MED3_USESTATIC)
