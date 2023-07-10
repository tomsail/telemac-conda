SET(medimport310_SOURCES
  MAJ_310_320_familles.c
)

# On Win platform, we need to make sure symbols are properly exported in
# the intermediary OBJECT targets
# The definition "-D<target>_EXPORTS" is normally added automatically by CMake
# when building Win DLL,
# but this doesn't work when using intermediary targets (ADD_LIBRARY(... OBJECT ...)
IF(WIN32)
  ADD_DEFINITIONS(-Dmedimport_EXPORTS)
ENDIF()

ADD_LIBRARY(_medimport310 OBJECT ${medimport310_SOURCES})

ADD_LIBRARY(_medimport310_static OBJECT ${medimport310_SOURCES})
MED_SET_DEFINITIONS(_medimport310_static MED3_USESTATIC)
