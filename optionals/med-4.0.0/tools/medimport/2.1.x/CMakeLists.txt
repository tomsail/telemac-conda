SET(medimport21x_SOURCES
  MAJ_21_22_chaine.c
  MAJ_21_22_champs.c
  MAJ_21_22_elements_maillage.c
  MAJ_21_22_familles_maillage.c
  MAJ_21_22_localisation_Gauss.c
  MAJ_21_22_maillages.c
  MAJ_21_22_noeuds_maillage.c
  MAJ_21_22_profils.c
  MED21attrNumLire.c
  MED21datasetNumLire.c
)

# Include are specific for the 2.3.6 version - make sure they get incl first:
INCLUDE_DIRECTORIES(BEFORE 
    "${PROJECT_BINARY_DIR}/include/2.3.6"
    "${PROJECT_SOURCE_DIR}/include/2.3.6")

ADD_DEFINITIONS(-DMED_API_23)

# On Win platform, we need to make sure symbols are properly exported in
# the intermediary OBJECT targets
# The definition "-D<target>_EXPORTS" is normally added automatically by CMake
# when building Win DLL,
# but this doesn't work when using intermediary targets (ADD_LIBRARY(... OBJECT ...)
IF(WIN32)
  ADD_DEFINITIONS(-Dmedimport_EXPORTS)
ENDIF()

ADD_LIBRARY(_medimport21x OBJECT ${medimport21x_SOURCES})

ADD_LIBRARY(_medimport21x_static OBJECT ${medimport21x_SOURCES})
MED_SET_DEFINITIONS(_medimport21x_static MED3_USESTATIC)
