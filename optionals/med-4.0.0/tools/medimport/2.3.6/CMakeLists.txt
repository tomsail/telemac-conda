SET(medimport236_SOURCES
    MAJ_236_300_chaine.c
    MAJ_236_300_champs.c
    MAJ_236_300_entites.c
    MAJ_236_300_equivalence.c
    MAJ_236_300_fieldOnEntity.c
    MAJ_236_300_joint.c
    MAJ_236_300_maillages.c
    MAJ_236_300_mesh_datasets.c
    MAJ_236_300_string_datasets.c
    MED30linkWr.c
    _MEDconvertStringDatasets.c
)

# On Win platform, we need to make sure symbols are properly exported in
# the intermediary OBJECT targets
# The definition "-D<target>_EXPORTS" is normally added automatically by CMake
# when building Win DLL,
# but this doesn't work when using intermediary targets (ADD_LIBRARY(... OBJECT ...)
IF(WIN32)
  ADD_DEFINITIONS(-Dmedimport_EXPORTS)
ENDIF()

ADD_LIBRARY(_medimport236 OBJECT ${medimport236_SOURCES})

ADD_LIBRARY(_medimport236_static OBJECT ${medimport236_SOURCES})
MED_SET_DEFINITIONS(_medimport236_static MED3_USESTATIC)
