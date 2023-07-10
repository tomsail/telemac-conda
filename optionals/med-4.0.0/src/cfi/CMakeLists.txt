SET(cfi_SOURCES
  librarycf.c
  medlocalizationcf.c
  medprofilecf.c
  medinterpcf.c
  medlinkcf.c
  medequivalencecf.c
  medfieldcf.c
  meshcf.c
  medfamilycf.c
  medstructelementcf.c
  medsupportcf.c
  medjointcf.c
  medparametercf.c
  medfiltercf.c
  filecf.c
  MEDversionedApi3F.cxx
  )

IF(MEDFILE_USE_MPI)
  LIST(APPEND cfi_SOURCES parfilecf.c)
ENDIF(MEDFILE_USE_MPI)

ADD_DEFINITIONS(-DNOGDI)
  
# Intermediary target
ADD_LIBRARY(_cfi OBJECT ${cfi_SOURCES})

ADD_LIBRARY(_cfi_static OBJECT ${cfi_SOURCES})
MED_SET_DEFINITIONS(_cfi_static MED3_USESTATIC)
 
