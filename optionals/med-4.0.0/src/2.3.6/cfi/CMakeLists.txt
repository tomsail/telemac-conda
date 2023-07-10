SET(cfi236_SOURCES
  equcf.c
  fichcf.c
  intercf.c
  jntcf.c
  rescf.c
  entmaacf.c
  famcf.c
  maacf.c
  procf.c
  scalairecf.c
  gausscf.c
  exitcf.c
  famcf231.c
  famcf232.c
  MEDversionedApiF.cxx
  )
  
ADD_DEFINITIONS(-DNOGDI)
  
# Intermediary targets
ADD_LIBRARY(_cfi236 OBJECT ${cfi236_SOURCES})

ADD_LIBRARY(_cfi236_static OBJECT ${cfi236_SOURCES})
MED_SET_DEFINITIONS(_cfi236_static MED3_USESTATIC)
