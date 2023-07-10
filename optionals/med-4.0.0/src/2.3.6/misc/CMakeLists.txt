SET(misc236_SOURCES
  MEDnomEntite.c
  MEDnomGeometrie.c
  MEDnomGeometrie30.c
  MEDnomDataset.c
  MEDparametresGeometrie.c
  MEDGeometrieElement.c
  MEDcheckVersion.c
  MEDversionedApi.cxx
  MEDversionedApiC.c
  )

ADD_LIBRARY(_misc236  OBJECT ${misc236_SOURCES})

ADD_LIBRARY(_misc236_static  OBJECT ${misc236_SOURCES})
MED_SET_DEFINITIONS(_misc236_static MED3_USESTATIC)
