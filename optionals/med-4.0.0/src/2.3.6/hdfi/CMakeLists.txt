SET(hdfi236_SOURCES
  MEDattrStringEcrire.c
  MEDattrNumEcrire.c
  MEDdatasetNumEcrire231.c
  MEDdatasetNumEcrire232.c
  MEDdatasetNumEcrire.c
  MEDdatasetNumLire231.c
  MEDdatasetNumLire232.c
  MEDdatasetNumLire.c
  MEDdatasetStringEcrire.c
  MEDdatasetStringLire.c
  MEDfichierCreer.c
  MEDfichierOuvrir.c
  MEDindiceInfo.c
  MEDindiceNum.c
  MEDnObjets.c
  MEDobjetIdentifer.c
  )
  
ADD_LIBRARY(_hdfi236  OBJECT ${hdfi236_SOURCES})

ADD_LIBRARY(_hdfi236_static  OBJECT ${hdfi236_SOURCES})
MED_SET_DEFINITIONS(_hdfi236_static MED3_USESTATIC)
