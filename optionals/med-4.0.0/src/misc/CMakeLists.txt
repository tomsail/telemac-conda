SET(misc_SOURCES
  MEDsetFilter.c
  MEDcstring.c
  MED1cstring.c
  MED2cstring.c
  MEDcstringFree.c
  MEDfstring.c
  _MEDc2fString.c
  MEDexitIf.c
  MEDversionedApi3.cxx
  MEDgetGeometryTypeFromName.cxx
  MEDversionedApi3C.c
  MEDmodeAcces.cxx
  MEDmodeAccesC.c
  MEDerreur.cxx
  MEDfieldChecked.cxx
  MEDfileVersion.cxx
  _MEDgetGeometricParameter.c
  _MEDgetDynGeometricParameter.c
  _MEDgetDatasetName.c
  _MEDgetDatasetParameter.c
  _MEDgetDatatype.c
  _MEDmeshtypeCompatibility.c
  _MEDgetEntityTypeName.c
  _MEDgetFieldEntityGeoTypeName.c
  _MEDgetInternalGeometryTypeName.c
  _MEDgetInternalGeometryTypeName30.c
  _MEDgetInternalGeometryTypeName33.c
  _MEDgetExternalGeometryTypeName.c
  _MEDcheckVersion30.c
  _MED23v30stringConvert.c
  _MEDgetSupportMeshNbOfEntities.c
  )
  
ADD_LIBRARY(_misc OBJECT ${misc_SOURCES})

ADD_LIBRARY(_misc_static OBJECT ${misc_SOURCES})
MED_SET_DEFINITIONS(_misc_static MED3_USESTATIC)
