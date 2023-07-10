include $(top_srcdir)/config/automake.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

noinst_LTLIBRARIES = libmisc.la

libmisc_la_SOURCES   = MEDsetFilter.c

libmisc_la_SOURCES   += \
MEDcstring.c MED1cstring.c  \
MED2cstring.c               \
MEDcstringFree.c           \
MEDfstring.c _MEDc2fString.c  MEDexitIf.c

libmisc_la_SOURCES +=   MEDversionedApi3.cxx MEDgetGeometryTypeFromName.cxx \
			MEDversionedApi3C.c \
			MEDmodeAcces.cxx MEDmodeAccesC.c \
			MEDfileVersion.cxx MEDfieldChecked.cxx MEDerreur.cxx

libmisc_la_SOURCES += \
_MEDgetGeometricParameter.c \
_MEDgetDynGeometricParameter.c \
_MEDgetDatasetName.c \
_MEDgetDatasetParameter.c \
_MEDgetDatatype.c \
_MEDmeshtypeCompatibility.c \
_MEDgetEntityTypeName.c \
_MEDgetFieldEntityGeoTypeName.c \
_MEDgetInternalGeometryTypeName.c \
_MEDgetInternalGeometryTypeName30.c \
_MEDgetInternalGeometryTypeName33.c \
_MEDgetExternalGeometryTypeName.c \
_MEDcheckVersion30.c \
_MED23v30stringConvert.c \
_MEDgetSupportMeshNbOfEntities.c

EXTRA_DIST = CMakeLists.txt

