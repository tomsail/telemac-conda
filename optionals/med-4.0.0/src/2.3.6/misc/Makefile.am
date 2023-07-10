include $(top_srcdir)/config/automake.common.2.3.6

DEFAULT_INCLUDES = -I. -I$(srcdir)

noinst_LTLIBRARIES = libmisc.la

libmisc_la_SOURCES   = \
MEDnomEntite.c MEDnomGeometrie.c MEDnomGeometrie30.c MEDnomDataset.c MEDparametresGeometrie.c MEDGeometrieElement.c

libmisc_la_SOURCES += MEDcheckVersion.c MEDversionedApi.cxx MEDversionedApiC.c 

#noinst_LTLIBRARIES += libmisc23v30.la

#libmisc23v30_la_SOURCES = _MED23v30stringConvert.c

EXTRA_DIST = CMakeLists.txt

