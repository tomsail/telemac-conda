include $(top_srcdir)/config/automake.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

noinst_LTLIBRARIES = libcfi.la
libcfi_la_SOURCES   =  librarycf.c filecf.c meshcf.c medfamilycf.c medequivalencecf.c medfiltercf.c medfieldcf.c medlinkcf.c medlocalizationcf.c medprofilecf.c medparametercf.c medsupportcf.c medjointcf.c medinterpcf.c medstructelementcf.c

libcfi_la_SOURCES   += MEDversionedApi3F.cxx

if WITHPMED
libcfi_la_SOURCES   +=  parfilecf.c 
endif

EXTRA_DIST = CMakeLists.txt

