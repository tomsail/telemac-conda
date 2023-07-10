#include $(top_srcdir)/config/automake.common.2.3.6
include $(top_srcdir)/config/automake.common
include $(srcdir)/../config/medimport.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

AM_CPPFLAGS+=-I$(srcdir)/../include -DMED_MESGERR_FOR_TOOLS

noinst_LTLIBRARIES = libMaj320To330.la
libMaj320To330_la_SOURCES   = \
MAJ_320_330_champs.c

# buildCxxFile =  \
# MAJ_320_330_champsbuilt.cxx

# noinst_LTLIBRARIES += libMaj320To330cxx.la
# libMaj320To330cxx_la_SOURCES   = $(buildCxxFile)

# CLEANFILES = $(buildCxxFile)
EXTRA_DIST = CMakeLists.txt
