#include $(top_srcdir)/config/automake.common.2.3.6
include $(top_srcdir)/config/automake.common
include $(srcdir)/../config/medimport.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

AM_CPPFLAGS+=-I$(srcdir)/../include -DMED_MESGERR_FOR_TOOLS

noinst_LTLIBRARIES = libMaj310To320.la
libMaj310To320_la_SOURCES   = \
MAJ_310_320_familles.c

# buildCxxFile =  \
# MAJ_310_320_famillesbuilt.cxx

# noinst_LTLIBRARIES += libMaj310To320cxx.la
# libMaj310To320cxx_la_SOURCES   = $(buildCxxFile)

# CLEANFILES = $(buildCxxFile)
EXTRA_DIST = CMakeLists.txt
