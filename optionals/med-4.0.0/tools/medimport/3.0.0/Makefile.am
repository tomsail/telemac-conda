#include $(top_srcdir)/config/automake.common.2.3.6
include $(top_srcdir)/config/automake.common
include $(srcdir)/../config/medimport.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

AM_CPPFLAGS+=-I$(srcdir)/../include -DMED_MESGERR_FOR_TOOLS

noinst_LTLIBRARIES = libMaj300To310.la
libMaj300To310_la_SOURCES   = \
MAJ_300_310_champs.c

# buildCxxFile =  \
# MAJ_300_310_champsbuilt.cxx

# noinst_LTLIBRARIES += libMaj300To310cxx.la
# libMaj300To310cxx_la_SOURCES   = $(buildCxxFile)

# CLEANFILES = $(buildCxxFile)
EXTRA_DIST = CMakeLists.txt
