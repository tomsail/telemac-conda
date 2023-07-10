include $(top_srcdir)/config/automake.common

DEFAULT_INCLUDES = -I. -I$(srcdir)
AM_CPPFLAGS+= -DMED_MESGERR_FOR_TOOLS

bin_PROGRAMS = medconforme
medconforme_SOURCES = medconforme.c
nodist_EXTRA_medconforme_SOURCES = dummy.cxx
medconforme_LDADD  = $(top_builddir)/src/libmed.la
EXTRA_DIST = CMakeLists.txt

