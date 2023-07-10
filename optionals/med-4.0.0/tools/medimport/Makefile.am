#include $(top_srcdir)/config/automake.common.2.3.6
include $(top_srcdir)/config/automake.common
include $(srcdir)/config/medimport.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

AUTOMAKE_OPTIONS = foreign

SUBDIRS 	 = include 2.1.x 2.3.1 2.3.6 3.0.0 3.1.0 3.2.0 .

AM_CPPFLAGS+= -I$(srcdir)/include -DMED_MESGERR_FOR_TOOLS

lib_LTLIBRARIES   = libmedimport.la 
libmedimport_la_SOURCES = libmedimport.c MAJ_version.c
nodist_EXTRA_libmedimport_la_SOURCES = dummy1.cxx
libmedimport_la_LDFLAGS = --no-undefined -version-info @MEDIMPORT_LIBTOOL_MAJ@:@MEDIMPORT_LIBTOOL_MIN@:@MEDIMPORT_LIBTOOL_REL@
libmedimport_la_LIBADD  = $(top_builddir)/src/libmed.la \
$(top_builddir)/src/2.3.6/ci/libci.la $(top_builddir)/src/2.3.6/hdfi/libhdfi.la $(top_builddir)/src/2.3.6/misc/libmisc.la 2.1.x/libMaj21To22.la 2.3.1/libMaj231To232.la 2.3.6/libMaj236To300.la 3.0.0/libMaj300To310.la 3.1.0/libMaj310To320.la 3.2.0/libMaj320To330.la \
$(top_builddir)/src/hdfi/MEDattrFermer.lo \
$(top_builddir)/src/hdfi/MEDattrNumLire.lo \
$(top_builddir)/src/hdfi/MEDattrOuvrir.lo \
$(top_builddir)/src/hdfi/MEDattrStringLire.lo \
$(top_builddir)/src/hdfi/MEDdatagroupCreer.lo \
$(top_builddir)/src/hdfi/MEDdatagroupFermer.lo \
$(top_builddir)/src/hdfi/MEDdatagroupLienCreer.lo \
$(top_builddir)/src/hdfi/MEDdatagroupLienSupprimer.lo \
$(top_builddir)/src/hdfi/MEDdatagroupOuvrir.lo \
$(top_builddir)/src/hdfi/MEDdatasetFermer.lo \
$(top_builddir)/src/hdfi/MEDdatasetOuvrir.lo \
$(top_builddir)/src/hdfi/MEDfichierDemonter.lo \
$(top_builddir)/src/hdfi/MEDfichierFermer.lo \
$(top_builddir)/src/hdfi/MEDfichierMonter.lo \
$(top_builddir)/src/hdfi/MEDmodeErreurVerrouiller.lo \
$(top_builddir)/src/misc/MEDmodeAccesC.lo \
$(top_builddir)/src/misc/MEDexitIf.lo

#lib_LTLIBRARIES   += libmedimportcxx.la 
#libmedimportcxx_la_SOURCES = libmedimportbuilt.cxx
#libmedimportcxx_la_LDFLAGS = -L$(top_builddir)/src/ -lmed -no-undefined -version-info 0:0:0
#libmedimportcxx_la_LIBADD  = 2.1.x/libMaj21To22cxx.la 2.3.1/libMaj231To232cxx.la
#CLEANFILES = libmedimportbuilt.cxx

bin_PROGRAMS = medimport
medimport_SOURCES = medimport.c
nodist_EXTRA_medimport_SOURCES = dummy2.cxx
medimport_LDADD  = ./libmedimport.la $(top_builddir)/src/libmed.la

noinst_PROGRAMS = medimportcxx
medimportcxx_SOURCES = medimportcxx.cxx
#medimportcxx_LDADD  = ./libmedimportcxx.la $(top_builddir)/src/libmed.la
medimportcxx_LDADD  = ./libmedimport.la $(top_builddir)/src/libmed.la

EXTRA_DIST = CMakeLists.txt CMakeLists.txt.in

