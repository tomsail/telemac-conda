include $(top_srcdir)/config/automake.common

DEFAULT_INCLUDES = -I. -I$(srcdir)
AM_CPPFLAGS+= -DMED_MESGERR_FOR_TOOLS

bin_PROGRAMS = mdump2
mdump2_SOURCES = mdump2.c MED23meshIterators.c MED23fieldIterators.c
nodist_EXTRA_mdump2_SOURCES = dummy.cxx
mdump2_LDADD  = $(top_builddir)/src/libmed.la

EXTRA_DIST = xmdump2.in CMakeLists.txt
bin_SCRIPTS = xmdump2

bin_PROGRAMS += mdump3
mdump3_SOURCES = mdump3.c MED23meshIterators.c MED23fieldIterators.c
nodist_EXTRA_mdump3_SOURCES = dummy.cxx
mdump3_LDADD  = $(top_builddir)/src/libmed.la

EXTRA_DIST += xmdump3.in
bin_SCRIPTS += xmdump3

install-exec-hook:
	(cd $(DESTDIR)$(bindir) && $(LN_S) -f mdump3 mdump && $(LN_S) -f xmdump3 xmdump)
