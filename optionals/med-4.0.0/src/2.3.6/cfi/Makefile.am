include $(top_srcdir)/config/automake.common.2.3.6

DEFAULT_INCLUDES = -I. -I$(srcdir)

noinst_LTLIBRARIES = libcfi.la
libcfi_la_SOURCES   = equcf.c fichcf.c intercf.c jntcf.c rescf.c \
entmaacf.c famcf.c maacf.c procf.c scalairecf.c gausscf.c exitcf.c
libcfi_la_SOURCES   += famcf231.c famcf232.c
libcfi_la_SOURCES   += MEDversionedApiF.cxx

EXTRA_DIST = CMakeLists.txt

