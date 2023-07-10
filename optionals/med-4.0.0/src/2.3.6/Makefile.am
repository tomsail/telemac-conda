include $(top_srcdir)/config/automake.common.2.3.6

DEFAULT_INCLUDES = -I. -I$(srcdir)

AUTOMAKE_OPTIONS = foreign


if ENABLE_FORTRAN
SUBDIRS          = ci misc hdfi fi cfi .
else
SUBDIRS 	 = ci misc hdfi .
endif

noinst_LTLIBRARIES   = libmed23.la libmedC23.la libmedVV.la

nodist_EXTRA_libmed23_la_SOURCES = dummy.cxx
if ENABLE_FORTRAN
libmed23_la_LIBADD  = ci/libci.la misc/libmisc.la hdfi/libhdfi.la fi/libfi.la cfi/libcfi.la
else
libmed23_la_LIBADD  = ci/libci.la misc/libmisc.la hdfi/libhdfi.la
endif
libmed23_la_SOURCES  = MEDaffiche.c 

nodist_EXTRA_libmedC23_la_SOURCES = dummy.cxx
libmedC23_la_LIBADD  = ci/libci.la misc/libmisc.la hdfi/libhdfi.la
libmedC23_la_SOURCES  = MEDaffiche.c 

nodist_EXTRA_libmedVV_la_SOURCES = dummy.cxx
libmedVV_la_LIBADD  = ci/libci23v30.la 
# misc/libmisc23v30.la
libmedVV_la_SOURCES  = foo3.c 

EXTRA_DIST = CMakeLists.txt

