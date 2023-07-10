include236dir = $(includedir)/2.3.6

## For having included files in DISTribution & INSTallation (public headers)
include236_HEADERS =  med.hf med_proto.h med23v30_proto.h \
		 medC_win_dll.h MEDerreur.hxx med_exit_if.h

## For having included files in  INSTallation (public headers)
nodist_include236_HEADERS = med.h med23v30.h med_utils.h

## For having included files in DISTribution only (private headers)
noinst_HEADERS = med_outils.h  med_misc.h med23v30_misc.h med23v30_hdfi.h \
	med_hdfi.h  med_versioned_proto.h \
	med_versioned_hdfi.h med_versioned_cfi.h med_versioned.h \
	MEDversionedApi.hxx MEDmodeAcces.hxx

#nobase_include_HEADERS = med/med_config.h

BUILT_SOURCES = med_config.h
med_config.h: Makefile
	${CP} -f $(builddir)/../$@  $@ 

CLEANFILES = med_config.h med.h med_utils.h 
MAINTAINERCLEANFILES = med_config.h.in Makefile.in
EXTRA_DIST=CMakeLists.txt

