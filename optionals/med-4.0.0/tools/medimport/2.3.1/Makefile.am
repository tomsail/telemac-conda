include $(top_srcdir)/config/automake.common.2.3.6
include $(srcdir)/../config/medimport.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

AM_CPPFLAGS+=-I$(srcdir)/../include -DMED_MESGERR_FOR_TOOLS

noinst_LTLIBRARIES = libMaj231To232.la
libMaj231To232_la_SOURCES   = \
MAJ_231_232_maillages.c\
MAJ_231_232_champs.c\
MAJ_231_232_chaine.c\
MED231champLireEtUnlink.c\
MED231datasetNumEcrire.c\
MED231datasetNumLire.c\
MED231champNormaliser.c\
MED231champRefInfoEtRenMaa.c\
MED231champInfoEtRen.c

# buildCxxFile =  \
# MAJ_231_232_maillagesbuilt.cxx\
# MAJ_231_232_champsbuilt.cxx\
# MAJ_231_232_chainebuilt.cxx\
# MED231champLireEtUnlinkbuilt.cxx\
# MED231datasetNumEcrirebuilt.cxx\
# MED231datasetNumLirebuilt.cxx\
# MED231champNormaliserbuilt.cxx\
# MED231champRefInfoEtRenMaabuilt.cxx\
# MED231champInfoEtRenbuilt.cxx

# noinst_LTLIBRARIES += libMaj231To232cxx.la
# libMaj231To232cxx_la_SOURCES   = $(buildCxxFile)

# CLEANFILES = $(buildCxxFile)
EXTRA_DIST = CMakeLists.txt
