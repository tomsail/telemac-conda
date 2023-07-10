#include $(top_srcdir)/config/automake.common.2.3.6
include $(top_srcdir)/config/automake.common
include $(srcdir)/../config/medimport.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

AM_CPPFLAGS+=-I$(srcdir)/../include -DMED_MESGERR_FOR_TOOLS

noinst_LTLIBRARIES = libMaj236To300.la
libMaj236To300_la_SOURCES   = \
MAJ_236_300_maillages.c\
MAJ_236_300_equivalence.c\
MAJ_236_300_joint.c \
MAJ_236_300_entites.c\
MAJ_236_300_mesh_datasets.c\
MAJ_236_300_string_datasets.c\
MAJ_236_300_chaine.c \
_MEDconvertStringDatasets.c \
MAJ_236_300_champs.c \
MAJ_236_300_fieldOnEntity.c \
MED30linkWr.c

# buildCxxFile =  \
# MAJ_236_300_maillagesbuilt.cxx\
# MAJ_236_300_equivalencebuilt.cxx\
# MAJ_236_300_jointbuilt.cxx \
# MAJ_236_300_entitesbuilt.cxx\
# MAJ_236_300_mesh_datasetsbuilt.cxx\
# MAJ_236_300_string_datasetsbuilt.cxx\
# MAJ_236_300_chainebuilt.cxx \
# _MEDconvertStringDatasetsbuilt.cxx \
# MAJ_236_300_champsbuilt.cxx \
# MAJ_236_300_fieldOnEntitybuilt.cxx \
# MED30linkWrbuilt.cxx

# noinst_LTLIBRARIES += libMaj236To300cxx.la
# libMaj236To300cxx_la_SOURCES   = $(buildCxxFile)

# CLEANFILES = $(buildCxxFile)
EXTRA_DIST = CMakeLists.txt
