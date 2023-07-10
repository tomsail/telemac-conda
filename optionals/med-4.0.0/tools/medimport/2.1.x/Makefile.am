include $(top_srcdir)/config/automake.common.2.3.6
include $(srcdir)/../config/medimport.common

DEFAULT_INCLUDES = -I. -I$(srcdir)


AM_CPPFLAGS+=-I$(srcdir)/../include -DMED_MESGERR_FOR_TOOLS

noinst_LTLIBRARIES = libMaj21To22.la
libMaj21To22_la_SOURCES   = \
MAJ_21_22_chaine.c \
MAJ_21_22_champs.c            \
MAJ_21_22_localisation_Gauss.c\
MAJ_21_22_profils.c\
MAJ_21_22_elements_maillage.c\
MAJ_21_22_maillages.c\
MED21attrNumLire.c\
MAJ_21_22_familles_maillage.c\
MAJ_21_22_noeuds_maillage.c\
MED21datasetNumLire.c

# buildCxxFile =  \
# MAJ_21_22_chainebuilt.cxx \
# MAJ_21_22_champsbuilt.cxx            \
# MAJ_21_22_localisation_Gaussbuilt.cxx\
# MAJ_21_22_profilsbuilt.cxx\
# MAJ_21_22_elements_maillagebuilt.cxx\
# MAJ_21_22_maillagesbuilt.cxx\
# MED21attrNumLirebuilt.cxx\
# MAJ_21_22_familles_maillagebuilt.cxx\
# MAJ_21_22_noeuds_maillagebuilt.cxx\
# MED21datasetNumLirebuilt.cxx

# noinst_LTLIBRARIES += libMaj21To22cxx.la
# libMaj21To22cxx_la_SOURCES   = $(buildCxxFile)

# CLEANFILES = $(buildCxxFile)
EXTRA_DIST = CMakeLists.txt
