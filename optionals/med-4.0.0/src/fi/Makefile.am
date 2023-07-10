include $(top_srcdir)/config/automake.common


#Regénère les objets fortran si le compilateur Fortran a changé dans le Makefile
$(libfi_la_OBJECTS): Makefile

DEFAULT_INCLUDES = -I. -I$(srcdir)

noinst_LTLIBRARIES = libfi.la
libfi_la_SOURCES = medflibrary.f medfile.f medmesh.f medfamily.f medequivalence.f medfilter.f medfield.f medlink.f medlocalization.f medprofile.f medparameter.f medsupport.f medjoint.f medinterp.f medstructelement.f

if WITHPMED
libfi_la_SOURCES += medparfile.f
endif

EXTRA_DIST = CMakeLists.txt

