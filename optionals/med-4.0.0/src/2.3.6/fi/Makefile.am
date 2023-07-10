include $(top_srcdir)/config/automake.common.2.3.6

#Regénère les objets fortran si le compilateur Fortran a changé dans le Makefile
$(libfi_la_OBJECTS): Makefile

noinst_LTLIBRARIES = libfi.la
libfi_la_SOURCES = equf.f     fichf.f  interf.f jntf.f resf_i.f resf_r.f \
entmaaf.f  famf.f   maaf.f    prof.f	scalaire.f   gauss.f exit.f

EXTRA_DIST = CMakeLists.txt

