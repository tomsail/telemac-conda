include $(top_srcdir)/config/automake.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

if ENABLE_FORTRAN

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR=$(srcdir) SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no"

#TESTS_ENVIRONMENT += OUTPUT="yes" 
#TESTS_ENVIRONMENT += OUTPUT="yes" CHECKER="libtool\ --mode=execute\ valgrind"

AM_LDFLAGS+= $(FTLDFLAGS)
LIBS += $(FTLIBS)
AM_LIBTOOLFLAGS = --tag=F77

testfdir = $(bindir)/testf

TESTS_LEC = Unittest_MEDlibrary_1.sh  Unittest_MEDsupportMesh_2.sh Unittest_MEDsupportMesh_3.sh \
Unittest_MEDstructElement_2.sh Unittest_MEDstructElement_3.sh \
Unittest_MEDstructElement_5.sh Unittest_MEDstructElement_6.sh \
Unittest_MEDstructElement_8.sh Unittest_MEDstructElement_10.sh \
Unittest_MEDstructElement_11.sh Unittest_MEDlink_2.sh Unittest_MEDlink_3.sh \
Unittest_MEDlocalization_2.sh Unittest_MEDlocalization_3.sh \
Unittest_MEDinterp_2.sh Unittest_MEDinterp_3.sh \
Unittest_MEDparameter_2.sh Unittest_MEDparameter_3.sh

TESTS_ECR = Unittest_MEDfile_1.sh Unittest_MEDsupportMesh_1.sh Unittest_MEDstructElement_1.sh  \
Unittest_MEDstructElement_4.sh Unittest_MEDstructElement_7.sh  Unittest_MEDstructElement_9.sh \
Unittest_MEDprofile_1.sh Unittest_MEDlink_1.sh Unittest_MEDlocalization_1.sh \
Unittest_MEDinterp_1.sh Unittest_MEDparameter_1.sh Unittest_MEDfilter_1.sh \
Unittest_MEDfilter_2.sh

TESTS =  $(TESTS_ECR) $(TESTS_LEC)

EXTRA_DIST = Unittest_MEDlibrary_1.sh Unittest_MEDfile_1.sh Unittest_MEDsupportMesh_1.sh Unittest_MEDsupportMesh_2.sh \
Unittest_MEDsupportMesh_3.sh Unittest_MEDstructElement_1.sh  Unittest_MEDstructElement_2.sh Unittest_MEDstructElement_3.sh \
Unittest_MEDstructElement_4.sh Unittest_MEDstructElement_5.sh Unittest_MEDstructElement_6.sh Unittest_MEDstructElement_7.sh \
Unittest_MEDstructElement_8.sh Unittest_MEDstructElement_9.sh Unittest_MEDstructElement_10.sh \
Unittest_MEDstructElement_11.sh Unittest_MEDprofile_1.sh Unittest_MEDprofile_2.sh Unittest_MEDlink_1.sh \
Unittest_MEDlink_2.sh Unittest_MEDlink_3.sh \
Unittest_MEDlocalization_1.sh Unittest_MEDlocalization_2.sh Unittest_MEDlocalization_3.sh \
Unittest_MEDinterp_1.sh Unittest_MEDinterp_2.sh Unittest_MEDinterp_3.sh \
Unittest_MEDparameter_1.sh Unittest_MEDparameter_2.sh Unittest_MEDparameter_3.sh \
Unittest_MEDfilter_1.sh Unittest_MEDfilter_2.sh

LDADD = $(top_builddir)/src/libmed.la

# Les tests unitaires n'ont pas besoin d'être installés 
#testf_PROGRAMS = 
check_PROGRAMS = \
Unittest_MEDlibrary_1 Unittest_MEDfile_1 Unittest_MEDsupportMesh_1 Unittest_MEDsupportMesh_2 \
Unittest_MEDsupportMesh_3 Unittest_MEDstructElement_1  Unittest_MEDstructElement_2 Unittest_MEDstructElement_3 \
Unittest_MEDstructElement_4  Unittest_MEDstructElement_5  Unittest_MEDstructElement_6 Unittest_MEDstructElement_7 \
Unittest_MEDstructElement_8 Unittest_MEDstructElement_9 Unittest_MEDstructElement_10 \
Unittest_MEDstructElement_11 Unittest_MEDprofile_1 Unittest_MEDprofile_2 Unittest_MEDlink_1 \
Unittest_MEDlink_2 Unittest_MEDlink_3 \
Unittest_MEDlocalization_1 Unittest_MEDlocalization_2 Unittest_MEDlocalization_3 \
Unittest_MEDinterp_1 Unittest_MEDinterp_2 Unittest_MEDinterp_3 \
Unittest_MEDparameter_1 Unittest_MEDparameter_2 Unittest_MEDparameter_3 \
Unittest_MEDfilter_1 Unittest_MEDfilter_2


Unittest_MEDlibrary_1_SOURCES = Unittest_MEDlibrary_1.f 
Unittest_MEDfile_1_SOURCES = Unittest_MEDfile_1.f 
Unittest_MEDsupportMesh_1_SOURCES = Unittest_MEDsupportMesh_1.f 
Unittest_MEDsupportMesh_2_SOURCES = Unittest_MEDsupportMesh_2.f 
Unittest_MEDsupportMesh_3_SOURCES = Unittest_MEDsupportMesh_3.f 
Unittest_MEDstructElement_1_SOURCES = Unittest_MEDstructElement_1.f
Unittest_MEDstructElement_2_SOURCES = Unittest_MEDstructElement_2.f 
Unittest_MEDstructElement_3_SOURCES = Unittest_MEDstructElement_3.f 
Unittest_MEDstructElement_4_SOURCES = Unittest_MEDstructElement_4.f 
Unittest_MEDstructElement_5_SOURCES = Unittest_MEDstructElement_5.f 
Unittest_MEDstructElement_6_SOURCES = Unittest_MEDstructElement_6.f 
Unittest_MEDstructElement_7_SOURCES = Unittest_MEDstructElement_7.f 
Unittest_MEDstructElement_8_SOURCES = Unittest_MEDstructElement_8.f 
Unittest_MEDstructElement_9_SOURCES = Unittest_MEDstructElement_9.f
Unittest_MEDstructElement_10_SOURCES = Unittest_MEDstructElement_10.f 
Unittest_MEDstructElement_11_SOURCES = Unittest_MEDstructElement_11.f 
Unittest_MEDprofile_1_SOURCES = Unittest_MEDprofile_1.f 
Unittest_MEDprofile_2_SOURCES = Unittest_MEDprofile_2.f 
Unittest_MEDlink_1_SOURCES = Unittest_MEDlink_1.f 
Unittest_MEDlink_2_SOURCES = Unittest_MEDlink_2.f 
Unittest_MEDlink_3_SOURCES = Unittest_MEDlink_3.f 
Unittest_MEDlocalization_1_SOURCES = Unittest_MEDlocalization_1.f
Unittest_MEDlocalization_2_SOURCES = Unittest_MEDlocalization_2.f
Unittest_MEDlocalization_3_SOURCES = Unittest_MEDlocalization_3.f
Unittest_MEDinterp_1_SOURCES = Unittest_MEDinterp_1.f
Unittest_MEDinterp_2_SOURCES = Unittest_MEDinterp_2.f
Unittest_MEDinterp_3_SOURCES = Unittest_MEDinterp_3.f
Unittest_MEDparameter_1_SOURCES = Unittest_MEDparameter_1.f
Unittest_MEDparameter_2_SOURCES = Unittest_MEDparameter_2.f
Unittest_MEDparameter_3_SOURCES = Unittest_MEDparameter_3.f
Unittest_MEDfilter_1_SOURCES = Unittest_MEDfilter_1.f
Unittest_MEDfilter_2_SOURCES = Unittest_MEDfilter_2.f


CLEANFILES= \
Unittest_MEDfile_1.med \
Unittest_MEDfile_1.dump \
Unittest_MEDsupportMesh_1.med \
Unittest_MEDsupportMesh_1.dump \
Unittest_MEDstructElement_1.med \
Unittest_MEDstructElement_1.dump \
Unittest_MEDstructElement_4.med \
Unittest_MEDstructElement_4.dump \
Unittest_MEDstructElement_7.med \
Unittest_MEDstructElement_7.dump \
Unittest_MEDstructElement_9.med \
Unittest_MEDstructElement_9.dump \
Unittest_MEDprofile_1.med \
Unittest_MEDprofile_1.dump \
Unittest_MEDlink_1.med \
Unittest_MEDlink_1.dump \
Unittest_MEDlocalization_1.med \
Unittest_MEDlocalization_1.dump \
Unittest_MEDinterp_1.med \
Unittest_MEDinterp_1.dump \
Unittest_MEDparameter_1.med \
Unittest_MEDparameter_1.dump \
Unittest_MEDfilter_1.med \
Unittest_MEDfilter_1.dump \
Unittest_MEDfilter_2.med \
Unittest_MEDfilter_2.dump 


SUBDIRS = dumps.ref .

endif