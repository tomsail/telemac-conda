include $(top_srcdir)/config/automake.common

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no" 

#TESTS_ENVIRONMENT += OUTPUT="yes" CHECKER="libtool\ --mode=execute\ valgrind"

unittestsdir= $(bindir)/unittests

##check_PROGRAMS
##bin_PROGRAMS

AM_CFLAGS = -DDEF_LECT_ECR
#-DUSER_INTERLACE=MED_FULL_INTERLACE
#-DDEF_LECT_AJOUT

# Les tests unitaires n'ont pas besoin d'être installés 
#unittests_PROGRAMS =
check_PROGRAMS = \
Unittest_MEDlibrary_1 \
Unittest_MEDfile_1 \
Unittest_MEDparameter_1 \
Test_MEDsupportMesh \
Test_MEDstructElementCr \
Test_MEDstructElementConstAttWithProfileWr \
Test_MEDstructElementVarAttCr \
Test_MEDmeshStructElementVarAttWr \
Test_MEDnStructElement \
Test_MEDstructElementInfoByName \
Test_MEDstructElementInfo \
Test_MEDstructElementConstAttInfo \
Test_MEDstructElementConstAttRd \
Test_MEDstructElementVarAttInfo \
Test_MEDmeshStructElementVarAttRd \
Test_MEDmeshEntityInfo

LDADD = $(top_builddir)/src/libmedC.la

nodist_EXTRA_Unittest_MEDfile_1_SOURCES = dummy.cxx
nodist_EXTRA_Unittest_MEDlibrary_1_SOURCES = dummy.cxx
nodist_EXTRA_Unittest_MEDparameter_1_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDsupportMesh_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDstructElementCr_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDstructElementConstAttWithProfileWr_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDnStructElement_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDstructElementInfoByName_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDstructElementInfo_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDstructElementConstAttInfo_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDstructElementConstAttRd_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDstructElementVarAttCr_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDstructElementVarAttInfo_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDmeshStructElementVarAttWr_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDmeshStructElementVarAttRd_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDmeshEntityInfo_SOURCES = dummy.cxx


TESTS_LEC = Unittest_MEDlibrary_1.sh \
Test_MEDnStructElement.sh \
Test_MEDstructElementInfoByName.sh \
Test_MEDstructElementInfo.sh \
Test_MEDstructElementConstAttInfo.sh \
Test_MEDstructElementConstAttRd.sh \
Test_MEDstructElementVarAttInfo.sh \
Test_MEDmeshStructElementVarAttRd.she \
Test_MEDmeshEntityInfo.sh

TESTS_ECR = Unittest_MEDfile_1.sh \
Unittest_MEDparameter_1.sh \
Test_MEDsupportMesh.shn \
Test_MEDstructElementCr.shn \
Test_MEDstructElementConstAttWithProfileWr.shn \
Test_MEDstructElementVarAttCr.shn \
Test_MEDmeshStructElementVarAttWr.shn

TESTS = $(TESTS_ECR) $(TESTS_LEC)

EXTRA_DIST = Unittest_MEDfile_1.sh \
Unittest_MEDlibrary_1.sh \
Unittest_MEDparameter_1.sh \
Test_MEDnStructElement.sh \
Test_MEDstructElementInfoByName.sh \
Test_MEDstructElementInfo.sh \
Test_MEDstructElementConstAttInfo.sh \
Test_MEDstructElementConstAttRd.sh \
Test_MEDstructElementVarAttInfo.sh \
Test_MEDmeshStructElementVarAttRd.she \
Test_MEDsupportMesh.shn \
Test_MEDstructElementCr.shn \
Test_MEDstructElementConstAttWithProfileWr.shn \
Test_MEDstructElementVarAttCr.shn \
Test_MEDmeshStructElementVarAttWr.shn \
Test_MEDmeshEntityInfo.sh 

#check_local:
#	$(MAKE) check_test_ecr
#	$(MAKE) check_test_lec SRC_REP=$(srcdir)
#	$(MAKE) check_test_lec SRC_REP=$(srcdir)/2.3.1

#check_test_ecr: $(TESTS_ECR)
#	@for testfile in $1 \
#	do \
#	  $$testfile \
#	done

#check_test_lec: $(TESTS_LEC)
#	@for testfile in $1 \
#	do \
#	  dump.sh $$testfile @H5DUMP@ SRCDIR=$(srcdir) $(SRC_REP)\
#	done


CLEANFILES = \
./Unittest_MEDfile_1.med \
./Unittest_MEDparameter_1.med \
./Test_MEDsupportMesh.med \
./Test_MEDstructElementCr.med \
./Test_MEDstructElementConstAttWithProfileWr.med \
./Test_MEDstructElementVarAttCr.med \
./Test_MEDmeshStructElementVarAttWr.med \
./Unittest_MEDfile_1.dump \
./Unittest_MEDparameter_1.dump \
./Test_MEDsupportMesh.dump \
./Test_MEDstructElementCr.dump \
./Test_MEDstructElementConstAttWithProfileWr.dump \
./Test_MEDstructElementVarAttCr.dump \
./Test_MEDmeshStructElementVarAttWr.dump \
./current.med


SUBDIRS = dumps.ref . MEDinterp MEDmesh
