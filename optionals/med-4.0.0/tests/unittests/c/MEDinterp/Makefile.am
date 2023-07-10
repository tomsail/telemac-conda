include $(top_srcdir)/config/automake.common

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no" 

#TESTS_ENVIRONMENT += OUTPUT="yes" CHECKER="libtool --mode=execute valgrind"

unittestsdir= $(bindir)/unittests

##check_PROGRAMS
##bin_PROGRAMS

AM_CFLAGS = -DDEF_LECT_ECR
#-DUSER_INTERLACE=MED_FULL_INTERLACE
#-DDEF_LECT_AJOUT

check_PROGRAMS = \
Test_MEDinterpCr \
Test_MEDinterpBaseFunctionWr \
Test_MEDinterpInfo \
Test_MEDinterpBaseFunctionRd \
Test_MEDinterpBaseFunctionCoeffSize \
Test_MEDfieldInterpWr \
Test_MEDfieldInterpInfo

# Les tests unitaires n'ont pas besoin d'être installés 
# unittests_PROGRAMS = \
# Test_MEDinterpBaseFunctionWr \
# Test_MEDinterpInfo \
# Test_MEDinterpBaseFunctionRd \
# Test_MEDinterpBaseFunctionCoeffSize \
# Test_MEDfieldInterpWr \
# Test_MEDfieldInterpInfo

LDADD = $(top_builddir)/src/libmedC.la

nodist_EXTRA_Test_MEDinterpCr_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDinterpBaseFunctionWr_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDinterpInfo_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDinterpBaseFunctionRd_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDinterpBaseFunctionCoeffSize_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDfieldInterpWr_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDfieldInterpInfo_SOURCES = dummy.cxx


TESTS_LEC = \
Test_MEDinterpInfo.sh \
Test_MEDinterpBaseFunctionRd.sh \
Test_MEDinterpBaseFunctionCoeffSize.sh \
Test_MEDfieldInterpInfo.she

TESTS_ECR = \
Test_MEDinterpCr.shn \
Test_MEDinterpBaseFunctionWr.shn \
Test_MEDfieldInterpWr.shn

TESTS = $(TESTS_ECR) $(TESTS_LEC)

EXTRA_DIST = \
Test_MEDinterpCr.shn \
Test_MEDinterpBaseFunctionWr.shn \
Test_MEDfieldInterpWr.shn \
Test_MEDinterpInfo.sh \
Test_MEDinterpBaseFunctionRd.sh \
Test_MEDinterpBaseFunctionCoeffSize.sh \
Test_MEDfieldInterpInfo.she

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
./Test_MEDinterpCr.med \
./Test_MEDinterpBaseFunctionWr.med \
./Test_MEDfieldInterpWr.med \
./Test_MEDinterpCr.dump \
./Test_MEDinterpBaseFunctionWr.dump \
./Test_MEDfieldInterpWr.dump \
./current.med


SUBDIRS = dumps.ref . 
