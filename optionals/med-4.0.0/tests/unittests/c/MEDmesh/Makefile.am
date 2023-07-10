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

# Les tests unitaires n'ont pas besoin d'être installés 
#unittests_PROGRAMS = 
check_PROGRAMS = \
Test_MEDmeshNodeCoordinateTrsfWr \
Test_MEDmeshNodeCoordinateTrsfRd

LDADD = $(top_builddir)/src/libmedC.la

nodist_EXTRA_Test_MEDmeshNodeCoordinateTrsfWr_SOURCES = dummy.cxx
nodist_EXTRA_Test_MEDmeshNodeCoordinateTrsfRd_SOURCES = dummy.cxx


TESTS_LEC = \
Test_MEDmeshNodeCoordinateTrsfRd.sh

TESTS_ECR = \
Test_MEDmeshNodeCoordinateTrsfWr.sh

TESTS = $(TESTS_ECR) $(TESTS_LEC)

EXTRA_DIST = \
Test_MEDmeshNodeCoordinateTrsfWr.sh \
Test_MEDmeshNodeCoordinateTrsfRd.sh

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
./Test_MEDmeshNodeCoordinateTrsfWr.med \
./Test_MEDmeshNodeCoordinateTrsfWr.dump \
./Test_MEDmeshNodeCoordinateTrsfRd.med \
./Test_MEDmeshNodeCoordinateTrsfRd.dump \
./current.med


SUBDIRS = dumps.ref . 
