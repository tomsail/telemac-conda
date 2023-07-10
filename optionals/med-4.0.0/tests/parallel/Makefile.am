include $(top_srcdir)/config/automake.common

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no"

#TESTS_ENVIRONMENT += OUTPUT="yes" KEEPOUTPUTFILES="yes"
#TESTS_ENVIRONMENT +=  CHECKER="valgrind"

testcdir = $(bindir)/testc

##check_PROGRAMS
##bin_PROGRAMS

AM_CFLAGS = -DDEF_LECT_ECR 
#-DUSER_INTERLACE=MED_NO_INTERLACE
#-DDEF_LECT_AJOUT 

if WITHPMED

noinst_HEADERS = generateDatas.h  getBlocksOfEntitiesPartition.h generateFilterArray.h

if INSTALLTEST

testc_PROGRAMS = \
filterBlockOfentities 

else

check_PROGRAMS = \
filterBlockOfentities 

endif

filterBlockOfentities_SOURCES = \
filterBlockOfentities.c \
generateDatas.c \
generateFilterArray.c \
getBlocksOfEntitiesPartition.c


LDADD = $(top_builddir)/src/libmedC.la @HDF5_LDFLAGS@ @HDF5_LIBS@

nodist_EXTRA_filterBlockOfentities_SOURCES = dummy.cxx

TESTS = filterBlockOfentities.sh

endif

EXTRA_DIST = filterBlockOfentities.sh

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
./filterBlockOfentities.med\
./filterBlockOfentities.dump\
./filterBlockOfentities.out

SUBDIRS = dumps.ref . f
