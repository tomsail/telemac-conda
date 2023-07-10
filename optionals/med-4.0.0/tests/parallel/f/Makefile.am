include $(top_srcdir)/config/automake.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

if ENABLE_FORTRAN

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR=$(srcdir) SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no"
#TESTS_ENVIRONMENT += OUTPUT="yes" CHECKER="libtool\ --mode=execute\ valgrind\ --error-limit=no"

AM_LDFLAGS+= $(FTLDFLAGS)
LIBS += $(FTLIBS)
AM_LIBTOOLFLAGS = --tag=F77

testfdir = $(bindir)/testf

##F90=@FC@
##SUFFIXES = .f90

##.f90.o:
##$(F90) $(AM_FFLAGS) $(FFLAGS) -c -o $@ $< 

if WITHPMED
if WITHF90

mycheckPROGRAMS = Parallel_test1

myTESTS_LEC = Parallel_test1.sh

if INSTALLTEST

testf_PROGRAMS = $(mycheckPROGRAMS)

else

check_PROGRAMS = $(mycheckPROGRAMS)

endif
Parallel_test1_SOURCES = Parallel_test1.f90
##test30_LINK = $(LIBTOOL)  --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

LDADD = $(top_builddir)/src/libmed.la

TESTS_LEC = $(myTESTS_LEC)
#TESTS_ECR = test1.sh 


TESTS = $(TESTS_ECR) $(TESTS_LEC)

endif
endif

EXTRA_DIST = Parallel_test1.sh dumps.ref/NENT-942_NVAL-008_NCST-007.med


#CLEANFILES= 

#SUBDIRS = dumps.ref . 
all-local:
	${LN_S} -f $(abs_srcdir)/dumps.ref/*.med . && chmod 644 *.med


endif
