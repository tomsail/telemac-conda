include $(top_srcdir)/config/automake.common.2.3.6

DEFAULT_INCLUDES = -I. -I$(srcdir)

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)/../2.3.6/" CP=${CP} CPYMED="yes" AWK=${AWK}

AM_LDFLAGS+= $(FTLDFLAGS)
LIBS      += $(FTLIBS)
AM_LIBTOOLFLAGS = --tag=F77

if WITHF90

myTESTS_LEC = ../2.3.6/test5.sh ../2.3.6/test7.sh ../2.3.6/test9.sh \
	test11.sh ../2.3.6/test13.sh ../2.3.6/test15.sh ../2.3.6/test17.sh \
	../2.3.6/test30.sh 

test11_SOURCES  = test11.f90
PROG_LEC        = test11

endif
EXTRA_LEC       = test11.sh 

TESTS_LEC = ../2.3.6/test3.sh ../2.3.6/test22.sh ../2.3.6/test24.sh ../2.3.6/test26.sh ../2.3.6/test28.sh \
	    ../2.3.6/test32.sh ../2.3.6/test33.sh $(myTESTS_LEC)

TESTS_ECR = ../2.3.6/test1.sh test2.sh ../2.3.6/test4.sh ../2.3.6/test6.sh \
	../2.3.6/test8.sh test10.sh ../2.3.6/test12.sh ../2.3.6/test14.sh ../2.3.6/test16.sh \
	../2.3.6/test18.sh ../2.3.6/test19.sh ../2.3.6/test20.sh ../2.3.6/test21.sh ../2.3.6/test23.sh \
	../2.3.6/test25.sh test27.sh ../2.3.6/test29.sh test31.sh

test10_SOURCES = test10.f
EXTRA_ECR = test10.sh test2.sh test27.sh test31.sh
PROG_ECR  = test10

TESTS =  $(TESTS_ECR) $(TESTS_LEC) 

EXTRA_DIST = test.med.ref $(EXTRA_LEC) $(EXTRA_ECR)

check_PROGRAMS = $(PROG_ECR) $(PROG_LEC)
LDADD = $(top_builddir)/src/libmed.la

CLEANFILES = \
./test1.med\
./test2.med\
./test4.med\
./test6.med\
./test8.med\
./test10.med\
./test12.med\
./test14.med\
./test16.med\
./test18.med\
./test19.med\
./test20.med\
./test21.med\
./test23.med\
./test25.med\
./test27.med\
./test29.med\
./test31.med\
./test10.dump\
./test12.dump\
./test14.dump\
./test16.dump\
./test18.dump\
./test19.dump\
./test1.dump\
./test20.dump\
./test21.dump\
./test23.dump\
./test25.dump\
./test27.dump\
./test29.dump\
./test30.dump\
./test31.dump\
./test32.dump\
./test33.dump\
./test2.dump\
./test4.dump\
./test6.dump\
./test8.dump

SUBDIRS = dumps.ref .
