include $(top_srcdir)/config/automake.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)/../" CP=${CP} CPYMED="yes" AWK=${AWK}
#TESTS_ENVIRONMENT += OUTPUT="yes" 

AM_LDFLAGS+= $(FTLDFLAGS)
LIBS      += $(FTLIBS)
AM_LIBTOOLFLAGS = --tag=F77

if WITHF90

myTESTS_LEC = ../test5_Rd.sh ../test7_Rd.sh ../test9_Rd.sh \
	../test11_Rd.sh ../test11b_Rd.sh ../test13_Rd.sh ../test15_Rd.sh ../test17_Rd.sh \
	../test30_Rd.sh 

# test11_SOURCES  = test11.f90
# PROG_LEC        = test11

endif
# EXTRA_LEC       = test11.sh 

TESTS_LEC = ../test3_Rd.sh ../test22_Rd.sh ../test24_Rd.sh ../test26_Rd.sh ../test28_Rd.sh \
	    ../test32_Rd.sh ../test33_Rd.sh $(myTESTS_LEC)

TESTS_ECR = ../test1_Wr.sh ../test2_Wr.sh ../test4_Wr.sh ../test6_Wr.sh \
	../test8_Wr.sh ./test10.sh ../test12_Wr.sh ../test14_Wr.sh ../test16_Wr.sh \
	../test18_Wr.sh ../test20_Wr.sh ../test21_Wr.sh ../test23_Wr.sh \
	../test25_Wr.sh ../test27_Wr.sh ../test29_Wr.sh ../test31_Wr.sh
#  ../test19.sh

test10_SOURCES = test10.f
#EXTRA_ECR = test10.sh test31.sh
PROG_ECR  = test10

TESTS =  $(TESTS_ECR) $(TESTS_LEC) 

EXTRA_DIST = test.med.ref test10.sh 
#$(EXTRA_LEC) $(EXTRA_ECR)

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
./test20.med\
./test21.med\
./test23.med\
./test25.med\
./test27.med\
./test29.med\
./test31.med\
./test10-0.med\
./test20-0.med\
./test10.dump\
./test12.dump\
./test14.dump\
./test16.dump\
./test18.dump\
./test19.dump\
./test1.dump\
./test20.dump\
./test2.dump\
./test4.dump\
./test6.dump\
./test8.dump\
./test21.dump\
./test23.dump\
./test25.dump\
./test27.dump\
./test28.dump\
./test29.dump\
./test31.dump

SUBDIRS = dumps.ref .
