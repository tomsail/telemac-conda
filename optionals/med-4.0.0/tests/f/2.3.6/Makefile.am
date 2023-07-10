include $(top_srcdir)/config/automake.common.2.3.6

DEFAULT_INCLUDES = -I. -I$(srcdir)

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR=$(srcdir) SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no"

AM_LDFLAGS+= $(FTLDFLAGS)
LIBS += $(FTLIBS)
AM_LIBTOOLFLAGS = --tag=F77

#testfdir = $(bindir)/testf

##F90=@FC@
##SUFFIXES = .f90

##.f90.o:
##$(F90) $(AM_FFLAGS) $(FFLAGS) -c -o $@ $< 

if WITHF90

mycheckPROGRAMS = test5 test7 test9 test11 test13 test15 test17 test30

myTESTS_LEC = test5.sh test7.sh test9.sh test11.sh test13.sh test15.sh test17.sh test30.sh

test5_SOURCES = test5.f90
##test5_LINK = $(LIBTOOL) --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

test7_SOURCES = test7.f90
##test7_LINK = $(LIBTOOL)  --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

test9_SOURCES = test9.f90
##test9_LINK = $(LIBTOOL)  --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

test11_SOURCES = test11.f90
##test11_LINK = $(LIBTOOL)  --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

test13_SOURCES = test13.f90
##test13_LINK = $(LIBTOOL)  --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

test15_SOURCES = test15.f90
##test15_LINK = $(LIBTOOL)  --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

test17_SOURCES = test17.f90
##test17_LINK = $(LIBTOOL)  --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

test30_SOURCES = test30.f90
##test30_LINK = $(LIBTOOL)  --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

endif

TESTS_LEC = test3.sh  \
	test22.sh test24.sh test26.sh test28.sh \
	test32.sh test33.sh $(myTESTS_LEC)

TESTS_ECR = test1.sh test2.sh \
	test4.sh test6.sh \
	test8.sh test10.sh test12.sh test14.sh test16.sh \
	test18.sh test19.sh test20.sh test21.sh test23.sh \
	test25.sh test27.sh test29.sh test31.sh

TESTS = $(TESTS_ECR) $(TESTS_LEC)


EXTRA_DIST = test1.sh test2.sh test3.sh test4.sh test6.sh test8.sh  test10.sh test12.sh test14.sh test16.sh test18.sh test19.sh test20.sh test21.sh test22.sh test23.sh test24.sh test25.sh test26.sh test27.sh test28.sh test29.sh test5.sh test7.sh test9.sh test11.sh test13.sh test15.sh test17.sh test30.sh test31.sh test32.sh test33.sh

LDADD = $(top_builddir)/src/libmed.la
#test1_LDADD = ....la

check_PROGRAMS = test1 test2 test3 test4 test6 test8  test10 test12 test14 test16 test18 test19 test20 test21 test22 test23 test24 test25 test26 test27 test28 test29 test31 test32 test33   $(mycheckPROGRAMS)


test1_SOURCES = test1.f
test2_SOURCES = test2.f
test3_SOURCES = test3.f
test4_SOURCES = test4.f
test6_SOURCES = test6.f
test8_SOURCES = test8.f
test10_SOURCES = test10.f
test12_SOURCES = test12.f
test14_SOURCES = test14.f
test16_SOURCES = test16.f
test18_SOURCES = test18.f
test19_SOURCES = test19.f
test20_SOURCES = test20.f
test21_SOURCES = test21.f
test22_SOURCES = test22.f
test23_SOURCES = test23.f
test24_SOURCES = test24.f
test25_SOURCES = test25.f
test26_SOURCES = test26.f
test27_SOURCES = test27.f
test28_SOURCES = test28.f
test29_SOURCES = test29.f
test31_SOURCES = test31.f
test32_SOURCES = test32.f
test33_SOURCES = test33.f



CLEANFILES= \
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
./test28.dump\
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

