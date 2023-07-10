include $(top_srcdir)/config/automake.common.2.3.6

DEFAULT_INCLUDES = -I. -I$(srcdir)

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no"

#TESTS_ENVIRONMENT += OUTPUT="yes" CHECKER="valgrind"

#testcdir = $(bindir)/testc

##check_PROGRAMS
##bin_PROGRAMS

AM_CFLAGS = -DDEF_LECT_ECR 
#-DUSER_INTERLACE=MED_NO_INTERLACE
#-DDEF_LECT_AJOUT 


check_PROGRAMS = test1 test2 test3 test4 test5 test6  \
test7  test8  test9  test10 test11 test12 \
test13  test14 test15 test16 test17 test18 \
test19 test20 test21 test22 test23 test24 test25 test26 test27 test28 \
test29 test30 test31 test32 test33

LDADD = $(top_builddir)/src/libmedC.la

nodist_EXTRA_test1_SOURCES = dummy.cxx
nodist_EXTRA_test2_SOURCES = dummy.cxx
nodist_EXTRA_test3_SOURCES = dummy.cxx 
nodist_EXTRA_test4_SOURCES = dummy.cxx
nodist_EXTRA_test5_SOURCES = dummy.cxx
nodist_EXTRA_test6_SOURCES = dummy.cxx
nodist_EXTRA_test7_SOURCES = dummy.cxx
nodist_EXTRA_test8_SOURCES = dummy.cxx
nodist_EXTRA_test9_SOURCES = dummy.cxx
nodist_EXTRA_test10_SOURCES = dummy.cxx
nodist_EXTRA_test11_SOURCES = dummy.cxx
nodist_EXTRA_test12_SOURCES = dummy.cxx
nodist_EXTRA_test13_SOURCES = dummy.cxx
nodist_EXTRA_test14_SOURCES = dummy.cxx
nodist_EXTRA_test15_SOURCES = dummy.cxx
nodist_EXTRA_test16_SOURCES = dummy.cxx
nodist_EXTRA_test17_SOURCES = dummy.cxx
nodist_EXTRA_test18_SOURCES = dummy.cxx
nodist_EXTRA_test19_SOURCES = dummy.cxx
nodist_EXTRA_test20_SOURCES = dummy.cxx
nodist_EXTRA_test21_SOURCES = dummy.cxx
nodist_EXTRA_test22_SOURCES = dummy.cxx
nodist_EXTRA_test23_SOURCES = dummy.cxx
nodist_EXTRA_test24_SOURCES = dummy.cxx
nodist_EXTRA_test25_SOURCES = dummy.cxx
nodist_EXTRA_test26_SOURCES = dummy.cxx
nodist_EXTRA_test27_SOURCES = dummy.cxx
nodist_EXTRA_test28_SOURCES = dummy.cxx
nodist_EXTRA_test29_SOURCES = dummy.cxx
nodist_EXTRA_test30_SOURCES = dummy.cxx
nodist_EXTRA_test31_SOURCES = dummy.cxx
nodist_EXTRA_test32_SOURCES = dummy.cxx
nodist_EXTRA_test33_SOURCES = dummy.cxx

TESTS_LEC = test3.sh test5.sh test7.sh test9.sh \
	test11.sh test13.sh test15.sh test17.sh \
	test22.sh test24.sh test26.sh test28.sh \
	test30.sh test32.sh test33.sh

TESTS_ECR = test1.sh test2.sh test4.sh test6.sh \
	test8.sh test10.sh test12.sh test14.sh test16.sh \
	test18.sh test19.sh test20.sh test21.sh test23.sh \
	test25.sh test27.sh test29.sh test31.sh

TESTS = $(TESTS_ECR) $(TESTS_LEC)

EXTRA_DIST = test1.sh test2.sh test3.sh test4.sh test5.sh test6.sh  \
test7.sh  test8.sh  test9.sh  test10.sh test11.sh test12.sh \
test13.sh  test14.sh test15.sh test16.sh test17.sh test18.sh \
test19.sh test20.sh test21.sh test22.sh test23.sh test24.sh \
test25.sh test26.sh test27.sh test28.sh \
test29.sh test30.sh test31.sh test32.sh test33.sh

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
./test20-0.med\
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
./test31.dump\
./test2.dump\
./test4.dump\
./test6.dump\
./test8.dump

SUBDIRS = dumps.ref .
