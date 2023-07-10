include $(top_srcdir)/config/automake.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no" MEDINT="_@LONG_OR_INT@"

TESTS_ENVIRONMENT += OUTPUT="yes" DIFFOUTPUT="yes"
#TESTS_ENVIRONMENT += OUTPUT="yes" CHECKER="../../libtool\ --mode=execute\ valgrind"

testcdir = $(bindir)/testc

##check_PROGRAMS
##bin_PROGRAMS

AM_CXXFLAGS=
AM_CFLAGS = -DDEF_LECT_ECR
#Par défaut FULL_INTERLACE
#AM_CFLAGS = -DDEF_LECT_ECR -DUSER_INTERLACE=MED_NO_INTERLACE
#-DDEF_LECT_AJOUT 

SUFFIXES= _f32.c _f32_i32.c _f32_i64.c _i32.c _i64.c _Rd.sh _Wr.sh

# Inference rules can have only one target before the colon (POSIX)
.c_f32.c:
	${CP} -f $< $@
.c_f32_i64.c:
	${CP} -f $< $@
.c_f32_i32.c:
	${CP} -f $< $@
.c_i32.c:
	${CP} -f $< $@
.c_i64.c:
	${CP} -f $< $@

.c_Rd.sh: testRd.sh
	${LN_S} -f $(srcdir)/testRd.sh $@
.c_Wr.sh: testWr.sh
	${LN_S} -f $(srcdir)/testWr.sh $@

BUILT_SOURCES= test10_f32.c test10_f32_i32.c test10_f32_i64.c test10_i32.c test10_i64.c
test10_f32_CFLAGS     =  -DDEF_MED_FLOAT32
test10_f32_i32_CFLAGS =  -DDEF_MED_FLOAT32 -DDEF_MED_INT32
test10_f32_i64_CFLAGS =  -DDEF_MED_FLOAT32 -DDEF_MED_INT64
test10_i32_CFLAGS     =  -DDEF_MED_INT32
test10_i64_CFLAGS     =  -DDEF_MED_INT64


if INSTALLTEST

testc_PROGRAMS = test1 test2 test3 test4 test5 test6  \
test7  test8  test9  \
test10 test10_mem test10_f32 test10_f32_i32 test10_f32_i64 test10_i32 test10_i64 \
test11 test11b test12 \
test13 test13b  test14 test15 test16 test17 test18 \
test20 test23 test24 test25 test26 test27 test28 \
test29 test30 test31 test32 test33 test21 test22 
#$(medfloat32File)
#test19

else

check_PROGRAMS = test1 test2 test3 test4 test5 test6  \
test7  test8  test9  \
test10 test10_mem test10_f32 test10_f32_i32 test10_f32_i64 test10_i32 test10_i64 \
test11 test11b test12 \
test13 test13b test14 test15 test16 test17 test18 \
test20 test23 test24 test25 test26 test27 test28 \
test29 test30 test31 test32 test33 test21 test22
#$(medfloat32File)
#test19

endif


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
nodist_EXTRA_test10_mem_SOURCES     = dummy.cxx
nodist_EXTRA_test10_f32_SOURCES     = dummy.cxx
nodist_EXTRA_test10_f32_i32_SOURCES = dummy.cxx
nodist_EXTRA_test10_f32_i64_SOURCES = dummy.cxx
nodist_EXTRA_test10_i32_SOURCES     = dummy.cxx
nodist_EXTRA_test10_i64_SOURCES     = dummy.cxx
nodist_EXTRA_test11_SOURCES  = dummy.cxx
nodist_EXTRA_test11b_SOURCES = dummy.cxx
nodist_EXTRA_test12_SOURCES = dummy.cxx
nodist_EXTRA_test13_SOURCES = dummy.cxx
nodist_EXTRA_test13b_SOURCES = dummy.cxx
nodist_EXTRA_test14_SOURCES = dummy.cxx
nodist_EXTRA_test15_SOURCES = dummy.cxx
nodist_EXTRA_test16_SOURCES = dummy.cxx
nodist_EXTRA_test17_SOURCES = dummy.cxx
nodist_EXTRA_test18_SOURCES = dummy.cxx
#nodist_EXTRA_test19_SOURCES = dummy.cxx
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

TESTS_LEC = test3_Rd.sh test5_Rd.sh test7_Rd.sh test9_Rd.sh \
	test11_Rd.sh test11b_Rd.sh  \
	test11_f32_Rd.sh test11_f32_i32_Rd.sh test11_f32_i64_Rd.sh test11_i32_Rd.sh test11_i64_Rd.sh \
	test13_Rd.sh test13b_Rd.sh test15_Rd.sh test17_Rd.sh \
	test22_Rd.sh test24_Rd.sh test26_Rd.sh test28_Rd.sh \
	test30_Rd.sh test32_Rd.sh test33_Rd.sh

TESTS_ECR = test1_Wr.sh test2_Wr.sh test4_Wr.sh test6_Wr.sh test8_Wr.sh \
        test10_Wr.sh  test10_f32_Wr.sh test10_f32_i32_Wr.sh \
	test10_f32_i64_Wr.sh test10_i32_Wr.sh test10_i64_Wr.sh \
	test12_Wr.sh test14_Wr.sh test16_Wr.sh \
	test20_Wr.sh test21_Wr.sh test18_Wr.sh  test23_Wr.sh \
	test25_Wr.sh test27_Wr.sh test29_Wr.sh test31_Wr.sh

#test19.sh n'a plus d'utilité, conversion famille/groupe
#test10_mem_Wr.sh

BUILT_SOURCES+=$(TESTS_ECR) $(TESTS_LEC)

TESTS = $(TESTS_ECR) $(TESTS_LEC)

EXTRA_DIST = testRd.sh testWr.sh
# \
# est1.sh test2.sh test3.sh test4.sh test5.sh test6.sh  \
# test7.sh  test8.sh  test9.sh  test10.sh test10_f32.sh test11.sh test11b.sh test12.sh \
# test13.sh  test14.sh test15.sh test16.sh test17.sh test18.sh \
# test20.sh test23.sh test24.sh \
# test25.sh test26.sh test27.sh test28.sh \
# test29.sh test30.sh test31.sh test32.sh test33.sh test21.sh test22.sh
##test19.sh

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
./test10_mem.med\
./test10.med\
./test10_f32.med\
./test10_f32_i32.med\
./test10_f32_i64.med\
./test10_i32.med\
./test10_i64.med\
./test12.med\
./test14.med\
./test16.med\
./test18.med\
./test19.med\
./test20-0.med\
./test20b.med\
./test10-med.hdf\
./test2-med.hdf\
./test20.med\
./test21.med\
./test23.med\
./test25.med\
./test27.med\
./test29.med\
./test31.med\
./test10.dump\
./test10_f32.dump\
./test10_f32_i32.dump\
./test10_f32_i64.dump\
./test10_i32.dump\
./test10_i64.dump\
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

CLEANFILES+= $(TESTS_ECR) $(TESTS_LEC)
CLEANFILES+= $(BUILT_SOURCES) *med.out *.out *.out.ref *.out.tmp

if ENABLE_API23
SUBDIRS          = dumps.ref . 3.2.1 3.1.0 3.0.8 2.3.6 2.3.1 2.3.2 2.3v3.0 2.3.1v3.0 2.3.2v3.0
else
SUBDIRS          = dumps.ref . 3.2.1 3.1.0 3.0.8
endif

EXTRA_DIST+=CMakeLists.txt

#/local/hdf5-1.8.8_par_openmpi.bin/bin/h5copy -p -i test2.med -o test2-med.hdf -s /INFOS_GENERALES -d /byid2/INFOS_GENERALES
#/local/hdf5-1.8.8_par_openmpi.bin/bin/h5copy -p -i test2.med -o test2-med.hdf -s /ENS_MAA -d /byid2/ENS_MAA

#/local/hdf5-1.8.8_par_openmpi.bin/bin/h5copy -p -i test10.med -o test10-med.hdf -s /INFOS_GENERALES -d /byid10/INFOS_GENERALES
#/local/hdf5-1.8.8_par_openmpi.bin/bin/h5copy -p -i test10.med -o test10-med.hdf -s /CHA -d /byid10/CHA
