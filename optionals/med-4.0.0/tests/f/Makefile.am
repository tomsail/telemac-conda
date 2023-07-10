include $(top_srcdir)/config/automake.common

DEFAULT_INCLUDES = -I. -I$(srcdir)

if ENABLE_FORTRAN

BUILT_SOURCES=
#Pour la génération des fichiers sources i32 i64 f32 f64 à partir d'un même fichier source  
SUFFIXES = _f32.F90 _f32_i32.F90 _f32_i64.F90 _i32.F90 _i64.F90
#Pour la génération des tests associés en _Rd.sh et _Wr.sh à partir des deux scripts
# testWr.sh testRd.sh
SUFFIXES += _f32.f90 _f32_i32.f90 _f32_i64.f90 _i32.f90 _i64.f90 _Rd.sh _Wr.sh

#La solution de pré-compilation des sources Fortran en utilisant CPP ne fonctionne pas correctement
# pour les include fortran et si on les remplace par des includes CPP, cela fonctionne uniquement sur le premier
#niveau d'include pas sur les includes qui possèdent des include.
# .F90.f90:
# 	${CPP} $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
# 	$(AM_FCFLAGS) $(FCFLAGS) $< > $(builddir)/$@

#Pour la génération des fichiers sources i32 i64 f32 f64 à partir d'un même fichier source  
.F90_f32.F90 :
	${CP} -f $< $@
.F90_f32_i64.F90:
	${CP} -f $< $@
.F90_f32_i32.F90:
	${CP} -f $< $@
.F90_i32.F90:
	${CP} -f $< $@
.F90_i64.F90:
	${CP} -f $< $@

#Pour la génération des tests en _Rd.sh et _Wr.sh à partir des deux scripts testWr.sh et testRd.sh
.f_Rd.sh: testRd.sh
	${LN_S} -f $(srcdir)/testRd.sh $@
.f_Wr.sh: testWr.sh
	${LN_S} -f $(srcdir)/testWr.sh $@
.f90_Rd.sh: testRd.sh
	${LN_S} -f $(srcdir)/testRd.sh $@
.f90_Wr.sh: testWr.sh
	${LN_S} -f $(srcdir)/testWr.sh $@
.F90_Rd.sh: testRd.sh
	${LN_S} -f $(srcdir)/testRd.sh $@
.F90_Wr.sh: testWr.sh
	${LN_S} -f $(srcdir)/testWr.sh $@

#Cet ensemble de règles est uniquement utilisé transitivement par les règles précédentes
#pour la génération des tests
.f90_f32.f90 :
	echo  $@
.f90_f32_i64.f90:
	echo  $@
.f90_f32_i32.f90:
	echo  $@
.f90_i32.f90:
	echo  $@
.f90_i64.f90:
	echo  $@

TESTS_ENVIRONMENT = eval H5DUMP=@H5DUMP@ SRCDIR=$(srcdir) SRCDIR="$(abs_srcdir)" BUILDDIR="$(abs_builddir)" \
		EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no" MEDINT="_@LONG_OR_INT@"
#On n'active pas par défaut le diff des sorties en Fortran car les affichages Fortran
#ne sont pas standardisés (dépendent des compilateurs utilisés)
#TESTS_ENVIRONMENT += OUTPUT="yes" DIFFOUTPUT="yes"
TESTS_ENVIRONMENT += OUTPUT="yes" 
#TESTS_ENVIRONMENT += CHECKER="libtool\ --mode=execute\ valgrind\ --error-limit=no"

AM_LDFLAGS+= $(FTLDFLAGS)
LIBS += $(FTLIBS)
AM_LIBTOOLFLAGS = --tag=F77

testfdir = $(bindir)/testf

##F90=@FC@
##SUFFIXES = .f90
##.f90.o:
##$(F90) $(AM_FFLAGS) $(FFLAGS) -c -o $@ $< 

if WITHF90

mycheckPROGRAMS = test5 test7 test9 test11 test11b test13 test15 test17 test30 
#test4b

myTESTS_LEC = test5_Rd.sh test7_Rd.sh test9_Rd.sh test11_Rd.sh test11b_Rd.sh \
test11_f32_Rd.sh test11_f32_i32_Rd.sh test11_f32_i64_Rd.sh test11_i32_Rd.sh test11_i64_Rd.sh \
test13_Rd.sh test15_Rd.sh test17_Rd.sh test30_Rd.sh

#test4b_SOURCES = test4b.f90
test5_SOURCES   = test5.f90
##test5_LINK = $(LIBTOOL) --mode=link $(F90) $(AM_FFLAGS) \
##        $(FFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@
test7_SOURCES   = test7.f90
test9_SOURCES   = test9.f90
test11_SOURCES  = test11.f90
test11b_SOURCES = test11b.f90
test13_SOURCES  = test13.f90
test15_SOURCES  = test15.f90
test17_SOURCES  = test17.f90
test30_SOURCES  = test30.f90

#Si le préprocesseur Fortran est disponible
#on génère les tests d'écriture en _f32 _f64 _i32 _i64
if WITHFCPP

test10_SOURCES = test10.F90
test10_FCFLAGS = $(FC_DEFINE)DEF_MED_FLOAT64 $(FCFLAGS_F90) $(FC_DEFINE)TEST10_FILENAME=\"test10.med\"
BUILT_SOURCES+=test10_f32.F90 test10_f32_i32.F90 test10_f32_i64.F90 test10_i32.F90 test10_i64.F90
test10_f32_FCFLAGS     =  $(FC_DEFINE)DEF_MED_FLOAT32 $(FCFLAGS_F90)                 $(FC_DEFINE)TEST10_FILENAME=\"test10_f32.med\"
test10_f32_i32_FCFLAGS =  $(FC_DEFINE)DEF_MED_FLOAT32 -DDEF_MED_INT32 $(FCFLAGS_F90) $(FC_DEFINE)TEST10_FILENAME=\"test10_f32_i32.med\"
test10_f32_i64_FCFLAGS =  $(FC_DEFINE)DEF_MED_FLOAT32 -DDEF_MED_INT64 $(FCFLAGS_F90) $(FC_DEFINE)TEST10_FILENAME=\"test10_f32_i64.med\"
test10_i32_FCFLAGS     =  $(FC_DEFINE)DEF_MED_INT32 $(FCFLAGS_F90)                   $(FC_DEFINE)TEST10_FILENAME=\"test10_i32.med\"
test10_i64_FCFLAGS     =  $(FC_DEFINE)DEF_MED_INT64 $(FCFLAGS_F90)                   $(FC_DEFINE)TEST10_FILENAME=\"test10_i64.med\"

test10_f32_SOURCES     =  test10_f32.F90
test10_f32_i32_SOURCES =  test10_f32_i32.F90
test10_f32_i64_SOURCES =  test10_f32_i64.F90
test10_i32_SOURCES     =  test10_i32.F90
test10_i64_SOURCES     =  test10_i64.F90

mycheckPROGRAMS3264 = test10_f32 test10_f32_i32 test10_f32_i64 test10_i32 test10_i64 test10
myTESTS_ECR         = test10_f32_Wr.sh test10_f32_i32_Wr.sh test10_f32_i64_Wr.sh test10_i32_Wr.sh test10_i64_Wr.sh test10_Wr.sh
#sinon on compile uniquement le test10.f
else !WITHFCPP

test10f_SOURCES = test10f.f
mycheckPROGRAMS3264 = test10f
myTESTS_ECR = test10f_Wr.sh

endif !WITHFCPP

else !WITHF90

test10f_SOURCES = test10f.f
mycheckPROGRAMS3264 = test10f
myTESTS_ECR = test10f_Wr.sh

endif !WITHF90

TESTS_LEC = test3_Rd.sh test22_Rd.sh test24_Rd.sh test26_Rd.sh test28_Rd.sh test32_Rd.sh test33_Rd.sh $(myTESTS_LEC)

TESTS_ECR = test1_Wr.sh test2_Wr.sh test4_Wr.sh test6_Wr.sh $(myTESTS_ECR) \
	test8_Wr.sh test12_Wr.sh test14_Wr.sh test16_Wr.sh \
	test18_Wr.sh test20_Wr.sh test21_Wr.sh test23_Wr.sh test25_Wr.sh test27_Wr.sh test29_Wr.sh test31_Wr.sh 

BUILT_SOURCES+=$(TESTS_ECR) $(TESTS_LEC)

TESTS = $(TESTS_ECR) $(TESTS_LEC)


EXTRA_DIST= testRd.sh testWr.sh
EXTRA_DIST+=CMakeLists.txt

LDADD = $(top_builddir)/src/libmed.la
#test1_LDADD = ....la


if INSTALLTEST

testf_PROGRAMS = test1 test2 test3 test4 test6 test8 test12 test14 test16 test18 test20 test21 test22 test23 test24 test25 test26 \
test27 test28 test29 test31 test32 test33 $(mycheckPROGRAMS) $(mycheckPROGRAMS3264)
#test19  

else

check_PROGRAMS = test1 test2 test3 test4 test6 test8 test12 test14 test16 test18 test20 test21 test22 test23 test24 test25 test26 \
test27 test28 test29 test31 test32 test33 $(mycheckPROGRAMS) $(mycheckPROGRAMS3264)
#test19  

endif

test1_SOURCES = test1.f
#Test d'édition des liens avec le C++ sur test1
#nodist_EXTRA_test1_SOURCES = dummy.cxx
#test1_LDADD =  $(top_builddir)/src/libmed.la $(FLIBS)
test2_SOURCES = test2.f
test3_SOURCES = test3.f
test4_SOURCES = test4.f
test6_SOURCES = test6.f
test8_SOURCES = test8.f
test12_SOURCES = test12.f
test14_SOURCES = test14.f
test16_SOURCES = test16.f
test18_SOURCES = test18.f
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
./test10-0.med\
./test10f.med\
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
./test20-0.med\
./test20.med\
./test21.med\
./test23.med\
./test25.med\
./test27.med\
./test29.med\
./test31.med\
./test10f.dump\
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

CLEANFILES+= $(BUILT_SOURCES) *med.out *.out *.out.ref *.out.tmp

SUBDIRS = dumps.ref . 3.2.1 3.1.0 3.0.8 2.3.6 2.3.1 2.3.2 2.3v3.0 2.3.1v3.0 2.3.2v3.0

endif
