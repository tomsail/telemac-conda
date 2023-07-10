include $(top_srcdir)/config/automake.common
include $(top_srcdir)/config/test.am

if WITH_PYTHON

testpydir = $(bindir)/testpy

# Depuis swig 3.0.9 les long C sont convertis en PyInt 
# En swig 3.0.9 __define SWIG_From_long   PyInt_FromLong 
# En swig 3.0.8 __define SWIG_From_long   PyLong_FromLong
if MED_SWIG_GT_30008
 MEDINT="_int"
 MED_PY3_SUFFIX="_py3"
else

if MED_HAVE_PYTHON3
 MEDINT="_int"
 MED_PY3_SUFFIX="_py3"
else
 MEDINT="_@LONG_OR_INT@"
 MED_PY3_SUFFIX=""
endif

endif
#MED_PY3_SUFFIX est utile pour choisir le fichier de sortie de référence car les int d'affichent différemment selon la version de python et swig

INSTALLCHECK_TESTS_ENVIRONMENT = AUTOTEST_PATH=$(testpydir) PYTHON=$(PYTHON) PYTHONPATH=$(pyexecdir) H5DUMP=@H5DUMP@ SRCDIR=$(srcdir) ABSSRCDIR="$(abs_srcdir)" BUILDDIR=$(abs_builddir) EXECDIR="$(abs_builddir)" CP=${CP} CPYMED="no" MEDINT=$(MEDINT) MED_PY3_SUFFIX=$(MED_PY3_SUFFIX)


# TESTS = test1.sh $(TESTS_ECR) $(TESTS_LEC)

TESTS_LEC = \
UsesCase_MEDmesh_2.py\
UsesCase_MEDmesh_3.py\
UsesCase_MEDmesh_5.py\
UsesCase_MEDmesh_7.py\
UsesCase_MEDmesh_8.py\
UsesCase_MEDmesh_11.py\
UsesCase_MEDmesh_12.py\
UsesCase_MEDmesh_14.py\
UsesCase_MEDmesh_16.py

TESTS_ECR =  \
UsesCase_MEDmesh_1.py \
UsesCase_MEDmesh_4.py \
UsesCase_MEDmesh_6.py \
UsesCase_MEDmesh_9.py \
UsesCase_MEDmesh_10.py \
UsesCase_MEDmesh_13.py \
UsesCase_MEDmesh_15.py

# EXTRA_DIST = test1.py
# EXTRA_DIST += \
# test11_f32_i32_int.expout\
# test11_f32_i64_int.expout\
# test11_f64_i32_int.expout\
# test11_f64_i64_int.expout\
# test11_f32_i32_long.expout\
# test11_f32_i64_long.expout\
# test11_f64_i32_long.expout\
# test11_f64_i64_long.expout


CHECK_TESTS_LEC = \
$(srcdir)/UsesCase_MEDmesh_2.atpy\
$(srcdir)/UsesCase_MEDmesh_3.atpy\
$(srcdir)/UsesCase_MEDmesh_5.atpy\
$(srcdir)/UsesCase_MEDmesh_7.atpy\
$(srcdir)/UsesCase_MEDmesh_8.atpy\
$(srcdir)/UsesCase_MEDmesh_11.atpy\
$(srcdir)/UsesCase_MEDmesh_12.atpy\
$(srcdir)/UsesCase_MEDmesh_14.atpy\
$(srcdir)/UsesCase_MEDmesh_16.atpy

CHECK_TESTS_ECR = \
$(srcdir)/UsesCase_MEDmesh_1.atpy\
$(srcdir)/UsesCase_MEDmesh_4.atpy\
$(srcdir)/UsesCase_MEDmesh_6.atpy\
$(srcdir)/UsesCase_MEDmesh_9.atpy\
$(srcdir)/UsesCase_MEDmesh_10.atpy\
$(srcdir)/UsesCase_MEDmesh_13.atpy\
$(srcdir)/UsesCase_MEDmesh_15.atpy

if INSTALLTEST
testpy_PYTHON= ${TESTS_PARAMS} $(TESTS_ECR) $(TESTS_LEC)
endif

PYTHONINSTALLCHECK_TESTS += $(CHECK_TESTS_LEC) $(CHECK_TESTS_ECR) 
# PYTHONINSTALLCHECK_PROGS += $(CHECK_TESTS_LEC) $(CHECK_TESTS_ECR)

endif

CLEANFILES+= \
UsesCase_MEDmesh_1.med \
UsesCase_MEDmesh_4.med \
UsesCase_MEDmesh_6.med \
UsesCase_MEDmesh_9.med \
UsesCase_MEDmesh_10.med \
UsesCase_MEDmesh_13.med \
UsesCase_MEDmesh_15.med

# CLEANFILES+=  ${BUILT_SOURCES} tests_params*.py.tmp

# SUBDIRS = dumps.ref .
SUBDIRS =  .

# EXTRA_DIST += CMakeLists.txt

