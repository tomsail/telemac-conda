include $(top_srcdir)/config/automake.common


AM_CPPFLAGS += -DSWIG_TYPE_TABLE=medtable
if MED_SWIG_INT64
AM_CPPFLAGS += -DSWIGWORDSIZE64
endif


#SWIG_PYTHON_OPT += -keyword
SWIG_PYTHON_OPT += -c++ 
#-relativeimport -py3
#-debug-module 4 -debug-tmsearch -debug-tmused

swigdir    = $(includedir)/swig

BUILT_SOURCES           = swig_runtime.h
pkgpyexec_PYTHON        =
nodist_pkgpyexec_PYTHON =
pyexec_LTLIBRARIES      =
pkgpyexec_LTLIBRARIES   =
CLEANFILES              = medenum.py medenumtest.py medequivalence.py medfamily.py medfield.py medfile.py medfilter.py medinterp.py medlibrary.py medlink.py medlocalization.py medmesh.py medparameter.py medprofile.py medsubdomain.py medstructelement.py 
DISTCLEANFILES          =
#DISTCLEANFILES          = $(BUILT_SOURCES) $(pkgpyexec_PYTHON)
EXTRA_DIST              = pyiterators_881.i med_881.i med_common.i med_array_typemap.i med_bool_typemap.i med_char_typemap.i med_enumtest_typemap.i med_enum_typemap.i
if WITH_PYTHON
swig_DATA               = pyiterators_881.i med_881.i med_common.i med_array_typemap.i med_bool_typemap.i med_char_typemap.i med_enumtest_typemap.i med_enum_typemap.i
endif
AM_LDLFLAGS             = -shared
AM_CFLAGS               = -shared

swig_runtime.h:
	$(SWIG) $(SWIG_PYTHON_OPT) $(AM_CPPFLAGS) $(SWIGFLAGS) -external-runtime $@

# Rules to build *.py, *_wrap.c  and *_module.i.dep from a *_module.i file
SUFFIXES = _int_wrap.cc _long_wrap.cc _module.i .py _int.py _long.py
EXTRA_DIST += \
medenum_int_wrap.cc         medfamily_int_wrap.cc  medfilter_int_wrap.cc   medlink_int_wrap.cc          medparameter_int_wrap.cc \
medenumtest_int_wrap.cc     medfield_int_wrap.cc   medinterp_int_wrap.cc   medlocalization_int_wrap.cc  medprofile_int_wrap.cc   \
medequivalence_int_wrap.cc  medfile_int_wrap.cc    medlibrary_int_wrap.cc  medmesh_int_wrap.cc          medsubdomain_int_wrap.cc   medstructelement_int_wrap.cc \
medenum_long_wrap.cc         medfamily_long_wrap.cc  medfilter_long_wrap.cc   medlink_long_wrap.cc          medparameter_long_wrap.cc \
medenumtest_long_wrap.cc     medfield_long_wrap.cc   medinterp_long_wrap.cc   medlocalization_long_wrap.cc  medprofile_long_wrap.cc   \
medequivalence_long_wrap.cc  medfile_long_wrap.cc    medlibrary_long_wrap.cc  medmesh_long_wrap.cc          medsubdomain_long_wrap.cc medstructelement_long_wrap.cc

EXTRA_DIST += \
medenum_int.py         medfamily_int.py  medfilter_int.py   medlink_int.py          medparameter_int.py \
medenumtest_int.py     medfield_int.py   medinterp_int.py   medlocalization_int.py  medprofile_int.py   \
medequivalence_int.py  medfile_int.py    medlibrary_int.py  medmesh_int.py          medsubdomain_int.py        medstructelement_int.py \
medenum_long.py         medfamily_long.py  medfilter_long.py   medlink_long.py          medparameter_long.py \
medenumtest_long.py     medfield_long.py   medinterp_long.py   medlocalization_long.py  medprofile_long.py   \
medequivalence_long.py  medfile_long.py    medlibrary_long.py  medmesh_long.py          medsubdomain_long.py   medstructelement_long.py

if WITH_SWIG
# Crée une dépendance de reconstruction des wrappers si SWIG est activé
# Crée aussi une dépendance au Makefile (inconvénients à voir)
MED_SWIG_SRC = Makefile

STARBASENAME=`expr $* : '.*/\(.*\)' '|' $*`

_module.i_int_wrap.cc:
if AMDEP
	$(SWIG) $(SWIG_PYTHON_OPT) $(DEFS) $(AM_CPPFLAGS) $(SWIGFLAGS) -MT $@ -MM $< > $(DEPDIR)/$(STARBASENAME)_module.i.dep.tmp
	mv $(DEPDIR)/$(STARBASENAME)_module.i.dep.tmp $(DEPDIR)/$(STARBASENAME)_module.i.dep
endif
	$(SWIG) $(SWIG_PYTHON_OPT) $(DEFS) $(AM_CPPFLAGS) $(SWIGFLAGS) -o $@.tmp $<
	mv $*.py $*_int.py
	mv $@.tmp $@

_module.i_long_wrap.cc:
if AMDEP
	$(SWIG) $(SWIG_PYTHON_OPT) $(DEFS) $(AM_CPPFLAGS) $(SWIGFLAGS) -MT $@ -MM $< > $(DEPDIR)/$(STARBASENAME)_module.i.dep.tmp
	mv $(DEPDIR)/$(STARBASENAME)_module.i.dep.tmp $(DEPDIR)/$(STARBASENAME)_module.i.dep
endif
	$(SWIG) $(SWIG_PYTHON_OPT) $(DEFS) $(AM_CPPFLAGS) $(SWIGFLAGS) -o ${@}.tmp $<
	mv $*.py $*_long.py
	mv ${@}.tmp ${@}
endif

#_int.py  -> .py:
#_long.py -> _long.py:
_@LONG_OR_INT@.py.py:
	${CP} -f $< $@

# _int_wrap.cc.py:
# 	@:
# _long_wrap.cc.py:
# 	@:

if WITH_PYTHON

## Python module : med

pyexec_DATA                 =
pkgpyexec_PYTHON            +=  __init__.py

########################################

## Python module : medlibrary
nodist_pkgpyexec_PYTHON   +=  medlibrary.py
pkgpyexec_LTLIBRARIES     += _medlibrary.la

_medlibrary_la_LDFLAGS         = -module -no-undefined
#_medlibrary_la_LDFLAGS = -avoid-version -module
_medlibrary_la_LIBADD          = $(top_builddir)/src/libmed.la
_medlibrary_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medlibrary_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medlibrary_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medlibrary_la_CXXFLAGS        = $(AM_CFLAGS)
_medlibrary_la_SOURCES  = medlibrary_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medlibrary_la_SOURCES         += medlibrary_module.i

swig_DATA               += medlibrary_module.i

MAINTAINERCLEANFILES += medlibrary.py medlibrary_@LONG_OR_INT@.py medlibrary_@LONG_OR_INT@_wrap.cc
medlibrary_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medlibrary_module.i.dep@am__quote@

## Python module : medfile

nodist_pkgpyexec_PYTHON   +=  medfile.py
pkgpyexec_LTLIBRARIES     += _medfile.la

_medfile_la_LDFLAGS         = -module -no-undefined
_medfile_la_LIBADD          = $(top_builddir)/src/libmed.la
_medfile_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medfile_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medfile_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medfile_la_CXXFLAGS        = $(AM_CFLAGS)
_medfile_la_SOURCES  = medfile_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medfile_la_SOURCES         += medfile_module.i

swig_DATA               += medfile_module.i

MAINTAINERCLEANFILES += medfile.py medfile_@LONG_OR_INT@.py medfile_@LONG_OR_INT@_wrap.cc
medfile_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medfile_module.i.dep@am__quote@

## Python module : medenum

nodist_pkgpyexec_PYTHON   +=  medenum.py
pkgpyexec_LTLIBRARIES     += _medenum.la

_medenum_la_LDFLAGS         = -module -no-undefined

_medenum_la_LIBADD          = $(top_builddir)/src/libmed.la
_medenum_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medenum_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medenum_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medenum_la_CXXFLAGS        = $(AM_CFLAGS)
_medenum_la_SOURCES  = medenum_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medenum_la_SOURCES         += medenum_module.i

swig_DATA               += medenum_module.i

MAINTAINERCLEANFILES += medenum.py medenum_@LONG_OR_INT@.py medenum_@LONG_OR_INT@_wrap.cc
medenum_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medenum_module.i.dep@am__quote@


## Python module : medmesh

nodist_pkgpyexec_PYTHON   +=  medmesh.py
pkgpyexec_LTLIBRARIES     += _medmesh.la

_medmesh_la_LDFLAGS         = -module -no-undefined

_medmesh_la_LIBADD          = $(top_builddir)/src/libmed.la
_medmesh_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medmesh_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medmesh_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medmesh_la_CXXFLAGS        = $(AM_CFLAGS)
_medmesh_la_SOURCES  = medmesh_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medmesh_la_SOURCES         += medmesh_module.i

swig_DATA               += medmesh_module.i

MAINTAINERCLEANFILES += medmesh.py medmesh_@LONG_OR_INT@.py medmesh_@LONG_OR_INT@_wrap.cc
medmesh_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medmesh_module.i.dep@am__quote@


## Python module : medfield

nodist_pkgpyexec_PYTHON   +=  medfield.py
pkgpyexec_LTLIBRARIES     += _medfield.la

_medfield_la_LDFLAGS         = -module -no-undefined

_medfield_la_LIBADD          = $(top_builddir)/src/libmed.la
_medfield_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medfield_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medfield_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medfield_la_CXXFLAGS        = $(AM_CFLAGS)
_medfield_la_SOURCES  = medfield_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medfield_la_SOURCES         += medfield_module.i

swig_DATA               += medfield_module.i

MAINTAINERCLEANFILES += medfield.py medfield_@LONG_OR_INT@.py  medfield_@LONG_OR_INT@_wrap.cc
medfield_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medfield_module.i.dep@am__quote@

## Python module : medfamily

nodist_pkgpyexec_PYTHON   +=  medfamily.py
pkgpyexec_LTLIBRARIES     += _medfamily.la

_medfamily_la_LDFLAGS         = -module -no-undefined

_medfamily_la_LIBADD          = $(top_builddir)/src/libmed.la
_medfamily_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medfamily_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medfamily_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medfamily_la_CXXFLAGS        = $(AM_CFLAGS)
_medfamily_la_SOURCES  = medfamily_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medfamily_la_SOURCES         += medfamily_module.i

swig_DATA               += medfamily_module.i

MAINTAINERCLEANFILES += medfamily.py medfamily_@LONG_OR_INT@.py medfamily_@LONG_OR_INT@_wrap.cc
medfamily_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medfamily_module.i.dep@am__quote@

## Python module : medequivalence

nodist_pkgpyexec_PYTHON   +=  medequivalence.py
pkgpyexec_LTLIBRARIES     += _medequivalence.la

_medequivalence_la_LDFLAGS         = -module -no-undefined

_medequivalence_la_LIBADD          = $(top_builddir)/src/libmed.la
_medequivalence_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medequivalence_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medequivalence_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medequivalence_la_CXXFLAGS        = $(AM_CFLAGS)
_medequivalence_la_SOURCES  = medequivalence_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medequivalence_la_SOURCES         += medequivalence_module.i

swig_DATA               += medequivalence_module.i

MAINTAINERCLEANFILES += medequivalence.py medequivalence_@LONG_OR_INT@.py medequivalence_@LONG_OR_INT@_wrap.cc
medequivalence_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medequivalence_module.i.dep@am__quote@


## Python module : medenumtest

nodist_pkgpyexec_PYTHON   +=  medenumtest.py
pkgpyexec_LTLIBRARIES     += _medenumtest.la

_medenumtest_la_LDFLAGS         = -module -no-undefined

_medenumtest_la_LIBADD          = $(top_builddir)/src/libmed.la
_medenumtest_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medenumtest_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medenumtest_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medenumtest_la_CXXFLAGS        = $(AM_CFLAGS)
_medenumtest_la_SOURCES  = medenumtest_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medenumtest_la_SOURCES         += medenumtest_module.i

swig_DATA               += medenumtest_module.i

MAINTAINERCLEANFILES += medenumtest.py medenumtest_@LONG_OR_INT@.py medenumtest_@LONG_OR_INT@_wrap.cc
medenumtest_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medenumtest_module.i.dep@am__quote@

## Python module : medprofile

nodist_pkgpyexec_PYTHON   +=  medprofile.py
pkgpyexec_LTLIBRARIES     += _medprofile.la

_medprofile_la_LDFLAGS         = -module -no-undefined

_medprofile_la_LIBADD          = $(top_builddir)/src/libmed.la
_medprofile_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medprofile_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medprofile_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medprofile_la_CXXFLAGS        = $(AM_CFLAGS)
_medprofile_la_SOURCES  = medprofile_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medprofile_la_SOURCES         += medprofile_module.i

swig_DATA               += medprofile_module.i

MAINTAINERCLEANFILES += medprofile.py medprofile_@LONG_OR_INT@.py medprofile_@LONG_OR_INT@_wrap.cc
medprofile_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medprofile_module.i.dep@am__quote@

## Python module : medlocalization

nodist_pkgpyexec_PYTHON   +=  medlocalization.py
pkgpyexec_LTLIBRARIES     += _medlocalization.la

_medlocalization_la_LDFLAGS         = -module -no-undefined

_medlocalization_la_LIBADD          = $(top_builddir)/src/libmed.la
_medlocalization_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medlocalization_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medlocalization_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medlocalization_la_CXXFLAGS        = $(AM_CFLAGS)
_medlocalization_la_SOURCES  = medlocalization_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medlocalization_la_SOURCES         += medlocalization_module.i

swig_DATA               += medlocalization_module.i

MAINTAINERCLEANFILES += medlocalization.py medlocalization_@LONG_OR_INT@.py medlocalization_@LONG_OR_INT@_wrap.cc
medlocalization_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medlocalization_module.i.dep@am__quote@

## Python module : medlink

nodist_pkgpyexec_PYTHON   +=  medlink.py
pkgpyexec_LTLIBRARIES     += _medlink.la

_medlink_la_LDFLAGS         = -module -no-undefined

_medlink_la_LIBADD          = $(top_builddir)/src/libmed.la
_medlink_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medlink_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medlink_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medlink_la_CXXFLAGS        = $(AM_CFLAGS)
_medlink_la_SOURCES  = medlink_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medlink_la_SOURCES         += medlink_module.i

swig_DATA               += medlink_module.i

MAINTAINERCLEANFILES += medlink.py medlink_@LONG_OR_INT@.py medlink_@LONG_OR_INT@_wrap.cc
medlink_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medlink_module.i.dep@am__quote@

## Python module : medparameter

nodist_pkgpyexec_PYTHON   +=  medparameter.py
pkgpyexec_LTLIBRARIES     += _medparameter.la

_medparameter_la_LDFLAGS         = -module -no-undefined

_medparameter_la_LIBADD          = $(top_builddir)/src/libmed.la
_medparameter_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medparameter_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medparameter_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medparameter_la_CXXFLAGS        = $(AM_CFLAGS)
_medparameter_la_SOURCES  = medparameter_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medparameter_la_SOURCES         += medparameter_module.i

swig_DATA               += medparameter_module.i

MAINTAINERCLEANFILES += medparameter.py medparameter_@LONG_OR_INT@.py medparameter_@LONG_OR_INT@_wrap.cc
medparameter_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medparameter_module.i.dep@am__quote@

## Python module : medfilter

nodist_pkgpyexec_PYTHON   +=  medfilter.py
pkgpyexec_LTLIBRARIES     += _medfilter.la

_medfilter_la_LDFLAGS         = -module -no-undefined

_medfilter_la_LIBADD          = $(top_builddir)/src/libmed.la
_medfilter_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medfilter_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medfilter_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medfilter_la_CXXFLAGS        = $(AM_CFLAGS)
_medfilter_la_SOURCES  = medfilter_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medfilter_la_SOURCES         += medfilter_module.i

swig_DATA               += medfilter_module.i

MAINTAINERCLEANFILES += medfilter.py medfilter_@LONG_OR_INT@.py medfilter_@LONG_OR_INT@_wrap.cc
medfilter_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medfilter_module.i.dep@am__quote@

## Python module : medsubdomain

nodist_pkgpyexec_PYTHON   +=  medsubdomain.py
pkgpyexec_LTLIBRARIES     += _medsubdomain.la

_medsubdomain_la_LDFLAGS         = -module -no-undefined

_medsubdomain_la_LIBADD          = $(top_builddir)/src/libmed.la
_medsubdomain_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medsubdomain_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medsubdomain_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medsubdomain_la_CXXFLAGS        = $(AM_CFLAGS)
_medsubdomain_la_SOURCES  = medsubdomain_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medsubdomain_la_SOURCES         += medsubdomain_module.i

swig_DATA               += medsubdomain_module.i

MAINTAINERCLEANFILES += medsubdomain.py medsubdomain_@LONG_OR_INT@.py medsubdomain_@LONG_OR_INT@_wrap.cc
medsubdomain_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medsubdomain_module.i.dep@am__quote@

## Python module : medinterp

nodist_pkgpyexec_PYTHON   +=  medinterp.py
pkgpyexec_LTLIBRARIES     += _medinterp.la

_medinterp_la_LDFLAGS         = -module -no-undefined

_medinterp_la_LIBADD          = $(top_builddir)/src/libmed.la
_medinterp_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medinterp_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medinterp_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medinterp_la_CXXFLAGS        = $(AM_CFLAGS)
_medinterp_la_SOURCES  = medinterp_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medinterp_la_SOURCES         += medinterp_module.i

swig_DATA               += medinterp_module.i

MAINTAINERCLEANFILES += medinterp.py medinterp_@LONG_OR_INT@.py medinterp_@LONG_OR_INT@_wrap.cc
medinterp_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medinterp_module.i.dep@am__quote@

## Python module : medstructelement

nodist_pkgpyexec_PYTHON   +=  medstructelement.py
pkgpyexec_LTLIBRARIES     += _medstructelement.la

_medstructelement_la_LDFLAGS         = -module -no-undefined

_medstructelement_la_LIBADD          = $(top_builddir)/src/libmed.la
_medstructelement_la_CPPFLAGS        = $(SWIG_PYTHON_CPPFLAGS)
_medstructelement_la_CPPFLAGS       += $(PYTHON_CPPFLAGS)
_medstructelement_la_CPPFLAGS       += $(AM_CPPFLAGS)
_medstructelement_la_CXXFLAGS        = $(AM_CFLAGS)
_medstructelement_la_SOURCES  = medstructelement_@LONG_OR_INT@_wrap.cc swig_runtime.h
_medstructelement_la_SOURCES         += medstructelement_module.i

swig_DATA               += medstructelement_module.i

MAINTAINERCLEANFILES += medstructelement.py medstructelement_@LONG_OR_INT@.py medstructelement_@LONG_OR_INT@_wrap.cc
medstructelement_@LONG_OR_INT@_wrap.cc : $(MED_SWIG_SRC)

@AMDEP_TRUE@@am__include@ @am__quote@./$(DEPDIR)/medstructelement_module.i.dep@am__quote@

endif

EXTRA_DIST += CMakeLists.txt

DISTCLEANFILES+=\
./medlink_int.py               \
./medenumtest_int.py           \
./medsubdomain_int_wrap.cc     \
./medmesh_int.py               \
./medfile_int_wrap.cc          \
./medfamily_int.py             \
./medparameter_int_wrap.cc     \
./medlibrary_int.py            \
./medfilter_int_wrap.cc        \
./medequivalence_int.py        \
./medfield_int.py              \
./medprofile_int.py            \
./medlocalization_int.py       \
./medlibrary_int_wrap.cc       \
./medinterp_int.py             \
./medlocalization_int_wrap.cc  \
./medequivalence_int_wrap.cc   \
./medlink_int_wrap.cc          \
./medenumtest_int_wrap.cc      \
./medfamily_int_wrap.cc        \
./medsubdomain_int.py          \
./medmesh_int_wrap.cc          \
./medinterp_int_wrap.cc        \
./medfield_int_wrap.cc         \
./medparameter_int.py          \
./medfile_int.py               \
./medenum_int.py               \
./medenum_int_wrap.cc          \
./medprofile_int_wrap.cc       \
./medfilter_int.py             \
./medlink_long.py               \
./medenumtest_long.py           \
./medsubdomain_long_wrap.cc     \
./medmesh_long.py               \
./medfile_long_wrap.cc          \
./medfamily_long.py             \
./medparameter_long_wrap.cc     \
./medlibrary_long.py            \
./medfilter_long_wrap.cc        \
./medequivalence_long.py        \
./medfield_long.py              \
./medprofile_long.py            \
./medlocalization_long.py       \
./medlibrary_long_wrap.cc       \
./medlongerp_long.py            \
./medlocalization_long_wrap.cc  \
./medequivalence_long_wrap.cc   \
./medlink_long_wrap.cc          \
./medenumtest_long_wrap.cc      \
./medfamily_long_wrap.cc        \
./medsubdomain_long.py          \
./medmesh_long_wrap.cc          \
./medlongerp_long_wrap.cc       \
./medfield_long_wrap.cc         \
./medparameter_long.py          \
./medfile_long.py               \
./medenum_long.py               \
./medenum_long_wrap.cc          \
./medprofile_long_wrap.cc       \
./medfilter_long.py
