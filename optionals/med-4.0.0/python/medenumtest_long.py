# This file was automatically generated by SWIG (http://www.swig.org).
# Version 3.0.12
#
# Do not make changes to this file unless you know what you are doing--modify
# the SWIG interface file instead.

from sys import version_info as _swig_python_version_info
if _swig_python_version_info >= (2, 7, 0):
    def swig_import_helper():
        import importlib
        pkg = __name__.rpartition('.')[0]
        mname = '.'.join((pkg, '_medenumtest')).lstrip('.')
        try:
            return importlib.import_module(mname)
        except ImportError:
            return importlib.import_module('_medenumtest')
    _medenumtest = swig_import_helper()
    del swig_import_helper
elif _swig_python_version_info >= (2, 6, 0):
    def swig_import_helper():
        from os.path import dirname
        import imp
        fp = None
        try:
            fp, pathname, description = imp.find_module('_medenumtest', [dirname(__file__)])
        except ImportError:
            import _medenumtest
            return _medenumtest
        try:
            _mod = imp.load_module('_medenumtest', fp, pathname, description)
        finally:
            if fp is not None:
                fp.close()
        return _mod
    _medenumtest = swig_import_helper()
    del swig_import_helper
else:
    import _medenumtest
del _swig_python_version_info

try:
    _swig_property = property
except NameError:
    pass  # Python < 2.2 doesn't have 'property'.

try:
    import builtins as __builtin__
except ImportError:
    import __builtin__

def _swig_setattr_nondynamic(self, class_type, name, value, static=1):
    if (name == "thisown"):
        return self.this.own(value)
    if (name == "this"):
        if type(value).__name__ == 'SwigPyObject':
            self.__dict__[name] = value
            return
    method = class_type.__swig_setmethods__.get(name, None)
    if method:
        return method(self, value)
    if (not static):
        if _newclass:
            object.__setattr__(self, name, value)
        else:
            self.__dict__[name] = value
    else:
        raise AttributeError("You cannot add attributes to %s" % self)


def _swig_setattr(self, class_type, name, value):
    return _swig_setattr_nondynamic(self, class_type, name, value, 0)


def _swig_getattr(self, class_type, name):
    if (name == "thisown"):
        return self.this.own()
    method = class_type.__swig_getmethods__.get(name, None)
    if method:
        return method(self)
    raise AttributeError("'%s' object has no attribute '%s'" % (class_type.__name__, name))


def _swig_repr(self):
    try:
        strthis = "proxy of " + self.this.__repr__()
    except __builtin__.Exception:
        strthis = ""
    return "<%s.%s; %s >" % (self.__class__.__module__, self.__class__.__name__, strthis,)

try:
    _object = object
    _newclass = 1
except __builtin__.Exception:
    class _object:
        pass
    _newclass = 0

ABSOLUTE_H5IPUBLIC_H = _medenumtest.ABSOLUTE_H5IPUBLIC_H
ABSOLUTE_H5PUBLIC_H = _medenumtest.ABSOLUTE_H5PUBLIC_H
HAVE_CC_C99 = _medenumtest.HAVE_CC_C99
HAVE_CUSERID = _medenumtest.HAVE_CUSERID
HAVE_DLFCN_H = _medenumtest.HAVE_DLFCN_H
HAVE_FTIME = _medenumtest.HAVE_FTIME
HAVE_GETEUID = _medenumtest.HAVE_GETEUID
HAVE_GETPWUID = _medenumtest.HAVE_GETPWUID
HAVE_GETTIMEOFDAY = _medenumtest.HAVE_GETTIMEOFDAY
HAVE_H5IPUBLIC_H = _medenumtest.HAVE_H5IPUBLIC_H
HAVE_H5PUBLIC_H = _medenumtest.HAVE_H5PUBLIC_H
HAVE_INTTYPES_H = _medenumtest.HAVE_INTTYPES_H
HAVE_LIBHDF5 = _medenumtest.HAVE_LIBHDF5
HAVE_MALLOC_H = _medenumtest.HAVE_MALLOC_H
HAVE_MEMORY_H = _medenumtest.HAVE_MEMORY_H
HAVE_PWD_H = _medenumtest.HAVE_PWD_H
HAVE_PYTHON = _medenumtest.HAVE_PYTHON
HAVE_STDBOOL_H = _medenumtest.HAVE_STDBOOL_H
HAVE_STDINT_H = _medenumtest.HAVE_STDINT_H
HAVE_STDLIB_H = _medenumtest.HAVE_STDLIB_H
HAVE_STRINGS_H = _medenumtest.HAVE_STRINGS_H
HAVE_STRING_H = _medenumtest.HAVE_STRING_H
HAVE_SYS_STAT_H = _medenumtest.HAVE_SYS_STAT_H
HAVE_SYS_TIMEB_H = _medenumtest.HAVE_SYS_TIMEB_H
HAVE_SYS_TIME_H = _medenumtest.HAVE_SYS_TIME_H
HAVE_SYS_TYPES_H = _medenumtest.HAVE_SYS_TYPES_H
HAVE_UNISTD_H = _medenumtest.HAVE_UNISTD_H
HAVE__BOOL = _medenumtest.HAVE__BOOL
LT_OBJDIR = _medenumtest.LT_OBJDIR
MED_API_23 = _medenumtest.MED_API_23
MED_CHECK_23FORMAT = _medenumtest.MED_CHECK_23FORMAT
MED_HAVE_FORTRAN = _medenumtest.MED_HAVE_FORTRAN
MED_HAVE_PYTHON = _medenumtest.MED_HAVE_PYTHON
MESGERR = _medenumtest.MESGERR
PACKAGE = _medenumtest.PACKAGE
PACKAGE_BUGREPORT = _medenumtest.PACKAGE_BUGREPORT
PACKAGE_NAME = _medenumtest.PACKAGE_NAME
PACKAGE_STRING = _medenumtest.PACKAGE_STRING
PACKAGE_TARNAME = _medenumtest.PACKAGE_TARNAME
PACKAGE_URL = _medenumtest.PACKAGE_URL
PACKAGE_VERSION = _medenumtest.PACKAGE_VERSION
SIZEOF_FORTRAN_INTEGER = _medenumtest.SIZEOF_FORTRAN_INTEGER
SIZEOF_FORTRAN_INTEGERp8 = _medenumtest.SIZEOF_FORTRAN_INTEGERp8
SIZEOF_HID_T = _medenumtest.SIZEOF_HID_T
SIZEOF_INT = _medenumtest.SIZEOF_INT
SIZEOF_LONG = _medenumtest.SIZEOF_LONG
SIZEOF_LONG_LONG = _medenumtest.SIZEOF_LONG_LONG
STDC_HEADERS = _medenumtest.STDC_HEADERS
TIME_WITH_SYS_TIME = _medenumtest.TIME_WITH_SYS_TIME
VERSION = _medenumtest.VERSION
HDF_VERSION_MAJOR_REF = _medenumtest.HDF_VERSION_MAJOR_REF
HDF_VERSION_MINOR_REF = _medenumtest.HDF_VERSION_MINOR_REF
HDF_VERSION_RELEASE_REF = _medenumtest.HDF_VERSION_RELEASE_REF
HDF_VERSION_NUM_REF = _medenumtest.HDF_VERSION_NUM_REF
H5F_LIBVER_18 = _medenumtest.H5F_LIBVER_18
MED_MAJOR_NUM = _medenumtest.MED_MAJOR_NUM
MED_MINOR_NUM = _medenumtest.MED_MINOR_NUM
MED_RELEASE_NUM = _medenumtest.MED_RELEASE_NUM
MED_3_LATEST_MINOR = _medenumtest.MED_3_LATEST_MINOR
MED_4_LATEST_MINOR = _medenumtest.MED_4_LATEST_MINOR
MED_NUM_MAJEUR = _medenumtest.MED_NUM_MAJEUR
MED_NUM_MINEUR = _medenumtest.MED_NUM_MINEUR
MED_NUM_RELEASE = _medenumtest.MED_NUM_RELEASE
MED_VERSION_STR = _medenumtest.MED_VERSION_STR
MED_MAX_PARA = _medenumtest.MED_MAX_PARA
MED_COMMENT_SIZE = _medenumtest.MED_COMMENT_SIZE
MED_IDENT_SIZE = _medenumtest.MED_IDENT_SIZE
MED_NAME_SIZE = _medenumtest.MED_NAME_SIZE
MED_SNAME_SIZE = _medenumtest.MED_SNAME_SIZE
MED_LNAME_SIZE = _medenumtest.MED_LNAME_SIZE
MED_SNAME_BLANK = _medenumtest.MED_SNAME_BLANK
MED_NAME_BLANK = _medenumtest.MED_NAME_BLANK
MED_PATHNAME_SIZE = _medenumtest.MED_PATHNAME_SIZE
MED_MAX_CHFID_PATH = _medenumtest.MED_MAX_CHFID_PATH
MED_FULL_INTERLACE = _medenumtest.MED_FULL_INTERLACE
MED_NO_INTERLACE = _medenumtest.MED_NO_INTERLACE
MED_UNDEF_INTERLACE = _medenumtest.MED_UNDEF_INTERLACE
MED_UNDEF_STMODE = _medenumtest.MED_UNDEF_STMODE
MED_GLOBAL_STMODE = _medenumtest.MED_GLOBAL_STMODE
MED_COMPACT_STMODE = _medenumtest.MED_COMPACT_STMODE
MED_GLOBAL_PFLMODE = _medenumtest.MED_GLOBAL_PFLMODE
MED_COMPACT_PFLMODE = _medenumtest.MED_COMPACT_PFLMODE
MED_UNDEF_PFLMODE = _medenumtest.MED_UNDEF_PFLMODE
MED_ACC_RDONLY = _medenumtest.MED_ACC_RDONLY
MED_ACC_RDWR = _medenumtest.MED_ACC_RDWR
MED_ACC_RDEXT = _medenumtest.MED_ACC_RDEXT
MED_ACC_CREAT = _medenumtest.MED_ACC_CREAT
MED_ACC_UNDEF = _medenumtest.MED_ACC_UNDEF
MED_UNSTRUCTURED_MESH = _medenumtest.MED_UNSTRUCTURED_MESH
MED_STRUCTURED_MESH = _medenumtest.MED_STRUCTURED_MESH
MED_UNDEF_MESH_TYPE = _medenumtest.MED_UNDEF_MESH_TYPE
MED_CARTESIAN_GRID = _medenumtest.MED_CARTESIAN_GRID
MED_POLAR_GRID = _medenumtest.MED_POLAR_GRID
MED_CURVILINEAR_GRID = _medenumtest.MED_CURVILINEAR_GRID
MED_UNDEF_GRID_TYPE = _medenumtest.MED_UNDEF_GRID_TYPE
MED_CELL = _medenumtest.MED_CELL
MED_DESCENDING_FACE = _medenumtest.MED_DESCENDING_FACE
MED_DESCENDING_EDGE = _medenumtest.MED_DESCENDING_EDGE
MED_NODE = _medenumtest.MED_NODE
MED_NODE_ELEMENT = _medenumtest.MED_NODE_ELEMENT
MED_STRUCT_ELEMENT = _medenumtest.MED_STRUCT_ELEMENT
MED_ALL_ENTITY_TYPE = _medenumtest.MED_ALL_ENTITY_TYPE
MED_UNDEF_ENTITY_TYPE = _medenumtest.MED_UNDEF_ENTITY_TYPE
MED_N_ENTITY_TYPES = _medenumtest.MED_N_ENTITY_TYPES
MED_COORDINATE = _medenumtest.MED_COORDINATE
MED_CONNECTIVITY = _medenumtest.MED_CONNECTIVITY
MED_NAME = _medenumtest.MED_NAME
MED_NUMBER = _medenumtest.MED_NUMBER
MED_FAMILY_NUMBER = _medenumtest.MED_FAMILY_NUMBER
MED_COORDINATE_AXIS1 = _medenumtest.MED_COORDINATE_AXIS1
MED_COORDINATE_AXIS2 = _medenumtest.MED_COORDINATE_AXIS2
MED_COORDINATE_AXIS3 = _medenumtest.MED_COORDINATE_AXIS3
MED_INDEX_FACE = _medenumtest.MED_INDEX_FACE
MED_INDEX_NODE = _medenumtest.MED_INDEX_NODE
MED_GLOBAL_NUMBER = _medenumtest.MED_GLOBAL_NUMBER
MED_VARIABLE_ATTRIBUTE = _medenumtest.MED_VARIABLE_ATTRIBUTE
MED_COORDINATE_TRSF = _medenumtest.MED_COORDINATE_TRSF
MED_UNDEF_DATATYPE = _medenumtest.MED_UNDEF_DATATYPE
MED_INTERNAL_FLOAT32 = _medenumtest.MED_INTERNAL_FLOAT32
MED_INTERNAL_FLOAT64 = _medenumtest.MED_INTERNAL_FLOAT64
MED_INTERNAL_INT32 = _medenumtest.MED_INTERNAL_INT32
MED_INTERNAL_INT64 = _medenumtest.MED_INTERNAL_INT64
MED_INTERNAL_INT = _medenumtest.MED_INTERNAL_INT
MED_INTERNAL_NAME = _medenumtest.MED_INTERNAL_NAME
MED_INTERNAL_SNAME = _medenumtest.MED_INTERNAL_SNAME
MED_INTERNAL_LNAME = _medenumtest.MED_INTERNAL_LNAME
MED_INTERNAL_IDENT = _medenumtest.MED_INTERNAL_IDENT
MED_INTERNAL_CHAR = _medenumtest.MED_INTERNAL_CHAR
MED_INTERNAL_UNDEF = _medenumtest.MED_INTERNAL_UNDEF
MED_DOUBLE = _medenumtest.MED_DOUBLE
MED_FLOAT64 = _medenumtest.MED_FLOAT64
MED_FLOAT32 = _medenumtest.MED_FLOAT32
MED_INT32 = _medenumtest.MED_INT32
MED_INT64 = _medenumtest.MED_INT64
MED_INT = _medenumtest.MED_INT
MED_ATT_FLOAT64 = _medenumtest.MED_ATT_FLOAT64
MED_ATT_INT = _medenumtest.MED_ATT_INT
MED_ATT_NAME = _medenumtest.MED_ATT_NAME
MED_ATT_UNDEF = _medenumtest.MED_ATT_UNDEF
MED_MESH = _medenumtest.MED_MESH
MED_FIELD = _medenumtest.MED_FIELD
MED_LIBRARY = _medenumtest.MED_LIBRARY
MED_FILE = _medenumtest.MED_FILE
MED_MESH_SUPPORT = _medenumtest.MED_MESH_SUPPORT
MED_ELSTRUCT = _medenumtest.MED_ELSTRUCT
MED_FAMILY = _medenumtest.MED_FAMILY
MED_EQUIVALENCE = _medenumtest.MED_EQUIVALENCE
MED_GROUP = _medenumtest.MED_GROUP
MED_JOINT = _medenumtest.MED_JOINT
MED_LOCALIZATION = _medenumtest.MED_LOCALIZATION
MED_PROFILE = _medenumtest.MED_PROFILE
MED_FILTER = _medenumtest.MED_FILTER
MED_INTERPOLATION = _medenumtest.MED_INTERPOLATION
MED_NUMERICAL_DATA = _medenumtest.MED_NUMERICAL_DATA
MED_LINK = _medenumtest.MED_LINK
MED_CLASS_UNDEF = _medenumtest.MED_CLASS_UNDEF
MED_CLASS_ALL = _medenumtest.MED_CLASS_ALL
MED_POINT1 = _medenumtest.MED_POINT1
MED_SEG2 = _medenumtest.MED_SEG2
MED_SEG3 = _medenumtest.MED_SEG3
MED_SEG4 = _medenumtest.MED_SEG4
MED_TRIA3 = _medenumtest.MED_TRIA3
MED_QUAD4 = _medenumtest.MED_QUAD4
MED_TRIA6 = _medenumtest.MED_TRIA6
MED_TRIA7 = _medenumtest.MED_TRIA7
MED_QUAD8 = _medenumtest.MED_QUAD8
MED_QUAD9 = _medenumtest.MED_QUAD9
MED_TETRA4 = _medenumtest.MED_TETRA4
MED_PYRA5 = _medenumtest.MED_PYRA5
MED_PENTA6 = _medenumtest.MED_PENTA6
MED_HEXA8 = _medenumtest.MED_HEXA8
MED_TETRA10 = _medenumtest.MED_TETRA10
MED_OCTA12 = _medenumtest.MED_OCTA12
MED_PYRA13 = _medenumtest.MED_PYRA13
MED_PENTA15 = _medenumtest.MED_PENTA15
MED_PENTA18 = _medenumtest.MED_PENTA18
MED_HEXA20 = _medenumtest.MED_HEXA20
MED_HEXA27 = _medenumtest.MED_HEXA27
MED_POLYGON = _medenumtest.MED_POLYGON
MED_POLYGON2 = _medenumtest.MED_POLYGON2
MED_POLYHEDRON = _medenumtest.MED_POLYHEDRON
MED_STRUCT_GEO_INTERNAL = _medenumtest.MED_STRUCT_GEO_INTERNAL
MED_STRUCT_GEO_SUP_INTERNAL = _medenumtest.MED_STRUCT_GEO_SUP_INTERNAL
MED_NONE = _medenumtest.MED_NONE
MED_NO_GEOTYPE = _medenumtest.MED_NO_GEOTYPE
MED_UNDEF_GEOTYPE = _medenumtest.MED_UNDEF_GEOTYPE
MED_UNDEF_GEOMETRY_TYPE = _medenumtest.MED_UNDEF_GEOMETRY_TYPE
MED_ALL_GEOTYPE = _medenumtest.MED_ALL_GEOTYPE
MED_GEO_ALL = _medenumtest.MED_GEO_ALL
MED_N_CELL_GEO = _medenumtest.MED_N_CELL_GEO
MED_N_CELL_FIXED_GEO = _medenumtest.MED_N_CELL_FIXED_GEO
MED_N_CELL_GEO_FIXED_CON = _medenumtest.MED_N_CELL_GEO_FIXED_CON
MED_N_FACE_GEO = _medenumtest.MED_N_FACE_GEO
MED_N_FACE_FIXED_GEO = _medenumtest.MED_N_FACE_FIXED_GEO
MED_N_FACE_GEO_FIXED_CON = _medenumtest.MED_N_FACE_GEO_FIXED_CON
MED_N_EDGE_TYPES = _medenumtest.MED_N_EDGE_TYPES
MED_N_EDGE_FIXED_GEO = _medenumtest.MED_N_EDGE_FIXED_GEO
MED_N_EDGE_GEO_FIXED_CON = _medenumtest.MED_N_EDGE_GEO_FIXED_CON
MED_N_NODE_GEO = _medenumtest.MED_N_NODE_GEO
MED_N_NODE_FIXED_GEO = _medenumtest.MED_N_NODE_FIXED_GEO
MED_N_NODE_GEO_FIXED_CON = _medenumtest.MED_N_NODE_GEO_FIXED_CON
MED_NODAL = _medenumtest.MED_NODAL
MED_DESCENDING = _medenumtest.MED_DESCENDING
MED_UNDEF_CONNECTIVITY_MODE = _medenumtest.MED_UNDEF_CONNECTIVITY_MODE
MED_NO_CMODE = _medenumtest.MED_NO_CMODE
MED_CARTESIAN = _medenumtest.MED_CARTESIAN
MED_CYLINDRICAL = _medenumtest.MED_CYLINDRICAL
MED_SPHERICAL = _medenumtest.MED_SPHERICAL
MED_UNDEF_AXIS_TYPE = _medenumtest.MED_UNDEF_AXIS_TYPE
MED_FALSE = _medenumtest.MED_FALSE
MED_TRUE = _medenumtest.MED_TRUE
MED_GAUSS_ELNO = _medenumtest.MED_GAUSS_ELNO
MED_IPOINT_ELNO = _medenumtest.MED_IPOINT_ELNO
MED_NO_NAME = _medenumtest.MED_NO_NAME
MED_NO_MESHNAME = _medenumtest.MED_NO_MESHNAME
MED_NO_MESH = _medenumtest.MED_NO_MESH
MED_NO_MESH_SUPPORT = _medenumtest.MED_NO_MESH_SUPPORT
MED_NO_LOCALIZATION = _medenumtest.MED_NO_LOCALIZATION
MED_NO_INTERPOLATION = _medenumtest.MED_NO_INTERPOLATION
MED_NO_IPOINT_INTERNAL = _medenumtest.MED_NO_IPOINT_INTERNAL
MED_NO_PROFILE = _medenumtest.MED_NO_PROFILE
MED_NO_GROUP = _medenumtest.MED_NO_GROUP
MED_ALLENTITIES_PROFILE = _medenumtest.MED_ALLENTITIES_PROFILE
MED_NO_PROFILE_INTERNAL = _medenumtest.MED_NO_PROFILE_INTERNAL
MED_SAME_PROFILE_INTERNAL = _medenumtest.MED_SAME_PROFILE_INTERNAL
MED_ALL_CONSTITUENT = _medenumtest.MED_ALL_CONSTITUENT
MED_UNDEF_SIZE = _medenumtest.MED_UNDEF_SIZE
MED_NO_PROFILE_SIZE = _medenumtest.MED_NO_PROFILE_SIZE
MED_SORT_DTIT = _medenumtest.MED_SORT_DTIT
MED_SORT_ITDT = _medenumtest.MED_SORT_ITDT
MED_SORT_UNDEF = _medenumtest.MED_SORT_UNDEF
MED_NO_DT = _medenumtest.MED_NO_DT
MED_NO_IT = _medenumtest.MED_NO_IT
MED_UNDEF_DT = _medenumtest.MED_UNDEF_DT
MED_ATT_NOT_FILLED = _medenumtest.MED_ATT_NOT_FILLED
MED_MAX_FILTER_SPACES = _medenumtest.MED_MAX_FILTER_SPACES
class med_filter(_object):
    __swig_setmethods__ = {}
    __setattr__ = lambda self, name, value: _swig_setattr(self, med_filter, name, value)
    __swig_getmethods__ = {}
    __getattr__ = lambda self, name: _swig_getattr(self, med_filter, name)
    __repr__ = _swig_repr
    __swig_setmethods__["nspaces"] = _medenumtest.med_filter_nspaces_set
    __swig_getmethods__["nspaces"] = _medenumtest.med_filter_nspaces_get
    if _newclass:
        nspaces = _swig_property(_medenumtest.med_filter_nspaces_get, _medenumtest.med_filter_nspaces_set)
    __swig_setmethods__["memspace"] = _medenumtest.med_filter_memspace_set
    __swig_getmethods__["memspace"] = _medenumtest.med_filter_memspace_get
    if _newclass:
        memspace = _swig_property(_medenumtest.med_filter_memspace_get, _medenumtest.med_filter_memspace_set)
    __swig_setmethods__["diskspace"] = _medenumtest.med_filter_diskspace_set
    __swig_getmethods__["diskspace"] = _medenumtest.med_filter_diskspace_get
    if _newclass:
        diskspace = _swig_property(_medenumtest.med_filter_diskspace_get, _medenumtest.med_filter_diskspace_set)
    __swig_setmethods__["nentity"] = _medenumtest.med_filter_nentity_set
    __swig_getmethods__["nentity"] = _medenumtest.med_filter_nentity_get
    if _newclass:
        nentity = _swig_property(_medenumtest.med_filter_nentity_get, _medenumtest.med_filter_nentity_set)
    __swig_setmethods__["nvaluesperentity"] = _medenumtest.med_filter_nvaluesperentity_set
    __swig_getmethods__["nvaluesperentity"] = _medenumtest.med_filter_nvaluesperentity_get
    if _newclass:
        nvaluesperentity = _swig_property(_medenumtest.med_filter_nvaluesperentity_get, _medenumtest.med_filter_nvaluesperentity_set)
    __swig_setmethods__["nconstituentpervalue"] = _medenumtest.med_filter_nconstituentpervalue_set
    __swig_getmethods__["nconstituentpervalue"] = _medenumtest.med_filter_nconstituentpervalue_get
    if _newclass:
        nconstituentpervalue = _swig_property(_medenumtest.med_filter_nconstituentpervalue_get, _medenumtest.med_filter_nconstituentpervalue_set)
    __swig_setmethods__["constituentselect"] = _medenumtest.med_filter_constituentselect_set
    __swig_getmethods__["constituentselect"] = _medenumtest.med_filter_constituentselect_get
    if _newclass:
        constituentselect = _swig_property(_medenumtest.med_filter_constituentselect_get, _medenumtest.med_filter_constituentselect_set)
    __swig_setmethods__["switchmode"] = _medenumtest.med_filter_switchmode_set
    __swig_getmethods__["switchmode"] = _medenumtest.med_filter_switchmode_get
    if _newclass:
        switchmode = _swig_property(_medenumtest.med_filter_switchmode_get, _medenumtest.med_filter_switchmode_set)
    __swig_setmethods__["filterarraysize"] = _medenumtest.med_filter_filterarraysize_set
    __swig_getmethods__["filterarraysize"] = _medenumtest.med_filter_filterarraysize_get
    if _newclass:
        filterarraysize = _swig_property(_medenumtest.med_filter_filterarraysize_get, _medenumtest.med_filter_filterarraysize_set)
    __swig_setmethods__["filterarray23v30"] = _medenumtest.med_filter_filterarray23v30_set
    __swig_getmethods__["filterarray23v30"] = _medenumtest.med_filter_filterarray23v30_get
    if _newclass:
        filterarray23v30 = _swig_property(_medenumtest.med_filter_filterarray23v30_get, _medenumtest.med_filter_filterarray23v30_set)
    __swig_setmethods__["profilearraysize"] = _medenumtest.med_filter_profilearraysize_set
    __swig_getmethods__["profilearraysize"] = _medenumtest.med_filter_profilearraysize_get
    if _newclass:
        profilearraysize = _swig_property(_medenumtest.med_filter_profilearraysize_get, _medenumtest.med_filter_profilearraysize_set)
    __swig_setmethods__["storagemode"] = _medenumtest.med_filter_storagemode_set
    __swig_getmethods__["storagemode"] = _medenumtest.med_filter_storagemode_get
    if _newclass:
        storagemode = _swig_property(_medenumtest.med_filter_storagemode_get, _medenumtest.med_filter_storagemode_set)
    __swig_setmethods__["profilename"] = _medenumtest.med_filter_profilename_set
    __swig_getmethods__["profilename"] = _medenumtest.med_filter_profilename_get
    if _newclass:
        profilename = _swig_property(_medenumtest.med_filter_profilename_get, _medenumtest.med_filter_profilename_set)

    def __init__(self):
        this = _medenumtest.new_med_filter()
        try:
            self.this.append(this)
        except __builtin__.Exception:
            self.this = this
    __swig_destroy__ = _medenumtest.delete_med_filter
    __del__ = lambda self: None
med_filter_swigregister = _medenumtest.med_filter_swigregister
med_filter_swigregister(med_filter)

MED_NO_FILTER_SIZE = _medenumtest.MED_NO_FILTER_SIZE
MED_NO_PROFILE_F = _medenumtest.MED_NO_PROFILE_F
class med_file_version(_object):
    __swig_setmethods__ = {}
    __setattr__ = lambda self, name, value: _swig_setattr(self, med_file_version, name, value)
    __swig_getmethods__ = {}
    __getattr__ = lambda self, name: _swig_getattr(self, med_file_version, name)
    __repr__ = _swig_repr
    __swig_setmethods__["majeur"] = _medenumtest.med_file_version_majeur_set
    __swig_getmethods__["majeur"] = _medenumtest.med_file_version_majeur_get
    if _newclass:
        majeur = _swig_property(_medenumtest.med_file_version_majeur_get, _medenumtest.med_file_version_majeur_set)
    __swig_setmethods__["mineur"] = _medenumtest.med_file_version_mineur_set
    __swig_getmethods__["mineur"] = _medenumtest.med_file_version_mineur_get
    if _newclass:
        mineur = _swig_property(_medenumtest.med_file_version_mineur_get, _medenumtest.med_file_version_mineur_set)
    __swig_setmethods__["release"] = _medenumtest.med_file_version_release_set
    __swig_getmethods__["release"] = _medenumtest.med_file_version_release_get
    if _newclass:
        release = _swig_property(_medenumtest.med_file_version_release_get, _medenumtest.med_file_version_release_set)

    def __init__(self):
        this = _medenumtest.new_med_file_version()
        try:
            self.this.append(this)
        except __builtin__.Exception:
            self.this = this
    __swig_destroy__ = _medenumtest.delete_med_file_version
    __del__ = lambda self: None
med_file_version_swigregister = _medenumtest.med_file_version_swigregister
med_file_version_swigregister(med_file_version)

class med_memfile(_object):
    __swig_setmethods__ = {}
    __setattr__ = lambda self, name, value: _swig_setattr(self, med_memfile, name, value)
    __swig_getmethods__ = {}
    __getattr__ = lambda self, name: _swig_getattr(self, med_memfile, name)
    __repr__ = _swig_repr
    __swig_setmethods__["app_image_ptr"] = _medenumtest.med_memfile_app_image_ptr_set
    __swig_getmethods__["app_image_ptr"] = _medenumtest.med_memfile_app_image_ptr_get
    if _newclass:
        app_image_ptr = _swig_property(_medenumtest.med_memfile_app_image_ptr_get, _medenumtest.med_memfile_app_image_ptr_set)
    __swig_setmethods__["app_image_size"] = _medenumtest.med_memfile_app_image_size_set
    __swig_getmethods__["app_image_size"] = _medenumtest.med_memfile_app_image_size_get
    if _newclass:
        app_image_size = _swig_property(_medenumtest.med_memfile_app_image_size_get, _medenumtest.med_memfile_app_image_size_set)
    __swig_setmethods__["ref_count"] = _medenumtest.med_memfile_ref_count_set
    __swig_getmethods__["ref_count"] = _medenumtest.med_memfile_ref_count_get
    if _newclass:
        ref_count = _swig_property(_medenumtest.med_memfile_ref_count_get, _medenumtest.med_memfile_ref_count_set)
    __swig_setmethods__["fapl_image_ptr"] = _medenumtest.med_memfile_fapl_image_ptr_set
    __swig_getmethods__["fapl_image_ptr"] = _medenumtest.med_memfile_fapl_image_ptr_get
    if _newclass:
        fapl_image_ptr = _swig_property(_medenumtest.med_memfile_fapl_image_ptr_get, _medenumtest.med_memfile_fapl_image_ptr_set)
    __swig_setmethods__["fapl_image_size"] = _medenumtest.med_memfile_fapl_image_size_set
    __swig_getmethods__["fapl_image_size"] = _medenumtest.med_memfile_fapl_image_size_get
    if _newclass:
        fapl_image_size = _swig_property(_medenumtest.med_memfile_fapl_image_size_get, _medenumtest.med_memfile_fapl_image_size_set)
    __swig_setmethods__["fapl_ref_count"] = _medenumtest.med_memfile_fapl_ref_count_set
    __swig_getmethods__["fapl_ref_count"] = _medenumtest.med_memfile_fapl_ref_count_get
    if _newclass:
        fapl_ref_count = _swig_property(_medenumtest.med_memfile_fapl_ref_count_get, _medenumtest.med_memfile_fapl_ref_count_set)
    __swig_setmethods__["vfd_image_ptr"] = _medenumtest.med_memfile_vfd_image_ptr_set
    __swig_getmethods__["vfd_image_ptr"] = _medenumtest.med_memfile_vfd_image_ptr_get
    if _newclass:
        vfd_image_ptr = _swig_property(_medenumtest.med_memfile_vfd_image_ptr_get, _medenumtest.med_memfile_vfd_image_ptr_set)
    __swig_setmethods__["vfd_image_size"] = _medenumtest.med_memfile_vfd_image_size_set
    __swig_getmethods__["vfd_image_size"] = _medenumtest.med_memfile_vfd_image_size_get
    if _newclass:
        vfd_image_size = _swig_property(_medenumtest.med_memfile_vfd_image_size_get, _medenumtest.med_memfile_vfd_image_size_set)
    __swig_setmethods__["vfd_ref_count"] = _medenumtest.med_memfile_vfd_ref_count_set
    __swig_getmethods__["vfd_ref_count"] = _medenumtest.med_memfile_vfd_ref_count_get
    if _newclass:
        vfd_ref_count = _swig_property(_medenumtest.med_memfile_vfd_ref_count_get, _medenumtest.med_memfile_vfd_ref_count_set)
    __swig_setmethods__["flags"] = _medenumtest.med_memfile_flags_set
    __swig_getmethods__["flags"] = _medenumtest.med_memfile_flags_get
    if _newclass:
        flags = _swig_property(_medenumtest.med_memfile_flags_get, _medenumtest.med_memfile_flags_set)

    def __init__(self):
        this = _medenumtest.new_med_memfile()
        try:
            self.this.append(this)
        except __builtin__.Exception:
            self.this = this
    __swig_destroy__ = _medenumtest.delete_med_memfile
    __del__ = lambda self: None
med_memfile_swigregister = _medenumtest.med_memfile_swigregister
med_memfile_swigregister(med_memfile)

MED_PARTICLE_NAME = _medenumtest.MED_PARTICLE_NAME
MED_BALL_NAME = _medenumtest.MED_BALL_NAME
MED_BEAM_NAME = _medenumtest.MED_BEAM_NAME
MED_PARTICLE_LABEL = _medenumtest.MED_PARTICLE_LABEL
MED_BALL_DIAMETER = _medenumtest.MED_BALL_DIAMETER
MED_BEAM_THICKNESS = _medenumtest.MED_BEAM_THICKNESS
# This file is compatible with both classic and new-style classes.

cvar = _medenumtest.cvar
MED_GET_ENTITY_TYPENAME = cvar.MED_GET_ENTITY_TYPENAME
MED_GET_CELL_GEOMETRY_TYPENAME = cvar.MED_GET_CELL_GEOMETRY_TYPENAME
MED_GET_FACE_GEOMETRY_TYPENAME = cvar.MED_GET_FACE_GEOMETRY_TYPENAME

