# -*- coding:utf-8 -*-
%module (package="med") medenum


%include "typemaps.i"

%{
#include "med.h"
#include <utility>
%}

%include "H5public_extract.h"

%include "med_config.h"
%include "medC_win_dll.h"
%include "med.h"


%import "med_enum_typemap.i"
/* %import "med_enumtest_typemap.i" */

//Gestion des types enums en ARGOUT
//Swig crée les MOT CLES/VARIABLES pour les paramètres IN
%{
  typedef std::pair<int,const char * const> enum_;
%}

%{
  const enum_ MED_MESH_TYPE_init[] = {
    enum_(0 ,"MED_UNSTRUCTURED_MESH"),
    enum_(1 ,"MED_STRUCTURED_MESH"),
    enum_(-1,"MED_UNDEF_MESH_TYPE")
  };
%}
%med_enum_typemaps(medenum,med_mesh_type,MED_MESH_TYPE)

%{
  const enum_ MED_SORTING_TYPE_init[] = {
    enum_(0 ,"MED_MED_SORT_DTIT"),
    enum_(1 ,"MED_SORT_ITDT"),
    enum_(-1,"MED_SORT_UNDEF")
  };
%}
%med_enum_typemaps(medenum,med_sorting_type,MED_SORTING_TYPE)

%{
  const enum_ MED_GRID_TYPE_init[] = {
    enum_(0 ,"MED_CARTESIAN_GRID"),
    enum_(1 ,"MED_POLAR_GRID"),
    enum_(2 ,"MED_CURVILINEAR_GRID"),
    enum_(-1,"MED_UNDEF_GRID_TYPE")
  };
%}
%med_enum_typemaps(medenum,med_grid_type,MED_GRID_TYPE)

%{
  const enum_ MED_ENTITY_TYPE_init[] = {
    enum_(0 ,"MED_CELL"),
    enum_(1 ,"MED_DESCENDING_FACE"),
    enum_(2 ,"MED_DESCENDING_EDGE"),
    enum_(3 ,"MED_NODE"),
    enum_(4 ,"MED_NODE_ELEMENT"),
    enum_(5 ,"MED_STRUCT_ELEMENT"),
    enum_(6 ,"MED_ALL_ENTITY_TYPE"),
    enum_(-1,"MED_UNDEF_ENTITY_TYPE")
  };
%}
%med_enum_typemaps(medenum,med_entity_type,MED_ENTITY_TYPE)


%{
  const enum_ MED_FIELD_TYPE_init[] = {
    enum_(4,"MED_FLOAT32"),
    enum_(6,"MED_FLOAT64"),
    /* enum_(6,"MED_DOUBLE"), */
    enum_(24,"MED_INT32"),
    enum_(26,"MED_INT64"),
    enum_(28,"MED_INT")
  };
%}
%med_enum_typemaps(medenum,med_field_type,MED_FIELD_TYPE)

%{
  const enum_ MED_ATTRIBUTE_TYPE_init[] = {
    enum_(6,"MED_ATT_FLOAT64"),
    enum_(28,"MED_ATT_INT"),
    enum_(30,"MED_ATT_NAME"),
    enum_(0,"MED_ATT_UNDEF")
  };
%}
%med_enum_typemaps(medenum,med_attribute_type,MED_ATTRIBUTE_TYPE)

%{
  const enum_ MED_AXIS_TYPE_init[] = {
    enum_(0 ,"MED_CARTESIAN"),
    enum_(1 ,"MED_CYLINDRICAL"),
    enum_(2 ,"MED_SPHERICAL"),
    enum_(-1,"MED_UNDEF_AXIS_TYPE")
  };
%}
%med_enum_typemaps(medenum,med_axis_type,MED_AXIS_TYPE)

/** Essai **/

/* %{ */
/*   const enum_ MMED_GET_ENTITY_TYPE_init[MED_N_ENTITY_TYPES+2]={ */
/*     enum_((int)MED_UNDEF_ENTITY_TYPE,    "MED_UNDEF_ENTITY_TYPE"), */
/*     enum_((int)MED_CELL,		  "MED_CELL"), */
/*     enum_((int)MED_DESCENDING_FACE,	  "MED_DESCENDING_FACE"), */
/*     enum_((int)MED_DESCENDING_EDGE,	  "MED_DESCENDING_EDGE"), */
/*     enum_((int)MED_NODE,		  "MED_NODE"), */
/*     enum_((int)MED_NODE_ELEMENT,	  "MED_NODE_ELEMENT"), */
/*     enum_((int)MED_STRUCT_ELEMENT,	  "MED_STRUCT_ELEMENT"), */
/*     enum_((int)MED_UNDEF_ENTITY_TYPE,	  "MED_UNDEF_ENTITY_TYPE") */
/*   }; */
/* %} */

/* %med_enum_typemaps(medenum,med_entity_type,MMED_GET_ENTITY_TYPE) */

/* %extend MMED_GET_ENTITY_TYPE { */
/*    int __getitem__(int i) { */
/*      MMED_GET_ENTITY_TYPE::Get__str__::iterator it = (self->_get__str__).find(i); */
/*      if (it != (self->_get__str__).end() ) */
/*        return (*it).first; */
/*      else */
/*        return (int)MED_UNDEF_ENTITY_TYPE; */
/*    } */
/* }; */

%pythoncode %{

MED_GET_ENTITY_TYPE=[
#    (MED_UNDEF_ENTITY_TYPE,"MED_UNDEF_ENTITY_TYPE"),
    (MED_NODE,		   "MED_NODE"),
    (MED_CELL,		   "MED_CELL"),
    (MED_DESCENDING_FACE,  "MED_DESCENDING_FACE"),
    (MED_DESCENDING_EDGE,  "MED_DESCENDING_EDGE"),
    (MED_NODE_ELEMENT,	   "MED_NODE_ELEMENT"),
    (MED_STRUCT_ELEMENT,   "MED_STRUCT_ELEMENT")
#    (MED_UNDEF_ENTITY_TYPE,"MED_UNDEF_ENTITY_TYPE")
]

MED_GET_NODAL_ENTITY_TYPE=[
#    (MED_UNDEF_ENTITY_TYPE,"MED_UNDEF_ENTITY_TYPE"),
    (MED_NODE,		   "MED_NODE"),
    (MED_CELL,		   "MED_CELL"),
    (MED_NODE_ELEMENT,	   "MED_NODE_ELEMENT"),
    (MED_STRUCT_ELEMENT,   "MED_STRUCT_ELEMENT")
#    (MED_UNDEF_ENTITY_TYPE,"MED_UNDEF_ENTITY_TYPE")
]

MED_GET_DESCENDING_ENTITY_TYPE=[
#    (MED_UNDEF_ENTITY_TYPE,"MED_UNDEF_ENTITY_TYPE"),
    (MED_NODE,		   "MED_NODE"),
    (MED_CELL,		   "MED_CELL"),
    (MED_DESCENDING_FACE,  "MED_DESCENDING_FACE"),
    (MED_DESCENDING_EDGE,  "MED_DESCENDING_EDGE"),
    (MED_NODE_ELEMENT,	   "MED_NODE_ELEMENT"),
    (MED_STRUCT_ELEMENT,   "MED_STRUCT_ELEMENT")
#    (MED_UNDEF_ENTITY_TYPE,"MED_UNDEF_ENTITY_TYPE")
]

#Ordre iteration important pour la numerotation globale induite
MED_GET_CELL_GEOMETRY_TYPE=[
# (MED_NO_GEOTYPE, "MED_NO_GEOTYPE"),
 (MED_POINT1,     "MED_POINT1"),
 (MED_SEG2,       "MED_SEG2"),
 (MED_SEG3,       "MED_SEG3"),
 (MED_SEG4,       "MED_SEG4"),
 (MED_TRIA3,      "MED_TRIA3"),
 (MED_QUAD4,      "MED_QUAD4"),
 (MED_TRIA6,      "MED_TRIA6"),
 (MED_TRIA7,      "MED_TRIA7"),
 (MED_QUAD8,      "MED_QUAD8"),
 (MED_QUAD9,      "MED_QUAD9"),
 (MED_TETRA4,     "MED_TETRA4"),
 (MED_PYRA5,      "MED_PYRA5"),
 (MED_PENTA6,     "MED_PENTA6"),
 (MED_HEXA8,      "MED_HEXA8"),
 (MED_TETRA10,    "MED_TETRA10"),
 (MED_OCTA12,     "MED_OCTA12"),
 (MED_PYRA13,     "MED_PYRA13"),
 (MED_PENTA15,    "MED_PENTA15"),
 (MED_PENTA18,    "MED_PENTA18"),
 (MED_HEXA20,     "MED_HEXA20"),
 (MED_HEXA27,     "MED_HEXA27"),
 (MED_POLYGON,    "MED_POLYGON"),
 (MED_POLYGON2,    "MED_POLYGON2"),
 (MED_POLYHEDRON, "MED_POLYHEDRON")
# (MED_NO_GEOTYPE, "MED_NO_GEOTYPE")
]

#Ordre iteration important pour la numerotation globale induite
MED_GET_FACE_GEOMETRY_TYPE=[
#  (MED_NO_GEOTYPE, "MED_NO_GEOTYPE"),
  (MED_TRIA3,	   "MED_TRIA3"),
  (MED_QUAD4,	   "MED_QUAD4"),
  (MED_TRIA6,	   "MED_TRIA6"),
  (MED_TRIA7,	   "MED_TRIA7"),
  (MED_QUAD8,	   "MED_QUAD8"),
  (MED_QUAD9,	   "MED_QUAD9"),
  (MED_POLYGON,	   "MED_POLYGON")
#  (MED_NO_GEOTYPE, "MED_NO_GEOTYPE")
]

#Ordre iteration important pour la numerotation globale induite
MED_GET_EDGE_GEOMETRY_TYPE=[
#  (MED_NO_GEOTYPE,"MED_NO_GEOTYPE"),
  (MED_SEG2,	 "MED_SEG2"),
  (MED_SEG3,	 "MED_SEG3"),
  (MED_SEG4,	 "MED_SEG4")
#  (MED_NO_GEOTYPE,"MED_NO_GEOTYPE")
]

#Ordre iteration important pour la numerotation globale induite
MED_GET_NODE_GEOMETRY_TYPE=[
# (MED_NO_GEOTYPE,"MED_NO_GEOTYPE"),
 (MED_NO_GEOTYPE,"MED_NO_GEOTYPE")
# (MED_NO_GEOTYPE,"MED_NO_GEOTYPE")
]


MED_GET_GEOTYPE=dict([
    (MED_NODE,		   MED_GET_NODE_GEOMETRY_TYPE),
    (MED_DESCENDING_FACE,  MED_GET_FACE_GEOMETRY_TYPE),
    (MED_DESCENDING_EDGE,  MED_GET_EDGE_GEOMETRY_TYPE),
    (MED_CELL,		   MED_GET_CELL_GEOMETRY_TYPE),
    (MED_NODE_ELEMENT,	   MED_GET_CELL_GEOMETRY_TYPE),
    (MED_STRUCT_ELEMENT,   [(MED_NO_GEOTYPE,"MED_NO_GEOTYPE")])
])

%}
