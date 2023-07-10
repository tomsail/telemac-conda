/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
 *  MED is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MED is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with MED.  If not, see <http://www.gnu.org/licenses/>.
 */


#include <med.h>
#include <med_config.h>

med_entity_type MED_GET_ENTITY_TYPE[MED_N_ENTITY_TYPES+2]={
  MED_UNDEF_ENTITY_TYPE,
  MED_CELL,
  MED_DESCENDING_FACE,
  MED_DESCENDING_EDGE,
  MED_NODE,
  MED_NODE_ELEMENT,
  MED_STRUCT_ELEMENT,
  MED_UNDEF_ENTITY_TYPE
};

const char * const MED_GET_ENTITY_TYPENAME[MED_N_ENTITY_TYPES+2]={
  "MED_UNDEF_ENTITY_TYPE",
  "MED_CELL",
  "MED_DESCENDING_FACE",
  "MED_DESCENDING_EDGE",
  "MED_NODE",
  "MED_NODE_ELEMENT",
  "MED_STRUCT_ELEMENT",
  "MED_UNDEF_ENTITY_TYPE"
};

med_geometry_type MED_GET_CELL_GEOMETRY_TYPE[MED_N_CELL_FIXED_GEO+2]={
  MED_NO_GEOTYPE,
  MED_POINT1,
  MED_SEG2,
  MED_SEG3,
  MED_SEG4,
  MED_TRIA3,
  MED_QUAD4,
  MED_TRIA6,
  MED_TRIA7,
  MED_QUAD8,
  MED_QUAD9,
  MED_TETRA4,
  MED_PYRA5,
  MED_PENTA6,
  MED_HEXA8,
  MED_TETRA10,
  MED_OCTA12,
  MED_PYRA13,
  MED_PENTA15,
  MED_PENTA18,
  MED_HEXA20,
  MED_HEXA27,
  MED_POLYGON,
  MED_POLYGON2,
  MED_POLYHEDRON,
  MED_NO_GEOTYPE
};

const char * const MED_GET_CELL_GEOMETRY_TYPENAME[MED_N_CELL_FIXED_GEO+2]={
  "MED_NO_GEOTYPE",
  "MED_POINT1",
  "MED_SEG2",
  "MED_SEG3",
  "MED_SEG4",
  "MED_TRIA3",
  "MED_QUAD4",
  "MED_TRIA6",
  "MED_TRIA7",
  "MED_QUAD8",
  "MED_QUAD9",
  "MED_TETRA4",
  "MED_PYRA5",
  "MED_PENTA6",
  "MED_HEXA8",
  "MED_TETRA10",
  "MED_OCTA12",
  "MED_PYRA13",
  "MED_PENTA15",
  "MED_PENTA18",
  "MED_HEXA20",
  "MED_HEXA27",
  "MED_POLYGON",
  "MED_POLYGON2",
  "MED_POLYHEDRON",
  "MED_NO_GEOTYPE"
};


med_geometry_type MED_GET_FACE_GEOMETRY_TYPE[MED_N_FACE_FIXED_GEO+2]={
  MED_NO_GEOTYPE,
  MED_TRIA3,
  MED_QUAD4,
  MED_TRIA6,
  MED_TRIA7,
  MED_QUAD8,
  MED_QUAD9,
  MED_POLYGON,
  MED_POLYGON2,
  MED_NO_GEOTYPE
};

const char * const MED_GET_FACE_GEOMETRY_TYPENAME[MED_N_FACE_FIXED_GEO+2]={
  "MED_NO_GEOTYPE",
  "MED_TRIA3",
  "MED_QUAD4",
  "MED_TRIA6",
  "MED_TRIA7",
  "MED_QUAD8",
  "MED_QUAD9",
  "MED_POLYGON",
  "MED_POLYGON2",
  "MED_NO_GEOTYPE"
};

med_geometry_type MED_GET_EDGE_GEOMETRY_TYPE[MED_N_EDGE_FIXED_GEO+2]={
  MED_NO_GEOTYPE,
  MED_SEG2,
  MED_SEG3,
  MED_SEG4,
  MED_NO_GEOTYPE
};

const char * MED_GET_EDGE_GEOMETRY_TYPENAME[MED_N_EDGE_FIXED_GEO+2]={
  "MED_NO_GEOTYPE",
  "MED_SEG2",
  "MED_SEG3",
  "MED_SEG4",
  "MED_NO_GEOTYPE"
};

med_geometry_type MED_GET_NODE_GEOMETRY_TYPE[MED_N_NODE_FIXED_GEO+2]={
  MED_NO_GEOTYPE,
  MED_NO_GEOTYPE,
  MED_NO_GEOTYPE
};

const char * MED_GET_NODE_GEOMETRY_TYPENAME[MED_N_NODE_FIXED_GEO+2]={
  "MED_NO_GEOTYPE",
  "MED_NO_GEOTYPE",
  "MED_NO_GEOTYPE"
};

