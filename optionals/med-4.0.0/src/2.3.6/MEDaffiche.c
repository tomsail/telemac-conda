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

char * MED_GEOMETRIE_MAILLE_AFF[MED_NBR_GEOMETRIE_MAILLE+2] = \
{ "MED_POINT1", "MED_SEG2", "MED_SEG3", "MED_TRIA3",
    "MED_QUAD4", "MED_TRIA6","MED_QUAD8", "MED_TETRA4",
    "MED_PYRA5", "MED_PENTA6", "MED_HEXA8", "MED_TETRA10", 
    "MED_PYRA13", "MED_PENTA15", "MED_HEXA20", 
    "MED_POLYGONE", "MED_POLYEDRE"};

char * MED_GEOMETRIE_FACE_AFF[MED_NBR_GEOMETRIE_FACE+1] = {"MED_TRIA3","MED_TRIA6",
							    "MED_QUAD4","MED_QUAD8",
							    "MED_POLYGONE"};
char * MED_GEOMETRIE_ARETE_AFF[MED_NBR_GEOMETRIE_ARETE] = {"MED_SEG2","MED_SEG3"};  

char * MED_GEOMETRIE_NOEUD_AFF[1] = {"(AUCUN)"};  

char * MED_ENTITE_MAILLAGE_AFF[5]={"MED_MAILLE",
				    "MED_FACE", 
				    "MED_ARETE", 
				    "MED_NOEUD",
				    "MED_NOEUD_MAILLE"};
