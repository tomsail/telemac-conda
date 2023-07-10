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
#include <med_outils.h>
#include <string.h>


med_err _MEDgetExternalGeometryTypeName(char * const geotypename,med_geometry_type geotype)
{
  /*TODO : Créer une map */
  switch (geotype)
    {
    case MED_POINT1 :
      strcpy(geotypename,"MED_POINT1");
      break;

    case MED_SEG2 :
      strcpy(geotypename,"MED_SEG2");
      break;

    case MED_SEG3 :
      strcpy(geotypename,"MED_SEG3");
      break;

    case MED_SEG4 :
      strcpy(geotypename,"MED_SEG4");
      break;

    case MED_TRIA3 :
      strcpy(geotypename,"MED_TRIA3");
      break;

    case MED_TRIA6 :
      strcpy(geotypename,"MED_TRIA6");
      break;

    case MED_TRIA7 :
      strcpy(geotypename,"MED_TRIA7");
      break;

    case MED_QUAD4 :
      strcpy(geotypename,"MED_QUAD4");
      break;

    case MED_QUAD8 :
      strcpy(geotypename,"MED_QUAD8");
      break;

    case MED_QUAD9 :
      strcpy(geotypename,"MED_QUAD9");
      break;

    case MED_TETRA4 :
      strcpy(geotypename,"MED_TETRA4");
      break;

    case MED_TETRA10 :
      strcpy(geotypename,"MED_TETRA10");
      break;

    case MED_OCTA12 :
      strcpy(geotypename,"MED_OCTA12");
      break;

    case MED_HEXA8 :
      strcpy(geotypename,"MED_HEXA8");
      break;

    case MED_HEXA20 :
      strcpy(geotypename,"MED_HEXA20");
      break;

    case MED_HEXA27 :
      strcpy(geotypename,"MED_HEXA27");
      break;

    case MED_PENTA6 :
      strcpy(geotypename,"MED_PENTA6");
      break;

    case MED_PENTA15 :
      strcpy(geotypename,"MED_PENTA15");
      break;

    case MED_PENTA18 :
      strcpy(geotypename,"MED_PENTA18");
      break;

    case MED_PYRA5 :
      strcpy(geotypename,"MED_PYRA5");
      break;

    case MED_PYRA13 :
      strcpy(geotypename,"MED_PYRA13");
      break;

    case MED_POLYGON :
      strcpy(geotypename,"MED_POLYGON");
      break;

    case MED_POLYGON2 :
      strcpy(geotypename,"MED_POLYGON2");
      break;

    case MED_POLYHEDRON :
      strcpy(geotypename,"MED_POLYHEDRON");
      break;

    default :
      strcpy(geotypename,"MED_NO_GEOTYPE");
      break;
    }

  return 0;
}
