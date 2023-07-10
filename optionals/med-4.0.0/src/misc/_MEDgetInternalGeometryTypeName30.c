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

/*
 * ATTENTION : LES NOMS RENVOYES SONT LES CHAINES UTILISEES EN INTERNE
 * ELLES N'ONT PAS LA FORME MED_...
 */

void  _MEDgetInternalGeometryTypeName30(int dummy,...)
{

  MED_VARGS_DECL(     , char *          , const , geotypename      );
  MED_VARGS_DECL(const, med_geometry_type     , , geotype          );
  MED_VARGS_DECL(, med_err *                   ,, fret             );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(     , char *          , const , geotypename      );
  MED_VARGS_DEF(const, med_geometry_type     , , geotype          );
  MED_VARGS_DEF(, med_err *                   ,, fret             );

  /*TODO : Créer une map */
  *fret = 0;

  switch (geotype)
    {
    case MED_NO_GEOTYPE :
      strcpy(geotypename,"MED_NO_GEOTYPE");
      break;

    case MED_POINT1 :
      strcpy(geotypename,MED_NOM_PO1);
      break;

    case MED_SEG2 :
      strcpy(geotypename,MED_NOM_SE2);
      break;

    case MED_SEG3 :
      strcpy(geotypename,MED_NOM_SE3);
      break;

    case MED_SEG4 :
      strcpy(geotypename,MED_NOM_SE4);
      break;

    case MED_TRIA3 :
      strcpy(geotypename,MED_NOM_TR3);
      break;

    case MED_TRIA6 :
      strcpy(geotypename,MED_NOM_TR6);
      break;

    case MED_TRIA7 :
      strcpy(geotypename,MED_NOM_TR7);
      break;

    case MED_QUAD4 :
      strcpy(geotypename,MED_NOM_QU4);
      break;

    case MED_QUAD8 :
      strcpy(geotypename,MED_NOM_QU8);
      break;

    case MED_QUAD9 :
      strcpy(geotypename,MED_NOM_QU9);
      break;

    case MED_TETRA4 :
      strcpy(geotypename,MED_NOM_TE4);
      break;

    case MED_TETRA10 :
      strcpy(geotypename,MED_NOM_T10);
      break;

    case MED_OCTA12 :
      strcpy(geotypename,MED_NOM_O12);
      break;

    case MED_HEXA8 :
      strcpy(geotypename,MED_NOM_HE8);
      break;

    case MED_HEXA20 :
      strcpy(geotypename,MED_NOM_H20);
      break;

    case MED_HEXA27 :
      strcpy(geotypename,MED_NOM_H27);
      break;

    case MED_PENTA6 :
      strcpy(geotypename,MED_NOM_PE6);
      break;

    case MED_PENTA15 :
      strcpy(geotypename,MED_NOM_P15);
      break;

    case MED_PYRA5 :
      strcpy(geotypename,MED_NOM_PY5);
      break;

    case MED_PYRA13 :
      strcpy(geotypename,MED_NOM_P13);
      break;

    case MED_POLYGON :
      strcpy(geotypename,MED_NOM_POG);
      break;

    case MED_POLYGON2 :
      strcpy(geotypename,MED_NOM_PO2);
      break;

    case MED_POLYHEDRON :
      strcpy(geotypename,MED_NOM_POE);
      break;

    default :
      strcpy(geotypename,"MED_INVALID_GEOTYPE");
      *fret = MED_ERR_INVALID MED_ERR_GEOMETRIC;
      break;
    }

  va_end(params);
  return;
}
