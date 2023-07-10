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

/*
 * - Nom de la fonction : _MEDparametresGeometrie
 * - Description : fournit les parametres geometriques des differents
 *                 entites et elements MED
 * - Parametres :
 *     - typ_ent (IN)  : type d'entite de l'element
 *     - type_geo (IN) : le type geometrique de l'element
 *     - dim (OUT)     : dimension de l'element
 *     - nnoe (OUT)    : nombre de noeuds composant l'element (connectivite
 *                       nodale)
 *     - ndes (OUT)    : nombre de composants dans l'elements (connectivite
 *                       descendante)
 * - Resultat : 0 en cas de succes, -1 sinon
 */
med_err _MEDparametresGeometrie(med_entite_maillage type_ent,
				med_geometrie_element type_geo, int *dim,
				int *nnoe,int *ndes)
{
  *nnoe = type_geo % 100;
  *dim = type_geo / 100;

  switch(type_ent)
    {
    case MED_MAILLE :
      switch (type_geo)
	{
	case MED_POINT1 :
	  *ndes = 0;
	  break;

	case MED_SEG2 :
	  *ndes = 2;
	  break;

	case MED_SEG3 :
	  *ndes = 3;
	  break;

	case MED_TRIA3 :
	  *ndes = 3;
	  break;

	case MED_TRIA6 :
	  *ndes = 3;
	  break;

	case MED_QUAD4 :
	  *ndes = 4;
	  break;

	case MED_QUAD8 :
	  *ndes = 4;
	  break;

	case MED_TETRA4 :
	  *ndes = 4;
	  break;

	case MED_TETRA10 :
	  *ndes = 4;
	  break;

	case MED_HEXA8 :
	  *ndes = 6;
	  break;

	case MED_HEXA20 :
	  *ndes = 6;
	  break;

	case MED_PENTA6 :
	  *ndes = 5;
	  break;

	case MED_PENTA15 :
	  *ndes = 5;
	  break;

	case MED_PYRA5 :
	  *ndes = 5;
	  break;

	case MED_PYRA13 :
	  *ndes = 5;
	  break;

	default :
	  return -1;
	}
      break;

    case MED_FACE :
      switch(type_geo)
	{
	case MED_TRIA3 :
	  *ndes = 3;
	  break;

	case MED_TRIA6 :
	  *ndes = 3;
	  break;

	case MED_QUAD4 :
	  *ndes = 4;
	  break;

	case MED_QUAD8 :
	  *ndes = 4;
	  break;

	default :
	  return -1;
	}
      break;

    case MED_ARETE :
      switch(type_geo)
	{
	case MED_SEG2 :
	  *ndes = 2;
	  break;

	case MED_SEG3 :
	  *ndes = 3;
	  break;

	default :
	  return -1;
	}
      break;

    default :
      return -1;
    }

  return 0;
}
