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

med_err _MEDgetGeometricParameter(const med_entity_type       entitytype,
				  const med_geometry_type     geotype,
				  med_int * const             entdim,
				  med_int * const             nnodes,
				  med_int * const             nndes)
{

  med_err _ret=-1;

  /* TODO : MED_NODE_ELEMENT */
  if ( entitytype == MED_NODE     ) {*nnodes=0;*nndes=0;*entdim=1; return 0;}
  /*Le nombre de noeuds des polygones des utilisateurs est dynamiquement fixé.
   Il n'y a donc pas de gestion d'entrelacement.
   Le tableau d'index permettrait tout de même de developper une selection par filtre
   en deux passes : 
   1) Filtre sur les indices 2) Création du tableau de filtre à partir de la selection des indices
  */
  if ( geotype    == MED_POLYGON  )   {*nnodes=1;*nndes=1;*entdim=-1;return 0;}
  if ( geotype    == MED_POLYGON2 )   {*nnodes=1;*nndes=1;*entdim=-1;return 0;}
  if ( geotype    == MED_POLYHEDRON ) {*nnodes=1;*nndes=1;*entdim=-1;return 0;}

  *nnodes = geotype % 100;
  *entdim = geotype / 100;

  switch(entitytype)
    {
    case MED_CELL :
      switch (geotype)
	{
	case MED_POINT1 :
	  *nndes = 0;
	  break;

	case MED_SEG2 :
	  *nndes = 2;
	  break;

	case MED_SEG3 :
	  *nndes = 3;
	  break;

	case MED_SEG4 :
	  *nndes = 4;
	  break;

	case MED_TRIA3 :
	  *nndes = 3;
	  break;

	case MED_TRIA6 :
	  *nndes = 3;
	  break;

	case MED_TRIA7 :
	  *nndes = 3;
	  break;

	case MED_QUAD4 :
	  *nndes = 4;
	  break;

	case MED_QUAD8 :
	  *nndes = 4;
	  break;

	case MED_QUAD9 :
	  *nndes = 4;
	  break;

	case MED_TETRA4 :
	  *nndes = 4;
	  break;

	case MED_TETRA10 :
	  *nndes = 4;
	  break;

	case MED_OCTA12 :
	  *nndes = 8;
	  break;

	case MED_HEXA8 :
	  *nndes = 6;
	  break;

	case MED_HEXA20 :
	  *nndes = 6;
	  break;

	case MED_HEXA27 :
	  *nndes = 6;
	  break;

	case MED_PENTA6 :
	  *nndes = 5;
	  break;

	case MED_PENTA15 :
	  *nndes = 5;
	  break;

	case MED_PENTA18 :
	  *nndes = 5;
	  break;

	case MED_PYRA5 :
	  *nndes = 5;
	  break;

	case MED_PYRA13 :
	  *nndes = 5;
	  break;

	default :
	  return -1;
	}
      break;

    case MED_DESCENDING_FACE :
      switch(geotype)
	{
	case MED_TRIA3 :
	  *nndes = 3;
	  break;

	case MED_TRIA6 :
	  *nndes = 3;
	  break;

	case MED_QUAD4 :
	  *nndes = 4;
	  break;

	case MED_QUAD8 :
	  *nndes = 4;
	  break;

	default :
	  return -1;
	}
      break;

    case MED_DESCENDING_EDGE :
      switch(geotype)
	{
	case MED_SEG2 :
	  *nndes = 2;
	  break;

	case MED_SEG3 :
	  *nndes = 3;
	  break;

	default :
	  return -1;
	}
      break;

    default :
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
      ISCRUTE_int(entitytype); goto ERROR;
    }

  _ret = 0;
 ERROR:
  return _ret;
}
