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

med_err _MEDGeometrieElement(med_geometrie_element typ_geo[],med_entite_maillage typ_ent)
{
  int i;
  med_geometrie_element typ_mai[MED_NBR_GEOMETRIE_MAILLE] = {MED_POINT1,MED_SEG2, 
							 MED_SEG3,MED_TRIA3,
							 MED_TRIA6,MED_QUAD4,
							 MED_QUAD8,MED_TETRA4,
							 MED_TETRA10,MED_HEXA8,
							 MED_HEXA20,MED_PENTA6,
							 MED_PENTA15,MED_PYRA5,
							 MED_PYRA13};
  med_geometrie_element typ_fac[MED_NBR_GEOMETRIE_FACE] = {MED_TRIA3,MED_TRIA6,
						       MED_QUAD4,MED_QUAD8};
  med_geometrie_element typ_are[MED_NBR_GEOMETRIE_ARETE] = {MED_SEG2,MED_SEG3};  

  switch(typ_ent)
    {
    case MED_MAILLE :
      for (i=0;i<MED_NBR_GEOMETRIE_MAILLE;i++)
	typ_geo[i] = typ_mai[i];
      break;

    case MED_FACE :
      for (i=0;i<MED_NBR_GEOMETRIE_FACE;i++)
	typ_geo[i] = typ_fac[i];
      break;
      
    case MED_ARETE :
      for (i=0;i<MED_NBR_GEOMETRIE_ARETE;i++)
	typ_geo[i] = typ_are[i];
      break;

    default :
      return -1;
    }
  return 0;
}
