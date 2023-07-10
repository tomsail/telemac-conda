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

med_int
MEDnEntites(med_idt fid,char *maa,med_entite_maillage type_ent, 
            med_connectivite typ_con)
{
  med_int total = 0;
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
  med_entite_maillage _type_ent=type_ent;

  if ( type_ent == MED_NOEUD_MAILLE ) _type_ent=MED_NOEUD ;

  switch (_type_ent)
  {
     case MED_MAILLE :
        for (i=0;i<MED_NBR_GEOMETRIE_MAILLE;i++)
          total += MEDnEntMaa(fid,maa,MED_CONN,MED_MAILLE,typ_mai[i],typ_con);
        break;

     case MED_FACE :
        for (i=0;i<MED_NBR_GEOMETRIE_FACE;i++)
          total += MEDnEntMaa(fid,maa,MED_CONN,MED_FACE,typ_fac[i],typ_con);
        break;

     case MED_ARETE :
        for (i=0;i<MED_NBR_GEOMETRIE_ARETE;i++)
          total += MEDnEntMaa(fid,maa,MED_CONN,MED_ARETE,typ_are[i],typ_con);
        break;

     case MED_NOEUD :
	total = MEDnEntMaa(fid,maa,MED_COOR,MED_NOEUD,0,0);
        break;

     default :
        total = -1;
  }

  return total;
}
