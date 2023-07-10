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

med_err
MEDelementsLire(med_idt fid,char *maa,med_int mdim,med_int *connectivite,med_mode_switch mode_switch,
	       char *nom,med_booleen *inom,med_int *num,med_booleen *inum,
	       med_int *fam,med_int nele,med_entite_maillage type_ent, 
	       med_geometrie_element typ_geo,med_connectivite typ_conn)
{
  med_int i;
  med_entite_maillage _type_ent=type_ent;

  if ( type_ent == MED_NOEUD_MAILLE ) _type_ent=MED_NOEUD ;

  /* 
   * Lecture de la connectivite :
   * - On renvoie -1 en cas d'échec
   */
  if (MEDconnLire(fid,maa,mdim,connectivite,mode_switch,0,MED_NOPF,
			 _type_ent,typ_geo,typ_conn) < 0)
    return -1;

  /* 
   * Lecture des noms (presence optionnelle dans un maillage MED)
   */
  if (MEDnomLire(fid,maa,nom,nele,_type_ent,typ_geo) < 0)
    *inom = MED_FAUX;
  else
    *inom = MED_VRAI;

  /* 
   * Lecture des numeros (presence optionnelle dans un maillage MED)
   */
  if (MEDnumLire(fid,maa,num,nele,_type_ent,typ_geo) < 0)
    *inum = MED_FAUX;
  else
    *inum = MED_VRAI;

  /* Lecture des numeros de familles :
   *  - si pas de numeros de familles, cela siginifie qu'ils 
   *    tous egaux a 0
   */
  if (MEDfamLire(fid,maa,fam,nele,_type_ent,typ_geo) < 0)
    for (i=0;i<nele;i++)
      *(fam+i) = 0;
  
  /*
   * Tout s'est bien passé
   */
  return 0;
}
