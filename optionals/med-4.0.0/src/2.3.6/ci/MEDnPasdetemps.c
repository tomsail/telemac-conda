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
#include <stdlib.h>

/*
 * Nombre de Couple (PDT,NOR) pour le champ <cha>
 */

med_int 
MEDnPasdetemps(med_idt fid,char *cha,med_entite_maillage type_ent, 
	       med_geometrie_element type_geo)

{
  med_err ret;
  int n1;
  char nomdatagroup1[MED_TAILLE_NOM+1];
  char tmp1         [MED_TAILLE_NOM_ENTITE+1];
  char chemin       [MED_TAILLE_CHA+(MED_TAILLE_NOM+1)+MED_TAILLE_NOM+1];

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;

  
  /* 
   * Creation du chemin d'accès aux différents (PDT,NOR) pour les différents <type_ent>[.<type_geo>]
   */
  strcpy(chemin,MED_CHA);
  strcat(chemin,cha);
  strcat(chemin,"/");

  if ((ret = _MEDnomEntite(nomdatagroup1,type_ent)) < 0)
    return -1;
  if ((type_ent != MED_NOEUD))
    {
      if ((ret = _MEDnomGeometrie30(tmp1,type_geo)) < 0)
	return -1;
      strcat(nomdatagroup1,".");
      strcat(nomdatagroup1,tmp1);
    }
  strcat(chemin,nomdatagroup1);

  n1 =0;
  _MEDnObjets(fid,chemin,&n1);
      
  return (med_int) n1;

}
