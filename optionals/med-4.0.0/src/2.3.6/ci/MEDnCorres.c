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

med_int
MEDnCorres(med_idt fid,char *maa,char *eq,med_entite_maillage type_ent,
	   med_geometrie_element typ_geo)
{
  med_idt eqid, datagroup;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_EQS+2*MED_TAILLE_NOM+1];
  char nomdatagroup[MED_TAILLE_NOM+1];
  char tmp[MED_TAILLE_NOM_ENTITE+1];
  med_int n=-1;
  med_entite_maillage _type_ent=type_ent;

  if ( type_ent == MED_NOEUD_MAILLE ) _type_ent=MED_NOEUD ;


  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
   _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * Si le Data Group de "eq" n'existe pas => erreur
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  strcat(chemin,MED_EQS);
  strcat(chemin,eq);
  if ((eqid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    goto ERROR;  
  if ( _MEDnomEntite(nomdatagroup,_type_ent) < 0)
    goto ERROR;
  if ((_type_ent != MED_NOEUD))
    {
      if ( _MEDnomGeometrie30(tmp,typ_geo) < 0)
	goto ERROR;
      strcat(nomdatagroup,".");
      strcat(nomdatagroup,tmp);
    }
  if ((datagroup = _MEDdatagroupOuvrir(eqid,nomdatagroup)) < 0) n=0;

  if ( n ) /*peut valoir -1 : OK */
    if ( _MEDattrEntierLire(datagroup,MED_NOM_NBR,&n) < 0)
      goto ERROR;

  /*
   * On ferme tout
   */
 ERROR:

  if ( datagroup> 0 ) if ( _MEDdatagroupFermer(datagroup) < 0) {
  MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_int(datagroup);n = -1; 
  }
    if ( eqid> 0 ) if ( _MEDdatagroupFermer(eqid) < 0) {
  MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(eqid);n = -1; 
  }
  
  return n;  
}


