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

extern int mode_interlace; 

med_int
MEDjointnCorres (med_idt fid, char *maa, char *jn, 
		 med_entite_maillage type_ent_local,   med_geometrie_element typ_geo_local,
		 med_entite_maillage type_ent_distant, med_geometrie_element typ_geo_distant)
{
  med_entite_maillage _type_ent_local   = (med_entite_maillage) ( (int)(type_ent_local) % 10 );
  med_entite_maillage _type_ent_distant = (med_entite_maillage) ( (int)(type_ent_distant) % 10 );
  med_idt datagroup1=0,datagroup2=0;
  med_int n=0, ret=-1;

  char chemin[MED_TAILLE_MAA+MED_TAILLE_JNT+2*MED_TAILLE_NOM+1]; 
  char nomdatagroup[MED_TAILLE_NOM_ENTITE*4+3+1];
  char tmp[MED_TAILLE_NOM_ENTITE+1];
  med_size dimd[1];

/*   if (typ_geo_local   == MED_TETRA4 || typ_geo_local    == MED_TETRA10 || */
/*       typ_geo_local   == MED_HEXA8  || typ_geo_local    == MED_HEXA20  || */
/*       typ_geo_local   == MED_PENTA6 || typ_geo_local    == MED_PENTA15 || */
/*       typ_geo_local   == MED_PYRA5  || typ_geo_local    == MED_PYRA13  || */
/*       typ_geo_distant == MED_TETRA4 || typ_geo_distant == MED_TETRA10 || */
/*       typ_geo_distant == MED_HEXA8  || typ_geo_distant == MED_HEXA20  || */
/*       typ_geo_distant == MED_PENTA6 || typ_geo_distant == MED_PENTA15 || */
/*       typ_geo_distant == MED_PYRA5  || typ_geo_distant == MED_PYRA13) */
/*     return -1; */

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /* 
   * Si le Data Group de "JNT/Corres" n'existe pas => erreur
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  strcat(chemin,MED_JNT);
  strcat(chemin,jn);


  if ((datagroup1 = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Impossible d'ouvrir le datagroup  : ");
    SSCRUTE(chemin); 
    goto ERREUR;
  }

  /*
   * Ecriture de la correspondance
   *   construction du tag HDF "reperant" la correspondance 
   *   
   */
  if ( _MEDnomEntite(nomdatagroup,_type_ent_local) < 0)
    goto ERREUR;
  if ((_type_ent_local != MED_NOEUD)) {
    if ( _MEDnomGeometrie30(tmp,typ_geo_local) < 0) goto ERREUR;
    strcat(nomdatagroup,".");
    strcat(nomdatagroup,tmp);
  }


  if ( _MEDnomEntite(tmp,_type_ent_distant) < 0)  goto ERREUR;
  strcat(nomdatagroup,".");
  strcat(nomdatagroup,tmp);
  if ((_type_ent_distant != MED_NOEUD)) {
    if ( _MEDnomGeometrie30(tmp,typ_geo_distant) < 0) goto ERREUR;
    strcat(nomdatagroup,".");
    strcat(nomdatagroup,tmp);
  }


  /* le couple d'entite n'existe pas, on renvoie 0 */

  if ((datagroup2 = _MEDdatagroupOuvrir(datagroup1,nomdatagroup)) < 0 ) goto SORTIE;

  /* erreur : le couple d'entite existe mais on
     ne peut lire l'attribut NBR */

  if ( _MEDattrEntierLire(datagroup2,MED_NOM_NBR,&n) < 0) {
    MESSAGE("Impossible de lire l'attribut NBR : ");
    SSCRUTE(chemin);SSCRUTE(MED_NOM_NBR); goto ERREUR;
  }

  /*
   * On ferme tout 
   */
  
 SORTIE:
  ret= n;
 ERREUR:

  if (datagroup2 > 0 ) if ( _MEDdatagroupFermer(datagroup2) < 0) {
    MESSAGE("Impossible de fermer le groupe  : ");
    SSCRUTE(chemin);SSCRUTE(nomdatagroup);ret=-1;
  }

  if (datagroup1 > 0 ) if ( _MEDdatagroupFermer(datagroup1) < 0) {
    MESSAGE("Impossible de fermer le groupe  : ");
    SSCRUTE(chemin);ret= -1;
  }

  return (med_int) ret;  

}



