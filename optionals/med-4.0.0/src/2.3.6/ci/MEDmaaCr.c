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

med_err 
MEDmaaCr(med_idt fid, char *maillage, med_int dim, med_maillage type,char *desc)
{
  med_idt maaid=0, root=0, entid=0, geoid=0 ;
  char chemin[MED_TAILLE_MAA+1];
  char nom_ent[MED_TAILLE_NOM_ENTITE+1];
  char nom_geo[MED_TAILLE_NOM_ENTITE+1];
  med_err ret=-1;
  med_int tmp;
  med_mode_acces MED_MODE_ACCES;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier.");
    goto ERROR;
  }

  if ( MED_MODE_ACCES == MED_LECTURE ) {
    MESSAGE("Impossible de créer un maillage en mode MED_LECTURE.");
    goto ERROR;
  };

  /*
   * Si la racine n'existe pas on la cree
   */
  strncpy(chemin,MED_MAA,strlen(MED_MAA)-1);
  chemin[MED_TAILLE_MAA-1] = '\0';
  if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((root = _MEDdatagroupCreer(fid,chemin)) < 0) {
      MESSAGE("Erreur à la création du datagroup : ");
      SSCRUTE(chemin);
      goto ERROR;
    }

  /*
   * Création du maillage
   */
  NOFINALBLANK(maillage,ERROR);
  if ( (maaid = _MEDdatagroupCreer(root,maillage)) < 0) {
    MESSAGE("Erreur à la création du maillage : ");
    SSCRUTE(maillage);
    goto ERROR;
  }

  /*
   * Creation de l'attribut dimension
   */
  if ((ret = _MEDattrEntierEcrire(maaid,MED_NOM_DIM,&dim)) < 0) {
    MESSAGE("Erreur à l'écriture de la dimension du maillage : ");
    ISCRUTE(dim);
    goto ERROR;
  }
  
  /*
   * La description associee au maillage
   */
  if ((ret = _MEDattrStringEcrire(maaid,MED_NOM_DES,MED_TAILLE_DESC,desc)) < 0) {
    MESSAGE("Erreur à l'écriture de la description du maillage : ");
    SSCRUTE(desc);
    goto ERROR;
  }

  /*
   * Creation de l'attribut correspondant au type du maillage (MED_STRUCTURE, MED_NON_STRUCTURE)
   */
  tmp = (med_int) type;
  if ((ret = _MEDattrEntierEcrire(maaid,MED_NOM_TYP,&tmp)) < 0) {
    MESSAGE("Erreur à l'écriture du type de maillage : ");
    ISCRUTE(tmp);
    goto ERROR;
  }

  if ( type == MED_STRUCTURE )  {

    strcpy(nom_ent,MED_NOM_MAI);
    if ((entid = _MEDdatagroupCreer(maaid,nom_ent)) < 0) {
      MESSAGE("Impossible de Cr\351er le datagroup :");
      SSCRUTE(nom_ent);
      MESSAGE("pour le maillage structur\351 :");
      SSCRUTE(maillage);
      goto ERROR;
    }

    switch ( dim )  {
    case 1 : strcpy(nom_geo,MED_NOM_SE2);
      break;
    case 2 : strcpy(nom_geo,MED_NOM_QU4);
      break;
    case 3 : strcpy(nom_geo,MED_NOM_HE8);
      break;
    case 0 : strcpy(nom_geo,MED_NOM_PO1);
      break;
    default :
      MESSAGE("La dimension doit être comprise entre 0 et 3");
      goto ERROR;
    }

    if ((geoid = _MEDdatagroupCreer(entid,nom_geo)) < 0) {
      MESSAGE("Impossible de Créer le datagroup :");
      SSCRUTE(nom_geo);
      MESSAGE("pour le maillage structuré :");
      SSCRUTE(maillage);
      goto ERROR;
    }
  }

  /* 
   * Nettoyages divers
   */
  ret = 0;
 ERROR:
  if (geoid>0)     if (_MEDdatagroupFermer(geoid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(geoid);ret = -1; 
  }

  if (entid>0)     if (_MEDdatagroupFermer(entid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(entid);ret = -1; 
  }

  if (maaid>0)     if (_MEDdatagroupFermer(maaid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(maaid);ret = -1; 
  }
  
  if (root>0)     if (_MEDdatagroupFermer(root) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(root); ret = -1; 
  }

  return ret;
}
  

  
