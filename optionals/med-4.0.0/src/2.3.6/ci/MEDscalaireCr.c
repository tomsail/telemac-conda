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

med_err
MEDscalaireCr(med_idt fid,char *scalaire, med_type_champ type, char *desc)
{
  med_err ret=-1;
  med_idt root=0,gid=0;
  char chemin[MED_TAILLE_NUM_DATA+1];
  med_int _type = (med_int) type;
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
    MESSAGE("Impossible de créer une variable scalaire en mode MED_LECTURE.");
    goto ERROR;
  };

  /* 
   * Si le Groupe  HDF "/NUM_DATA" n'existe pas, on le cree
   */
  strncpy(chemin,MED_NUM_DATA,MED_TAILLE_NUM_DATA-1);
  chemin[MED_TAILLE_NUM_DATA-1] = '\0';
  if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((root = _MEDdatagroupCreer(fid,chemin)) < 0){
      MESSAGE("Erreur à la création du datagroup : ");
      SSCRUTE(chemin);
      goto ERROR;
    }

  /* 
   * Si le groupe HDF "scalaire" n'existe pas, on le cree
   * Sinon => erreur
   */
  NOFINALBLANK(scalaire,ERROR);
  if ((gid = _MEDdatagroupCreer(root,scalaire)) < 0) {
    MESSAGE("Erreur à la création de la variable scalaire : ");
    SSCRUTE(scalaire);
    goto ERROR;
  }

  /* 
   * On stocke l'unite, et le type de la valeur scalaire
   */
  if ((ret = _MEDattrEntierEcrire(gid,MED_NOM_TYP,&_type)) < 0) {
    MESSAGE("Erreur à l'écriture du type de la varaible scalaire : ");
    ISCRUTE(_type);
    goto ERROR;
  }
  if ((ret = _MEDattrStringEcrire(gid,MED_NOM_DES,MED_TAILLE_DESC,desc)) < 0) {
    MESSAGE("Erreur à l'écriture de la description de la variable scalaire : ");
    SSCRUTE(desc);
    goto ERROR;
  }

  /*
   * On ferme tout
   */
  ret = 0;
 ERROR:
  if (gid>0)     if (_MEDdatagroupFermer(gid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(gid);ret=-1; 
  }
  
  if (root>0)     if (_MEDdatagroupFermer(root) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(root);ret=-1; 
  }

  return ret;
}
