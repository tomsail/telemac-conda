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
MEDequivCr(med_idt fid,char *maa, char *eq, char *desc)
{
  med_idt root=0,eqid=0;
  med_err ret=-1;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_EQS+MED_TAILLE_NOM+1];
  char tmp[MED_TAILLE_EQS+1];
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
    MESSAGE("Impossible de créer une équivalence en mode MED_LECTURE.");
    goto ERROR;
  };

  /* 
   * Si le Data Group "EQS" n'existe pas, on le cree
   */
  strcpy(chemin,MED_MAA);
  NOFINALBLANK(maa,ERROR);
  strcat(chemin,maa);
  strncpy(tmp,MED_EQS,MED_TAILLE_EQS-1);
  tmp[MED_TAILLE_EQS-1] = '\0';
  strcat(chemin,tmp);
  if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((root = _MEDdatagroupCreer(fid,chemin)) < 0) {
      MESSAGE("Erreur à la création du datagroup : ");
      SSCRUTE(chemin);
      goto ERROR;
    }

  /*
   * Si une equivalence du meme nom existe => erreur
   * Sinon on la cree
   */
  NOFINALBLANK(eq,ERROR);
  if ((eqid = _MEDdatagroupCreer(root,eq)) < 0) {
    MESSAGE("Erreur à la création de l'équivalence : ");
    SSCRUTE(eq);
    goto ERROR;
  }

  /*
   * L'attribut "DES"
   */
  if ((ret = _MEDattrStringEcrire(eqid,MED_NOM_DES,MED_TAILLE_DESC,desc)) < 0) {
    MESSAGE("Erreur à l'écriture de la description de l'équivalence : ");
    SSCRUTE(desc);
    goto ERROR;
  }

  /*
   * On ferme tout 
   */
  ret=0;
 ERROR:
  if (eqid>0)     if (_MEDdatagroupFermer(eqid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(eqid);ret = -1; 
  }
  
  if (root>0)     if (_MEDdatagroupFermer(root) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(root); ret = -1; 
  }

  return ret;
}



