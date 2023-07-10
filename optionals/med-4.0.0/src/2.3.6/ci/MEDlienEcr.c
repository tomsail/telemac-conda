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
 * - Nom de la fonction : MEDlienEcrire
 * - Description : Ecrit le chemin d'accès à un maillage distant
 * - Parametres :
 *   - fid     (IN) : ID du fichier HDF courant
 *   - lienval (IN) : le chemin d'accès au fichier contenant le maillage distant
 *   - maa     (IN) : le nom du maillage distant
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

med_err
MEDlienEcr(med_idt fid, char *lienval, char *maa)
{
  med_idt gid=0, chid=0, did=0;
  med_size dimd[1];
  med_err ret=-1;
  char chemin[MED_TAILLE_LIENS+1];
  med_int n ;
  
  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /* 
   * Si le groupe "LIENS" n'existe pas, on le cree
   */
  strncpy(chemin,MED_LIENS,MED_TAILLE_LIENS-1);
  chemin[MED_TAILLE_LIENS-1] = '\0';
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((gid = _MEDdatagroupCreer(fid,chemin)) < 0) {
      MESSAGE("Impossible de creer le groupe MED_LIENS : ");
      SSCRUTE(chemin); goto ERROR;
    }

  /*
   * Si le groupe "maa" n'existe pas, on le cree
   * Sinon => erreur
   */
  NOFINALBLANK(maa,ERROR);
  if ((chid = _MEDdatagroupOuvrir(gid,maa)) < 0) 
    if ((chid = _MEDdatagroupCreer(gid,maa)) < 0) {
      MESSAGE("Erreur à la création du lien au maillage : ");
      SSCRUTE(maa);
      goto ERROR;
    }

  /*
   * On stocke "n" sous forme d'attribut
   */
  n = strlen(lienval);
  if ((ret = _MEDattrEntierEcrire(chid,MED_NOM_NBR,&n)) < 0) {
    MESSAGE("Erreur à l'écriture de l'attribut n : ");
    ISCRUTE(n); goto ERROR;
  }

  /*
   * On stocke le lienval dans un dataset
   */
  dimd[0] = n;
  if ((ret = _MEDdatasetStringEcrire(chid,MED_NOM_LIE,dimd,lienval))<0) {
    MESSAGE("Impossible d'ecrire le dataset lienval de taille  : ");
    ISCRUTE(n); goto ERROR;
  }

  /*
   * On ferme tout
   */
  ret =0;
 ERROR:
  if (chid>0)     if (_MEDdatagroupFermer(chid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(chid);ret=-1; 
  }
  
  if (gid>0)     if (_MEDdatagroupFermer(gid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(gid);ret=-1;
  }

  return ret;
}
