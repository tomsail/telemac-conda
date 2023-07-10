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
MEDjointInfo(med_idt fid,  char *maa_lcl,  int ind, char *jn, char *des,
             med_int *dom, char *maa_dist)
{
  med_idt jntid;
  med_err ret;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_JNT+MED_TAILLE_NOM+1];
  int num;
  int idx;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On recupere le nom du joint
   */
  num = ind - 1;
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa_lcl);
  strcat(chemin,MED_JNT); 
  if ((idx = _MEDobjetIdentifier(fid,chemin,num,jn)) < 0)
    return -1;

  /* 
   * Si le Data Group JNT n'existe pas => erreur
   */
  strcat(chemin,jn);
  if ((jntid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
      return -1;

  /*
   * L'attribut "DES"
   */
  if ((ret = _MEDattrStringLire(jntid,MED_NOM_DES,MED_TAILLE_DESC,des)) < 0)
    return -1;

  /*
   * L'attribut "MAI"
   */
  if ((ret = _MEDattrStringLire(jntid,MED_NOM_MAI,MED_TAILLE_NOM,maa_dist)) < 0)
    return -1;

  /*
   * L'attribut "DOM"
   */
  if ((ret = _MEDattrEntierLire(jntid,MED_NOM_DOM,dom)) < 0)
    return -1;

  /* 
   * Nombre du nombre de correspondances dans le joint
   */
  /*  *nc = 0;
     _MEDnObjets(fid,chemin,nc);
  */


  /*
   * On ferme tout 
   */
  if ((ret = _MEDdatagroupFermer(jntid)) < 0)
    return -1;

  return ret;
}



