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

int
MEDequivInfo(med_idt fid, char *maa, int ind, char *eq, char *des)
{
  med_idt eqid;
  med_err ret;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_EQS+2*MED_TAILLE_NOM+1];
  int num;
  int idx;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On recupere le nom de l'equivalence
   */
  num = ind - 1;
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  strcat(chemin,MED_EQS); 
  if ((idx = _MEDobjetIdentifier(fid,chemin,num,eq)) < 0)
    return -1;

  /* 
   * Si le Data Group eq n'existe pas => erreur
   */
  strcat(chemin,eq);
  if ((eqid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
      return -1;

  /*
   * L'attribut "DES"
   */
  if ((ret = _MEDattrStringLire(eqid,MED_NOM_DES,MED_TAILLE_DESC,des)) < 0)
    return -1;

  /*
   * On ferme tout 
   */
  if ((ret = _MEDdatagroupFermer(eqid)) < 0)
    return -1;

  return 0;
}



