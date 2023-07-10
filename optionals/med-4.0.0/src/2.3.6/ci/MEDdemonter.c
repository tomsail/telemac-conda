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
MEDdemonter(med_idt fid,med_idt mid, med_type_donnee type)
{
  med_err ret;
  med_idt root,did;
  char chemin[2*MED_TAILLE_NOM+1];
  char acces_montage[2*MED_TAILLE_NOM+1];

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On accede au type de la donnee
   */
  switch(type) {

  case MED_MAILLAGE :
   strcpy(chemin,MED_MAA);
    break;

  case MED_CHAMP :
    strcpy(chemin,MED_CHA);
    break;

  default :
    return -1;
  }


  /* 
   * On supprime le lien avec les champs|maillages du fichier a demonter 
   */
  chemin[strlen(chemin)-1] = '\0'; 
  if ((ret = _MEDdatagroupLienSupprimer(fid,chemin)) < 0)
    return -1;    

  /*
   * On demonte le fichier dans MED_MNT
   */
  strncpy(acces_montage,MED_MNT,strlen(MED_MNT)-1);
  acces_montage[strlen(MED_MNT)-1] = '\0';
  if ((ret = _MEDfichierDemonter(fid,acces_montage)) < 0)
    return -1;

  /*
   * On ferme le fichier que l'on vient de demonter
   */
  if ((ret = _MEDfichierFermer(mid)) < 0)
    return -1;  

  return ret;
}
