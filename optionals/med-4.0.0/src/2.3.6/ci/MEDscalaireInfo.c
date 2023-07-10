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
MEDscalaireInfo(med_idt fid,int indice,char *scalaire,
		med_type_champ *type,char *desc)
{
  med_err ret=0;
  med_idt gid;
  char chemin[MED_TAILLE_NUM_DATA+MED_TAILLE_NOM+1];
  int num;
  med_int typechamp;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On recupere le nom du scalaire
   */
  num = indice - 1;
  strcpy(chemin,MED_NUM_DATA);
  if ((ret = _MEDobjetIdentifier(fid,chemin,num,scalaire)) < 0)
    return -1;


  /* 
   * Si le Groupe HDF "scalaire" n'existe pas => erreur
   */
  strcat(chemin,scalaire);
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    return -1;


  /*
   * La liste des attributs
   */
  if ((ret = _MEDattrEntierLire(gid,MED_NOM_TYP, & typechamp)) < 0)
    return -1;
  *type = (med_type_champ) typechamp;

  if ((ret = _MEDattrStringLire(gid,MED_NOM_DES,MED_TAILLE_DESC,
				desc)) < 0)
    return -1;

  /*
   * On ferme tout
   */
  if ((ret = _MEDdatagroupFermer(gid)) < 0)
    return -1; 

  return ret;
}
