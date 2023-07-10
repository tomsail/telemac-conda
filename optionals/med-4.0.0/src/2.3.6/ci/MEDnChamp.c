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
#include <string.h>
#include <stdlib.h>
#include <med_outils.h>

med_int 
MEDnChamp(med_idt fid, int indice)
{
  int n1;
  med_int n2;
  med_idt datagroup;
  med_err ret;
  char nomdatagroup[MED_TAILLE_NOM+1];
  int num;
  char chemin[MED_TAILLE_CHA+MED_TAILLE_NOM+1];

  if (indice < 0)
    return -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;

  
  /* 
   * Si le Data Group cha n'existe pas et indice == 0 => 0
   * sinon erreur => erreur
   */
  strcpy(chemin,MED_CHA);

  /*
   * Si indice == 0 => nombre de champs
   */
  if (indice == 0)
    {
      n1 = 0;
      _MEDnObjets(fid,chemin,&n1);
      n2 = n1;
    }

  /*
   * Si indice > 0 => nbre de composants
   */
  if (indice > 0)
    {
      /*
       * On recupere le nom du champ 
       */
      num = indice-1;
      if ((ret = _MEDobjetIdentifier(fid,chemin,num,nomdatagroup)) < 0)
	return -1;
      strcat(chemin,nomdatagroup);
      /*
       * On recupere le nombre de composants
       */
      if ((datagroup = _MEDdatagroupOuvrir(fid,chemin)) < 0) 
	return -1;
      if ((ret = _MEDattrEntierLire(datagroup,MED_NOM_NCO,&n2)) < 0)
	return -1;
      if ((ret = _MEDdatagroupFermer(datagroup)) < 0)
	return -1;
    }

  return n2;
}

