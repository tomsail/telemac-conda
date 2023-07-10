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
MEDmaaInfo(med_idt fid, int indice, char *maillage, med_int *dim, 
	   med_maillage *type,char *desc)
{
  int numero;
  med_idt maaid;
  med_err ret;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  med_int tmp;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On recupere le nom du groupe de rang "indice"
   */ 
  numero = indice-1;
  if ((ret = _MEDobjetIdentifier(fid,MED_MAA,numero,maillage)) < 0)
    return -1;

  strcpy(chemin,MED_MAA);
  strcat(chemin,maillage);
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    return -1;   

  /*
   * On va chercher l'attribut dimension 
   */
  if ((ret = _MEDattrEntierLire(maaid,MED_NOM_DIM,dim)) < 0)
    return -1;

  /*
   * On va chercher le descripteur
   */
  if ((ret = _MEDattrStringLire(maaid,MED_NOM_DES,MED_TAILLE_DESC,desc)) < 0)
    return -1;

  /*
   * On va chercher l'attribut "type" 
   */
  if ((ret = _MEDattrEntierLire(maaid,MED_NOM_TYP,&tmp)) < 0)
    return -1;
  *type = (med_maillage) tmp;

  /*
   * On ferme tout
   */
  if ((ret = _MEDdatagroupFermer(maaid)) < 0)
    return -1;

  return 0;
}


