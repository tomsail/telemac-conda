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
MEDfichDesEcr(med_idt fid, char *des)
{ 
  med_idt attr; 
  med_idt root;
  med_err ret;
  char nom[] = MED_NOM_DESCRIPTEUR;
  char chemin[MED_TAILLE_MAA+1];

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On ouvre le Data Group racine
   * s'il n'existe pas on le cree
   */
  strncpy(chemin,MED_MAA,MED_TAILLE_MAA-1);
  chemin[MED_TAILLE_MAA-1] = '\0';
  if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((root = _MEDdatagroupCreer(fid,chemin)) < 0)
      return -1;

  /*
   * On regarde si l'attribut existe
   * Si oui on le met a jour en fonction
   * du mode d'ouverture, sinon on le cree
   */

  if ((ret = _MEDattrStringEcrire(root,nom,MED_TAILLE_DESC,des)) < 0)
    return -1;

  /*
   * Fermetures 
   */

  if ((ret = _MEDdatagroupFermer(root)) < 0)
    return -1;
	  
  return 0; 
}




