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

med_int
MEDlFichDes(med_idt fid)
{
  med_idt attr, root;
  med_err ret=0;
  char des[MED_TAILLE_DESC+1];
  med_int longueur=0;
  char nom[MED_TAILLE_NOM+1];
  char chemin[MED_TAILLE_MAA+1];

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On ouvre le Data Group racine
   */
  strncpy(chemin,MED_MAA,MED_TAILLE_MAA-1);
  chemin[MED_TAILLE_MAA-1] = '\0';
  if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    return -1;

  /*
   * On regarde si l'attribut existe
   * Si non => erreur
   * Si oui => on retourne sa longueur
   */
  strcpy(nom,MED_NOM_DESCRIPTEUR);
 
  if ((attr = _MEDattrOuvrir(root,nom)) < 0) {
    _MEDdatagroupFermer(root);
    longueur=0;
    return 0;
  }
 
  if ((ret = _MEDattrFermer(attr)) < 0) {
    _MEDdatagroupFermer(root);
    return -1;
  }
  
  if ((ret = _MEDattrStringLire(root,nom,MED_TAILLE_DESC,des)) < 0) {
    _MEDdatagroupFermer(root);
    return -1;
  }
  
  longueur = strlen(des);

  /*
   * fermetures 
   */
  if ( _MEDdatagroupFermer(root) < 0)
    return -1;

  return longueur;
}
