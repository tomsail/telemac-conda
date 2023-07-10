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

#include <stdlib.h>
#include <string.h>

med_err
MEDstructureCoordLire(med_idt fid,char *maillage,med_int mdim,med_int *structure)
{
  med_idt maaid, noeid, dataset;
  med_err ret;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  char nom_dataset[MED_TAILLE_NOM_ENTITE+1];
  char nom_attribut[MED_TAILLE_NOM_ENTITE+1];
  med_int att;
  med_maillage maillage_type;
  med_type_grille type;
  med_int i;

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * Si le maillage n'existe pas => erreur
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maillage);
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
      return -1;

  /*
   * Si le maillage est de type MED_NON_STRUCTURE => erreur 
   */
  if ((ret = _MEDattrEntierLire(maaid,MED_NOM_TYP,&att)) < 0)
    return -1;
  maillage_type = (med_maillage) att;
  if (maillage_type == MED_NON_STRUCTURE)
    return -1;

  /*
   * Si la grille n'est pas de type MED_GRILLE_STANDARD  
   * => erreur
   */
  if ((ret = _MEDattrEntierLire(maaid,MED_NOM_GTY,&att)) < 0)
    return -1;
  type = (med_type_grille) att;
  if (type != MED_GRILLE_STANDARD)
    return -1;

  /*
   * Si le groupe HDF "NOE" n'existe pas => erreur
   */
  if ((noeid = _MEDdatagroupOuvrir(maaid,MED_NOM_NOE)) < 0)
      return -1;
  
  /*
   * On ouvre le dataset HDF pour y lire les attributs
   * (taille de chaque dimension)
   */
  strcpy(nom_dataset,MED_NOM_COO);
  if ((dataset = _MEDdatasetOuvrir(noeid,nom_dataset)) < 0)
    return -1;
  
  /*
   * Attribut NBR (taille du tableau d'indices)
   */
  for (i=0;i<mdim;i++) {

    switch(i) {
    case 0 :
      strcpy(nom_attribut,MED_NOM_IN1);
      break;
      
    case 1 :
      strcpy(nom_attribut,MED_NOM_IN2);
      break;

    case 2 :
      strcpy(nom_attribut,MED_NOM_IN3);
      break;

    default :
      return -1;
    }

    if ((ret = _MEDattrEntierLire(dataset,nom_attribut,&att)) < 0)
      return -1;
    *(structure+i) = att; 

  }

  /*
   * On ferme tout
   */
  if ((ret = _MEDdatasetFermer(dataset)) < 0)
    return -1;
  if ((ret = _MEDdatagroupFermer(noeid)) < 0)
    return -1;
  if ((ret = _MEDdatagroupFermer(maaid)) < 0)
    return -1;

  return 0; 
}
