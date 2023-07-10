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
MEDindicesCoordEcr(med_idt fid,char *maillage,med_int mdim,med_float *indices,
		   med_int n,med_int axe,char *comp,char *unit)
{
  med_idt maaid, noeid, dataset;
  med_err ret;
  med_size dimd[1];
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  char nom_dataset[MED_TAILLE_NOM_ENTITE+1];
  med_int att;
  med_maillage maillage_type;
  med_type_grille type;

  /*
   * Si axe > mdim => erreur
   */
  if (axe > mdim)
    return -1;

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
   * Si la grille n'est pas de type MED_GRILLE_CARTESIENNE ou 
   * MED_GRILLE_POLAIRE => erreur
   */
  if ((ret = _MEDattrEntierLire(maaid,MED_NOM_GTY,&att)) < 0)
    return -1;
  /* sizeof(enum) tjrs = sizeof(int) en C, or
     sur machines 64 bits par défaut med_int==long (normalement 64bits),
     du coup sur  machines 64 bits _MEDattrEntierLire utilise
     le type hdf NATIVE_LONG, ce genere parfois un warning
  */
  type = (med_type_grille) att;
  if ((type != MED_GRILLE_CARTESIENNE) && (type != MED_GRILLE_POLAIRE))
    return -1;

  /*
   * Si le groupe HDF "NOE" n'existe pas => erreur
   */
  if ((noeid = _MEDdatagroupOuvrir(maaid,MED_NOM_NOE)) < 0)
    if ((noeid = _MEDdatagroupCreer(maaid,MED_NOM_NOE)) < 0)
      return -1;

  /*
   * On ecrit le tableau d'indice dans un dataset HDF
   */
  switch(axe) {

  case 1 :
    strcpy(nom_dataset,MED_NOM_IN1);
    break;

  case 2 :
    strcpy(nom_dataset,MED_NOM_IN2);
    break;

  case 3 :
    strcpy(nom_dataset,MED_NOM_IN3);
    break;

  default :
    return -1;

  }

  dimd[0] = n;
  if ((ret = _MEDdatasetNumEcrire(noeid,nom_dataset,MED_FLOAT64,MED_FULL_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,
				  MED_NOPG,dimd,(unsigned char*) indices)) < 0)
    return -1;
  
  /*
   * On re-ouvre le dataset HDF pour y placer des attributs
   */
  if ((dataset = _MEDdatasetOuvrir(noeid,nom_dataset)) < 0)
    return -1;

  /*
   * Attribut NBR (taille du tableau d'indices)
   */
  if ((ret = _MEDattrEntierEcrire(dataset,MED_NOM_NBR,&n)) < 0)
    return -1;

  /*
   * Attribut "NOM" (nom de la composante)
   */
  if ((ret = _MEDattrStringEcrire(dataset,MED_NOM_NOM,MED_TAILLE_PNOM,comp)) < 0)
    return -1;

  /*
   * Attribut "UNI" (unite de la composante)
   */
  if ((ret = _MEDattrStringEcrire(dataset,MED_NOM_UNI,MED_TAILLE_PNOM,unit)) < 0)
    return -1;

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
