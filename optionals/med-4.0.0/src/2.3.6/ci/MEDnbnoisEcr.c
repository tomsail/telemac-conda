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
MEDnbnoisEcr(med_idt fid, char *nom_maillage,med_int n)
{
  med_idt maaid;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  med_err ret;

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * Si le maillage n'existe pas => erreur
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,nom_maillage);
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
      return -1;

  /*
   * Creation de l'attribut "Nombre de Noeuds Isoles"
   */
  if ((ret = _MEDattrEntierEcrire(maaid,MED_NOM_NNI,&n)) < 0)
    return -1;

  /* 
   * Fermetures des objets
   */
  if ((ret = _MEDdatagroupFermer(maaid)) < 0)
    return -1;

  return 0;
}
  

  
