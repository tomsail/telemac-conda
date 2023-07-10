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

/*
 * - Nom de la fonction : MEDlienLire
 * - Description : Ecrit le chemin d'accès à un maillage distant
 * - Parametres :
 *   - fid     (IN) : ID du fichier HDF courant
 *   - lienval (IN) : le chemin d'accès au fichier contenant le maillage distant
 *   - maa     (IN) : le nom du lien à lire <=> nom du maillage
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

med_err
MEDlienLire(_IN med_idt fid, _OUT char * lienval, _IN char * maa)
{
  med_err ret = 0;
  med_idt gid;
  char chemin[MED_TAILLE_LIENS+MED_TAILLE_NOM+1]; 

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;

/*   SSCRUTE(maa); */
/*   SSCRUTE(lienval); */
  /* 
   * ouverture du groupe /LIENS/"maa"
   */  
  strcpy(chemin,MED_LIENS);
  strcat(chemin,maa); 
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Impossible d'ouvrir le datagroup <maa> : ");
    SSCRUTE(maa); return -1;
  }
/*   SSCRUTE(maa); */
/*   SSCRUTE(lienval); */

  /*
   * Lecture du lien
   */

  if ((ret = _MEDdatasetStringLire(gid,MED_NOM_LIE,lienval)) < 0) {
    MESSAGE("Erreur à la lecture du dataset lienval : ");
    SSCRUTE(lienval); return -1;
  }
/*   SSCRUTE(maa); */
/*   SSCRUTE(lienval); */
  
  /*
   * On ferme tout
   */
  if ((ret = _MEDdatagroupFermer(gid)) < 0)
    return -1; 

  return ret;
}
