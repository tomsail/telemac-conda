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
#include "med_config.h"
#include "med_outils.h"
#include <string.h>

med_err MEDlienInfo(_IN med_idt fid, _IN int indice, _OUT char * maa, _OUT med_int * n)
{

  int numero;
  med_idt lnid;
  char chemin[MED_TAILLE_LIENS+MED_TAILLE_NOM+1];

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On recupere le nom du groupe de rang "indice"
   */ 
  
  strcpy(chemin,MED_LIENS); 
  numero = indice-1;
  
  if ( _MEDobjetIdentifier(fid,chemin,numero,maa) < 0) {
    MESSAGE("Impossible d'itérer sur le groupe : ");
    SSCRUTE(MED_LIENS); return -1;
  }
  
  /*
   * On va chercher l'attribut taille du profil 
   */
  strcat(chemin,maa);

  if ((lnid = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Impossible d'ouvrir le datagroup  : ");
    SSCRUTE(chemin); return -1;
  }
  
  if ( _MEDattrEntierLire(lnid,MED_NOM_NBR,n) < 0) {
    MESSAGE("Impossible de lire l'attribut  : ");
    SSCRUTE(chemin);SSCRUTE(MED_NOM_NBR); return -1;
  }

  if ( _MEDdatagroupFermer(lnid) < 0) {
    MESSAGE("Impossible de fermer le groupe  : ");
    SSCRUTE(chemin);return -1;
  }

  return 0;

}
