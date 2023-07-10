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
MEDprofilInfo(med_idt fid, int indice, char *profilname, med_int *n)
{
  int numero=0;
  med_idt proid=0;
  med_err ret=-1;
  char chemin[MED_TAILLE_PROFILS+MED_TAILLE_NOM+1]="";

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On recupere le nom du groupe de rang "indice"
   */ 
  numero = indice-1;
  if ( _MEDobjetIdentifier(fid,MED_PROFILS,numero,profilname) < 0)
    goto ERROR;

  /*
   * On va chercher l'attribut taille du profil 
   */
  strcpy(chemin,MED_PROFILS);
  strcat(chemin,profilname);
  if ((proid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    goto ERROR;   
  if (_MEDattrEntierLire(proid,MED_NOM_NBR,n) < 0)
    goto ERROR;
 
  ret = 0;

 ERROR:
  if ( proid > 0 ) if (_MEDdatagroupFermer(proid) < 0)
    goto ERROR;
  
  return 0;
}


