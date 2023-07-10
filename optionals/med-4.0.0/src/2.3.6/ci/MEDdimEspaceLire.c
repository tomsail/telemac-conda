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

med_int 
MEDdimEspaceLire(med_idt fid, char *maillage)
{
  med_idt maaid=0;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  med_int dim=-1;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On regarde si le groupe existe => erreur si non 
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maillage);  
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    goto ERROR;

  /*
   * On va lire l'attribut dimension de l'espace
   */
  if (_MEDattrEntierLire(maaid,MED_NOM_ESP,&dim) < 0)
    goto ERROR;

  /*
   * Fermetures des objets HDF 
   */
  
 ERROR:

  if ( maaid> 0 ) if ( _MEDdatagroupFermer(maaid) < 0) {
  MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(maaid);dim = -1; 
  }

  return dim;
}
