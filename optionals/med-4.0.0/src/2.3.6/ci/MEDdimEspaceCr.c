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
MEDdimEspaceCr(med_idt fid, char *maillage, med_int dim)
{
  med_idt maaid=0;
  med_err ret=-1;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  med_int maadim;
  med_mode_acces MED_MODE_ACCES;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier.");
    goto ERROR;
  }

  if ( MED_MODE_ACCES == MED_LECTURE ) {
    MESSAGE("Impossible de créer un maillage en mode MED_LECTURE.");
    goto ERROR;
  };

  /*
   * On regarde si le groupe existe => erreur si non
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maillage);  
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Erreur à l'ouverture du maillage : ");
    SSCRUTE(chemin);
    goto ERROR;
  }

  /*
   * On va lire l'attribut dimension du maillage et on controle 
   * la coherence
   */
  if ((ret = _MEDattrEntierLire(maaid,MED_NOM_DIM,&maadim)) < 0) {
    MESSAGE("Erreur à la lecture de la dimension du maillage : ");
    ISCRUTE(maadim);
    goto ERROR;
  }

  if (maadim > dim) {
    MESSAGE("La dimension de l'espace : ");
    ISCRUTE(dim);
    MESSAGE("doit être supérieur à la dimension du maillage : ");
    ISCRUTE(maadim);
    goto ERROR;
  }

  /*
   * On ecrit la dimension de l'espace
   */
  if ((ret = _MEDattrEntierEcrire(maaid,MED_NOM_ESP,&dim)) < 0)  {
    MESSAGE("Erreur à l'écriture de la dimension de l'espace : ");
    ISCRUTE(dim);
    goto ERROR;
  }

  /*
   * Fermetures des objets HDF 
   */
  ret = 0;
 ERROR:
  if (maaid>0)     if (_MEDdatagroupFermer(maaid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(maaid);ret = -1; 
  }
  
  return ret;

}
