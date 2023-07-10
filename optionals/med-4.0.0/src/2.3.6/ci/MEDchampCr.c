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

med_err MEDchampCr( med_idt fid,  char *  champ,  med_type_champ type,  char *  comp,
		    char *  unit, med_int ncomp)
{
  med_err ret=-1;
  med_idt root=0,gid=0;
  char chemin[MED_TAILLE_CHA+1];
  med_size dimd[1];
  med_int _type = (med_int) type;
  med_mode_acces MED_MODE_ACCES;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier.");
    goto ERROR;
  }

  if ( MED_MODE_ACCES == MED_LECTURE ) {
    MESSAGE("Impossible de créer un champ en mode MED_LECTURE.");
    goto ERROR;
  };
  
  /* 
   * Si le Data Group "/CHA/" n'existe pas, on le cree
   */
  strncpy(chemin,MED_CHA,MED_TAILLE_CHA-1);
  chemin[MED_TAILLE_CHA-1] = '\0';
  if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((root = _MEDdatagroupCreer(fid,chemin)) < 0) {
      MESSAGE("Erreur à la création du datagroup : ");
      SSCRUTE(chemin);
      goto ERROR;
    }

  /* 
   * Si le Data Group cha n'existe pas, on le cree
   * Sinon => erreur
   */

  NOFINALBLANK(champ,ERROR);

  if ((gid = _MEDdatagroupCreer(root,champ)) < 0) {
    MESSAGE("Erreur à la création du champ : ");
    SSCRUTE(champ);
    goto ERROR;
  }

  /*
   * Les infos sur les composants du champ
   */
  if ( _MEDattrEntierEcrire(gid,MED_NOM_NCO,&ncomp) < 0 ) {
    MESSAGE("Erreur à l'écriture du nombre de composantes : ");
    ISCRUTE(ncomp);
    goto ERROR;
  }
  if ( _MEDattrEntierEcrire(gid,MED_NOM_TYP,&_type) < 0) {
    MESSAGE("Erreur à l'écriture du type du champ : ");
    ISCRUTE(_type);
    goto ERROR;
  }
  if ( _MEDattrStringEcrire(gid,MED_NOM_NOM,MED_TAILLE_PNOM*ncomp,comp) < 0) {
    MESSAGE("Erreur à l'écriture des noms des composantes : ");
    SSCRUTE(comp);
    goto ERROR;
  }
  if ( _MEDattrStringEcrire(gid,MED_NOM_UNI,MED_TAILLE_PNOM*ncomp,unit) < 0) {
    MESSAGE("Erreur à l'écriture des unités : ");
    SSCRUTE(unit);
    goto ERROR;
  }
  /*
   * On ferme tout
   */
  
  ret=0;
 ERROR:
  if (gid>0)     if (_MEDdatagroupFermer(gid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(gid);ret = -1; 
  }
  
  if (root>0)     if (_MEDdatagroupFermer(root) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(root); ret = -1; 
  }

  return ret;
}
