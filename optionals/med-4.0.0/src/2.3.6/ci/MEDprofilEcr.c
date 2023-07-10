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
MEDprofilEcr(med_idt fid,med_int *pflval,med_int n,char *profilname)
{
  med_idt gid, chid;
  med_size dimd[1];
  med_err ret=-1;
  char chemin[MED_TAILLE_PROFILS+1];
  med_mode_acces MED_MODE_ACCES;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier ");
    goto ERROR;
  }

  /* 
   * Si le groupe "PROFILS" n'existe pas, on le cree
   */
  strncpy(chemin,MED_PROFILS,MED_TAILLE_PROFILS-1);
  chemin[MED_TAILLE_PROFILS-1] = '\0';
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((gid = _MEDdatagroupCreer(fid,chemin)) < 0) {
      MESSAGE("Impossible de creer le groupe MED_PROFILS : ");
      SSCRUTE(chemin); goto ERROR;
    }

  /* 
   * Si le groupe "profilname" n'existe pas, on le cree
   * Sinon => erreur
   */
  NOFINALBLANK(profilname,ERROR);
  if ((chid = _MEDdatagroupOuvrir(gid,profilname)) >= 0) {
    if ( MED_MODE_ACCES != MED_LECTURE_ECRITURE ) {
      MESSAGE("Le profil existe déjà : ");
      SSCRUTE(profilname); goto ERROR;
    }
  } else
    if ((chid = _MEDdatagroupCreer(gid,profilname)) < 0)
      goto ERROR;

  /*
   * On stocke "n" sous forme d'attribut
   */
  if ((ret = _MEDattrEntierEcrire(chid,MED_NOM_NBR,&n)) < 0) {
    MESSAGE("Erreur à l'écriture de l'attribut MED_NOM_NBR : ");
    ISCRUTE(n); goto ERROR;
  };

  /*
   * On stocke le profil dans un dataset
   */
  dimd[0] = n;
#if defined(HAVE_F77INT64)
  ret =  _MEDdatasetNumEcrire(chid,MED_NOM_PFL,MED_INT64,MED_NO_INTERLACE,MED_DIM1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
			      (unsigned char*) pflval);
#else
  ret =  _MEDdatasetNumEcrire(chid,MED_NOM_PFL,MED_INT32,MED_NO_INTERLACE,MED_DIM1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
			      (unsigned char*) pflval);
#endif
  if (ret < 0 ) {
    MESSAGE("Impossible d'ecrire le dataset pflval de taille  : ");
    ISCRUTE(n); goto ERROR;
  }

  ret = 0;
 ERROR:
  /*
   * On ferme tout
   */
  if (chid>0)     if (_MEDdatagroupFermer(chid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(chid); ret = -1;
  }

  if (gid>0)     if (_MEDdatagroupFermer(gid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(gid); ret = -1;
  }

  return ret;
}




