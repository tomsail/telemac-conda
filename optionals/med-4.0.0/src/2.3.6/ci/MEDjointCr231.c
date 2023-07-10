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

void 
MEDjointCr231(int dummy,...) {


  med_idt   fid      ;
  char *    maa_lcl  ;
  char *    jn       ;
  char *    desc     ;
  med_int   dom      ;
  char *    maa_dist ;
  med_err * fret     ;
 
  med_idt root=0,jntid=0;
  med_err ret=-1;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_JNT+MED_TAILLE_NOM+1];
  char tmp[MED_TAILLE_JNT+1];
  med_int nbc=0;
  med_mode_acces MED_MODE_ACCES;

  va_list params;
  va_start(params,dummy);

  fid      = va_arg(params,med_idt);
  maa_lcl  = va_arg(params,char *);
  jn       = va_arg(params,char *);
  desc     = va_arg(params,char *);
  dom      = va_arg(params,med_int);
  maa_dist = va_arg(params,char*);
  fret     = va_arg(params,med_err *);

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) {*fret=-1;return;}


  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier.");
    goto ERROR;
  }

  if ( MED_MODE_ACCES == MED_LECTURE ) {
    MESSAGE("Impossible de créer un joint en mode MED_LECTURE.");
    goto ERROR;
  };

  /* 
   * Si le Data Group "JNT" n'existe pas, on le cree
   */
  strcpy(chemin,MED_MAA);
  NOFINALBLANK(maa_lcl,ERROR);
  strcat(chemin,maa_lcl);
  strncpy(tmp,MED_JNT,MED_TAILLE_JNT-1);
  tmp[MED_TAILLE_JNT-1] = '\0';
  strcat(chemin,tmp);
  if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((root = _MEDdatagroupCreer(fid,chemin)) < 0) {
      MESSAGE("Erreur à la création du datagroup : ");
      SSCRUTE(chemin);
      goto ERROR;
    }

  /*
   * Si un joint du meme nom existe => erreur
   * Sinon on le cree
   */
  NOFINALBLANK(jn,ERROR);
  if ((jntid = _MEDdatagroupCreer(root,jn)) < 0) {
    MESSAGE("Erreur à la création du joint : ");
    SSCRUTE(jn);
    goto ERROR;
  }

  /*
   * L'attribut "DES"
   */
  if ((ret = _MEDattrStringEcrire(jntid,MED_NOM_DES,MED_TAILLE_DESC,desc)) < 0) {
    MESSAGE("Erreur à l'écriture de la description du joint : ");
    SSCRUTE(desc);
    goto ERROR;
  }

  /*
   * L'attribut "MAI"
   */
  /* BUG CORRIGE EN 232 MED_TAILLE_NOM au lieu de MED_TAILLE_MAA */
  /* conservé ici pour éviter de reprendre en lib2.3.2 un fichier 2.3.1
     en écrivant l'attribut avec une taille plus grande que celle des fichiers 2.3.1 */
  if ((ret = _MEDattrStringEcrire(jntid,MED_NOM_MAI,MED_TAILLE_MAA,maa_dist)) < 0) {
    MESSAGE("Erreur à l'écriture du nom du maillage distant : ");
    SSCRUTE(maa_dist);
    goto ERROR;
  }

  /*
   * L'attribut "DOM"
   */
  if ((ret = _MEDattrEntierEcrire(jntid,MED_NOM_DOM,&dom)) < 0) {
    MESSAGE("Erreur à l'écriture du domaine : ");
    ISCRUTE(dom);
    goto ERROR;
  }

  /*
   * On ferme tout 
   */
  ret=0;
 ERROR:

  if (jntid>0)     if (_MEDdatagroupFermer(jntid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(jntid);ret = -1; 
  }
  
  if (root>0)     if (_MEDdatagroupFermer(root) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(root); ret = -1; 
  }

  va_end(params);
  *fret = ret;
  return;
}



