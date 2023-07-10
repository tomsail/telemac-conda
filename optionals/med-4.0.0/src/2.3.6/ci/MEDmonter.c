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
 
#ifdef PPRO_NT_CALL
#define F_OK 0
#else
#include <unistd.h>
#endif

med_idt
MEDmonter(med_idt fid, const char *acces,med_type_donnee type)
{
  med_err ret;
  med_idt root,id, did;
  char chemin[MED_TAILLE_NOM+1];
  char acces_montage[2*MED_TAILLE_NOM+1];
  med_mode_acces MED_MODE_ACCES;

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On regarde si le fichier de nom "acces" existe
   * Si ce n'est pas le cas => erreur
   */
  if (access(acces,F_OK))
    return -1;

  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier ");
    return -1;
  }

  /*
   * On ouvre le fichier "acces" selon le mode d'acces MED_MODE_ACCES
   */
  if ((id = _MEDfichierOuvrir((char *)acces,MED_MODE_ACCES)) < 0)
    return -1;

  /* 
   * Si le point de montage n'existe pas dans le fichier local, on le cree
   */
  root = _MEDdatagroupOuvrir(fid,MED_MNT);
  if (root < 0)
    if ((root = _MEDdatagroupCreer(fid,MED_MNT)) < 0)
      return -1;
  if ((ret = _MEDdatagroupFermer(root)) < 0)
    return -1;

  /*
   * Montage du fichier (id) sur le point de montage local (fid)
   */
  strncpy(acces_montage,MED_MNT,strlen(MED_MNT)-1);
  acces_montage[strlen(MED_MNT)-1] = '\0';
  if ((ret = _MEDfichierMonter(fid,acces_montage,id)) < 0) return -1;

  /*
   * On cree un lien vers le point de montage
   * selon qu'on l'on veuille acceder aux champs
   * ou aux maillages du fichier "acces" que l'on vient
   * de monter sur le fichier local "fid"
   */
  switch(type) {

  case MED_MAILLAGE :
    strcpy(chemin,MED_MAA);
    break;

  case MED_CHAMP :
    strcpy(chemin,MED_CHA);
    break;

  default :
    return -1;
  }
  
  /* 
   * Creation du lien HDF vers le(s) champ(s) ou le(s) maillage(s) 
   * dans le fichier local :
   * - Si ce type de donnee existe deja en local => erreur
   * - Sinon on le cree le lien vers le point de montage
   */
  strcat(acces_montage,chemin);
  acces_montage[strlen(acces_montage)-1] = '\0';
  chemin[strlen(chemin)-1] = '\0';
  if ((ret = _MEDdatagroupLienCreer(fid,acces_montage,chemin)) < 0)
    return -1;

  /*
   * On renvoie l'ID du fichier "acces" que l'on vient de monter
   */
  return id;
}
