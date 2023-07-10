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

#ifdef PPRO_NT_CALL
#define F_OK 0
#else
#include <unistd.h>
#endif

/* Variable globale qui gere le mode d'acces aux fichiers */
/* ATTENTION : si on ouvre plusieurs fichiers, il faut les
   ouvrir tous avec le meme mode d'acces */
med_mode_acces MED_MODE_ACCES;


med_idt
MEDouvrir(char *nom, med_mode_acces mode_acces)
{
  med_idt fid=0; 

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On ouvre le fichier MED sous HDF
   */
  switch(mode_acces)
    {
    case MED_LECTURE :
      if (access(nom,F_OK)) {

	MESSAGE("Impossible d'accéder aux fichier :");
	SSCRUTE(nom);
	return -1;
	
      } else { 
	if ((fid = _MEDfichierOuvrir(nom,mode_acces)) < 0)  return -1;
	/*_MEDsetModeAcces(fid,MED_LECTURE);*/
      };
      break;

    case MED_LECTURE_ECRITURE : 
      if (access(nom,F_OK)) {
	  if ((fid = _MEDfichierCreer(nom,mode_acces)) < 0)
	    return -1;
      } else 
	if ((fid = _MEDfichierOuvrir(nom,mode_acces)) < 0)
	  return -1;
      break;

    case MED_LECTURE_AJOUT    :
      if (access(nom,F_OK))
	{
	  if ((fid = _MEDfichierCreer(nom,mode_acces)) < 0)
	    return -1;
	}
      else 
	if ((fid = _MEDfichierOuvrir(nom,mode_acces)) < 0)
	  return -1;
      break;
      
    case MED_CREATION :
      if ((fid = _MEDfichierCreer(nom,MED_LECTURE_ECRITURE)) < 0)
	return -1;
      break;

    default :
      return -1;
    }

if (MEDcheckVersion(fid) < 0) {
	if (fid >0) _MEDfichierFermer(fid); 
        return -1;
}

  return fid;
}
