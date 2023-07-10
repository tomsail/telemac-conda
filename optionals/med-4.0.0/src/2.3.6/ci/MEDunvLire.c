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

#include <stdio.h>

#ifdef PPRO_NT_CALL
// Windows Header Files:
#include <sys/timeb.h>
#else

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#endif

med_err 
MEDunvLire(med_idt fid, char *maa,char *nomu)
{
  med_idt maaid=0;
  char chemin [MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  med_err ret=-1;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * Si le maillage n'existe pas => erreur
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
      goto ERROR;

  /*
   * Creation/Ecriture de l'attribut nom universel 
   */
  if ((ret = _MEDattrStringLire(maaid,MED_NOM_UNV,MED_TAILLE_LNOM,
			       nomu )) < 0)
    goto ERROR;

  /* 
   * Nettoyages divers
   */

  ret=0;
 ERROR:

  if ( maaid> 0 ) if ( _MEDdatagroupFermer(maaid) < 0) {
  MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(maaid);ret = -1; 
  }
    
  return ret;
}
  

  
