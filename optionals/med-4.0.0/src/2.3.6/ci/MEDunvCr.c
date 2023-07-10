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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef PPRO_NT_CALL
// Windows Header Files:
#include <windows.h>
#include <Lmcons.h>
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

#ifndef HAVE_UNISTD_H
#error "unistd.h required."
#endif

# include <unistd.h>

#if defined(HAVE_GETPWUID) && defined(HAVE_GETEUID)
# include <sys/types.h>
# include <pwd.h>
#endif

#endif

med_err 
MEDunvCr(med_idt fid, char *maa)
{
  med_idt maaid;
  char chemin [MED_TAILLE_MAA+MED_TAILLE_NOM+1]="";
  char nomu   [MED_TAILLE_LNOM+1]="";    
  time_t  temps;
#ifdef PPRO_NT_CALL
  struct timeb   tp;
  char   lpBuffer [UNLEN+1]="";
  long   nSize   = UNLEN+1;
#else
  struct passwd* mypasswd;
  struct timeval tp;
#endif 
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
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0) goto ERROR_;

  /*
   * Creation/Ecriture de l'attribut nom universel 
   */

#ifdef PPRO_NT_CALL
  if ( GetUserName(lpBuffer,&nSize) == 0 ) goto ERROR_;
  if ( nSize > MED_TAILLE_NOM ) nSize = MED_TAILLE_NOM;
  strncpy(nomu,lpBuffer,nSize);
  strcat(nomu," ");
  temps=time(&temps);
  strcat(nomu,ctime(&temps));
  ftime(&tp);
  nSize = strlen(nomu)-1;
  if ( sprintf(&nomu[nSize]," %hu",tp.millitm) < 0 ) goto ERROR_;
#else
#if defined(HAVE_GETPWUID) && defined(HAVE_GETEUID)
  mypasswd=getpwuid(geteuid());
  if ( mypasswd == NULL ) {
    MESSAGE("Impossible d'obtenir le nom de l'utilisateur effectif");
    goto ERROR_;
  }
  strcat(nomu,mypasswd->pw_name);
#elif defined(HAVE_CUSERID)
  if ( !cuserid(nomu) ) {
   MESSAGE("Impossible d'obtenir le nom de l'utilisateur effectif");
   goto ERROR_;
  }
#else
#error "There is no ( getpwuid && geteuid) nor cuserid"
#endif
  strcat(nomu," ");
  temps=time(&temps);
  strcat(nomu,ctime(&temps));
  if ( gettimeofday(&tp,NULL) < 0 ) goto ERROR_;
  if ( sprintf(&nomu[strlen(nomu)-1]," %li",tp.tv_usec) < 0 ) {
   MESSAGE("Erreur à la construction du nom universel.");
   goto ERROR_;
  }
#endif
  if ((ret = _MEDattrStringEcrire(maaid,MED_NOM_UNV,MED_TAILLE_LNOM,nomu)) < 0) 
   goto ERROR_; 

  /* 
   * Nettoyages divers
   */
ret = 0;
ERROR_:
  if ( maaid > 0) if (_MEDdatagroupFermer(maaid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_int(maaid); ret = -1;
  }

  return ret;
}

