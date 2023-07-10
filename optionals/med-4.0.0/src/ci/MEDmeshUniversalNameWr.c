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

/**\ingroup MEDmesh
  \brief \MEDmeshUniversalNameWrBrief
  \param fid \fid
  \param meshname \meshname
  \retval med_err \error
  \details \MEDmeshUniversalNameWrDetails
 */

med_err
MEDmeshUniversalNameWr(const med_idt fid, 
		       const char * const meshname)
{
  med_access_mode _MED_ACCESS_MODE;
  med_err _ret=-1;
  med_idt _meshid=0;
  char    _path [MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  char    _unvname [MED_LNAME_SIZE+1]="";
  time_t  _time;
#ifdef PPRO_NT_CALL
  struct timeb   _tp;
  char   _lpBuffer [UNLEN+1]="";
  long   _nSize   = UNLEN+1;
#else
  struct timeval _tp;
  struct passwd* mypasswd;
#endif


  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
 if (_MEDcheckVersion30(fid) < 0) goto ERROR_;

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR_;
  }

  if ( _MED_ACCESS_MODE == MED_ACC_RDONLY) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    ISCRUTE_int(_MED_ACCESS_MODE);
    goto ERROR_;
  }

  /*
   * Si le maillage n'existe pas => erreur
   */
  strcat(_path,meshname);
  if ((_meshid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_meshid);goto ERROR_;
  }

  /*
   * Creation/Ecriture de l'attribut nom universel
   */

#ifdef PPRO_NT_CALL

  if ( GetUserName(_lpBuffer,&_nSize) == 0 ) goto ERROR_;
  if ( _nSize > MED_NAME_SIZE ) _nSize = MED_NAME_SIZE;
  strncpy(_unvname,_lpBuffer,_nSize);
  strcat(_unvname," ");
  _time=time(&_time);
  strcat(_unvname,ctime(&_time));
  ftime(&_tp);
  _nSize = strlen(_unvname)-1;
  if ( sprintf(&_unvname[_nSize]," %hu",_tp.millitm) < 0 ) goto ERROR_;

#else
#if defined(HAVE_GETPWUID) && defined(HAVE_GETEUID)
  mypasswd=getpwuid(geteuid());
  if ( mypasswd == NULL ) {
    MESSAGE("Impossible d'obtenir le nom de l'utilisateur effectif");
    goto ERROR_;
  }
  strcat(_unvname,mypasswd->pw_name);
#elif defined(HAVE_CUSERID)
  if ( !cuserid(_unvname) ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"cuserid");
    SSCRUTE(_path); goto ERROR_;
  }
#else
#error "There is no ( getpwuid && geteuid) nor cuserid"
#endif
  strcat(_unvname," ");
  _time=time(&_time);
  strcat(_unvname,ctime(&_time));
  if ( gettimeofday(&_tp,NULL) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"gettimeofday");
    SSCRUTE(_path); goto ERROR_;
  }
  if ( sprintf(&_unvname[strlen(_unvname)-1]," %li",_tp.tv_usec) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"sprintf");
    SSCRUTE(_path); goto ERROR_;
  }
#endif

  if ( _MEDattributeStringWr(_meshid,MED_NOM_UNV,MED_LNAME_SIZE,_unvname) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_UNI);
    SSCRUTE(_unvname);goto ERROR_;
  }

  _ret = 0;
 ERROR_:

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_meshid);
  }

  return _ret;
}

