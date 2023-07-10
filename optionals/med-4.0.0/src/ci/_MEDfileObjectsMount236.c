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

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

void _MEDfileObjectsMount236(int dummy,...) {


  med_idt _ret = -1;
  med_idt _id, _rootId=0;
  char _mountPath[2*MED_NAME_SIZE+1];
  char _link[MED_NAME_SIZE+1];
  med_access_mode _accessMode;
  med_int  majeur=0, mineur=0, release=0;
  med_int  rfileversionMMR,rfileversionMM,rfileversionM;
  med_int  lfileversionMMR,lfileversionMM,lfileversionM;
  med_bool _datagroupexist=MED_FALSE,_isasoftlink=MED_FALSE;

  MED_VARGS_DECL(const, med_idt         , , fid           );
  MED_VARGS_DECL(const, med_idt         , , gid           );
  MED_VARGS_DECL(const, char*     , const , mountfilename );
  MED_VARGS_DECL(const, med_class       , , medclass      );
  MED_VARGS_DECL(, med_idt *             ,, fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt         , , fid           );
  MED_VARGS_DEF(const, med_idt         , , gid           );
  MED_VARGS_DEF(const, char*     , const , mountfilename );
  MED_VARGS_DEF(const, med_class       , , medclass      );
  MED_VARGS_DEF(, med_idt *             ,, fret          );

  _MEDmodeErreurVerrouiller();

  if ( MEDfileNumVersionRd(fid, &majeur, &mineur, &release) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDfileNumVersionRd");
    ISCRUTE(majeur);ISCRUTE(mineur);ISCRUTE(release);
    goto ERROR;
  }
  lfileversionM   = 100*majeur;
  lfileversionMM  = 100*majeur+10*mineur;
  lfileversionMMR = lfileversionMM+release;

  /*
   * does the file exist ?
   */
  if (access(mountfilename,F_OK)) {
    MED_ERR_(_ret,MED_ERR_EXIST,MED_ERR_FILE,mountfilename);
    goto ERROR;
  }

  if ( (_accessMode = (med_access_mode)_MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_FILE,"");
    ISCRUTE_int(_accessMode);
    goto ERROR;
  }

  /*
   * Open the file "mountfilename".
   */
  if ((_id = _MEDfileOpen(mountfilename,_accessMode)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_FILE,mountfilename);
    goto ERROR;
  }

  if ( MEDfileNumVersionRd(_id, &majeur, &mineur, &release) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDfileNumVersionRd");
    SSCRUTE(mountfilename);ISCRUTE(majeur);ISCRUTE(mineur); ISCRUTE(release);
    goto ERROR;
  }
  rfileversionM   = 100*majeur;
  rfileversionMM  = 100*majeur+10*mineur;
  rfileversionMMR = rfileversionMM+release;

  if ( !( ( (rfileversionMM >= 220) && (lfileversionMM >= 220)
	    && (rfileversionMMR <= 236) && (lfileversionMMR <= 236) ) 	  ) ){
	 MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_FILE,mountfilename);
	 ISCRUTE(rfileversionMMR);ISCRUTE(lfileversionMMR);
	 goto ERROR;
       }

  /*
   * Mount point creation in the local file
   */
  _rootId = _MEDdatagroupOuvrir(fid,MED_MNT);
  if (_rootId < 0)
    if ((_rootId = _MEDdatagroupCreer(fid,MED_MNT)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_FILE,MED_MNT);
      goto ERROR;
    }

  /*
   * The file "mountfilename" is mounted in the local file
   */
  strncpy(_mountPath,MED_MNT,strlen(MED_MNT)-1);
  _mountPath[strlen(MED_MNT)-1] = '\0';
  if (_MEDfichierMonter(fid,_mountPath,_id) < 0) {
    MED_ERR_(_ret,MED_ERR_MOUNT,MED_ERR_FILE,mountfilename);
    goto ERROR;
  }


  /*
   * Give access to the class object in the local file
   */
  switch(medclass) {

  case MED_MESH :
    strcpy(_link,MED_MESH_GRP);
    break;

  case MED_FIELD :
    strcpy(_link,MED_FIELD_GRP);
    break;

  default :
    goto ERROR;
  }

  strcat(_mountPath,_link);
  _mountPath[strlen(_mountPath)-1] = '\0';
  _link[strlen(_link)-1] = '\0';
  if (_MEDdatagroupLienCreer(fid,_mountPath,_link) < 0) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_LINK,_link);
    SSCRUTE(_mountPath);
    goto ERROR;
  }

  _ret = _id;
 ERROR:

  if (_rootId > 0)
    if (_MEDdatagroupFermer(_rootId) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILE,MED_MNT);
    }

  va_end(params);
  *fret = _ret;
  return;
}
