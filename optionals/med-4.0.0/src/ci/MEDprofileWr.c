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

/**\ingroup MEDprofile
   \brief \MEDprofileWrBrief
   \param fid \fid
   \param profilename \profilename
   \param profilesize \profilesize
   \param profilearray \profilearray
   \return \error
   \details
   \MEDprofileWrDetails
   \par Remarques
   \MEDprofileDef
*/

med_err
MEDprofileWr(const med_idt        fid,
	     const char* const    profilename,
	     const med_int        profilesize,
	     const med_int* const profilearray)
{
  med_access_mode _MED_ACCESS_MODE;
  med_idt        _root=0,_pfid=0;
  med_err        _ret=-1;
  char           _path[MED_PROFILE_GRP_SIZE+MED_NAME_SIZE+1]=MED_PROFILE_GRP;
  med_filter     _filter        = MED_FILTER_INIT;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
 if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  if ( _MED_ACCESS_MODE == MED_ACC_RDONLY) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    ISCRUTE_int(_MED_ACCESS_MODE);
    goto ERROR;
  }

 /*
   * Si le DataGroup /PROFILS/ n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_path)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,_path)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_path);
     goto ERROR;
    }

  NOFINALBLANK(profilename,ERROR);
  /*
   * Si le DataGroup /PROFILS/<profilename> n'existe pas, on le cree
   */
  if ((_pfid = _MEDdatagroupOuvrir(_root,profilename)) < 0)
    if ((_pfid = _MEDdatagroupCreer(_root,profilename)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,profilename);
      SSCRUTE(_path);goto ERROR;
    }

  strcat(_path,profilename);


  /*
   * On stocke "profilesize" sous forme d'attribut
   */
  if (_MEDattributeIntWr(_pfid,MED_NOM_NBR,&profilesize) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_PROFILE_MSG);
    SSCRUTE(profilename);SSCRUTE(MED_NOM_NBR);ISCRUTE(profilesize);
    goto ERROR;
  }

  /*
   * On stocke le profil dans un dataset
   */
  if ( MEDfilterEntityCr(fid, profilesize, 1, 1, MED_ALL_CONSTITUENT,
			 MED_NO_INTERLACE,MED_UNDEF_STMODE,
			 MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetWr(_pfid,MED_NOM_PFL,MED_INTERNAL_INT,&_filter, profilearray) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_PFL);SSCRUTE(_path);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_PROFILE_MSG); SSCRUTE(_path);
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  if (_pfid>0)            if (_MEDdatagroupFermer(_pfid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,profilename);
    ISCRUTE_id(_pfid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_PROFILE_GRP);
    ISCRUTE_id(_root);
  }

  return _ret;
}




