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
#include <string.h>
#include <stdlib.h>
#include <med_outils.h>

void
_MEDfamilyCr30(int dummy, ...) {

  med_access_mode       _MED_ACCESS_MODE;
  med_err               _ret=-1;
  med_idt               _root=0,_famid=0, _datagroup=0,_datagroup2=0,_datagroup3=0;
  char                  _fampath         [MED_FAMILY_GRP_SIZE+MED_NAME_SIZE+1]=MED_FAMILY_GRP;
  const char*           _datagroupname2="";
  med_filter            _filter        = MED_FILTER_INIT;
  const char * const    _MED_FAS_NOEUD_NOM = MED_FAS_NOEUD_NOM;
  const char * const    _MED_FAS_ELEME_NOM = MED_FAS_ELEME_NOM;
  const char * const    _FAMILLE_ZERO      = FAMILLE_ZERO;

  MED_VARGS_DECL(const, med_idt           , , fid           );
  MED_VARGS_DECL(const, char * , const      , meshname      );
  MED_VARGS_DECL(const, char * , const      , familyname    );
  MED_VARGS_DECL(const, med_int           , , familynumber  );
  MED_VARGS_DECL(const, med_int           , , ngroup        );
  MED_VARGS_DECL(const, char *, const       , groupname     );
  MED_VARGS_DECL(, med_err *              , , fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid           );
  MED_VARGS_DEF(const, char * , const      , meshname      );
  MED_VARGS_DEF(const, char * , const      , familyname    );
  MED_VARGS_DEF(const, med_int           , , familynumber  );
  MED_VARGS_DEF(const, med_int           , , ngroup        );
  MED_VARGS_DEF(const, char *, const       , groupname     );
  MED_VARGS_DEF(, med_err *              , , fret          );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
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

  NOFINALBLANK(meshname,ERROR);
  NOFINALBLANK(familyname,ERROR);

  /*
   * Si le DataGroup root n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_fampath)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,_fampath)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_fampath);
     goto ERROR;
    }

  strcat(_fampath,meshname);
/*   SSCRUTE(_fampath); */
  if ((_famid = _MEDdatagroupOuvrir(fid,_fampath)) < 0)
    if ((_famid = _MEDdatagroupCreer(fid,_fampath)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
      SSCRUTE(_fampath);
      goto ERROR;
    }

  if (familynumber > 0)
    _datagroupname2=_MED_FAS_NOEUD_NOM;
  else if (familynumber < 0)
    _datagroupname2=_MED_FAS_ELEME_NOM;
  else
    _datagroupname2=_FAMILLE_ZERO;

 if ((_datagroup2 = _MEDdatagroupOuvrir(_famid,_datagroupname2)) < 0)
    if ((_datagroup2 = _MEDdatagroupCreer(_famid,_datagroupname2)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_ERR_FAMILY_MSG);
      SSCRUTE(_datagroupname2);SSCRUTE(_fampath);SSCRUTE(MED_FAMILY_GRP);
      goto ERROR;
  }

 if (familynumber != 0) {

   if ((_datagroup = _MEDdatagroupCreer(_datagroup2,familyname)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_ERR_FAMILY_MSG);
      SSCRUTE(familyname);SSCRUTE(_fampath);SSCRUTE(MED_FAMILY_GRP);
      goto ERROR;
   }
 } else {
   _datagroup=_datagroup2;
 }


 if ( _MEDattributeIntWr(_datagroup,MED_NOM_NUM,&familynumber) < 0) {
   MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FAMILY_MSG);
   SSCRUTE(familyname);SSCRUTE(MED_NOM_NUM);ISCRUTE(familynumber);
   SSCRUTE(_fampath);SSCRUTE(MED_FAMILY_GRP);SSCRUTE(_datagroupname2);
   goto ERROR;
 }

 if ( (familynumber != 0) && (ngroup > 0) ) {

   if ((_datagroup3 = _MEDdatagroupCreer(_datagroup,MED_NOM_GRO)) < 0 ) {
     MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_ERR_FAMILY_MSG);
     SSCRUTE(familyname);SSCRUTE(_fampath);SSCRUTE(MED_FAMILY_GRP);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_GRO);
     goto ERROR;
   }

   if ( MEDfilterEntityCr(fid, ngroup, 1, 1, MED_ALL_CONSTITUENT,
			  MED_FULL_INTERLACE,MED_UNDEF_STMODE,
			  MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
     MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
     goto ERROR;
   }

   if ( _MEDdatasetWr(_datagroup3,MED_NOM_NOM,MED_INTERNAL_LNAME,&_filter, groupname) < 0) {
     MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_NOM);
     SSCRUTE(_fampath);SSCRUTE(MED_FAMILY_GRP);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_GRO);
     SSCRUTE(familyname);goto ERROR;
   }

   if ( MEDfilterClose(&_filter) < 0 ) {
     MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_FAMILY_MSG);
     SSCRUTE(familyname);SSCRUTE(_fampath);SSCRUTE(MED_FAMILY_GRP);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_GRO);
     goto ERROR;
   }

   if ( _MEDattributeIntWr(_datagroup3,MED_NOM_NBR,&ngroup) < 0) {
     MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FAMILY_MSG);
     SSCRUTE(familyname);SSCRUTE(_fampath);SSCRUTE(MED_FAMILY_GRP);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_GRO);
     SSCRUTE(MED_NOM_NBR);
     goto ERROR;
   }

 }
 _ret = 0;

 ERROR:

 if (_datagroup3>0)     if (_MEDdatagroupFermer(_datagroup3) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_NOM_GRO);
   ISCRUTE_id(_datagroup3);
 }

 if ( _datagroup != _datagroup2 )
   if (_datagroup > 0 )   if (_MEDdatagroupFermer(_datagroup) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,familyname);
   ISCRUTE_id(_datagroup);
 }

 if (_datagroup2>0)     if (_MEDdatagroupFermer(_datagroup2) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname2);
   ISCRUTE_id(_datagroup2);
 }

 if (_famid>0)         if (_MEDdatagroupFermer(_famid) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_fampath);
   ISCRUTE_id(_famid);
 }

 if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_FAMILY_GRP);
   ISCRUTE_id(_root);
 }

 va_end(params);
 *fret = _ret;
 return;

}
