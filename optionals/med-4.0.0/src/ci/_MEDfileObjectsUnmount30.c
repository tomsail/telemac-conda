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

void _MEDfileObjectsUnmount30(int dummy, ...) {

  med_err _ret = -1;
  char _link[2*MED_NAME_SIZE+1];
/*   char _mountPath[2*MED_NAME_SIZE+1]; */
  char    _mountPath[MED_TAILLE_MNT+2*MED_NAME_SIZE+2]=MED_MNT;

  MED_VARGS_DECL(const, med_idt         , , fid           );
  MED_VARGS_DECL(const, med_idt         , , mid           );
  MED_VARGS_DECL(const, med_class       , , medclass      );
  MED_VARGS_DECL(, med_err *             ,, fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt         , , fid           );
  MED_VARGS_DEF(const, med_idt         , , mid           );
  MED_VARGS_DEF(const, med_class       , , medclass      );
  MED_VARGS_DEF(, med_err *             ,, fret          );

  _MEDmodeErreurVerrouiller();

  switch(medclass) {

  case MED_MESH :
    strcpy(_link,MED_MESH_GRP);
    break;

  case MED_MESH_SUPPORT :
    strcpy(_link,MED_MESH_SUPPORT_GRP);
    break;

  case MED_ELSTRUCT :
    strcpy(_link,MED_ELSTRUCT_GRP);
    break;

  case MED_FAMILY :
    strcpy(_link,MED_FAMILY_GRP);
    break;

  case MED_EQUIVALENCE :
    strcpy(_link,MED_EQUIVALENCE_GRP);
    break;

  case MED_JOINT :
    strcpy(_link,MED_JOINT_GRP);
    break;

  case MED_FIELD :         
    /* Ajouter aussi MED_CHA_INTERP*/
    //strcpy(_link,MED_CHA_INTERP);
    strcpy(_link,MED_FIELD_GRP);
    break;

  case MED_LOCALIZATION :
    strcpy(_link,MED_LOCALIZATION_GRP);
    break;

  case MED_PROFILE :
    strcpy(_link,MED_PROFILE_GRP);
    break;

  case MED_INTERPOLATION :
    strcpy(_link,MED_INTERPOLATION_GRP);
    break;

  case MED_NUMERICAL_DATA :
    strcpy(_link,MED_NUMERICAL_DATA_GRP);
    break;

  default :
    goto ERROR;
  }

  /* link destruction in the local file */
  _link[strlen(_link)-1] = '\0';
  if (_MEDdatagroupLienSupprimer(fid,_link) < 0) {
    MED_ERR_(_ret,MED_ERR_ULINK,MED_ERR_FILE,_link);
    goto ERROR;
  }

  /*
   * The file is unmounted
   */
  strcpy(&_mountPath[strlen(_mountPath)-1],_link);

/*   strncpy(_mountPath,MED_MNT,strlen(MED_MNT)-1); */
/*   _mountPath[strlen(MED_MNT)-1] = '\0'; */
  if (_MEDfichierDemonter(fid,_mountPath) < 0) {
    MED_ERR_(_ret,MED_ERR_UMOUNT,MED_ERR_FILE,_mountPath);
    goto ERROR;
  }


  _ret = 0;
 ERROR:

  /*
   * The unmounted file is closed
   */
  if (mid > 0)
    if (_MEDfichierFermer(mid) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILE,"");
      _ret = -1;
  }

  va_end(params);
  *fret = _ret;
  return;
}
