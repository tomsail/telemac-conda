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
#include <string.h>

/**\ingroup MEDstructElement
  \brief \MEDstructElementConstAttInfoByNameBrief
  \param fid                   \fid
  \param modelname             \modelname
  \param constattname          \constattname
  \param constatttype          \constatttype
  \param ncomponent         \ncomponent
  \param sentitytype           \sentitytype
  \param profilename           \profilename
  \param profilesize           \profilesize

  \return \error

  \details \MEDstructElementConstAttInfoByNameDetails
  \see  MEDstructElementConstAttInfo
 */

med_err
MEDstructElementConstAttInfoByName(const med_idt             fid,
				   const char*         const modelname,
				   const char*         const constattname,
				   med_attribute_type* const constatttype,
				   med_int*            const ncomponent,
				   med_entity_type*    const sentitytype,
				   char*               const profilename,
				   med_int*            const profilesize
				   )
{

  med_err           _ret=-1;
  med_idt           _attid=0;
  char              _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1+MED_TAILLE_CSTATR+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  med_int           _intsentitytype = MED_UNDEF_ENTITY_TYPE;
  med_int           _profilesize=0;
  med_int           _medintconstatttype;

  strcat(_path,modelname);
  strcat(_path,MED_CSTATR);
  strcat(_path,constattname);

  /*
   * Si le DataGroup /STRUCT/<modelname>/CSTATT/<constattributename> n'existe pas => erreur
   */
  if ((_attid = _MEDdatagroupOpen(fid,_path)) < 0)  {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_ATT (type des valeurs de l'attribut.)
   */
  if ( _MEDattrEntierLire(_attid,MED_NOM_ATT,&_medintconstatttype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_ATT);ISCRUTE(_medintconstatttype);
    goto ERROR;
  }
  *constatttype=_medintconstatttype;

  /*
   * Lecture de l'attribut MED_NOM_NCO (nombre de composantes des valeurs de l'attribut.)
   */
  if ( _MEDattrEntierLire(_attid,MED_NOM_NCO,ncomponent) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NCO);ISCRUTE(*ncomponent);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_ENT (type d'entité support concerné par l'attribut)
   */
  if ( _MEDattrEntierLire(_attid,MED_NOM_ENT,&_intsentitytype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_ENT);ISCRUTE(_intsentitytype);
    goto ERROR;
  }
  *sentitytype = (med_entity_type) _intsentitytype;

  /*
   * Lecture de l'attribut MED_NOM_PFL
   */
  if ( _MEDattrStringLire(_attid,MED_NOM_PFL,MED_NAME_SIZE,profilename) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_PFL);SSCRUTE(profilename);
    goto ERROR;
  }

  if ( (_profilesize=MEDprofileSizeByName(fid, profilename) ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_STRUCT_MSG);
    SSCRUTE(modelname);SSCRUTE(_path);SSCRUTE("MEDprofileSizeByName");
    goto ERROR;
  }
  *profilesize=_profilesize;

  _ret=0;
 ERROR:

  if (_attid>0)            if (_MEDdatagroupFermer(_attid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,modelname);
    ISCRUTE_id(_attid);
  }

  return _ret;
}
