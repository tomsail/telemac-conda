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
  \brief \MEDstructElementVarAttInfoByNameBrief
  \param fid                 \fid
  \param modelname           \modelname
  \param varattname          \varattname
  \param varatttype          \varatttype
  \param ncomponent       \ncomponent

  \return \error

  \details \MEDstructElementVarAttInfoByNameDetails
 */

med_err
MEDstructElementVarAttInfoByName(const med_idt                   fid,
				 const char*               const modelname,
				 const char*               const varattname,
				       med_attribute_type* const varatttype,
				       med_int*            const ncomponent
				 )

{

  med_err           _ret=-1;
  med_idt           _attid=0;
  char              _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1+MED_TAILLE_VARATR+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  med_int           _intentitytype = MED_UNDEF_ENTITY_TYPE;
  med_int           _profilesize=0;
  med_int           _medintvaratttype=0;

  strcat(_path,modelname);
  strcat(_path,MED_VARATR);
  strcat(_path,varattname);

  /*
   * Si le DataGroup /STRUCT/<modelname>/VARATT/<varattributename> n'existe pas => erreur
   */
  if ((_attid = _MEDdatagroupOpen(fid,_path)) < 0)  {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_ATT (type des valeurs de l'attribut.)
   */
  if ( _MEDattrEntierLire(_attid,MED_NOM_ATT,&_medintvaratttype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_ATT);ISCRUTE(_medintvaratttype);
    goto ERROR;
  }
  *varatttype = _medintvaratttype;
  /*
   * Lecture de l'attribut MED_NOM_NCO (nombre de composantes des valeurs de l'attribut.)
   */
  if ( _MEDattrEntierLire(_attid,MED_NOM_NCO,ncomponent) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NCO);ISCRUTE(*ncomponent);
    goto ERROR;
  }

  _ret=0;
 ERROR:

  if (_attid>0)            if (_MEDdatagroupFermer(_attid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,modelname);
    ISCRUTE_id(_attid);
  }

  return _ret;
}
