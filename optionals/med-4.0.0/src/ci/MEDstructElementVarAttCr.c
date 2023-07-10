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

/**\ingroup MEDstructElement
  \brief \MEDstructElementVarAttCrBrief
  \param fid                 \fid
  \param modelname           \modelname
  \param varattname          \varattname
  \param varatttype          \varatttype
  \param ncomponent       \ncomponent

  \return \error

  \details \MEDstructElementVarAttCrDetails
  \see MEDmeshStructElementVarAttWr
  \see MEDmeshStructElementVarAttRd
 */

med_err
MEDstructElementVarAttCr(const med_idt                  fid,
			 const char*              const modelname,
			 const char*              const varattname,
			 const med_attribute_type       varatttype,
			 const med_int                  ncomponent
			 )
{
  med_access_mode   _MED_ACCESS_MODE;
  med_err           _ret=-1;
  med_idt           _attid=0, _elemid=0, _varid=0;
  char              _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1+MED_TAILLE_VARATR+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  med_int           _medintvaratttype=varatttype;

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

  NOFINALBLANK(modelname,ERROR);
  NOFINALBLANK(varattname,ERROR);

  if ( _medintvaratttype == MED_ATT_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_INVALID, MED_ERR_PARAMETER, MED_ERR_TYPEOF_MSG MED_ERR_ATTRIBUTE);
    SSCRUTE(MED_NOM_ATT);SSCRUTE(varattname);ISCRUTE_int(varatttype);
    goto ERROR;
  }

  strcat(_path,modelname);

  /*
   * Si le DataGroup /STRUCT/<modelname> n'existe pas => erreur
   */
  if ((_elemid = _MEDdatagroupOpen(fid,_path)) < 0)  {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * Si le DataGroup /STRUCT/<modelname>/VARATR/ n'existe pas on le crée
   */
  if ((_varid = _MEDdatagroupOpen(_elemid,MED_VARATR_NOM)) < 0)
    if ((_varid = _MEDdatagroupCreer(_elemid,MED_VARATR_NOM)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_VARATR_NOM);
      SSCRUTE(_path);goto ERROR;
    }
  strcat(_path,MED_VARATR);

  /*
   * Si le DataGroup /STRUCT/<modelname>/VARATR/<varattname> n'existe pas on le crée
   */
  if ((_attid = _MEDdatagroupOpen(_varid,varattname)) < 0)
    if ((_attid = _MEDdatagroupCreer(_varid,varattname)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,varattname);
      SSCRUTE(_path);goto ERROR;
    }
  strcat(_path,varattname);

  /*
   * Creation/Ecriture de l'attribut MED_NOM_ATT (type des valeurs de l'attribut.)
   */
  if ( _MEDattributeIntWr(_attid,MED_NOM_ATT,&_medintvaratttype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_ATT);ISCRUTE(_medintvaratttype);
    goto ERROR;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_NCO (nombre de composantes des valeurs de l'attribut.)
   */
  if ( _MEDattributeIntWr(_attid,MED_NOM_NCO,&ncomponent) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NCO);ISCRUTE(ncomponent);
    goto ERROR;
  }


  _ret=0;
 ERROR:

  if (_attid>0)            if (_MEDdatagroupFermer(_attid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_attid);
  }

  if (_varid>0)            if (_MEDdatagroupFermer(_varid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_VARATR_NOM);
    ISCRUTE_id(_varid);
  }

  if (_elemid>0)            if (_MEDdatagroupFermer(_elemid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,modelname);
    ISCRUTE_id(_elemid);
  }

  return _ret;
}
