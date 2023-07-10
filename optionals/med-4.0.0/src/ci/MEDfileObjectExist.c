/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2015  EDF R&D, CEA/DEN
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


/*
Les objets de class medclass dont il est ici question sont représentés par des noms de groupes
dans le modèle HDF.
*/
#define MY_CASE(OBJ)\
  case MED_##OBJ :\
  strcpy(_link,MED_##OBJ##_GRP);\
  _objsize = MED_##OBJ##_GRP_SIZE;\
  break;

/**\ingroup MEDfile
  \brief \MEDfileObjectExistBrief
  \param fid \fid
  \param medclass \medclass
  \param objectname \objectname
  \param objectexist \objectexist
  \retval med_err  \error
  \details \MEDfileObjectExistDetails
  \par Remarques
*/
med_err
MEDfileObjectExist(const med_idt           fid,
		   const med_class         medclass,
		   const char      * const objectname,
		         med_bool  * const objectexist )
{
  med_idt _fid = 0;
  med_err _ret = -1;
  char    _link[2*MED_NAME_SIZE+1]="";
  med_bool _datagroupexist=MED_FALSE,_isasoftlink=MED_FALSE;
  int _objsize = 0;

  /* 
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();

  *objectexist = MED_FALSE;
  /*
   * Give access to the class object in the local file
   */
  switch(medclass) {

    MY_CASE(MESH);
    MY_CASE(MESH_SUPPORT);
    MY_CASE(ELSTRUCT);
    MY_CASE(FAMILY);
    MY_CASE(EQUIVALENCE);
    MY_CASE(JOINT);
    MY_CASE(FIELD);
    MY_CASE(LOCALIZATION);
    MY_CASE(PROFILE);
    MY_CASE(INTERPOLATION);
    MY_CASE(NUMERICAL_DATA);

  default :
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_CLASS,_link);
   goto ERROR;
  }

  strncpy(_link+_objsize,objectname,MED_NAME_SIZE+1);
  _link[2*MED_NAME_SIZE]='\0';
	 
  if( _MEDdatagroupExist(fid,_link,&_datagroupexist,&_isasoftlink) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDdatagroupExist");
    SSCRUTE(_link);goto ERROR;
  }

  if ( _datagroupexist ) *objectexist = MED_TRUE;

  _ret=0;
 ERROR:

  return _ret;
}

