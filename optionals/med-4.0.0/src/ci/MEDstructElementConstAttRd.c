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
  \brief \MEDstructElementConstAttRdBrief
  \param fid                   \fid
  \param modelname             \modelname
  \param constattname          \constattname
  \param value                 \value

  \return \error

  \details \MEDstructElementConstAttRdDetails
  \remarks \MEDstructElementConstAttswitchCm
  \see      MEDstructElementConstAttWithProfileWr
  \see      MEDstructElementConstAttWr
 */

med_err
MEDstructElementConstAttRd(
			   const med_idt                  fid,
			   const char*              const modelname,
			   const char*              const constattname,
			   void*              const value
			   )
{
  med_err            _ret=-1;
  med_idt            _attid=0, _elemid=0 ;
  char               _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1+MED_TAILLE_CSTATR+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  med_int            _intentitytype = 0;
  med_int            _nentity       = 0;
  med_attribute_type _constatttype  = MED_ATT_UNDEF;
  med_int            _ncomponent = 0;
  med_entity_type    _entitytype    = MED_UNDEF_ENTITY_TYPE;
  char               _supportmeshname[MED_NAME_SIZE+1]="";
  char               _profilename    [MED_NAME_SIZE+1]="";
  med_int            _profilesize   = 0;
  med_filter         _filter        = MED_FILTER_INIT;
  med_int            _medintsgeotype      = MED_NONE;
  med_bool           _chgt=MED_FALSE,_trsf=MED_FALSE;

  NOFINALBLANK(modelname,ERROR);
  NOFINALBLANK(constattname,ERROR);

  strcat(_path,modelname);


  /*
   * Si le DataGroup /STRUCT/<modelname> n'existe pas => erreur
   */
  if ((_elemid = _MEDdatagroupOpen(fid,_path)) < 0)  {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

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
   * Lecture de l'attribut MED_NOM_NOM (nom du maillage support)
   */
  /* Chercher plutôt ds le maillage support et supprimer les attributs NBM et NBN */
  if ( _MEDattrStringLire(_elemid,MED_NOM_NOM,MED_NAME_SIZE,_supportmeshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NOM);SSCRUTE(_supportmeshname);
    goto ERROR;
  }

  if (
      MEDstructElementConstAttInfoByName(fid,
					 modelname,
					 constattname,
					 &_constatttype,
					 &_ncomponent,
					 &_entitytype,
					 _profilename,
					 &_profilesize
					 ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_STRUCT_MSG);
    SSCRUTE(modelname);SSCRUTE(_path);SSCRUTE("MEDstructElementConstAttInfoByName");
    goto ERROR;
  }


  if (strlen(_supportmeshname) ) {

    if (_entitytype == MED_CELL )

      /*
       * Lecture de l'attribut MED_NOM_GEO (type géométrique des mailles support)
       */
      if ( _MEDattrEntierLire(_elemid,MED_NOM_GEO,&_medintsgeotype) < 0 ) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
	SSCRUTE(MED_NOM_GEO);ISCRUTE(_medintsgeotype);
	goto ERROR;
      }

      if ( (_nentity = MEDmeshnEntity(fid,_supportmeshname,MED_NO_DT,MED_NO_IT,
				      MED_CELL,(med_geometry_type) _medintsgeotype,
				      MED_CONNECTIVITY,MED_NODAL,
				     &_chgt,&_trsf) )  < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      ISCRUTE(_nentity);goto ERROR;
    }

    if (_entitytype == MED_NODE )
      if ( (_nentity = MEDmeshnEntity(fid,_supportmeshname,MED_NO_DT,MED_NO_IT,
				      MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,
				      &_chgt,&_trsf) )  <= 0) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
	ISCRUTE(_nentity);goto ERROR;
      }
  } else
    _nentity = 1;

  if ( MEDfilterEntityCr(fid, _nentity, 1, _ncomponent, MED_ALL_CONSTITUENT,
			 MED_FULL_INTERLACE,MED_COMPACT_STMODE,
			 _profilename, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetRd(_attid,MED_NOM_COR,_constatttype,&_filter, (unsigned char * ) value) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_COR);
    SSCRUTE(_path);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_STRUCT_ELEMENT_MSG);
    SSCRUTE(_path);
    goto ERROR;
  }

  _ret=0;
 ERROR:

  if (_attid>0)            if (_MEDdatagroupFermer(_attid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,modelname);
    ISCRUTE_id(_attid);
  }

  if (_elemid>0)            if (_MEDdatagroupFermer(_elemid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,modelname);
    ISCRUTE_id(_elemid);
  }

  return _ret;
}

