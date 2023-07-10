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
  \brief \MEDstructElementConstAttWithProfileWrBrief
  \param fid \fid
  \param modelname \modelname
  \param constattname \constattname
  \param constatttype \constatttype
  \param ncomponent \ncomponent
  \param sentitytype \sentitytype
  \param profilename \profilename
  \param value \value
  \retval med_err \error
  \details \MEDstructElementConstAttWithProfileWrDetails
  \remarks \MEDstructElementConstAttswitchCm
  \see      MEDstructElementConstAttWr
 */


med_err
MEDstructElementConstAttWithProfileWr(const med_idt                  fid,
				      const char*              const modelname,
				      const char*              const constattname,
				      const med_attribute_type       constatttype,
				      const med_int                  ncomponent,
				      const med_entity_type          sentitytype,
				      const char*              const profilename,
				      const void*              const value
				      )
{
  med_access_mode   _MED_ACCESS_MODE;
  med_err           _ret=-1;
  med_idt           _attid=0, _elemid=0, _cstid=0;
  char              _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1+MED_TAILLE_CSTATR+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  char              _supportmeshname[MED_NAME_SIZE+1]="";
  med_int           _intentitytype                   = sentitytype;
  med_int           _nentity                         = 0;
  med_filter        _filter                          = MED_FILTER_INIT;
  med_int           _false=0,_true=1,_true_or_false  = _false;
  med_bool          _chgt=MED_FALSE,_trsf=MED_FALSE;
  med_int           _medintsgeotype                  =MED_NONE;
  med_geometry_type _sgeotype                        =MED_NONE;
  med_int           _medintconstatttype= (med_int) constatttype;

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
  NOFINALBLANK(constattname,ERROR);
  NOFINALBLANK(profilename,ERROR);

  strcat(_path,modelname);

  /*
   * Si le DataGroup /STRUCT/<modelname> n'existe pas => erreur
   */
  if ((_elemid = _MEDdatagroupOpen(fid,_path)) < 0)  {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_NOM (nom du maillage support)
   */
  if ( _MEDattrStringLire(_elemid,MED_NOM_NOM,MED_NAME_SIZE,_supportmeshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NOM);SSCRUTE(_supportmeshname);
    goto ERROR;
  }

  if (strlen(_supportmeshname) ) {

    if (sentitytype == MED_CELL ) {
      /*
       * Lecture de l'attribut MED_NOM_GEO (type géométrique des mailles support)
       */
      if ( _MEDattrEntierLire(_elemid,MED_NOM_GEO,&_medintsgeotype) < 0 ) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
	SSCRUTE(MED_NOM_GEO);ISCRUTE(_medintsgeotype);
	goto ERROR;
      }

      _sgeotype= (med_geometry_type) _medintsgeotype;
    }

    if ( (_nentity = MEDmeshnEntity(fid,_supportmeshname,MED_NO_DT,MED_NO_IT,
				    MED_CELL,_sgeotype,MED_CONNECTIVITY,MED_NODAL,
				    &_chgt,&_trsf) )  < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      ISCRUTE(_nentity);goto ERROR;
    }

    if (sentitytype == MED_NODE )
      if ( (_nentity = MEDmeshnEntity(fid,_supportmeshname,MED_NO_DT,MED_NO_IT,
				      MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,
				      &_chgt,&_trsf) )  <= 0) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
	ISCRUTE(_nentity);goto ERROR;
      }
  } else
    _nentity = 1;

  /*
   * Si le DataGroup /STRUCT/<modelname>/CSTATR/ n'existe pas on le crée
   */
  if ((_cstid = _MEDdatagroupOpen(_elemid,MED_CSTATR_NOM)) < 0)
    if ((_cstid = _MEDdatagroupCreer(_elemid,MED_CSTATR_NOM)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_CSTATR_NOM);
      SSCRUTE(_path);goto ERROR;
    }
  strcat(_path,MED_CSTATR);

  /*
   * Si le DataGroup /STRUCT/<modelname>/CSTATR/<constattname> n'existe pas on le crée
   */
  if ((_attid = _MEDdatagroupOpen(_cstid,constattname)) < 0)
    if ((_attid = _MEDdatagroupCreer(_cstid,constattname)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,constattname);
      SSCRUTE(_path);goto ERROR;
    }
  strcat(_path,constattname);

  /*
   * Creation/Ecriture de l'attribut MED_NOM_ATT (type des valeurs de l'attribut.)
   */
  if ( _MEDattributeIntWr(_attid,MED_NOM_ATT,&_medintconstatttype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_ATT);ISCRUTE(_medintconstatttype);
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

  /*
   * Creation/Ecriture de l'attribut MED_NOM_ENT (type d'entité support concerné par l'attribut)
   */
  if ( _MEDattributeIntWr(_attid,MED_NOM_ENT,&_intentitytype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_ENT);ISCRUTE(_intentitytype);
    goto ERROR;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_PFL (nom de profil sur le maillage support)
   */
  if ( strlen(profilename) ) {
/*     _profilename=(const char *)profilename; */
    _true_or_false=_true;
  }

  if ( _MEDattributeIntWr(_cstid,MED_NOM_PFU,&_true_or_false) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_STRUCT_MSG);
    SSCRUTE(modelname);;SSCRUTE(MED_NOM_PFU);
    goto ERROR;
  }

  if ( _MEDattributeStringWr(_attid,MED_NOM_PFL,MED_NAME_SIZE,profilename) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_PFL);SSCRUTE(profilename);
    goto ERROR;
  }

  if ( MEDfilterEntityCr(fid, _nentity, 1, ncomponent, MED_ALL_CONSTITUENT,
			 MED_FULL_INTERLACE,MED_COMPACT_STMODE,
			 profilename, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetWr(_attid,MED_NOM_COR,constatttype,&_filter, value) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_COR);
    SSCRUTE(_path);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_STRUCT_ELEMENT_MSG);
    SSCRUTE(_path);
    goto ERROR;
  }

  /* ??? */
/*   if ((_dataset = _MEDdatasetOuvrir(_datagroup2,MED_NOM_COR)) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATASET,MED_NOM_COR); */
/*     SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);SSCRUTE(_datagroupname2); */
/*     goto ERROR; */
/*   } */

/*   if ( _MEDattributeIntWr(_dataset,MED_NOM_NBR,&nentity) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_EQUIVALENCE_MSG); */
/*     SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);SSCRUTE(_datagroupname2); */
/*     SSCRUTE(MED_NOM_NBR);ISCRUTE(nentity);goto ERROR; */
/*   } */
  /* ??? */


  _ret=0;
 ERROR:

  if (_attid>0)            if (_MEDdatagroupFermer(_attid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_attid);
  }

  if (_cstid>0)            if (_MEDdatagroupFermer(_cstid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_CSTATR_NOM);
    ISCRUTE_id(_cstid);
  }

  if (_elemid>0)            if (_MEDdatagroupFermer(_elemid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,modelname);
    ISCRUTE_id(_elemid);
  }

  return _ret;
}
