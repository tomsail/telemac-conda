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
  \brief \MEDstructElementInfoByNameBrief
  \param fid                   \fid
  \param modelname             \modelname
  \param mgeotype              \mgeotype
  \param modeldim              \modeldim
  \param supportmeshname       \supportmeshname
  \param sentitytype           \sentitytype
  \param snnode             \snnode
  \param sncell             \sncell
  \param sgeotype              \sgeotype
  \param nconstantattribute \nconstantattribute
  \param anyprofile            \anyprofile
  \param nvariableattribute \nvariableattribute

  \return \error

  \details \MEDstructElementInfoByNameDetails
  \see MEDmeshEntityInfo
  \sa  MEDstructElementInfo
 */

med_err
MEDstructElementInfoByName(const med_idt             fid,
			   const char *        const modelname,
			   med_geometry_type * const mgeotype,
			   med_int*            const modeldim,
			   char*               const supportmeshname,
			   med_entity_type*    const sentitytype,
			   med_int*            const snnode,
			   med_int*            const sncell,
			   med_geometry_type*  const sgeotype,
			   med_int*            const nconstantattribute,
			   med_bool*           const anyprofile,
			   med_int*            const nvariableattribute
			   )
{

  med_err           _ret=-1;
  med_idt            _elemid=0, _cstid=0;
  char              _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  char              __profilename   [MED_NAME_SIZE+1]=MED_NO_PROFILE_INTERNAL;
  const char*       _profilename   = __profilename;
  med_int           _intentitytype = MED_UNDEF_ENTITY_TYPE;
  med_int           _nentity       = 0;
  med_size          _tmpn          = 0;
  med_bool          _chgt=MED_FALSE,_trsf=MED_FALSE;
  med_int           _medintmgeotype = MED_NONE;
  med_int           _medintsgeotype = MED_NONE;
  med_int           _intanyprofile  = 0;

  strcat(_path,modelname);

  /*
   * Si le DataGroup /STRUCT/<modelname> n'existe pas => erreur
   */
  if ((_elemid = _MEDdatagroupOpen(fid,_path)) < 0)  {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_NEO (numéro de type géométrique associé à un élément de structure)
   */
  if ( _MEDattrEntierLire(_elemid,MED_NOM_NEO,&_medintmgeotype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NEO);ISCRUTE(_medintmgeotype);
    goto ERROR;
  }
  *mgeotype=_medintmgeotype;

  /*
   * Lecture de l'attribut MED_NOM_DIM (dimension de l'élément)
   */
  if ( _MEDattrEntierLire(_elemid,MED_NOM_DIM,modeldim) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_DIM);ISCRUTE(*modeldim);
    goto ERROR;
  }

  /*
   * Lecture  de l'attribut MED_NOM_NOM (nom du maillage support)
   */
  /* TODO : Chercher plutôt ds le maillage support et supprimer les attributs NBM et NBN : .. A évaluer ...*/
  if ( _MEDattrStringLire(_elemid,MED_NOM_NOM,MED_NAME_SIZE,supportmeshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NOM);SSCRUTE(supportmeshname);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_ENT (type d'entité support)
   */
  if ( _MEDattrEntierLire(_elemid,MED_NOM_ENT,&_intentitytype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_ENT);ISCRUTE(_intentitytype);
    goto ERROR;
  }

  *sentitytype = (med_entity_type) _intentitytype;

  /*
   * Lecture de l'attribut MED_NOM_GEO (type géométrique des mailles support)
   */
  if ( _MEDattrEntierLire(_elemid,MED_NOM_GEO,&_medintsgeotype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_GEO);ISCRUTE(_medintsgeotype);
    goto ERROR;
  }
  *sgeotype=_medintsgeotype;

  /*
   * Lecture du nombre de noeuds support
   */
  if (strlen(supportmeshname)) {
    if ( (*snnode = MEDmeshnEntity(fid,supportmeshname,MED_NO_DT,MED_NO_IT,
				     MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,
				     &_chgt,&_trsf) )  <= 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      ISCRUTE(*snnode);goto ERROR;
    }
  } else {
    *snnode=1;
  }
  /*
   * Lecture du nombre de mailles support
   */

  if (strlen(supportmeshname)) {
    if ( (*sncell = MEDmeshnEntity(fid,supportmeshname,MED_NO_DT,MED_NO_IT,
				     MED_CELL,*sgeotype,MED_CONNECTIVITY,MED_NODAL,
				     &_chgt,&_trsf) )  < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      ISCRUTE(*sncell);goto ERROR;
    }
  } else {
    *sncell=0;
  }


  _MEDnObjects(_elemid,MED_CSTATR_NOM,&_tmpn);

  if ( _tmpn > 0) {

    if ((_cstid = _MEDdatagroupOpen(_elemid,MED_CSTATR_NOM)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_CSTATR_NOM);
      SSCRUTE(_path);goto ERROR;
    }

    if ( _MEDattrEntierLire(_cstid,MED_NOM_PFU,&_intanyprofile) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_STRUCT_MSG);
      SSCRUTE(modelname);SSCRUTE(MED_NOM_PFU);
      goto ERROR;
    }
    *anyprofile=_intanyprofile;

    *nconstantattribute = (med_int) _tmpn;

  } else {
    *anyprofile = MED_FALSE;
    *nconstantattribute = 0;
  }

  _MEDnObjects(_elemid,MED_VARATR_NOM,&_tmpn);
  if ( _tmpn > 0)
    *nvariableattribute = (med_int) _tmpn;
  else
    *nvariableattribute = (med_int) 0;

  _ret=0;

 ERROR:

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
