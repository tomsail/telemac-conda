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

/**\ingroup MEDmesh
  \brief \MEDmeshGridIndexCoordinateWrBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param dt \dt
  \param axis \axis
  \param indexsize \indexsize
  \param gridindex \gridindex
  \retval med_err \error
  \details \MEDmeshGridIndexCoordinateWrDetails
  \remarks
  \MEDmeshGridIndexCoordinateWrRem
  \see MEDmeshNodeCoordinateWr
 */

med_err
MEDmeshGridIndexCoordinateWr(const med_idt               fid,
			     const char*  const          meshname,
			     const med_int               numdt,
			     const med_int               numit,
			     const med_float             dt,
			     const med_int               axis,
			     const med_int               indexsize,
			     const med_float * const     gridindex)
{
  med_access_mode _MED_ACCESS_MODE;
  med_idt         _meshid=0;
  med_idt         _datagroup1=0,_datagroup2=0,_datagroup3=0;
  med_err         _ret         = -1;
  med_data_type   _datatype    = MED_UNDEF_DATATYPE;
  med_grid_type   _gridtype    = MED_UNDEF_GRID_TYPE;
  med_int         _intgridtype = 0;
  med_int         _intaxistype = 0;
  med_int         _meshdim     = 0;
  char            _meshpath[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  char            _geotypename[MED_TAILLE_NOM_ENTITE+1]="";
  med_int         _0=0;
  med_int         _medintgeotype = MED_NO_GEOTYPE;
  char            _profilename      [MED_NAME_SIZE+1]=MED_SAME_PROFILE_INTERNAL;
  

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

  strcat(_meshpath,meshname);
  if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_meshpath); goto ERROR;
  }


  /*
   * Lecture du type de grille (attribut MED_NOM_GTY)
   */
  if (_MEDattrEntierLire(_meshid,MED_NOM_GTY,&_intgridtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_GTY);ISCRUTE_int(_gridtype);goto ERROR;
  }
  _gridtype=(med_grid_type) _intgridtype;

  /*
   * Les grilles MED_CURVILINEAR ne sont pas définies en utilisant 
   * cette fonction, elles doivent définir leurs coordonnées des noeuds
   * comme pour les maillages non structurés.
   */
  if ((_gridtype != MED_CARTESIAN_GRID) && (_gridtype != MED_POLAR_GRID)) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_GRIDTYPE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE_int(_gridtype);goto ERROR;
  }

 /* Lecture de l'attribut MED_NOM_REP */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_REP,&_intaxistype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_REP);
    ISCRUTE(_intaxistype);goto ERROR;
  }


  /* Lecture de l'attribut MED_NOM_DIM  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_DIM,&_meshdim) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_DIM);ISCRUTE(_meshdim);goto ERROR;
  }

  if (axis > _meshdim ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(_meshdim);ISCRUTE(axis);goto ERROR;
  }

  if ( (_datagroup1 =_MEDmeshAssociatedGroupCr(fid, MED_MESH_GRP,
					       meshname, numdt, numit, dt, MED_FALSE,
					       "." ) ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAssociatedGroupCr");
    goto ERROR;
  }

  if ((_datagroup2 = _MEDdatagroupOuvrir(_datagroup1,MED_NOM_MAI)) < 0) {

    if ((_datagroup2 = _MEDdatagroupCreer(_datagroup1,MED_NOM_MAI)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_NOM_MAI);
      SSCRUTE(meshname);ISCRUTE(numdt);ISCRUTE(numit);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_datagroup2,MED_NOM_CGT,&_0) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(MED_NOM_CGT);
      goto ERROR;
    }

    /* 
       Le type géométrique de maille utilisé pour les grilles est fonction
       de la dimension de la grille == nombre d'axes 
    */
    switch ( _meshdim )  {
    case 1 : 
      strcpy(_geotypename,MED_NOM_SE2);
      _medintgeotype = MED_SEG2;
      break;
    case 2 : 
      strcpy(_geotypename,MED_NOM_QU4);
      _medintgeotype = MED_QUAD4;
      break;
    case 3 : 
      strcpy(_geotypename,MED_NOM_HE8);
      _medintgeotype = MED_HEXA8;
      break;
    case 0 : strcpy(_geotypename,MED_NOM_PO1);
      _medintgeotype = MED_POINT1;
      break;
    default :
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_PARAMETER,"");
      SSCRUTE(meshname);ISCRUTE(numdt);ISCRUTE(numit);
      goto ERROR;
    }

    if ((_datagroup3 = _MEDdatagroupCreer(_datagroup2,_geotypename)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_geotypename);
      SSCRUTE(meshname);ISCRUTE(numdt);ISCRUTE(numit);
      goto ERROR;
    }

    if ( _MEDattributeStringWr(_datagroup3,MED_NOM_PFL,MED_NAME_SIZE,MED_NO_PROFILE_INTERNAL) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
      goto ERROR;
    }

    if (_MEDattributeIntWr(_datagroup3,MED_NOM_GEO,&_medintgeotype) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_GEO);
      ISCRUTE(_medintgeotype);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_datagroup3,MED_NOM_CGT,&_0) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(MED_NOM_CGT);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_datagroup3,MED_NOM_CGS,&_0) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(MED_NOM_CGS);
      goto ERROR;
    }

  }


  /*
   * On ecrit le tableau d'indice dans un dataset HDF
   */
  switch(axis)
    {
    case 1 :
      _datatype    = MED_COORDINATE_AXIS1;
      _profilename[0] = '\0';
      break;
    case 2 :
      _datatype = MED_COORDINATE_AXIS2;
      break;
    case 3 :
      _datatype = MED_COORDINATE_AXIS3;
      break;
    default :
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_GRIDTYPE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE_int(_gridtype);goto ERROR;
    }

  if (_MEDmeshAdvancedWr(fid,
			 meshname,
			 _datatype,
			 MED_NO_NAME,
			 MED_INTERNAL_UNDEF,
			 numdt,
			 numit,
			 dt,
			 MED_NODE,
			 MED_NONE,
			 MED_NO_CMODE,
			 MED_UNDEF_STMODE,
			 _profilename,
			 MED_FULL_INTERLACE,
			 MED_ALL_CONSTITUENT,
			 NULL,
			 indexsize,
			 gridindex) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedWr");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  if (_datagroup3>0)     if (_MEDdatagroupFermer(_datagroup3) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_geotypename);
    ISCRUTE_id(_datagroup3);
  }

  if (_datagroup2>0)     if (_MEDdatagroupFermer(_datagroup2) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_NOM_MAI);
    ISCRUTE_id(_datagroup2);
  }

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,meshname);
    ISCRUTE_id(_datagroup1);
  }

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }

  return _ret;

}
