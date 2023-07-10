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
  \brief \MEDmeshGridStructWrBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param dt \dt
  \param gridstruct \gridstruct
  \retval med_err \error
  \details \MEDmeshGridStructWrDetails
  \remarks
  \MEDmeshGridStructRem
  \see MEDmeshNodeCoordinateWr
*/

med_err
MEDmeshGridStructWr(const med_idt               fid,
		    const char*  const          meshname,
		    const med_int               numdt,
		    const med_int               numit,
		    const med_float             dt,
		    const med_int * const       gridstruct)
{
  med_access_mode _MED_ACCESS_MODE;
  med_idt         _meshid=0;
  med_err         _ret         = -1;
  med_data_type   _datatype    = MED_UNDEF_DATATYPE;
  med_grid_type   _gridtype    = MED_UNDEF_GRID_TYPE;
  med_int         _intgridtype = 0;
  med_int         _meshdim     = 0;
  char            _meshpath[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  int             _i=0;

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

  /* Lecture de l'attribut MED_NOM_DIM  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_DIM,&_meshdim) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_DIM);ISCRUTE(_meshdim);goto ERROR;
  }

  /*
   * Lecture du type de grille (attribut MED_NOM_GTY)
   */
  if (_MEDattrEntierLire(_meshid,MED_NOM_GTY,&_intgridtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_GTY);ISCRUTE_int(_gridtype);goto ERROR;
  }
  _gridtype=(med_grid_type) _intgridtype;

  if ((_gridtype != MED_CURVILINEAR_GRID)) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_GRIDTYPE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE_int(_gridtype);goto ERROR;
  }

  /*
   * Attribut NBR (taille du tableau d'indices)
   */
  for (_i=0;_i<_meshdim;++_i) {

    switch(_i) {
     case 0 :
      _datatype = MED_COORDINATE_AXIS1;
      break;
    case 1 :
      _datatype = MED_COORDINATE_AXIS2;
      break;
    case 2 :
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
			   MED_INTERNAL_INT,
			   numdt,
			   numit,
			   dt,
			   MED_NODE,
			   MED_NONE,
			   MED_NO_CMODE,
			   MED_UNDEF_STMODE,
			   MED_NO_PROFILE,
			   MED_FULL_INTERLACE,
			   MED_ALL_CONSTITUENT,
			   NULL,
			   1,
			   (gridstruct+_i)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedWr");
      goto ERROR;
    }

  }

 _ret = 0;

 ERROR:

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }

  return _ret;

}
