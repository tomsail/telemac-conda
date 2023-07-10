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
  \brief \MEDmeshGridTypeWrBrief
  \param fid \fid
  \param meshname \meshname
  \param gridtype \gridtype
  \retval med_err \error
  \details \MEDmeshGridTypeWrDetails
  \see MEDmeshCr
 */

med_err
MEDmeshGridTypeWr(const med_idt fid,
		  const char * const meshname, 
		  const med_grid_type gridtype)
{
  med_err       _ret=-1;
  med_idt       _meshid=0;
  char          _path[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  med_mesh_type _meshtype;
  med_int       _intmeshtype=MED_UNDEF_MESH_TYPE;
  med_int       _intgridtype=gridtype;
  med_int       _intaxistype=0;
  med_axis_type _axistype=MED_UNDEF_AXIS_TYPE;
  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
 if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  /*
   * On regarde si le groupe existe => erreur si non
   */
  strcat(_path,meshname);
  if ((_meshid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_meshid);goto ERROR;
  }

  /*
   * Si le maillage est de type MED_UNSTRUCTURED_MESH => erreur
   */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_TYP,&_intmeshtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_TYP); goto ERROR;
  }
  _meshtype = (med_mesh_type) (_intmeshtype);

  if (_meshtype == MED_UNSTRUCTURED_MESH) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_TYP);ISCRUTE_int(_meshtype); goto ERROR;
  }

  /* - Une grille MED_CARTESIAN_GRID doit utiliser un système de coordonnées
   *  MED_CARTESIAN
   *  - Une grille MED_POLAR_GRID peut utiliser un système de coordonnées
   *  MED_CYLINDRICAL ou MED_SPHERICAL
   *  - Une Grille MED_CURVILINEAR_GRID peut utiliser un système de coordonnées
   *  quelconque.
   */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_REP,&_intaxistype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_REP); goto ERROR;
  }
  _axistype = (med_axis_type) (_intaxistype);

  if (
      ( (gridtype == MED_CARTESIAN_GRID) &&  (_axistype != MED_CARTESIAN  )     ) ||
      ( (gridtype == MED_POLAR_GRID    ) && ((_axistype != MED_CYLINDRICAL) && 
					     (_axistype != MED_SPHERICAL  )    ))
      ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_AXISTYPE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(_intaxistype);goto ERROR;
  }


  /*
   * Creation de l'attribut correspondant au type de grille
   * L'attribut "GTY"
   */
  if ( _MEDattributeIntWr(_meshid,MED_NOM_GTY,&_intgridtype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_GTY);
    ISCRUTE(_intgridtype);goto ERROR;
  }

  _ret = 0;

ERROR:

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_meshid);
  }

  return _ret;
}
