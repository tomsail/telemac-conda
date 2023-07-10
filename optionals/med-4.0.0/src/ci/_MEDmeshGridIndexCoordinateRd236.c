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

void _MEDmeshGridIndexCoordinateRd236(int dummy, ...) {


  med_access_mode _MED_ACCESS_MODE;
  med_idt         _meshid=0;
  med_err         _ret         = -1;
  med_data_type   _datatype    = MED_UNDEF_DATATYPE;
  med_grid_type   _gridtype    = MED_UNDEF_GRID_TYPE;
  med_int         _intgridtype = 0;
  med_int         _meshdim     = 0;
  char            _meshpath[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  med_int  _intaxistype=0;


  MED_VARGS_DECL(const, med_idt               , , fid       );
  MED_VARGS_DECL(const, char*  , const          , meshname  );
  MED_VARGS_DECL(const, med_int               , , numdt     );
  MED_VARGS_DECL(const, med_int               , , numit     );
  MED_VARGS_DECL(const, med_int               , , axis      );
  MED_VARGS_DECL(, med_float *, const           , gridindex );
  MED_VARGS_DECL(, med_err *                   ,, fret      );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt               , , fid       );
  MED_VARGS_DEF(const, char*  , const          , meshname  );
  MED_VARGS_DEF(const, med_int               , , numdt     );
  MED_VARGS_DEF(const, med_int               , , numit     );
  MED_VARGS_DEF(const, med_int               , , axis      );
  MED_VARGS_DEF(, med_float *, const           , gridindex );
  MED_VARGS_DEF(, med_err *                   ,, fret      );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

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
    SSCRUTE(meshname);SSCRUTE(MED_NOM_GTY);ISCRUTE(_intgridtype);goto ERROR;
  }
  _gridtype=(med_grid_type) _intgridtype;

  if ((_gridtype != MED_CARTESIAN_GRID) && (_gridtype != MED_POLAR_GRID)) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_GRIDTYPE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE_int(_gridtype);goto ERROR;
  }


 /* Lecture de l'attribut MED_NOM_REP */
  /* En 2.3.6, il n'y en a pas pour les grilles !=  MED_GRILLE_STANDARD*/
/*   if ( _MEDattrEntierLire(_meshid,MED_NOM_REP,&_intaxistype) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG); */
/*     SSCRUTE(meshname);SSCRUTE(MED_NOM_REP); */
/*     SSCRUTE(_intaxistype);goto ERROR; */
/*   } */

/*   if ((med_mesh_type)( _intaxistype) != MED_CARTESIAN ) { */
/*     MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_AXISTYPE,MED_ERR_MESH_MSG); */
/*     SSCRUTE(meshname);ISCRUTE_int(_intaxistype);goto ERROR; */
/*   } */

  /* Lecture de l'attribut MED_NOM_DIM  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_DIM,&_meshdim) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_DIM);ISCRUTE(_meshdim);goto ERROR;
  }

  if (axis > _meshdim ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(_meshdim);ISCRUTE(axis);goto ERROR;
  }

  /*
   * On ecrit le tableau d'indice dans un dataset HDF
   */
  switch(axis)
    {
    case 1 :
      _datatype = MED_COORDINATE_AXIS1;
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

  if (_MEDmeshAdvancedRd(fid,
			 meshname,
			 _datatype,
			 MED_NO_NAME,
			 MED_INTERNAL_UNDEF,
			 numdt,
			 numit,
			 MED_NODE,
			 MED_NONE,
			 MED_NO_CMODE,
			 MED_UNDEF_PFLMODE,
			 MED_NO_PROFILE,
			 MED_FULL_INTERLACE,
			 MED_ALL_CONSTITUENT,
			 NULL,
			 (unsigned char * const) gridindex) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedRd");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }


  va_end(params);
  *fret = _ret;
  return;
}
