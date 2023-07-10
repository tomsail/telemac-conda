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
#include <string.h>
#include <stdlib.h>
#include <med_outils.h>

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

#include "med_versioned.h"

void
_MEDmeshEntityInfo236(int dummy, ...) {


  med_access_mode       _MED_ACCESS_MODE;
  med_int               _ret=-1;
  med_idt               _meshid=0, _datagroup2=0,_datagroup3=0;
  char                  _meshpath         [MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  char                  _datagroupname2   [MED_TAILLE_NOM_ENTITE+1]="";
  char                  _datagroupname3   [MED_TAILLE_NOM_ENTITE+1]="";
  int                   _num;


  MED_VARGS_DECL(const, med_idt                   , , fid         );
  MED_VARGS_DECL(const, char *              , const , meshname    );
  MED_VARGS_DECL(const, med_int                   , , numdt       );
  MED_VARGS_DECL(const, med_int                   , , numit       );
  MED_VARGS_DECL(const, med_entity_type           , , entitype    );
  MED_VARGS_DECL(const, int                       , , geotypeit   );
  MED_VARGS_DECL(, char              *, const , geotypename       );
  MED_VARGS_DECL(, med_geometry_type *, const , geotype           );
  MED_VARGS_DECL(, med_err  *                ,, fret              );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt                   , , fid         );
  MED_VARGS_DEF(const, char *              , const , meshname    );
  MED_VARGS_DEF(const, med_int                   , , numdt       );
  MED_VARGS_DEF(const, med_int                   , , numit       );
  MED_VARGS_DEF(const, med_entity_type           , , entitype    );
  MED_VARGS_DEF(const, int                       , , geotypeit   );
  MED_VARGS_DEF(, char              *, const , geotypename       );
  MED_VARGS_DEF(, med_geometry_type *, const , geotype           );
  MED_VARGS_DEF(, med_err  *                ,, fret              );

  _num        = geotypeit -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  /*
   * Ouverture du datagroup de niveau 1 <_meshpath>/<meshname>
   */
  NOFINALBLANK(meshname,ERROR);
  strcat(_meshpath,meshname);
  if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);goto ERROR;
  }

  /*
   *  Ouverture du datagroup de niveau 3 <entitype>
   */
  if (_MEDgetEntityTypeName(_datagroupname2,entitype) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(entitype);SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);goto ERROR;
  }

  if ((_datagroup2 = _MEDdatagroupOuvrir(_meshid,_datagroupname2)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
    goto ERROR;
  }

  if ( entitype == MED_NODE ) {
    if ( _num ) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,"");
      ISCRUTE_int(entitype);ISCRUTE_int(geotypeit);
      goto ERROR;
    } else {
      *geotype = MED_UNDEF_GEOMETRY_TYPE;
      *geotypename='\0';
    }
  } else {

    /*
     * On recupere le nom du type géométrique
     */
    if ( _MEDobjectGetName(_datagroup2, "." ,_num, _datagroupname3) < 0 ) {
      MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_datagroupname3);
      ISCRUTE_int(geotypeit);
      goto ERROR;
    }

    strncpy(geotypename,_datagroupname3,MED_TAILLE_NOM_ENTITE+1);

    *geotype =  MEDgetGeometryTypeFromName(geotypename);

  }

  _ret = 0;

 ERROR:

  if (_datagroup3>0)     if (_MEDdatagroupFermer(_datagroup3) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname3);
    ISCRUTE_id(_datagroup3);
  }

  if (_datagroup2>0)     if (_MEDdatagroupFermer(_datagroup2) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname2);
    ISCRUTE_id(_datagroup2);
  }

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }

  *fret = _ret;
  va_end(params);
  return;
}

