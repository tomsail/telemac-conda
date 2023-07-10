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


#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

void _MEDequivalenceCorrespondenceSizeInfo236( int dummy, ...) {


  med_err   _ret=-1,_err=-1;
  char      _path[MED_MESH_GRP_SIZE+MED_EQUIVALENCE_GRP_SIZE+2*MED_TAILLE_NOM+1+
		 2*MED_TAILLE_NOM_ENTITE+1+1]=MED_MESH_GRP;
  char      _entgeoname[2*MED_TAILLE_NOM_ENTITE+2]="";
  med_size  _nocstpncorrespondence=0;
  med_int   _intentitytype;
  med_int   _intgeotype;
  int       _num;

  MED_VARGS_DECL(const, med_idt              , , fid      );
  MED_VARGS_DECL(const, char * , const         , meshname );
  MED_VARGS_DECL(const, char * , const         , equivname);
  MED_VARGS_DECL(const, med_int              , , numdt    );
  MED_VARGS_DECL(const, med_int              , , numit    );
  MED_VARGS_DECL(const, int                  , , corit    );
  MED_VARGS_DECL(, med_entity_type *, const    , entitype );
  MED_VARGS_DECL(, med_geometry_type*, const   , geotype  );
  MED_VARGS_DECL(, med_int *, const            , nentity  );
  MED_VARGS_DECL(, med_err *                  ,, fret     );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt              , , fid      );
  MED_VARGS_DEF(const, char * , const         , meshname );
  MED_VARGS_DEF(const, char * , const         , equivname);
  MED_VARGS_DEF(const, med_int              , , numdt    );
  MED_VARGS_DEF(const, med_int              , , numit    );
  MED_VARGS_DEF(const, int                  , , corit    );
  MED_VARGS_DEF(, med_entity_type *, const    , entitype );
  MED_VARGS_DEF(, med_geometry_type*, const   , geotype  );
  MED_VARGS_DEF(, med_int *, const            , nentity  );
  MED_VARGS_DEF(, med_err *                  ,, fret     );

  _num = corit -1;

  if ( (numdt != MED_NO_DT) || (numit != MED_NO_IT) ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,"");
    ISCRUTE(numdt);ISCRUTE(numit); goto ERROR;
  }

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

  strcat(_path,meshname);
  strcat(_path,MED_EQUIVALENCE_GRP);
  strcat(_path,equivname);
  strcat(_path,"/");


  /*
   * On recupere le nom de <entitype>[.<geotype>]
   */
  if ( _MEDobjectGetName(fid, _path ,_num, &_path[strlen(_path)]) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(corit);
    goto ERROR;
  }

  if ( _MEDattributeNumRdByName(fid, _path, MED_NOM_ENT,MED_INTERNAL_INT,(unsigned char *)&_intentitytype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_ENT);
    SSCRUTE(_path);ISCRUTE(_intentitytype);goto ERROR;
  }
  *entitype = (med_entity_type) _intentitytype;

  if ( _MEDattributeNumRdByName(fid, _path,MED_NOM_GEO,MED_INTERNAL_INT,(unsigned char *)&_intgeotype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_GEO);
    SSCRUTE(_path);ISCRUTE(_intgeotype);goto ERROR;
  }
  *geotype = (med_geometry_type) _intgeotype;


  if (  _MEDattributeNumRdByName(fid, _path, MED_NOM_NBR,MED_INTERNAL_INT, (unsigned char *) nentity) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_EQUIVALENCE_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_NBR);ISCRUTE(*nentity);
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  va_end(params);
  *fret = _ret;

  return;
}
