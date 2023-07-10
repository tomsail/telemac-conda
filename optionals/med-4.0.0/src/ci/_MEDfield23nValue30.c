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

#include "med_versioned.h"

void
_MEDfield23nValue30(int dummy, ...)
{
  char     _meshname          [MED_NAME_SIZE+1]="";
  char     _path[MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1]=MED_FIELD_GRP;
  med_int  _ret=-1;

  MED_VARGS_DECL(const, med_idt           , , fid                        );
  MED_VARGS_DECL(const, char * , const      , fieldname                  );
  MED_VARGS_DECL(const, med_int           , , numdt                      );
  MED_VARGS_DECL(const, med_int           , , numit                      );
  MED_VARGS_DECL(const, med_entity_type   , , entitytype                 );
  MED_VARGS_DECL(const, med_geometry_type , , geotype                    );
  MED_VARGS_DECL(const, char * , const      , meshname                   );
  MED_VARGS_DECL(, char *, const            , profilename                );
  MED_VARGS_DECL(const, int               , , profileit                  );
  MED_VARGS_DECL(const, med_storage_mode  , , storagemode                );
  MED_VARGS_DECL(, med_int *, const   , profilesize                );
  MED_VARGS_DECL(, char *, const      , localizationname           );
  MED_VARGS_DECL(, med_int *, const   , nintegrationpoint       );
  MED_VARGS_DECL(, med_int *         ,, fret                       );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid                        );
  MED_VARGS_DEF(const, char * , const      , fieldname                  );
  MED_VARGS_DEF(const, med_int           , , numdt                      );
  MED_VARGS_DEF(const, med_int           , , numit                      );
  MED_VARGS_DEF(const, med_entity_type   , , entitytype                 );
  MED_VARGS_DEF(const, med_geometry_type , , geotype                    );
  MED_VARGS_DEF(const, char * , const      , meshname                   );
  MED_VARGS_DEF(, char *, const            , profilename                );
  MED_VARGS_DEF(const, int               , , profileit                  );
  MED_VARGS_DEF(const, med_storage_mode  , , storagemode                );
  MED_VARGS_DEF(, med_int *, const   , profilesize                );
  MED_VARGS_DEF(, char *, const      , localizationname           );
  MED_VARGS_DEF(, med_int *, const   , nintegrationpoint       );
  MED_VARGS_DEF(, med_int *         ,, fret                       );


  strcat(_path,fieldname);

  /* Lecture de l'attribut MED_NOM_MAI */
  if ( _MEDattributeStringRdByName(fid,_path,MED_NOM_MAI,MED_NAME_SIZE,_meshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_MAI);SSCRUTE(_meshname);
    goto ERROR;
  }

  if ( strlen(meshname) )
    if (strcmp(_meshname,meshname) ) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,"meshname");
      SSCRUTE(_path);SSCRUTE(_meshname);SSCRUTE(meshname);
      goto ERROR;
    }


  _MEDfieldnValue30 (dummy,
		     fid,
		     fieldname,
		     numdt,
		     numit,
		     entitytype,
		     geotype,
		     profilename,
		     profileit,
		     storagemode,
		     profilesize,
		     localizationname,
		     nintegrationpoint,
		     fret);

  if (*fret  < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDfieldnValue30");
    goto ERROR;
  }

  _ret=*fret;

 ERROR:
  va_end(params);
  *fret =_ret;

  return;
}
