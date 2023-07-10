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


#include "med_config.h"
#include <med.h>
#include "med_outils.h"


#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

#include "med_versioned.h"

#include <string.h>

void _MEDmeshInfoByName236(int dummy, ... ) {


  med_err            _ret     = -1;
  med_err            _fret    = -1;
  med_int            _nmaa    = 0;
  med_int            _meshdim = 0;
  med_maillage       _type;
  char               _meshname[MED_TAILLE_NOM+1] ="";
  char               _desc    [MED_TAILLE_DESC+1]="";
  int                _it       =0;
  char *             _axisname =0;
  char *             _axisunit =0;
  med_repere         _axistype =0;
  med_int            _numdt=MED_NO_DT;
  med_int            _numit=MED_NO_IT;
  char               _dt[MED_SNAME_SIZE+1]="";


  MED_VARGS_DECL(, med_idt                    ,, fid         );
  MED_VARGS_DECL(, char *            , const   , meshname    );
  MED_VARGS_DECL(, med_int *         , const   , spacedim    );
  MED_VARGS_DECL(, med_int *         , const   , meshdim     );
  MED_VARGS_DECL(, med_mesh_type *   , const   , meshtype    );
  MED_VARGS_DECL(, char *            , const   , description );
  MED_VARGS_DECL(, char *            , const   , dtunit      );
  MED_VARGS_DECL(, med_sorting_type *, const   , sortingtype );
  MED_VARGS_DECL(, med_int *         , const   , nstep       );
  MED_VARGS_DECL(, med_axis_type *   , const   , axistype    );
  MED_VARGS_DECL(, char *            , const   , axisname    );
  MED_VARGS_DECL(, char *            , const   , axisunit    );
  MED_VARGS_DECL(, med_err *                  ,, fret        );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(, med_idt                    ,, fid         );
  MED_VARGS_DEF(, char *            , const   , meshname    );
  MED_VARGS_DEF(, med_int *         , const   , spacedim    );
  MED_VARGS_DEF(, med_int *         , const   , meshdim     );
  MED_VARGS_DEF(, med_mesh_type *   , const   , meshtype    );
  MED_VARGS_DEF(, char *            , const   , description );
  MED_VARGS_DEF(, char *            , const   , dtunit      );
  MED_VARGS_DEF(, med_sorting_type *, const   , sortingtype );
  MED_VARGS_DEF(, med_int *         , const   , nstep       );
  MED_VARGS_DEF(, med_axis_type *   , const   , axistype    );
  MED_VARGS_DEF(, char *            , const   , axisname    );
  MED_VARGS_DEF(, char *            , const   , axisunit    );
  MED_VARGS_DEF(, med_err *                  ,, fret        );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  if ( strlen(meshname) > MED_TAILLE_NOM ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_RANGE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE_int(MED_TAILLE_NOM);
    goto ERROR;
  }

  if ( (_nmaa =MEDnMaa(fid) ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE("MEDnMaa");
    goto ERROR;
  }

  for (_it=0; _it< _nmaa; ++_it ) {


    if ( MEDmaaInfo(fid, _it+1, _meshname, &_meshdim,  &_type, _desc) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_MESH_MSG);
      SSCRUTE(_meshname);SSCRUTE("MEDmaaInfo");
      ISCRUTE_int(_it);ISCRUTE(_meshdim);ISCRUTE_int(_type);SSCRUTE(_desc);
      goto ERROR;
    }

    if ( !strcmp(_meshname,meshname) ) {
/*       strcpy(meshname,_meshname); */
      *meshdim=_meshdim;
      *meshtype = (med_mesh_type) _type;
      strcpy(description,_desc);
      *sortingtype = MED_SORT_UNDEF;
      if ( ( (*spacedim) = MEDdimEspaceLire(fid, _meshname) ) < 0 ) (*spacedim)=_meshdim;
      _axisname = malloc(MED_TAILLE_PNOM*sizeof(char)* (*spacedim) + 1);
      _axisunit = malloc(MED_TAILLE_PNOM*sizeof(char)* (*spacedim) + 1);
      (*_axisname) = '\0';
      (*_axisunit) = '\0';

      if ( MED23v30axesInfo( fid, _meshname,
			     (med_repere * ) &_axistype, _axisname, _axisunit) < 0 )  {
	(*axistype) = MED_UNDEF_AXIS_TYPE;
	(*axisname) = '\0';
	(*axisunit) = '\0';
      } else {
	_MED23v30stringConvert(axisname,MED_SNAME_SIZE,_axisname,MED_TAILLE_PNOM,*spacedim);
	_MED23v30stringConvert(axisunit,MED_SNAME_SIZE,_axisunit,MED_TAILLE_PNOM,*spacedim);
	(*axistype) = (med_axis_type) _axistype;
      }

      _MEDmeshComputationStepInfo236(dummy,fid, meshname, 1,&_numdt,&_numit,_dt, MED_FALSE,&_fret);

      if (_fret >= 0) *nstep=1; else *nstep=0;
      *dtunit='\0';

      break;
    }

  }
  _ret = 0;

 ERROR:

  if (_axisname) free (_axisname);
  if (_axisunit) free (_axisunit);

  va_end(params);
  *fret = _ret;
  return;
}
