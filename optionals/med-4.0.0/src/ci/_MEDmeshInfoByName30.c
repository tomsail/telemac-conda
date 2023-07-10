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
#include <string.h>

void _MEDmeshInfoByName30(int dummy, ...) {

/*   med_idt                  fid; */
/*   char *             const meshname; */
/*   med_int *          const spacedim; */
/*   med_int *          const meshdim; */
/*   med_mesh_type *    const meshtype; */
/*   char *             const description; */
/*   char *             const dtunit; */
/*   med_sorting_type * const sortingtype; */
/*   med_int *          const nstep; */
/*   med_axis_type *    const axistype; */
/*   char *             const axisname; */
/*   char *             const axisunit; */

  med_err  _ret=-1;


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

  if ( _MEDmeshInfoByName(fid, MED_MESH_GRP, meshname, spacedim, meshdim, meshtype,
			  description, dtunit, sortingtype, nstep,axistype,
			  axisname, axisunit)  < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE("MEDmeshInfoByName");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  va_end(params);
  *fret = _ret;
  return;
}
