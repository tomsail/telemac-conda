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

#ifndef MESGERR
#define MESGERR 1
#endif

void _MEDobjetsOuverts(med_idt fid) {

  hid_t objlist[1000];
  char  objname[1000];
  int   count;
  int   obji;

  count = H5Fget_obj_ids(fid, H5F_OBJ_ALL, 1000, objlist);
  ISCRUTE_int(count);

  for (obji=0; obji < count; ++obji) {
    H5Iget_name(objlist[obji], objname, 1000 );
    SSCRUTE(objname);
  }
}
