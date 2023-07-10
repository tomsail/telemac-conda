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
#include <hdf5.h>


extern med_err _MEDcopyName(med_idt id,const char *lname, const H5L_info_t *linfo, void *data);

med_err
__MEDobjectGetName(const med_idt fid, const char * const path, const med_size ind, char *name,
		   const H5_index_t index_type, const H5_iter_order_t order
		   )
{

  med_err _ret=-1;
  med_size _n;
  med_idt  _gid1=0;
  hsize_t  _ind=0;
 /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /* old fashion */
/*   if ((idx = H5Giterate(fid,path,&ind,_MEDindiceInfo, */
/* 			nom)) < 0) */
/*     return -1; */

  

/*   if ( (_gid1 = H5Gopen(fid,path)) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,path); */
/*     goto ERROR; */
/*   } */

/*   if ( H5Literate(_gid1, index_type, order, (hsize_t *) &ind, _MEDcopyName, name  ) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_VISIT,MED_ERR_DATAGROUP,path); */
/*     H5Eprint1(stderr); */
/*     goto ERROR; */
/*   } */

  if ( H5Literate_by_name(fid,path, index_type, order, (hsize_t *) &ind, _MEDcopyName, name,H5P_DEFAULT ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_VISIT,MED_ERR_DATAGROUP,path);
    /* H5Eprint1(stderr); */
    goto ERROR;
  }


  /* Ne foctionne pas avec les liens soft */
/*   _n = H5Lget_name_by_idx(fid, path, H5_INDEX_NAME, H5_ITER_NATIVE, */
/* 			  ind, NULL, MED_NAME_SIZE,H5P_DEFAULT ); */

/*   ISCRUTE_long(_n); */
/*   if ( ((long ) _n ) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,path);ISCRUTE(ind); */
/*     H5Eprint1(stderr); */
/*     goto ERROR; */
/*   } */
/*   ISCRUTE_long(_n); */
/*   _n++; */
/*   if ( H5Lget_name_by_idx(fid, path, H5_INDEX_NAME, H5_ITER_NATIVE, */
/* 			  ind, name, _n,H5P_DEFAULT ) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,path);ISCRUTE(ind); */
/*     H5Eprint1(stderr); */
/*     goto ERROR; */
/*   } */
/*   SSCRUTE(name); */

  _ret = 0;
 ERROR:

  if (_gid1>0)            if ( H5Gclose(_gid1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,path);
    ISCRUTE_id(_gid1);
  }

  return _ret;
}

