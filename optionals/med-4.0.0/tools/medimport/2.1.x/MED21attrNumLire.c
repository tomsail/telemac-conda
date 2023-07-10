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


#include <med_config.h>
#include <med.h>
#include <med_outils.h>
#include <hdf5.h>

med_err _MED21attrNumLire(med_idt pere,med_type_champ type,char *nom,
			  unsigned char *val,hid_t hdf_file)
{
  med_idt attid,type_hdf;
  med_err ret;

  if ((attid = H5Aopen_name(pere,nom)) < 0)
    return -1;

  switch(type) 
    {
    case MED_FLOAT64 :
      if (H5Tequal(hdf_file,H5T_IEEE_F64BE))
	type_hdf = H5T_IEEE_F64LE;
      if (H5Tequal(hdf_file,H5T_IEEE_F64LE))
	type_hdf = H5T_IEEE_F64BE;
      break;
      
    case MED_INT :
#if defined(HAVE_F77INT64)
      type_hdf = H5T_NATIVE_LONG; 
#else
      type_hdf = H5T_NATIVE_INT;
#endif
      break;
      
    default :
      return -1;
    }

  if ((ret = H5Aread(attid,type_hdf,val)) < 0)
    return -1;

  if ((ret = H5Aclose(attid)) < 0)
    return -1;

  return 0;
}
