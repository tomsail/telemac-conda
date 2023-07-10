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

#ifndef MED_HDFI21_H
#define MED_HDFI21_H

/* #ifdef __cplusplus */
/*  extern "C" { */
/* #endif */

# include <med.h>

/* #ifdef __cplusplus */
/*  } */
/* #endif */

#ifdef __cplusplus
 extern "C" {
#endif

extern 
med_err _MED21attrNumLire(med_idt pere,med_type_champ type,char *nom,
				 unsigned char *val,hid_t hdf_file);

extern 
med_err _MED21datasetNumLire(med_idt pere,char *nom,med_type_champ type,
			     med_mode_switch interlace, med_size nbdim, med_size fixdim, 
			     med_size psize, med_ssize * pfltab, med_int ngauss,
			     unsigned char *val,hid_t hdf_file);
#ifdef __cplusplus
 }
#endif
#endif
