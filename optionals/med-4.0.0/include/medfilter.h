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

#ifndef MED_MEDFILTER_H
#define MED_MEDFILTER_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

MEDC_EXPORT med_err
MEDfilterEntityCr(const med_idt fid,
		  const med_int nentity,
		  const med_int nvaluesperentity,
		  const med_int nconstituentpervalue,
		  const med_int constituentselect,
		  const med_switch_mode switchmode,
		  const med_storage_mode storagemode,
		  const char * const profilename,
		  const med_int filterarraysize,
		  const med_int * const filterarray,
		  med_filter* const filter);

MEDC_EXPORT med_err
MEDfilterBlockOfEntityCr(const med_idt          fid,
			 const med_int          nentity,
			 const med_int          nvaluesperentity,
			 const med_int          nconstituentpervalue,
			 const med_int          constituentselect,
			 const med_switch_mode  switchmode,
			 const med_storage_mode storagemode,
			 const char * const     profilename,
			 const med_size         start,
			 const med_size         stride,
			 const med_size         count,
			 const med_size         blocksize,
			 const med_size         lastblocksize,
			 med_filter*    const   filter);

MEDC_EXPORT med_err
MEDfilterDeAllocate(const int nfilter,
		    med_filter * filter);

MEDC_EXPORT med_filter*
MEDfilterAllocate(const int nfilter);

MEDC_EXPORT med_err
MEDfilterClose(  med_filter * const filter);

#ifdef __cplusplus
}
#endif

#endif /* MED_MEDFILTER_H */

