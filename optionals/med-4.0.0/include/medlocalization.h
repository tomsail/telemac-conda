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

#ifndef MED_MEDLOCALIZATION_H
#define MED_MEDLOCALIZATION_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif


/* Localization  */
MEDC_EXPORT med_err
MEDlocalizationWr(const med_idt fid,
		  const char * const localizationname,
		  const med_geometry_type geotype,
		  const med_int spacedimension,
		  const med_float* const elementcoordinate,
		  const med_switch_mode switchmode,
		  const med_int nipoint,
		  const med_float* const ipointcoordinate,
		  const med_float* const weight,
		  const char *     const  geointerpname,
		  const char *     const  ipointstructmeshname);


MEDC_EXPORT med_err
MEDlocalizationRd(const med_idt                 fid,
		  const char*            const  localizationname,
		  const med_switch_mode         switchmode,
		  med_float*             const  elementcoordinate,
		  med_float*             const  ipointcoordinate,
		  med_float*             const  weight);

MEDC_EXPORT med_int
MEDnLocalization(const med_idt fid );

MEDC_EXPORT med_err
MEDlocalizationInfo(const med_idt             fid,
		    const int                 localizationit,
		    char              * const localizationname,
		    med_geometry_type * const geotype,
		    med_int           * const spacedimension,
		    med_int           * const nipoint,
		    char *              const geointerpname,
		    char *              const sectionmeshname,
		    med_int           * const nsectionmeshcell,
		    med_geometry_type * const sectiongeotype);

MEDC_EXPORT med_err
MEDlocalizationInfoByName(const med_idt             fid,
			  const char        * const localizationname,
			  med_geometry_type * const geotype,
			  med_int           * const spacedimension,
			  med_int           * const nipoint,
			  char *              const geointerpname,
			  char *              const sectionmeshname,
			  med_int           * const nsectionmeshcell,
			  med_geometry_type * const sectiongeotype);


#ifdef __cplusplus
}
#endif

#endif /* MED_MEDLOCALIZATION_H */

