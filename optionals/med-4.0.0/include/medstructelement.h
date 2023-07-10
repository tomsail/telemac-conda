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

#ifndef MED_MEDSTRUCTELEMENT_H
#define MED_MEDSTRUCTELEMENT_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Interface de l'API MED */
  
MEDC_EXPORT med_geometry_type
MEDstructElementCr(const med_idt                 fid,
		   const char*             const modelname,
		   const med_int                 modeldim,
		   const char*             const supportmeshname,
		   const med_entity_type         sentitytype,
		   const med_geometry_type       sgeotype
		   );

MEDC_EXPORT med_int
MEDnStructElement(const med_idt      fid);

MEDC_EXPORT med_err
MEDstructElementConstAttWithProfileWr(const med_idt                  fid,
				      const char*              const modelname,
				      const char*              const constattname,
				      const med_attribute_type       constatttype,
				      const med_int                  ncomponent,
				      const med_entity_type          sentitytype,
				      const char*              const profilename,
				      const void*              const value
				      );

MEDC_EXPORT med_err
MEDstructElementConstAttWr(const med_idt                  fid,
			   const char*              const modelname,
			   const char*              const constattname,
			   const med_attribute_type       constatttype,
			   const med_int                  ncomponent,
			   const med_entity_type          sentitytype,
			   const void*              const value
			   );

MEDC_EXPORT med_err
MEDstructElementInfoByName(const med_idt             fid,
			   const char *        const modelname,
			   med_geometry_type * const mgeotype,
			   med_int*            const modeldim,
			   char*               const supportmeshname,
			   med_entity_type*    const sentitytype,
			   med_int*            const snnode,
			   med_int*            const sncell,
			   med_geometry_type*  const sgeotype,
			   med_int*            const nconstantatribute,
			   med_bool*           const anyprofile,
			   med_int*            const nvariableattribute
			   );

MEDC_EXPORT med_err
MEDstructElementInfo(const med_idt             fid,
		     const int                 mit,
		     char *              const modelname,
		     med_geometry_type * const mgeotype,
		     med_int*            const modeldim,
		     char*               const supportmeshname,
		     med_entity_type*    const sentitytype,
		     med_int*            const snnode,
		     med_int*            const sncell,
		     med_geometry_type*  const sgeotype,
		     med_int*            const nconstantattribute,
		     med_bool*           const anyprofile,
		     med_int*            const nvariableattribute
		     );

MEDC_EXPORT med_err
MEDstructElementConstAttInfoByName(const med_idt             fid,
				   const char*         const modelname,
				   const char*         const constattname,
				   med_attribute_type* const constatttype,
				   med_int*            const ncomponent,
				   med_entity_type*    const sentitytype,
				   char*               const profilename,
				   med_int*            const profilesize
				   );
MEDC_EXPORT med_err
MEDstructElementConstAttInfo(const med_idt             fid,
			     const char*         const modelname,
			     const int                 attit,
			     char*               const constattname,
			     med_attribute_type* const constatttype,
			     med_int*            const ncomponent,
			     med_entity_type*    const sentitytype,
			     char*               const profilename,
			     med_int*            const profilesize
			     );

MEDC_EXPORT med_err
MEDstructElementConstAttRd(const med_idt                  fid,
			   const char*              const modelname,
			   const char*              const constattname,
			   void*                    const value
			   );

MEDC_EXPORT int
MEDstructElementAttSizeof(const med_attribute_type atttype );

MEDC_EXPORT med_err
MEDstructElementVarAttCr(const med_idt                  fid,
			 const char*              const modelname,
			 const char*              const varattname,
			 const med_attribute_type       varatttype,
			 const med_int                  ncomponent
			 );

MEDC_EXPORT med_err
MEDstructElementVarAttInfoByName(const med_idt                   fid,
				 const char*               const modelname,
				 const char*               const varattname,
				       med_attribute_type* const varatttype,
				       med_int*            const ncomponent
				 );

MEDC_EXPORT med_err
MEDstructElementVarAttInfo(const med_idt                   fid,
			   const char*               const modelname,
			   const int                       attit,
			         char*               const varattname,
			         med_attribute_type* const varatttype,
			         med_int*            const ncomponent
			   );

MEDC_EXPORT med_err
MEDmeshStructElementVarAttWr(const med_idt                  fid,
			     const char*              const meshname,
			     const med_int                  numdt,
			     const med_int                  numit,
			     const med_geometry_type        mgeotype,
			     const char*              const varattname,
			     const med_int                  nentity,
			     const void*              const value
			     );
MEDC_EXPORT med_err
MEDmeshStructElementVarAttRd(const med_idt                  fid,
			     const char*              const meshname,
			     const med_int                  numdt,
			     const med_int                  numit,
			     const med_geometry_type        mgeotype,
			     const char*              const varattname,
			     void*                    const value
			     );

MEDC_EXPORT med_err
MEDstructElementName(const med_idt                 fid,
		     const med_geometry_type       mgeotype,
		     char *                  const modelname);


MEDC_EXPORT med_geometry_type
MEDstructElementGeotype(const med_idt                 fid,
			const char *            const modelname);



#ifdef __cplusplus
}
#endif

#endif /* MED_MEDSTRUCTELEMENT_H */

