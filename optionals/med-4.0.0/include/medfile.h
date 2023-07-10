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

#ifndef MED_MEDFILE_H
#define MED_MEDFILE_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

/* File */
MEDC_EXPORT med_idt
MEDfileOpen(const char* const filename,
	    const med_access_mode accessmode);

extern MEDC_EXPORT med_idt
MEDfileVersionOpen(const char* const filename,
		   const med_access_mode accessmode,
		   const med_int major, const med_int minor, const med_int release);

MEDC_EXPORT med_err
MEDfileExist(const char* const     filename,
	     const med_access_mode accessmode,
	     med_bool * const      fileexist,
	     med_bool * const      accessok );

MEDC_EXPORT med_idt
MEDmemFileOpen(const char* const filename, med_memfile * const memfile, const med_bool filesync,
	       const med_access_mode accessmode);

#ifdef MED_HAVE_MPI
MEDC_EXPORT med_idt
MEDparFileOpen(const char* const filename,
	       const med_access_mode accessmode,
	       const MPI_Comm comm, const MPI_Info info);

#endif

MEDC_EXPORT med_int
MEDfileName(med_idt fid, char * const filename, const med_int filenamesize);

MEDC_EXPORT med_err
MEDfileClose(med_idt fid);

MEDC_EXPORT med_err
MEDfileCommentWr(const med_idt fid,
		 const char* const comment);
MEDC_EXPORT med_err
MEDfileCommentRd(const med_idt fid,
		 char* const comment);
MEDC_EXPORT med_err
MEDfileCompatibility(const char* const filename,
		     med_bool* const hdfok,
		     med_bool* const medok);
MEDC_EXPORT med_err
MEDfileNumVersionRd(const med_idt fid,
		    med_int* const major,
		    med_int* const minor,
		    med_int* const release);
MEDC_EXPORT med_err
MEDfileStrVersionRd(const med_idt fid,
		    char* const version);
MEDC_EXPORT med_idt
MEDfileObjectsMount(const med_idt fid,
		    const char* const filename,
		    const med_class medclass);
MEDC_EXPORT med_idt
MEDfileObjectsMountById(const med_idt         fid,
			const med_idt         chfid,
			const char * const    chpath,
			const med_class       medclass);

MEDC_EXPORT med_err
MEDfileObjectsUnmount(const med_idt fid,
		      const med_idt mid,
		      const med_class medclass);
MEDC_EXPORT med_err
MEDfileObjectExist(const med_idt           fid,
		   const med_class         medclass,
		   const char * const      objectname,
		   med_bool   * const      objectexist );

#ifdef __cplusplus
}
#endif

#endif /* MED_MEDFILE_H */

