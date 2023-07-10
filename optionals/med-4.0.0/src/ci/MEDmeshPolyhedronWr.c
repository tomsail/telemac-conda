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

/**\ingroup MEDmesh
  \brief \MEDmeshPolyhedronWrBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param dt \dt
  \param entitype \entitype
  \param cmode \cmode
  \param faceindexsize \faceindexsize
  \param faceindex \faceindex
  \param nodeindexsize \nodeindexsize
  \param nodeindex \nodeindex
  \param connectivity \connectivity
  \retval med_err \error
  \details \MEDmeshPolyhedronWrDetails
  \remarks
  \MEDmeshPolyhedronRem
 */


med_err
MEDmeshPolyhedronWr(const med_idt               fid,
		    const char*  const          meshname,
		    const med_int               numdt,
		    const med_int               numit,
		    const med_float             dt,
		    const med_entity_type       entitype,
		    const med_connectivity_mode cmode,
		    const med_int               faceindexsize,
		    const med_int * const       faceindex,
		    const med_int               nodeindexsize,
		    const med_int * const       nodeindex,
		    const med_int * const       connectivity )
{
  med_access_mode _MED_ACCESS_MODE;
  med_idt         _meshid=0;
  med_err         _ret         = -1;
  med_data_type   _datatype    = MED_UNDEF_DATATYPE;
  med_int         _connectivitysize = 0;
  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
 if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  if ( _MED_ACCESS_MODE == MED_ACC_RDONLY) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    ISCRUTE_int(_MED_ACCESS_MODE);
    goto ERROR;
  }


  /* Tester le type d'entité ?
   * MAILLE Ok
   * FACE   OK
   * ARETE  ?
   * NOEUD  KO
   * NOEUD_MAILLE  ?
   */

  switch(cmode)
    {
     case MED_NODAL :
       _connectivitysize = nodeindex[nodeindexsize-1] - nodeindex[0];
       break;

     case MED_DESCENDING :
       _connectivitysize = nodeindexsize;
       break;
       
     default :
       return -1;
     }

  if (_MEDmeshAdvancedWr(fid,
			 meshname,
			 MED_CONNECTIVITY,
			 MED_NO_NAME,
			 MED_INTERNAL_UNDEF,
			 numdt,
			 numit,
			 dt,
			 entitype,
			 MED_POLYHEDRON,
			 cmode,
			 MED_UNDEF_STMODE,
			 MED_NO_PROFILE,
			 MED_FULL_INTERLACE,
			 MED_ALL_CONSTITUENT,
			 NULL,
			 _connectivitysize,
			 connectivity) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedWr (MED_CONNECTIVITY) ");
    goto ERROR;
  }

  if (_MEDmeshAdvancedWr(fid,
			 meshname,
			 MED_INDEX_NODE,
			 MED_NO_NAME,
			 MED_INTERNAL_UNDEF,
			 numdt,
			 numit,
			 dt,
			 entitype,
			 MED_POLYHEDRON,
			 cmode,
			 MED_UNDEF_STMODE,
			 MED_SAME_PROFILE_INTERNAL,
			 MED_FULL_INTERLACE,
			 MED_ALL_CONSTITUENT,
			 NULL,
			 nodeindexsize,
			 nodeindex) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedWr (MED_INDEX_NODE) ");
    goto ERROR;
  }

  if (_MEDmeshAdvancedWr(fid,
			 meshname,
			 MED_INDEX_FACE,
			 MED_NO_NAME,
			 MED_INTERNAL_UNDEF,
			 numdt,
			 numit,
			 dt,
			 entitype,
			 MED_POLYHEDRON,
			 cmode,
			 MED_UNDEF_STMODE,
			 MED_SAME_PROFILE_INTERNAL,
			 MED_FULL_INTERLACE,
			 MED_ALL_CONSTITUENT,
			 NULL,
			 faceindexsize,
			 faceindex) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedWr (MED_INDEX_FACE) ");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;

}
