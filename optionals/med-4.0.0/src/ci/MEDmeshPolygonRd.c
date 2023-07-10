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
  \brief \MEDmeshPolygonRdBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param cmode \cmode
  \param polyindex \polyindex
  \param connectivity \connectivity
  \retval med_err \error
  \details \MEDmeshPolygonRdDetails
  \see MEDmeshnEntity
  \remarks
  \MEDmeshPolygonRem

 */

med_err
MEDmeshPolygonRd(const med_idt               fid,
		 const char*  const          meshname,
		 const med_int               numdt,
		 const med_int               numit,
		 const med_entity_type       entitype,
		 const med_connectivity_mode cmode,
		 med_int * const             polyindex,
		 med_int * const             connectivity )
{
  med_access_mode _MED_ACCESS_MODE;
  med_idt         _meshid=0;
  med_err         _ret         = -1;
  med_data_type   _datatype    = MED_UNDEF_DATATYPE;
  med_entity_type _entitytype=entitype;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  /* Tester le type d'entité ?
   * MAILLE Ok
   * FACE   OK
   * ARETE  ?
   * NOEUD  KO
   * NOEUD_MAILLE  ?
   */
  if (entitype == MED_NODE_ELEMENT ) _entitytype=MED_NODE ;

  if (_MEDmeshAdvancedRd(fid,
			 meshname,
			 MED_CONNECTIVITY,
			 MED_NO_NAME,
			 MED_INTERNAL_UNDEF,
			 numdt,
			 numit,
			 _entitytype,
			 MED_POLYGON,
			 cmode,
			 MED_UNDEF_STMODE,
			 MED_NO_PROFILE,
			 MED_FULL_INTERLACE,
			 MED_ALL_CONSTITUENT,
			 NULL,
			 (unsigned char * const)connectivity) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedRd (MED_CONNECTIVITY) ");
    goto ERROR;
  }

  if (_MEDmeshAdvancedRd(fid,
			 meshname,
			 MED_INDEX_NODE,
			 MED_NO_NAME,
			 MED_INTERNAL_UNDEF,
			 numdt,
			 numit,
			 _entitytype,
			 MED_POLYGON,
			 cmode,
			 MED_UNDEF_STMODE,
			 MED_NO_PROFILE,
			 MED_FULL_INTERLACE,
			 MED_ALL_CONSTITUENT,
			 NULL,
			 (unsigned char * const)polyindex) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedRd (MED_INDEX_NODE) ");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;

}
