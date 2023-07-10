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

/**\ingroup MEDsubdomainJoint
   \brief \MEDsubdomainCorrespondenceSizeInfoBrief
   \param fid \fid
   \param meshname \meshname
   \param jointname \jointname
   \param numdt \numdt
   \param numit \numit
   \param corit \corit
   \param localentitype \localentitype
   \param localgeotype \localgeotype
   \param remoteentitype \remoteentitype
   \param remotegeotype \remotegeotype
   \param nentitycor \nentitycor
   \return \error
   \details
   \MEDsubdomainCorrespondenceSizeInfoDetails
   \par Remarques
   \MEDjointDef
*/


med_err
MEDsubdomainCorrespondenceSizeInfo(const med_idt              fid,
				   const char * const         meshname,
				   const char * const         jointname,
				   const med_int              numdt,
				   const med_int              numit,
				   const int                  corit,
				   med_entity_type   * const  localentitype,
				   med_geometry_type * const  localgeotype,
				   med_entity_type   * const  remoteentitype,
				   med_geometry_type * const  remotegeotype,
				   med_int * const            nentitycor )
{
  char *  name = "_MEDsubdomainCorrespondenceSizeInfo";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDfileNumVersionRd(fid, &majeur, &mineur, &release);
  func = _MEDversionedApi3(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy,
	  fid,
	  meshname,
	  jointname,
	  numdt,
	  numit,
	  corit,
	  localentitype,
	  localgeotype,
	  remoteentitype,
	  remotegeotype,
	  nentitycor
	  , &fret);
  return fret;
}
