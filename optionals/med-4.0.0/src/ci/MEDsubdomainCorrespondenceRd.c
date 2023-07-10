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
   \brief \MEDsubdomainCorrespondenceRdBrief
   \param fid \fid
   \param localmeshname \localmeshname
   \param jointname \jointname
   \param numdt \numdt
   \param numit \numit
   \param localentitype \localentitype
   \param localgeotype \localgeotype
   \param remoteentitype \remoteentitype
   \param remotegeotype \remotegeotype
   \param correspondence \correspondence
   \return \error
   \details
   \MEDsubdomainCorrespondenceRdDetails
   \par Remarques
   \MEDjointDef
*/

med_err
MEDsubdomainCorrespondenceRd(const med_idt            fid,
			     const char * const       localmeshname,
			     const char * const       jointname,
			     const med_int            numdt,
			     const med_int            numit,
			     const med_entity_type    localentitype,
			     const med_geometry_type  localgeotype,
			     const med_entity_type    remoteentitype,
			     const med_geometry_type  remotegeotype,
			     med_int * const          correspondence )
{
  char *  name = "_MEDsubdomainCorrespondenceRd";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDfileNumVersionRd(fid, &majeur, &mineur, &release);
  func = _MEDversionedApi3(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy,
	  fid,
	  localmeshname,
	  jointname,
	  numdt,
	  numit,
	  localentitype,
	  localgeotype,
	  remoteentitype,
	  remotegeotype,
	  correspondence,
	  &fret);
  return fret;
}
