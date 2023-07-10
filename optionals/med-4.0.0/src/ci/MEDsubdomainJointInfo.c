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
   \brief \MEDsubdomainJointInfoBrief
   \param fid \fid
   \param localmeshname \localmeshname
   \param jointit \jointit
   \param jointname \jointname
   \param description \description
   \param domainnumber \domainnumber
   \param remotemeshname \remotemeshname
   \param nstep \nstep
   \param nocstpncorrespondence \nocstpncorrespondence
   \return \error
   \details
   \MEDsubdomainJointInfoDetails
   \par Remarques
   \MEDjointDef
*/

med_err
MEDsubdomainJointInfo(const med_idt      fid,
		      const char * const localmeshname,
		      const int          jointit,
		      char * const       jointname,
		      char * const       description,
		      med_int * const    domainnumber,
		      char * const       remotemeshname,
		      med_int * const    nstep,
		      med_int * const    nocstpncorrespondence)
{
  char *  name = "_MEDsubdomainJointInfo";
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
	  jointit,
	  jointname,
	  description,
	  domainnumber,
	  remotemeshname,
	  nstep,
	  nocstpncorrespondence,
	  &fret);
  return fret;
}
