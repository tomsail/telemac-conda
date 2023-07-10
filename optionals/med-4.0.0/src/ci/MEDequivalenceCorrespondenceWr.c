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


/**\ingroup MEDequivalence
  \brief \MEDequivalenceCorrespondenceWrBrief
  \param fid \fid
  \param meshname \meshname
  \param equivname \equivname
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param geotype \geotype
  \param nentity \nentityMEDequiv
  \param correspondence \correspondence
  \retval med_err  \error
  \details \MEDequivalenceCorrespondenceWrDetails
 */

med_err
MEDequivalenceCorrespondenceWr(const med_idt            fid,
			       const char * const       meshname,
			       const char * const       equivname,
			       const med_int            numdt,
			       const med_int            numit,
			       const med_entity_type    entitype,
			       const med_geometry_type  geotype,
			       const med_int const      nentity,
			       const med_int * const    correspondence) {


  char *  name = "_MEDequivalenceCorrespondenceWr";
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
	  equivname,
	  numdt,
	  numit,
	  entitype,
	  geotype,
	  nentity,
	  correspondence,
	  &fret);
  return fret;
}
