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
#include <hdf5.h>
#include <string.h>

/**\ingroup MEDfilter
   \MEDfilterBrief
   \param fid \fid
   \param nentity \nentity \nentityMEDfilterCm
   \param nvaluesperentity \nvaluesperentity \nvaluesperentityMEDfilterCm \nvaluesperentityMEDfilterEx
   \param nconstituentpervalue \nconstituentpervalue \nconstituentpervalueMEDfilterEx
   \param constituentselect \constituentselect \constituentselectMEDfilterEx
   \param switchmode \switchmode
   \param storagemode \storagemode
   \param profilename \profilename
   \param filterarraysize \filterarraysize \filterarraysizeMEDfilterCm
   \param filterarray \filterarray

   \retval filter \filter \filterMEDfilterCm
   \return \error

   \details
   \MEDfilterDetails

   \remarks
   \MEDfilterNote

   \see MEDmeshNodeCoordinateAdvancedRd
   \see MEDmeshNodeCoordinateAdvancedWr
   \see MEDmeshEntityAttributeAdvancedRd
   \see MEDmeshEntityAttributeAdvancedWr
   \see MEDmeshElementConnectivityAdvancedRd
   \see MEDmeshElementConnectivityAdvancedWr

*/

med_err MEDfilterEntityCr(const med_idt           fid,
			  const med_int           nentity,
			  const med_int           nvaluesperentity,
			  const med_int           nconstituentpervalue,
			  const med_int           constituentselect,
			  const med_switch_mode   switchmode,
			  const med_storage_mode  storagemode,
			  const char * const      profilename,
			  const med_int           filterarraysize,
			  const med_int * const   filterarray,
			  med_filter* const       filter )
{
  char *  name = "_MEDfilterEntityCr";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDfileNumVersionRd(fid, &majeur, &mineur, &release);
  func = _MEDversionedApi3(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy,
	  fid,
	  nentity,
	  nvaluesperentity,
	  nconstituentpervalue,
	  constituentselect,
	  switchmode,
	  storagemode,
	  profilename,
	  filterarraysize,
	  filterarray,
	  filter,
	  &fret);
  return fret;
}
