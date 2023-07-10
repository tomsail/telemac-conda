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

#ifdef PPRO_NT_CALL
#define F_OK 0
#else
#include <unistd.h>
#endif


/**\ingroup MEDfile
  \brief \MEDfileOpenBrief
  \param filename \filename
  \param accessmode \accessmode
  \retval med_idt  \fidDes
  \details \MEDfileOpenDetails
  \par Remarques
  \MEDfileOpenNote
*/

med_idt
MEDfileOpen(const char* const filename,
	    const med_access_mode accessmode)
{
  return MEDfileVersionOpen( filename, accessmode,
			     MED_NUM_MAJEUR, MED_NUM_MINEUR, MED_NUM_RELEASE );
}

