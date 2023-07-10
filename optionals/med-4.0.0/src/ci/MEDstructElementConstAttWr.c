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

/**\ingroup MEDstructElement
  \brief \MEDstructElementConstAttWrBrief
  \param fid \fid
  \param elementname \modelname
  \param constattname \constattname
  \param constatttype \constatttype
  \param ncomponent \ncomponent
  \param sentitytype \sentitytype
  \param value \value
  \retval med_err \error
  \details \MEDstructElementConstAttWrDetails
  \remarks \MEDstructElementConstAttswitchCm
  \see      MEDstructElementConstAttWithProfileWr
 */


med_err
MEDstructElementConstAttWr(const med_idt                  fid,
			   const char*              const elementname,
			   const char*              const constattname,
			   const med_attribute_type       constatttype,
			   const med_int                  ncomponent,
			   const med_entity_type          sentitytype,
			   const void*              const value
			   )
{

  return
    MEDstructElementConstAttWithProfileWr(fid,
					  elementname,
					  constattname,
					  constatttype,
					  ncomponent,
					  sentitytype,
					  MED_NO_PROFILE,
					  value
					  );

}
