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


#include <stdio.h>

#include <med.h>
#include <med_config.h>
#include <med_outils.h>


/**\ingroup MEDfile
  \brief \MEDfileStrVersionRdBrief
  \param fid \fid
  \param medversion \medversion
  \retval med_err \error
  \details \MEDfileStrVersionRdDetails
 */

med_err
MEDfileStrVersionRd(const med_idt fid,
		    char* const medversion)
{
  med_err _ret = -1;

  med_int major;
  med_int minor;
  med_int release;

  int _intmajor;
  int _intminor;
  int _intrelease;

  if (  MEDfileNumVersionRd(fid,&major,&minor,&release) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDfileNumVersionRd");
    goto ERROR;
  }

  _intmajor  = (int)  major;
  _intminor  = (int)  minor;
  _intrelease= (int)  release;

  if (medversion)
    sprintf(medversion,"MED-%d.%d.%d",_intmajor,_intminor,_intrelease);

  _ret = 0;
  ERROR :

  /* the hdf group is closed  */
  return _ret;
}
