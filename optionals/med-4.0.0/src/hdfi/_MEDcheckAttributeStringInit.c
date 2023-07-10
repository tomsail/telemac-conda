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

void * _MEDcheckAttributeStringInit( med_string_itdatas * const itdatas,
				     const char * const attname, med_int attsize)
{
  med_int _attnamesize=strlen(attname)+1;

  itdatas->attname=calloc(_attnamesize,sizeof(char));
  strcpy(itdatas->attname,attname);

  itdatas->attsize = attsize;
  itdatas->attval=calloc(attsize+1,sizeof(char));
  itdatas->attvalprec=calloc(attsize+1,sizeof(char));

  return itdatas;

}
