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

med_int 
MEDnGauss(med_idt fid)
{
  int n=0;
  med_idt datagroup=0;

  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;

  
  if ( (datagroup = _MEDdatagroupOuvrir(fid,MED_GAUSS)) < 0) 
    return 0;
  else
    if (datagroup) if ( _MEDdatagroupFermer(datagroup) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      ISCRUTE_int(datagroup); return -1; 
    }
  
  if ( _MEDnObjets(fid,MED_GAUSS,&n) < 0 ) return -1;
  
  return (med_int) n;
}
