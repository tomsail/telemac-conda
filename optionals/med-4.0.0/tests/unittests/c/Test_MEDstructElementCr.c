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
#define MESGERR 1
#include <med_utils.h>
#include <string.h>

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

int main (int argc, char **argv)

{
  med_err           _ret=0;
  med_idt           _fid=0;
  med_geometry_type _geotype=MED_NONE;

  const char        _elementname1[]="MED_BILLE";
  med_int           _elementdim1=3;
  const char        _supportmeshname1[]="MED_BILLE_SUPPORT";
  med_entity_type   _entitytype1=MED_NODE;
  med_int           _nnode1=1;
  med_int           _ncell1=0;
  med_int           _geocelltype1=MED_NONE;
/*   med_int           _nconstattribute1=0; */
/*   med_int           _nvariableattribute1=1; */

  const char        _elementname2[]="MED_PARTICULE";
  med_int           _elementdim2=3;
  const char        _supportmeshname2[]=MED_NO_MESHNAME;
  med_entity_type   _entitytype2=MED_NONE;
  med_int           _nnode2=1;
  med_int           _ncell2=0;
  med_int           _geocelltype2=MED_NONE;
/*   med_int           _nconstattribute2=0; */
/*   med_int           _nvariableattribute2=1; */

 /* Ouverture en mode creation du fichier test2.med */
  _fid = MEDfileOpen("current.med",MODE_ACCES);
  if (_fid < 0) {
    MESSAGE("Erreur a la creation du fichier current.med");
    return -1;
  }

  _geotype = MEDstructElementCr(_fid,
				_elementname1,
				_elementdim1,
				_supportmeshname1,
				_entitytype1,
				_geocelltype1
/* 				_nconstattribute1, */
/* 				_nvariableattribute1 */
				);

  ISCRUTE(_geotype);

  _geotype = MEDstructElementCr(_fid,
				_elementname2,
				_elementdim2,
				_supportmeshname2,
				_entitytype2,
				_geocelltype2
/* 				_nconstattribute2, */
/* 				_nvariableattribute2 */
				);

  ISCRUTE(_geotype);

  
  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  return _ret;


}

