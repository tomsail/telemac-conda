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

  const char                _varattname1_1[MED_NAME_SIZE+1]="MED_VFOO_ATR1_1";
  const med_attribute_type  _varatttype1_1=MED_ATT_INT;
  const med_int             _ncomponent1_1=1;

  const char                _varattname1_2[MED_NAME_SIZE+1]="MED_VFOO_ATR1_2";
  const med_attribute_type  _varatttype1_2=MED_ATT_FLOAT64;
  const med_int             _ncomponent1_2=2;

  const char                _varattname1_3[MED_NAME_SIZE+1]="MED_VFOO_ATR1_3";
  const med_attribute_type  _varatttype1_3=MED_ATT_NAME;
  const med_int             _ncomponent1_3=1;


  /* Ouverture en mode lecture du fichier Test_MEDstructuElement.med */
  _fid = MEDfileOpen("current.med",MODE_ACCES);
  if (_fid < 0) {
    MESSAGE("Erreur à la lecture du fichier current.med");
    return -1;
  }

  if ( (_ret = MEDstructElementVarAttCr(_fid,
					_elementname1,
					_varattname1_1,
					_varatttype1_1,
					_ncomponent1_1)) < 0 ) {
    return _ret;
  }

  if ( (_ret = MEDstructElementVarAttCr(_fid,
					_elementname1,
					_varattname1_2,
					_varatttype1_2,
					_ncomponent1_2)) < 0 ) {
    return _ret;
  }

  if ( (_ret = MEDstructElementVarAttCr(_fid,
					_elementname1,
					_varattname1_3,
					_varatttype1_3,
					_ncomponent1_3)) < 0 ) {
    return _ret;
  }

  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  return _ret;

}

