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
  med_int           _nstructelement=0;

 /* Ouverture en mode creation du fichier med */
  _fid = MEDfileOpen("current.med",MED_ACC_RDONLY);
  if (_fid < 0) {
    MESSAGE("Erreur a la creation du fichier current.med");
    return -1;
  }

  if ( (_nstructelement = MEDnStructElement(_fid)) <0) _ret=_nstructelement;

  ISCRUTE(_nstructelement);

  return _ret;

}

