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

  char    _fieldname1[MED_NAME_SIZE+1]  = "champ reel";
  char    _interpname[MED_NAME_SIZE+1]  = "";
  med_int _ninterp  = 0;
  int     _interpit=0;


  /* Ouverture en mode creation du fichier "current.med" */
  _fid = MEDfileOpen("current.med",MED_ACC_RDONLY);
  if (_fid < 0) {
    MESSAGE("Erreur a la creation du fichier current.med");
    return -1;
  }

  if ( (_ninterp =  MEDfieldnInterp(_fid,
				    _fieldname1
				    )  ) < 0 ) {
    MESSAGE("Erreur à la lecture du nombre de fonctions d'interpolation  sur le champ : ");
    SSCRUTE(_fieldname1);_ret=_ninterp; goto ERROR;
  }

  for (_interpit=0;_interpit < _ninterp; ++_interpit ) {
    if ( (_ret =	MEDfieldInterpInfo(_fid,
					   _fieldname1,
					   _interpit+1,
					   _interpname ) <0) ) {
    MESSAGE("Erreur à la lecture des informations de la fonction d'interpolation n° :");
    ISCRUTE(_interpit);SSCRUTE("sur le champ : ");SSCRUTE(_fieldname1);
    goto ERROR;
    }
    else
      fprintf(stdout,"Le nom de la fonction d'interpolation n°%d du champ %s est %s\n",
	      _interpit+1,_fieldname1,_interpname);

  }

 ERROR:
  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  return _ret;

}

