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

  char            _elementname1[MED_NAME_SIZE+1]="MED_PARTICULE";
  med_int         _elementdim1=3;
  char            _supportmeshname1[MED_NAME_SIZE+1]="";
  med_entity_type _entitytype1=MED_UNDEF_ENTITY_TYPE;
  med_int         _nnode1=0;
  med_int         _ncell1=0;
  med_int         _geocelltype1=MED_NONE;
  med_int         _nconstantatribute1=0;
  med_bool        _anyprofile1=0;
  med_int         _nvariableattribute1=0;


  /* Ouverture en mode lecture du fichier Test_MEDstructuElement.med */
  _fid = MEDfileOpen("current.med",MED_ACC_RDONLY);
  if (_fid < 0) {
    MESSAGE("Erreur à la lecture du fichier current.med");
    return -1;
  }

  if (
    MEDstructElementInfoByName(_fid,
			       _elementname1,
			       &_geotype,
			       &_elementdim1,
			       _supportmeshname1,
			       &_entitytype1,
			       &_nnode1,
			       &_ncell1,
			       &_geocelltype1,
			       &_nconstantatribute1,
			       &_anyprofile1,
			       &_nvariableattribute1
			       )
    ) return -1;

  fprintf(stdout,"Elément de structure |%s| de type géométrique n° %d et de dimension %d\n",_elementname1,_geotype,_elementdim1);
  if ( strlen(_supportmeshname1) ) {
    fprintf(stdout,"\t Maillage support de nom |%s|",_supportmeshname1);
    if (_ncell1)
      fprintf(stdout," avec %d mailles de type %d et ",_ncell1,_geocelltype1);
    if (_nnode1)
      fprintf(stdout," avec %d noeuds\n",_nnode1);
    else {
      fprintf(stderr,"\n Erreur : les noeuds doivent être définis s'il existe un maillage support\n");
    }
  } else
    fprintf(stdout," avec support implicite sur noeud\n");


  
  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  return _ret;


}

