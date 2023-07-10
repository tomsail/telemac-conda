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
  int      _i=0;
  med_err  _ret=-1;
  med_idt  _fid=0;
  med_int  _ngeotype=0;

  const char         _meshname[MED_NAME_SIZE+1]="maa1";
  med_bool           _chgt=MED_FALSE,_trsf=MED_FALSE;
  /*TODO : Traduire   MED_TAILLE_NOM_ENTITE */
  char               _geotypename[MED_NAME_SIZE+1]="";
  med_geometry_type  _geotype=MED_NO_GEOTYPE;
 /* Ouverture en mode creation du fichier med */
  _fid = MEDfileOpen("Test_MEDmeshStructElementVarAttWr.med",MED_ACC_RDONLY);
  if (_fid < 0) {
    MESSAGE("Erreur a la creation du fichier current.med");
    return -1;
  }

  /* TODO : Créer un itérateur sur les types d'entités*/
  if ( (_ngeotype = MEDmeshnEntity(_fid,_meshname,MED_NO_DT,MED_NO_IT,
				   MED_STRUCT_ELEMENT,MED_GEO_ALL,MED_CONNECTIVITY,MED_GLOBAL_STMODE,
				   &_chgt,&_trsf) ) < 0 ) {
    MESSAGE ("Erreur à la lecture du nombre de type géométrique pour le type d'entités MED_STRUCT_ELEMENT");
    goto ERROR;
  }

  ISCRUTE(_ngeotype);
  for (_i=0; _i < _ngeotype; _i++) {

    if ( MEDmeshEntityInfo(_fid,
			   _meshname,
			   MED_NO_DT,MED_NO_IT,
			   MED_STRUCT_ELEMENT,
			   _i+1,
			   _geotypename,&_geotype
			   ) <0 ){
      MESSAGE ("Erreur à la demande d'informations pour le type d'entités MED_STRUCT_ELEMENT");
      ISCRUTE_int(_i);
      goto ERROR;
    }
    SSCRUTE(_geotypename);
    ISCRUTE(_geotype);
  }

  _ret = 0;
 ERROR:
  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  return _ret;

}

