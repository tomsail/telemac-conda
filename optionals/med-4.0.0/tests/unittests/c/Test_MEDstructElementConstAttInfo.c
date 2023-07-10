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
  int               _i  =0;
  int               _j  =0;
  med_int           _nstructelement=0;

  med_geometry_type _geotype=MED_NONE;

  char              _elementname[MED_NAME_SIZE+1]="";
  med_int           _elementdim=0;
  char              _supportmeshname[MED_NAME_SIZE+1]="";
  med_entity_type   _entitytype=MED_UNDEF_ENTITY_TYPE;
  med_int           _nnode=0;
  med_int           _ncell=0;
  med_geometry_type _geocelltype=MED_NONE;
  med_int           _nconstantattribute=0;
  med_bool          _anyprofile=0;
  med_int           _nvariableattribute=0;

  char               _constattname[MED_NAME_SIZE+1]="";
  med_attribute_type _constatttype=MED_ATT_UNDEF;
  med_int            _ncomponent=0;
  med_entity_type    _attentitytype=MED_UNDEF_ENTITY_TYPE;
  char               _profilename[MED_NAME_SIZE+1]="";
  med_int            _profilesize=0;


  /* Ouverture en mode lecture du fichier Test_MEDstructuElement.med */
  _fid = MEDfileOpen("current.med",MED_ACC_RDONLY);
  if (_fid < 0) {
    MESSAGE("Erreur à la lecture du fichier current.med");
    return -1;
  }

  if ( (_nstructelement = MEDnStructElement(_fid)) <0) _ret=_nstructelement;

  for ( _i=1; _i<= _nstructelement; ++_i) {

    if (
	MEDstructElementInfo(_fid,
			     _i,
			     _elementname,
			     &_geotype,
			     &_elementdim,
			     _supportmeshname,
			     &_entitytype,
			     &_nnode,
			     &_ncell,
			     &_geocelltype,
			     &_nconstantattribute,
			     &_anyprofile,
			     &_nvariableattribute
			     )
    ) return -1;

    fprintf(stdout,"Elément de structure n° %d |%s| de type géométrique n° %d et de dimension %d\n",
	    _i,_elementname,_geotype,_elementdim);
    if ( strlen(_supportmeshname) ) {
      fprintf(stdout,"\t Maillage support de nom |%s|",_supportmeshname);
      if (_ncell)
	fprintf(stdout," avec %d maille(s) de type %d et ",_ncell,_geocelltype);
      if (_nnode)
	fprintf(stdout," avec %d noeud(s)\n",_nnode);
      else {
	fprintf(stderr,"\n Erreur : les noeuds doivent être définis s'il existe un maillage support\n");
      }
    } else
      fprintf(stdout,"\t Maillage support implicite sur noeud\n");
      fprintf(stdout,"\t Nombre d'attribut(s) constant(s) : %d",_nconstantattribute);
      if (_anyprofile) fprintf(stdout,", avec profil.\n"); else fprintf(stdout,", sans profil.\n");
      if ( _nconstantattribute ) {
	for (_j=1;_j<=_nconstantattribute;++_j) {
	  if ( MEDstructElementConstAttInfo(_fid,
					    _elementname,
					    _j,
					    _constattname,
					    &_constatttype,
					    &_ncomponent,
					    &_attentitytype,
					    _profilename,
					    &_profilesize
					    ) < 0
	       ) return -1;
	  fprintf(stdout,"\t\t Attribut constant de nom |%s| de type %d à %d composantes\n",
		  _constattname,_constatttype,_ncomponent);
	  fprintf(stdout,"\t\t  Cet Attribut est attaché au type d'entité  %d avec un profil |%s| de taille %d\n",
		  _attentitytype,_profilename,_profilesize);

	}
      }
      fprintf(stdout,"\t Nombre d'attributs variables : %d\n",_nvariableattribute);

  }

  
  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  return _ret;


}

