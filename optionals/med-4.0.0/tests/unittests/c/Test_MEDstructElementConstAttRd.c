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
  int               _i  =0,_j=0,_k=0,_l=0,_n=0;
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

  unsigned char *    _value=NULL;
  med_int            _allocsize=0;

  /* Ouverture en mode lecture du fichier Test_MEDstructuElement.med */
  _fid = MEDfileOpen("current.med",MED_ACC_RDONLY);
  if (_fid < 0) {
    MESSAGE("Erreur à la lecture du fichier current.med");
    return -1;
  }

  if ( (_nstructelement = MEDnStructElement(_fid)) <0) _ret=_nstructelement;

  for ( _i=1; _i<= _nstructelement; ++_i) {

    if (MEDstructElementInfo(_fid,
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
			     ) ) return -1;

    fprintf(stdout,"Elément de structure n° %d |%s| de type géométrique n° %d et de dimension %"IFORMAT"\n",
	    _i,_elementname,_geotype,_elementdim);

    if ( strlen(_supportmeshname) ) {
                   fprintf(stdout,"\t Maillage support de nom |%s|",_supportmeshname);
      if (_ncell)  fprintf(stdout," avec %d maille(s) de type %d et ",_ncell,_geocelltype);
      if (_nnode)  fprintf(stdout," avec %d noeud(s)\n",_nnode);
      else {
	           fprintf(stderr,"\n Erreur : les noeuds doivent être définis s'il existe un maillage support\n");
      }
    } else
                   fprintf(stdout,"\t Maillage support implicite sur noeud\n");

    fprintf(stdout,"\t Nombre d'attribut(s) constant(s) : %d",_nconstantattribute);
    if (_anyprofile) fprintf(stdout,", avec profil.\n"); else fprintf(stdout,", sans profil.\n");
    if ( _nconstantattribute ) {
      for (_j=1;_j<=_nconstantattribute;++_j) {
	if ( MEDstructElementConstAttInfo(_fid, _elementname,_j,
					  _constattname, &_constatttype, &_ncomponent,
					  &_attentitytype, _profilename, &_profilesize ) ) return -1;

	fprintf(stdout,"\t\t Attribut constant de nom |%s| de type %d à "IFORMAT" composantes\n",
		_constattname,_constatttype,_ncomponent);
	fprintf(stdout,"\t\t  Cet Attribut est attaché au type d'entité  %d avec un profil |%s| de taille "IFORMAT"\n",
		_attentitytype,_profilename,_profilesize);

	/*Il serait pratique que profilesize renvoie les valeurs suivantes (avec un _profilename=MED_NO_PROFILE) :*/
	if (!_profilesize)
	  if (_attentitytype == MED_NODE) _profilesize = _nnode; else _profilesize=_ncell;
	_n     = _ncomponent*_profilesize;

	_allocsize =_n*MEDstructElementAttSizeof(_constatttype);
	if ( _constatttype == MED_ATT_NAME) ++_allocsize;
	_value = (unsigned char *) malloc(_allocsize);

	/* ISCRUTE(_ncomponent);ISCRUTE(_profilesize);ISCRUTE(MEDstructElementAttSizeof(_constatttype)); */
	if ( MEDstructElementConstAttRd(_fid, _elementname,_constattname, _value ) < 0 ) return -1;

	fprintf(stdout,"\t\t  Cet Attribut a pour valeurs : \n");
	for (_k=0; _k < _n; ++_k) {
	  switch (_constatttype) {
	  case  MED_ATT_FLOAT64 :    printf("%f ", ((med_float*)(_value))[ _k]) ;
	    break;
	    
	  case MED_ATT_INT : 	     printf("%d ",((med_int*)(_value))[_k]);
	    break;
	    
	  case MED_ATT_NAME :	    for (_l=_k*MED_NAME_SIZE; _l < (_k+1)*MED_NAME_SIZE; ++_l) 
	                             printf("%c",((char *)_value)[_l]);printf("\n");
	    break;
	  default:
	    break;
	  }
	}
	printf("\n");
	free(_value);
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

