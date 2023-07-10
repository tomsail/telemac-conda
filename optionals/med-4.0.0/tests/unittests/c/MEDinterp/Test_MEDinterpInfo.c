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
  med_err           _ret=-1;
  med_idt           _fid=0;

  med_int           _ninterp=0;
  int               _interpit                   =0;
  char              _interpname[MED_NAME_SIZE+1]="";
  med_geometry_type _geotype                    =MED_NONE;
  med_bool          _cellnodes                  =MED_FALSE;
  med_int           _nbasisfunc              =0;
  med_int           _nvariable               =0;
  med_int           _maxdegree                  =0;
  med_int           _nmaxcoefficient            =0;


 /* Ouverture en mode creation du fichier "current.med" */
  _fid = MEDfileOpen("current.med",MED_ACC_RDONLY);
  if (_fid < 0) {
    MESSAGE("Erreur a la creation du fichier current.med");
    return -1;
  }

  if ( (_ninterp = MEDnInterp(_fid)) <0) _ret=_ninterp;

  ISCRUTE(_ninterp);

  for ( _interpit=1; _interpit<= _ninterp; ++_interpit) {

    if ( (_ret = MEDinterpInfo(_fid,
			       _interpit,
			       _interpname,
			       &_geotype,
			       &_cellnodes,
			       &_nbasisfunc,
			       &_nvariable,
			       &_maxdegree,
			       &_nmaxcoefficient
			       ) <0) ) {
      MESSAGE("Erreur à la création de la fonction d'interpolation n°");ISCRUTE(_interpit);
      goto ERROR;
    }

    fprintf(stdout,"Fonction d'interpolation n° %d |%s| sur le type géométrique n° %d\n",
	    _interpit,_interpname, _geotype);

    if ( _cellnodes )
      if ( _nbasisfunc == (_geotype % 100) )
	fprintf(stdout,"\t Les noeuds de construction sont les noeuds de la maille de référence.\n");
      else {
	MESSAGE("Erreur : le nombre de noeuds de construction "\
		"est différent du nombre de noeuds de la maille de référence.\n");
	ISCRUTE(_nbasisfunc); ISCRUTE(_geotype % 100);
      }

    if ( _nvariable != (_geotype / 100) ) {
	MESSAGE("Erreur : le nombre de variables  "\
		"est différent de la dimension de l'espace de la maille de référence.\n");
	ISCRUTE(_nvariable); ISCRUTE (_geotype / 100);
    } else
      fprintf(stdout,"\t Il y a  %d fonctions de base avec %d variables\n ",_nbasisfunc,_nvariable);
    fprintf(stdout,"\t Le degré maximum des fonctions de base est %d et possèdent au maximum %d coefficients\n"
	    ,_maxdegree,_nmaxcoefficient);

  }

  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }

  _ret=0;
 ERROR:

  return _ret;

}

