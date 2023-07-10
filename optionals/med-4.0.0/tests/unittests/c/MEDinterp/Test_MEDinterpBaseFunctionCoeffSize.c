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
  int               _basisfuncit                =0;
  int               _powerit                    =0;
  med_int           _ncoefficient            =0;
  med_int*          _power                      =NULL;
  med_float*        _coefficient                =NULL;
  int               _coefficientit              =0;


 /* Ouverture en mode creation du fichier "current.med" */
  _fid = MEDfileOpen("current.med",MED_ACC_RDONLY);
  if (_fid < 0) {
    MESSAGE("Erreur a la creation du fichier current.med");
    return -1;
  }

  if ( (_ninterp = MEDnInterp(_fid)) <0) {_ret=_ninterp;goto ERROR;}

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
      MESSAGE("Erreur à lecture d'informations de la fonction d'interpolation n°");ISCRUTE(_interpit);
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



    for ( _basisfuncit=1; _basisfuncit<= _nbasisfunc; ++_basisfuncit) {

      if ( (_ncoefficient = MEDinterpBaseFunctionCoefSize( _fid,
							       _interpname,
							       _basisfuncit) ) <0 ) {
	MESSAGE("Erreur à la lecture du nombre de coefficients de la fonction de base n°");ISCRUTE(_basisfuncit);
	goto ERROR;
      }

      _coefficient = (med_float*) calloc(sizeof(med_float),_ncoefficient);
      _power       = (med_int*)   calloc(sizeof(med_int),_nvariable*_ncoefficient);

      if ( (_ret = MEDinterpBaseFunctionRd( _fid,
					    _interpname,
					    _basisfuncit,
					    &_ncoefficient,
					    _power,
					    _coefficient
					    ) <0) ) {
	MESSAGE("Erreur à la lecture de la fonction de base n°");ISCRUTE(_basisfuncit);
	free(_coefficient);free(_power);goto ERROR;
      } else {
	fprintf(stdout,"\n\t Tableau de coefficients de la fonctions de base n° %d :\n\t",_basisfuncit);
	for ( _coefficientit=0; _coefficientit< _ncoefficient; ++_coefficientit)
	  fprintf(stdout," %4f ",_coefficient[_coefficientit]);

	fprintf(stdout,"\n\t Tableau de puissances de la fonctions de base n° %d :\n\t",_basisfuncit);
	for ( _powerit=0; _powerit< _nvariable*_ncoefficient; ++_powerit)
	  fprintf(stdout," %4d ",_power[_powerit]);
      }
      free(_coefficient);
      free(_power);
    }
    fprintf(stdout,"\n");

  }


  _ret=0;
 ERROR:
  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }


  return _ret;

}

