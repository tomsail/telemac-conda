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

  /*Exemple 1 :

  - Elément de référence de type géométrique MED_TRIA3
  - Point X(X1,X2) quelconque dans le plan de l'élément de référence
  - Fonctions de base : P1(X)=1-X1-X2 ; P2(X)=X1; P3(X)=X2;
    (issu du choix de la base polynomiale (1,X1,X2)
     et des trois noeuds de la maille de référence pour
     construire l'interpolation)
  */
  const char         _interpname1[]   ="interpname1";
  med_geometry_type  _geotype1        =MED_TRIA3;
  med_bool           _cellnodes1      =MED_TRUE;
/*   med_int            _nbasisfunc1  =3; */
  med_int            _nvariable1   =2;
  med_int            _maxdegree1      =1;
  med_int            _nmaxcoefficient1=3;

 /* Ouverture en mode creation du fichier "current.med" */
  _fid = MEDfileOpen("current.med",MODE_ACCES);
  if (_fid < 0) {
    MESSAGE("Erreur a la creation du fichier current.med");
    return -1;
  }

  if ( (_ret = MEDinterpCr(_fid,
			   _interpname1,
			   _geotype1,
			   _cellnodes1,
			   _nvariable1,
			   _maxdegree1,
			   _nmaxcoefficient1
			   ) <0) ) {
    MESSAGE("Erreur à la création de la fonction d'interpolation n°1");
  }


  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }


  return _ret;

}

