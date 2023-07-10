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

  char _meshname1[MED_NAME_SIZE+1]   = "meshname1";
  char _axisname[3*MED_SNAME_SIZE+1] = "x               y               z               ";
  char _unitname[3*MED_SNAME_SIZE+1] = "cm              cm              cm              ";

  char _fieldname1[MED_NAME_SIZE+1]  = "champ reel";
  char _componentname1[2*MED_SNAME_SIZE+1] = "comp1           comp2           ";
                                   /*12345678901234561234567890123456*/
  char _unitname1[2*MED_SNAME_SIZE+1] = "unit1           unit2           ";
  char _dtunit[MED_SNAME_SIZE+1] = "s";
  med_int _ncomponentname1  = 2;


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


  /* Ouverture en mode creation du fichier "current.med" */
  _fid = MEDfileOpen("current.med",MODE_ACCES);
  if (_fid < 0) {
    MESSAGE("Erreur a la creation du fichier current.med");
    return -1;
  }

  /* Creation de _meshname1 de dimension 2 dans un espace de dimension 3*/
  if (MEDmeshCr( _fid, _meshname1, 3, 2, MED_UNSTRUCTURED_MESH,
		 "Maillage vide","s", MED_SORT_DTIT,
		 MED_CARTESIAN, _axisname, _unitname) < 0) {
    MESSAGE("Erreur a la creation du maillage : "); SSCRUTE(_meshname1);
    goto ERROR;
  }


 /* Creation du champ réel n°1 */
  if ( MEDfieldCr(_fid,_fieldname1,MED_FLOAT64,
		  _ncomponentname1,_componentname1,_unitname1,_dtunit,_meshname1 ) < 0) {
    MESSAGE("Erreur à la création du champ : ");SSCRUTE(_fieldname1);
    goto ERROR;
  };


  if ( (_ret =	MEDfieldInterpWr(_fid,
				 _fieldname1,
				 _interpname1) <0) ) {
    MESSAGE("Erreur à l'écriture de la fonction d'interpolation n°1 sur le champ : ");SSCRUTE(_fieldname1);
  }


 ERROR:
  if (MEDfileClose(_fid) < 0) {
    MESSAGE("ERROR : file closing");
    return -1;
  }


  return _ret;

}

