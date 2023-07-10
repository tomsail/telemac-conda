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

  const char        _elementname1[]="MED_BILLE";
/*   med_int           _elementdim1=3; */
/*   const char        _supportmeshname1[]="MED_BILLE_SUPPORT"; */
/*   med_entity_type   _entitytype1=MED_NODE; */
/*   med_int           _nnode1=1; */
/*   med_int           _ncell1=0; */
/*   med_int           _geocelltype1=MED_NONE; */

  const char                _varattname1_1[MED_NAME_SIZE+1]="MED_VFOO_ATR1_1";
 /*  const med_attribute_type  _varatttype1_1=MED_ATT_INT; */
/*   const med_int             _ncomponent1_1=1; */
  const med_int             _varatrvalue1_1[3]={ 22 , 24, 25 };

/*   const char                _varattname1_2[MED_NAME_SIZE+1]="MED_VFOO_ATR1_2"; */
/*   const med_attribute_type  _varatttype1_2=MED_ATT_FLOAT64; */
/*   const med_int             _ncomponent1_2=2; */

  const char                _varattname1_3[MED_NAME_SIZE+1]="MED_VFOO_ATR1_3";
  const med_attribute_type  _varatttype1_3=MED_ATT_NAME;
  const med_int             _ncomponent1_3=1;
  /*Ce qui suit est une seule chaine */
  const char                _varatrvalue1_3[3*MED_NAME_SIZE+1]=
    { "*--------------------------------1-----------------------------*"
      "*--------------------------------2-----------------------------*"
      "*--------------------------------3-----------------------------*"
    };

  const char   _meshname[]="maa1";
  char         _axisname[3*MED_SNAME_SIZE+1]="";
  char         _axisunit[3*MED_SNAME_SIZE+1]="";

  strcat(_axisname,"x               ");
  strcat(_axisname,"y               ");
  strcat(_axisname,"z               ");
  strcat(_axisunit,"cm              ");
  strcat(_axisunit,"cm              ");
  strcat(_axisunit,"cm              ");

  const med_int _nentity = 3;
  const med_int _con[3]={ 2, 4 ,5}; /* (3 billes dont les centres sont les noeuds 2, 4 et 5 du maillage maa1 )*/


  /* Ouverture en mode lecture du fichier Test_MEDstructuElement.med */
  _fid = MEDfileOpen("current.med",MODE_ACCES);
  if (_fid < 0) {
    MESSAGE("Erreur à l'ouverture du fichier current.med");
    return -1;
  }

 /* Creation du maillage "maa1" de type MED_UNSTRUCTURED_MESH
     et de dimension 3 */
  if (MEDmeshCr(_fid,_meshname,3,3,MED_UNSTRUCTURED_MESH, "un premier maillage","s",MED_SORT_DTIT,
		MED_CARTESIAN,_axisname,_axisunit) < 0) {
    MESSAGE("Erreur a la creation du maillage maa1");
    return -1;
  }

  _geotype = MEDstructElementGeotype(_fid,_elementname1);

  /* ecriture des connectivites des segments */
  if ( MEDmeshElementConnectivityWr(_fid,_meshname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
				    MED_STRUCT_ELEMENT, _geotype, MED_NODAL,
                                    MED_FULL_INTERLACE, _nentity, _con) < 0 ) {
    MESSAGE("Impossible d'ecrire la connectivité des billes: ");
    return -1;
  }

  if (MEDmeshStructElementVarAttWr(_fid,
				   _meshname,
				   MED_NO_DT,
				   MED_NO_IT,
				   _geotype,
				   _varattname1_1,
				   _nentity,
				   _varatrvalue1_1
				   ) < 0 ) {
    return -1;
  }

  if (MEDmeshStructElementVarAttWr(_fid,
				   _meshname,
				   MED_NO_DT,
				   MED_NO_IT,
				   _geotype,
				   _varattname1_3,
				   _nentity,
				   _varatrvalue1_3
				   ) < 0 ) {
    return -1;
  }

  if ( MEDmeshComputationStepCr(_fid,_meshname,MED_NO_DT,MED_NO_IT,
				1,MED_NO_IT,0.5) < 0) {
    fprintf(stderr,"Erreur a la creation du pas de temps du maillage %s\n",_meshname);
    return -1;
  }

  return _ret;

}

