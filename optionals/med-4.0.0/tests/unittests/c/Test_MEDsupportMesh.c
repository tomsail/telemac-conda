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

  int     _i          = 0;
  int     _j          = 0;
  med_idt _fid        = 0;
  med_int _meshdim    = 3;
  med_int _rmeshdim   = 0;
  med_int _rspacedim  = 0;
  med_int _n          = 0;
  med_int _rnnoe      = 0;
  med_int _rnse2      = 0;
  med_int _rnaxis     = 0;

  const med_int _nnoe = 5;

  /* table des coordonnees
     (dimension * nombre de noeuds) */
  med_float coo[5*3] = {0.0, 0.0, 0.0, 0.1 , 0.0, 0.0,
			1.0, 0.0, 0.0, 1.1 , 0.0, 0.0,
			2.0, 0.0, 0.0  };
  med_float * _rcoo=NULL;

  med_int nse2 = 4;
  med_int se2[10] = {1,2, 2,3, 3,4, 4,5};
  med_int * _rse2=NULL;

  const char   _supportmeshname[]="SUPPORT_POUTRE";
  char         _axisname[3*MED_SNAME_SIZE+1]="";
  char         _axisunit[3*MED_SNAME_SIZE+1]="";

  char           _rsupportmeshname[MED_NAME_SIZE+1]="";
  char           _raxisname[3*MED_SNAME_SIZE+1]="";
  char           _raxisunit[3*MED_SNAME_SIZE+1]="";
  char           _rdescription[MED_COMMENT_SIZE+1]="";
  med_axis_type  _raxistype;
  med_bool       _chgt=MED_FALSE,_trsf=MED_FALSE;

  strcat(_axisname,"x               ");
  strcat(_axisname,"y               ");
  strcat(_axisname,"z               ");
  strcat(_axisunit,"cm              ");
  strcat(_axisunit,"cm              ");
  strcat(_axisunit,"cm              ");

 /* ouverture du fichier */
  if ((_fid = MEDfileOpen("current.med",MODE_ACCES)) < 0) {
    MESSAGE("Impossible de creer le fichier current.med");
    return -1;
  }

  if (MEDsupportMeshCr( _fid, _supportmeshname, _meshdim, _meshdim,"Maillage support ED de type poutre",
			MED_CARTESIAN,_axisname, _axisname) < 0) {
    MESSAGE("Erreur a la creation du maillage support : "); SSCRUTE(_supportmeshname);
    return -1;
  }

  /* Ecriture des coordonnees des noeuds en mode MED_FULL_INTERLACE :
     (X1,Y1, X2,Y2, X3,Y3, ...) dans un repere cartesien */
  if (MEDmeshNodeCoordinateWr(_fid,_supportmeshname,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
			      MED_FULL_INTERLACE, _nnoe,  coo) < 0) {
    MESSAGE("Erreur a l'écriture des coordonnees des noeuds du maillage support");
    return -1;
  }


  /* ecriture des connectivites des segments */
  if ( MEDmeshElementConnectivityWr(_fid,_supportmeshname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
                                    MED_CELL, MED_SEG2, MED_NODAL,
				    MED_FULL_INTERLACE, nse2, se2) < 0 ) {
    MESSAGE("Impossible d'ecrire la connectivité des segments du maillage support : ");
    return -1;
  }

  if (MEDsupportMeshCr( _fid, "MED_BILLE_SUPPORT", _meshdim, _meshdim,"Maillage support ED de type bille",
			MED_CARTESIAN,_axisname, _axisname) < 0) {
    MESSAGE("Erreur a la creation du maillage support : "); SSCRUTE(_supportmeshname);
    return -1;
  }

  /* Ecriture des coordonnees des noeuds en mode MED_FULL_INTERLACE :
     (X1,Y1, X2,Y2, X3,Y3, ...) dans un repere cartesien */
  if (MEDmeshNodeCoordinateWr(_fid,"MED_BILLE_SUPPORT",MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
			      MED_FULL_INTERLACE, 1,  coo) < 0) {
    MESSAGE("Erreur a l'écriture des coordonnees des noeuds du maillage support : MED_BILLE_SUPPORT.");
    return -1;
  }


  if ( (_n =MEDnSupportMesh(_fid)) < 0 ) {
    MESSAGE("Erreur à la lecture du nombre de maillages supports : ");
    return -1;
  }

  for (_i=1; _i <= _n; ++_i) {

    if ( (_rnaxis = MEDsupportMeshnAxis(_fid, _i)) < 0) {
      MESSAGE("Erreur à la lecture du nombre d'axe du repère d'un maillage support : "); 
      return -1;
    }
    printf("\t -Dimension de l'espace : %d\n",_rnaxis);

    if ( MEDsupportMeshInfo(_fid, _i ,
			    _rsupportmeshname, &_rspacedim, &_rmeshdim, _rdescription,
			    &_raxistype, _raxisname, _raxisunit) < 0 ) {
      MESSAGE("Erreur à la lecture des informations sur le  maillage support : "); SSCRUTE(_supportmeshname);
      return -1;
    }

    printf("Maillage support de nom : |%s| , de dimension : %d.\n",_rsupportmeshname, _rmeshdim);
    printf("\t -Dimension de l'espace : %d\n",_rspacedim);
    printf("\t -Description du maillage : %s\n",_rdescription);
    printf("\t -Noms des axes : %s\n",_raxisname);
    printf("\t -Unités des axes : %s\n",_raxisunit);

    /* Combien de noeuds a lire ? */
    _rnnoe = MEDmeshnEntity(_fid,_rsupportmeshname,MED_NO_DT,MED_NO_IT,
			    MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,
			    &_chgt,&_trsf);
    if (_rnnoe < 0) {
      MESSAGE("Erreur a la lecture du nombre de noeuds. ");
      return -1;
    } else
      printf("Nombre de noeuds du maillage support: "IFORMAT" \n",_rnnoe);

    /* Allocations memoires */
    if (_rnnoe > 0) {
      /* table des coordonnees (dimension * nombre de noeuds ) */
      _rcoo = (med_float*) calloc(_rnnoe*_rspacedim,sizeof(med_float));

      if ( MEDmeshNodeCoordinateRd(_fid, _rsupportmeshname, MED_NO_DT, MED_NO_IT,MED_FULL_INTERLACE, _rcoo) < 0 ) {
	MESSAGE("Erreur a la lecture des coordonnees des noeuds");
	return -1;
      } else {
	printf("Valeur de _rcoo : ");
	for (_j=0;_j<_rnnoe*_rspacedim;_j++)  printf("%4.2f ",_rcoo[_j]);
	printf("\n");
      }

      free(_rcoo);
    }

    /* Combien de segments à lire ? */
    _rnse2 = MEDmeshnEntity(_fid,_rsupportmeshname,MED_NO_DT,MED_NO_IT,
			    MED_CELL,MED_SEG2,MED_CONNECTIVITY,MED_NODAL,
			    &_chgt,&_trsf);

    if (_rnse2 < 0) {
      MESSAGE("Erreur a la lecture du nombre de segments.");
      return -1;
    } else
      printf("Nombre de segments du maillage support: "IFORMAT" \n",_rnse2);

    if (_rnse2 > 0) {
      /* table des connectivités (_rnse2 * nombre de noeuds ds un MED_SE2) */
      _rse2 = (med_int*) calloc(_rnse2*2,sizeof(med_int));

      if ( MEDmeshElementConnectivityRd(_fid, _rsupportmeshname, MED_NO_DT, MED_NO_IT,
				       MED_CELL,MED_SEG2,MED_NODAL,MED_FULL_INTERLACE, _rse2) < 0 ) {
	MESSAGE("Erreur a la lecture des connectivités des segments");
	return -1;
      } else {
	printf("Valeur de _rse2 : ");
	for (_j=0;_j<_rnse2*2;_j++)  printf("%d ",_rse2[_j]);
	printf("\n");
      }

      free(_rse2);
    }

  }

  return 0;
}
