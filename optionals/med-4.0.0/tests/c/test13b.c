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

/******************************************************************************
 * - Nom du fichier : test13.c
 *
 * - Description : lecture des equivalences d'un maillage MED.
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include <med_utils.h>


#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

const med_geometry_type * const typmai = MED_GET_CELL_GEOMETRY_TYPE+1;
const med_geometry_type * const typfac = MED_GET_FACE_GEOMETRY_TYPE+1;
const med_geometry_type * const typare = MED_GET_EDGE_GEOMETRY_TYPE+1;

const char * const *nommai = MED_GET_CELL_GEOMETRY_TYPENAME+1;
const char * const *nomfac = MED_GET_FACE_GEOMETRY_TYPENAME+1;
const char * const *nomare = MED_GET_EDGE_GEOMETRY_TYPENAME+1;

void lecture_equivalence_maillage(med_idt fid,const char * const nommaa,med_int nequ)
{
  med_int i,j,k;
  med_int ncor;
  med_int *cor;
  char    equ[MED_NAME_SIZE+1];
  char    des[MED_COMMENT_SIZE+1];
  med_err ret = 0;
  med_int nstep=0,nocstpncor=0;
  int     _cstpit=0;
  med_int _numdt,_numit;
  int structure = 0;



  if ( (nequ != 0) ) {
   fprintf(stdout,"\n(******************************)\n");
   fprintf(stdout,"(* EQUIVALENCES DU MAILLAGE : *)\n");
   fprintf(stdout,"(******************************)\n");
  }

  /* lecture de toutes les equivalences associes a nommaa */
  for (i = 0;i<nequ;i++) {

    /* lecture des infos sur l'equivalence */
    ret = MEDequivalenceInfo(fid,nommaa,i+1,equ,des,&nstep,&nocstpncor);
    EXIT_IF(ret < 0,"lors de la lecture des informations sur une equivalence",
	    NULL);

      fprintf(stdout,"- Equivalence numero : "IFORMAT" ",i+1);
      fprintf(stdout,"\n  - Nom de l'equivalence: %s \n",equ);
      fprintf(stdout,"\n  - Description de l'equivalence : %s \n",des);
      if (nstep > 1)
	fprintf(stdout,"\n  - L'equivalence est définie sur "IFORMAT" étapes de calcul\n",nstep);

    for (_cstpit=1; _cstpit <= nstep; ++_cstpit) {

      ret = MEDequivalenceComputingStepInfo   (fid, nommaa, equ, _cstpit,
					       & _numdt, &_numit,&nocstpncor);
      EXIT_IF(ret < 0,
	      "lors de la lecture des valeurs de séquence de calcul  d'une equivalence",
	      NULL);

      if ( (_numdt != MED_NO_DT) || (_numit != MED_NO_IT) )
	fprintf(stdout,"\n  - Séquence de calcul définie sur (numdt,numit) ("IFORMAT","IFORMAT") :\n",_numdt,_numit);

      /* lecture des correspondances sur les differents types d'entites */

      /* les noeuds */
      ret = MEDequivalenceCorrespondenceSize(fid,nommaa,equ,_numdt,_numit,MED_NODE,MED_NONE,&ncor);
      EXIT_IF(ret < 0,
	      "lors de la lecture du nombre de correspondances d'une equivalence",
	      NULL);
      fprintf(stdout,"\n  - Il y a "IFORMAT" correspondances sur les noeuds \n",ncor);

      if (ncor > 0) {

	/* allocation memoire */
	cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	EXIT_IF(cor == NULL,NULL,NULL);
	ret= MEDequivalenceCorrespondenceRd(fid,nommaa,equ,_numdt,_numit,
					    MED_NODE,MED_NONE,cor);
	EXIT_IF(ret < 0,"lors de la lecture du tableau des correspondances",
		NULL);
	if (!structure) {
	  for (j=0;j<ncor;j++)
	    fprintf(stdout,"\n  - Correspondance "IFORMAT" : "IFORMAT" et "IFORMAT" \n",j+1,*(cor+2*j),
		    *(cor+2*j+1));
	}
	free(cor);
      }

      /* sur les mailles : */
      for (j=0;j<MED_N_CELL_FIXED_GEO;j++) {

	ret = MEDequivalenceCorrespondenceSize(fid,nommaa,equ,_numdt,_numit,MED_CELL,typmai[j],&ncor);
	EXIT_IF(ret < 0,
		"lors de la lecture du nombre de correspondances dans une equivalence",
		NULL);
	fprintf(stdout,"\n  - Il y a "IFORMAT" correspondances sur les mailles %s \n",ncor,
		nommai[j]);

	if (ncor > 0) {

	  /* allocation memoire */
	  cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	  EXIT_IF(cor == NULL,NULL,NULL);
	  ret = MEDequivalenceCorrespondenceRd(fid,nommaa,equ,_numdt,_numit,
					       MED_CELL,typmai[j],cor);
	  EXIT_IF(ret < 0,"lors de la lecture du tableau des equivalences",
		  NULL);

	  if (!structure) {
	    for (k=0;k<ncor;k++)
	      fprintf(stdout,"\n  - Correspondance "IFORMAT" : "IFORMAT" et "IFORMAT" \n",k+1,
		      *(cor+2*k),*(cor+2*k+1));
	  }
	  free(cor);
	}
      }


      /* sur les faces */
      for (j=0;j<MED_N_FACE_FIXED_GEO;j++) {

	ret = MEDequivalenceCorrespondenceSize(fid,nommaa,equ,_numdt,_numit,
					       MED_DESCENDING_FACE,typfac[j],&ncor);

	EXIT_IF(ret < 0,
		"lors de la lecture du nombre de correspondances dans une equivalence",
		NULL);
	fprintf(stdout,"\n  - Il y a "IFORMAT" correspondances sur les faces %s\n",ncor,
	      nomfac[j]);

	if (ncor > 0) {

	  /* allocation memoire */
	  cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	  EXIT_IF(cor == NULL,NULL,NULL);
	  ret = MEDequivalenceCorrespondenceRd(fid,nommaa,equ,_numdt,_numit,
					       MED_DESCENDING_FACE,typfac[j],cor);
	  EXIT_IF(ret < 0,"lors de la lecture du tableau des equivalences",
		  NULL);

	  if (!structure) {
	    for (k=0;k<ncor;k++)
	      fprintf(stdout,"\n  - Correspondance "IFORMAT" : "IFORMAT" et "IFORMAT" \n",k+1,*(cor+2*k),
		      *(cor+2*k+1));
	  }
	  free(cor);
	}
      }


      /*  sur les aretes */
      for (j=0;j<MED_N_NODE_FIXED_GEO;j++) {

	ret = MEDequivalenceCorrespondenceSize(fid,nommaa,equ,_numdt,_numit,
					     MED_DESCENDING_EDGE,typare[j],&ncor);
	EXIT_IF(ret < 0,"lors de la lecture du nombre de correspondances",
		NULL);
	fprintf(stdout,"\n  - Il y a "IFORMAT" correspondances sur les aretes %s \n",
		ncor,nomare[j]);

	if (ncor > 0) {

	  /* allocation memoire */
	  cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	  EXIT_IF(cor == NULL,NULL,NULL);
	  ret =MEDequivalenceCorrespondenceRd(fid,nommaa,equ,_numdt,_numit,
					      MED_DESCENDING_EDGE,typare[j],cor);
	  EXIT_IF(ret < 0,"lors de la lecture du tableau des equivalences",
		  NULL);

	  if (!structure) {
	    for (k=0;k<ncor;k++)
	      fprintf(stdout,"\n  Correspondance "IFORMAT" : "IFORMAT" et "IFORMAT" \n",k+1,*(cor+2*k),
		      *(cor+2*k+1));
	  }

	  free(cor);
	}
      }
    }
  }

  return;
}


int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid = 0;
  char    maa[MED_NAME_SIZE+1]="";
  med_int mdim=0,sdim=0;
  med_int nequ=0,ncor=0,nstep=0,nocstpncor=0;
  med_int *cor;
  char equ[MED_NAME_SIZE+1]   ="";
  char des[MED_COMMENT_SIZE+1]="";

  int i,j,k;
  med_mesh_type type;
  med_sorting_type sort;
  char desc[MED_COMMENT_SIZE+1];
  char dtunit[MED_SNAME_SIZE+1]="";
  char nomcoo[3*MED_SNAME_SIZE+1];
  char unicoo[3*MED_SNAME_SIZE+1];
  med_axis_type rep;

  med_geometry_type *typmai = MED_GET_CELL_GEOMETRY_TYPE;
  med_geometry_type *typfac = MED_GET_FACE_GEOMETRY_TYPE;
  med_geometry_type *typare = MED_GET_EDGE_GEOMETRY_TYPE;

  if (argc != 2) {
    MESSAGE("Il faut passer un fichier MED en paramètre");
    return -1;
  }

  /* Ouverture du fichier passe en argument en lecture seule */
  if ((fid = MEDfileOpen(argv[1],MED_ACC_RDONLY)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier : "); SSCRUTE(argv[1]);
    return -1;
  }

  if ((sdim=MEDmeshnAxis(fid, 1)) <0) {
    MESSAGE("Erreur a la lecture de la dimension de l'espace du maillage :");
    SSCRUTE(maa);
    return -1;
  }

  /* Lecture des infos concernant le premier maillage */
  if ( MEDmeshInfo( fid, 1,  maa, &sdim, &mdim, &type, desc, dtunit, &sort,
		    &nstep,  &rep, nomcoo,unicoo) < 0 ) {
    MESSAGE("Erreur a la lecture des informations sur le maillage : ");SSCRUTE(maa);
    return -1;
  } else {
    printf("Maillage de nom : |%s| , de dimension : "IFORMAT" , et de type %d\n",maa,mdim,type);
    printf("\t -Dimension de l'espace : "IFORMAT"\n",sdim);
    printf("\t -Description du maillage : %s\n",desc);
    printf("\t -Noms des axes : |%s|\n",nomcoo);
    printf("\t -Unités des axes : |%s|\n",unicoo);
    printf("\t -Type de repère : %d\n",rep);
    printf("\t -Nombre d'étapes de calcul : "IFORMAT"\n",nstep);
    printf("\t -Unité des dates : |%s|\n",dtunit);
  }

  /* Lecture du nombre d'equivalence */
  if ((nequ = MEDnEquivalence(fid,maa)) < 0)
    {
    MESSAGE("Erreur a la lecture du nombre d'equivalence");
    return -1;
  }
  printf("Nombre d'equivalences : "IFORMAT" \n",nequ);

  /* Lecture de toutes les equivalences du maillage */
  lecture_equivalence_maillage(fid,maa,nequ);
    
  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier ");
    return -1;
  }

  return ret;
}




