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
 * - Nom du fichier : mdump.c
 *
 * - Description : utilitaire de dump pour fichier MED
 *                 Ce fichier contient les fonctions suivantes
 *                 qui constituent des modeles de programmation
 *                 pour une lecture generique d'un fichier MED :
 *                 - lecture_maillage_non_structure () :
 *                        1. Noeuds.
 *                        2. Mailles.
 *                        3. Faces (connectivite descendante).
 *                        4. Aretes (connectivite descendante).
 *                        5. Familles.
 *                        6. Equivalences.
 *                        7. Joints.
 *                 - lecture_maillage_structure ()     :
 *                        1. Noeuds.
 *                        2. Mailles.
 *                        3. Familles.
 *                        4. Equivalences.
 *                        5. Joints.
 *                 - lecture_resultats () :
 *                        1. Champs de resultats relatifs à un maillage.
 *                           - Entites :
 *                                - Noeuds
 *                                - Mailles
 *                                - Faces
 *                                - Aretes
 *                           - Gestion des pas de temps et numeros d'ordre :
 *                                  valeurs associees a un ou plusieurs maillages sous
 *                                  un meme pas de temps.
 *                           - Gestion des profils.
 *                           - Gestion des liens vers des maillages distants
 *                           - Gestion des points de Gauss :
 *                                - localisation des points de Gauss.
 *                 - lecture_parametres_scalaires () :
 *                           - Valeurs scalaires entieres ou flottantes.
 *                           - Gestion des pas de temps et numeros d'ordre.
 *                 - main() : infos generales + lecture de tous les champs et
 *                            du fichier MED passe en parametre.
 *
 *****************************************************************************/

#ifndef MESGERR
#define MESGERR 1
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <med.h>
#include <med_config.h>
#include <med_utils.h>
#include <med_misc.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef __cplusplus
}
#endif

#ifdef PPRO_NT
#define F_OK 0
#define snprintf _snprintf
#else
#include <unistd.h>
#endif


extern med_entity_type MED23MESH_GET_ENTITY_TYPE[MED_N_ENTITY_TYPES+2];
extern const char * const MED23MESH_GET_ENTITY_TYPENAME[MED_N_ENTITY_TYPES+2];
extern med_geometry_type MED23MESH_GET_CELL_GEOMETRY_TYPE[MED_N_CELL_FIXED_GEO+2];
extern const char * const MED23MESH_GET_CELL_GEOMETRY_TYPENAME[MED_N_CELL_FIXED_GEO+2];
extern med_geometry_type MED23MESH_GET_FACE_GEOMETRY_TYPE[MED_N_FACE_FIXED_GEO+2];
extern const char * const MED23MESH_GET_FACE_GEOMETRY_TYPENAME[MED_N_FACE_FIXED_GEO+2];
extern med_geometry_type MED23MESH_GET_EDGE_GEOMETRY_TYPE[MED_N_EDGE_FIXED_GEO+2];
extern const char * MED23MESH_GET_EDGE_GEOMETRY_TYPENAME[MED_N_EDGE_FIXED_GEO+2];
extern med_geometry_type MED23MESH_GET_NODE_GEOMETRY_TYPE[MED_N_NODE_FIXED_GEO+2];
extern const char * MED23MESH_GET_NODE_GEOMETRY_TYPENAME[MED_N_NODE_FIXED_GEO+2];

extern med_entity_type MED23FIELD_GET_ENTITY_TYPE[MED_N_ENTITY_TYPES+2];
extern const char * const MED23FIELD_GET_ENTITY_TYPENAME[MED_N_ENTITY_TYPES+2];
extern med_geometry_type MED23FIELD_GET_CELL_GEOMETRY_TYPE[MED_N_CELL_FIXED_GEO+2];
extern const char * const MED23FIELD_GET_CELL_GEOMETRY_TYPENAME[MED_N_CELL_FIXED_GEO+2];
extern med_geometry_type MED23FIELD_GET_FACE_GEOMETRY_TYPE[MED_N_FACE_FIXED_GEO+2];
extern const char * const MED23FIELD_GET_FACE_GEOMETRY_TYPENAME[MED_N_FACE_FIXED_GEO+2];
extern med_geometry_type MED23FIELD_GET_EDGE_GEOMETRY_TYPE[MED_N_EDGE_FIXED_GEO+2];
extern const char * MED23FIELD_GET_EDGE_GEOMETRY_TYPENAME[MED_N_EDGE_FIXED_GEO+2];
extern med_geometry_type MED23FIELD_GET_NODE_GEOMETRY_TYPE[MED_N_NODE_FIXED_GEO+2];
extern const char * MED23FIELD_GET_NODE_GEOMETRY_TYPENAME[MED_N_NODE_FIXED_GEO+2];

/* Stockage des fids des fichiers ouverts dans le cas où le montage des fichiers
   distants est demandé (variable mountage) 
*/
#define MDUMP_MAX_FILE_OPEN 200
#define MDUMP_MAX_FILE_OPEN_INIT  {INIT2X(INIT10X(INIT10X(0))) }

typedef struct {
  int    n;
  med_idt array[MDUMP_MAX_FILE_OPEN];
} FIDS_t;
FIDS_t FIDS ={1, MDUMP_MAX_FILE_OPEN_INIT};

/* Indique si on vérifie seulement la structure
   Lecture complète du fichier mais pas d'affichage des données volumineuses
*/
int structure = 0;

/* types geometriques des mailles references dans le modele MED */
const med_geometry_type * const typmai = MED23MESH_GET_CELL_GEOMETRY_TYPE+1;
const med_geometry_type * const typfac = MED23MESH_GET_FACE_GEOMETRY_TYPE+1;
const med_geometry_type * const typare = MED23MESH_GET_EDGE_GEOMETRY_TYPE+1;

const char * const *nommai = MED23MESH_GET_CELL_GEOMETRY_TYPENAME+1;
const char * const *nomfac = MED23MESH_GET_FACE_GEOMETRY_TYPENAME+1;
const char * const *nomare = MED23MESH_GET_EDGE_GEOMETRY_TYPENAME+1;

#define MED_LECTURE_MAILLAGE_SUPPORT_UNIQUEMENT 2
#define MED_LECTURE_ENTETE_SEULEMENT 1

#define USER_MODE MED_COMPACT_STMODE

#define xstr(s) str(s)
#define str(s) #s

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

#define MAXBANNERLEN 255

/*Afficheurs pour la fonction polymorphique : MEDstructPrintFunction*/
void affd(const void *pva)
{
  const double *pa = (const double *) pva;
	printf(" %f ",*pa);
}

void affi(const void *pva)
{
	const med_int *pa = (const med_int *) pva;
/* La ligne suivante ne fonctionne pas à cause du % contenu dans IFORMAT*/
/* 	printf(" %9"IFORMAT" " ,*pa); */
 	printf(" "IFORMAT" " ,*pa);
}

void affs(const void *pva)
{
  const char *pa = (const char *) pva;
        printf(" %.*s ",MED_NAME_SIZE,pa);
}

typedef void (*_myfuncptr)(const void*);

_myfuncptr MEDstructPrintFunction(med_attribute_type atttype) {
  switch (atttype) {
  case MED_ATT_INT :
    return affi;
  break;
  case MED_ATT_FLOAT64:
    return affd;
  break;
  case MED_ATT_NAME:
    return affs;
  break;
  default:
    EXIT_IF(-1,"lors de la lecture du type d'attribut à afficher.",NULL);
    return NULL;

  }
  return NULL;

}

med_int lecture_nombre_famille(med_idt fid,const char * const nommaa)
{
  med_int nfam = MEDnFamily(fid,nommaa);
  EXIT_IF(nfam < 0,"lors de la lecture du nombre de familles",NULL);
  fprintf(stdout,"- Nombre de familles : "IFORMAT" \n",nfam);

  return nfam;
}

void lecture_famille_maillage(med_idt fid,const char * const nommaa,med_int nfam)
{
  med_int i,j;
  med_int natt,ngro;
  char *attdes=NULL,*gro=NULL;
  med_int *attval=NULL,*attide=NULL;
  char nomfam[MED_NAME_SIZE+1];
  med_int numfam;
  char str1[MED_COMMENT_SIZE+1];
  char str2[MED_LNAME_SIZE+1];
  med_err ret = 0;
  int famille_0 = 0;

  if (nfam) {
   fprintf(stdout,"\n(**************************)\n");
   fprintf(stdout,"(* FAMILLES DU MAILLAGE : *)\n");
   fprintf(stdout,"(**************************)\n");
  }

  for (i=0;i<nfam;i++) {

    /* nombre de groupes */
    ngro = MEDnFamilyGroup(fid,nommaa,i+1);
    EXIT_IF(ngro < 0,"lors de la lecture du nombre de groupe d'une famille",
	    NULL);

    /* nombre d'attributs */
    natt = MEDnFamily23Attribute(fid,nommaa,i+1);
    EXIT_IF(natt < 0,"lors de la lecture du nombre d'attributs d'une famille",
	    NULL);

    fprintf(stdout,"- Famille "IFORMAT" a "IFORMAT" attributs et "IFORMAT" groupes \n",i+1,natt,
	    ngro);

    /* nom,numero,attributs,groupes */

    /* allocation memoire */
    attide = (med_int*) malloc(sizeof(med_int)*natt);
    EXIT_IF(attide == NULL,NULL,NULL);
    attval = (med_int*) malloc(sizeof(med_int)*natt);
    EXIT_IF(attval == NULL,NULL,NULL);
    attdes = (char *) malloc(MED_COMMENT_SIZE*natt+1);
    EXIT_IF(attdes == NULL,NULL,NULL);
    gro = (char*) malloc(MED_LNAME_SIZE*ngro+1);
    EXIT_IF(gro == NULL,NULL,NULL);
    ret = MEDfamily23Info(fid,nommaa,i+1,nomfam,attide,attval,
			  attdes,&numfam,gro);
    EXIT_IF(ret < 0,"lors de la lecture des informations d'une famille",
	    NULL);
	if (numfam == 0)
	  famille_0 = 1;

    if (!structure) {
    /* affichage des resultats */
    fprintf(stdout,"  - Famille de nom %s et de numero "IFORMAT" : \n",nomfam,numfam);
    fprintf(stdout,"  - Attributs : \n");
    for (j=0;j<natt;j++) {
      strncpy(str1,attdes+j*MED_COMMENT_SIZE,MED_COMMENT_SIZE);
      str1[MED_COMMENT_SIZE] = '\0';
      fprintf(stdout,"   ide = "IFORMAT" - val = "IFORMAT" - des = %s\n",*(attide+j),
	      *(attval+j),str1);
    }
    }

    /* on libere la memoire */
    if (attide) {free(attide);attide=NULL;}
    if (attval) {free(attval);attval=NULL;}
    if (attdes) {free(attdes);attdes=NULL;}

    if (!structure) {
    fprintf(stdout,"  - Groupes :\n");
    for (j=0;j<ngro;j++) {
      strncpy(str2,gro+j*MED_LNAME_SIZE,MED_LNAME_SIZE);
      str2[MED_LNAME_SIZE] = '\0';
      fprintf(stdout,"   gro = %s\n",str2);
    }
    }

    /* on libere la memoire */
    if (gro) {free(gro);gro=NULL;}
  }

  if (famille_0 != 1) {
    MESSAGE("Erreur : La famille FAMILLE_ZERO n'a pas été trouvée, elle est obligatoire. ");
  }

  return;
}

med_int lecture_nombre_equivalence(med_idt fid,const char * const nommaa)
{
  med_int nequ = MEDnEquivalence(fid,nommaa);
  EXIT_IF(nequ < 0,"lors de la lecture du nombre d'equivalences",NULL);
  fprintf(stdout,"- Nombre d'equivalences : "IFORMAT" \n",nequ);

  return nequ;
}

/* nombre de mailles concernees par les equivalences */
void lecture_equivalence_maillage(med_idt fid,const char * const nommaa,med_int nequ)
{
  med_int i,j,k;
  med_int ncor;
  med_int *cor;
  char equ[MED_NAME_SIZE+1];
  char des[MED_COMMENT_SIZE+1];
  med_err ret = 0;
  med_int nstep=0,nocstpncor=0;
  int     _cstpit=0;
  med_int _numdt,_numit;


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

/*     if (!structure) { */
      fprintf(stdout,"- Equivalence numero : "IFORMAT" ",i+1);
      fprintf(stdout,"\n  - Nom de l'equivalence: %s \n",equ);
      fprintf(stdout,"\n  - Description de l'equivalence : %s \n",des);
      if (nstep > 1)
	fprintf(stdout,"\n  - L'equivalence est définie sur "IFORMAT" étapes de calcul\n",nstep);
/*     } */

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


med_int lecture_nombre_joint(med_idt fid,const char * const nommaa)
{
  med_int njnt = MEDnSubdomainJoint(fid,nommaa);
  EXIT_IF(njnt < 0,"lors de la lecture du nombre de joints",NULL);
  fprintf(stdout,"- Nombre de joints : "IFORMAT" \n",njnt);

  return njnt;
}


void lecture_joint_maillage(med_idt fid,const char * const nommaa,med_int njnt)
{
  med_int i,k;
  char des[MED_COMMENT_SIZE+1];
  med_int ndom,nent;
  med_entity_type  typ_ent_local,typ_ent_distant;
  med_int          typ_geo_local,typ_geo_distant;
/*   med_int geo_ent_local,geo_ent_distant; */

  char jn                  [MED_NAME_SIZE+1]="";
  char maa_dist            [MED_NAME_SIZE+1]="";
  char nom_geo_ent_local   [MED_NAME_SIZE+1]="";
  char nom_geo_ent_distant [MED_NAME_SIZE+1]="";
  med_int *cortab;

  med_err ret = 0;
  med_int njstep=0,ncor=0,nodtitncor=0;
  int corit=0,csit=0;
  med_int _numdt,_numit;

 if ( (njnt != 0)  ) {
   fprintf(stdout,"\n(******************************)\n");
   fprintf(stdout,"(* JOINTS DU MAILLAGE       : *)\n");
   fprintf(stdout,"(******************************)\n");
 }

  /* lecture de touts les joints associes a nommaa */
  for (i = 0;i<njnt;i++) {
    fprintf(stdout,"- Joint numero : "IFORMAT" ",i+1);

    /* lecture des infos sur le joint */
    ret = MEDsubdomainJointInfo(fid,nommaa,i+1,jn,des,&ndom,maa_dist,&njstep,&nodtitncor);
    EXIT_IF(ret < 0,"lors de la lecture des informations sur un joint",
	    NULL);

    fprintf(stdout,"\n  - Nom du joint: %s \n",jn);
    fprintf(stdout,"\n  - Description du joint      : %s ",des);
    fprintf(stdout,"\n  - Domaine en regard         : "IFORMAT" ",ndom);
    fprintf(stdout,"\n  - Maillage distant          : %s ",maa_dist);
    if (njstep > 1 ) {
      printf("Nombre d'étapes de calcul : "IFORMAT" \n",njstep);
      printf("Nombre de correspondance pour (NO_DT,NO_IT) : "IFORMAT" \n",nodtitncor);
    }

    for (csit=1; csit <= njstep; ++csit) {

      ret = MEDsubdomainComputingStepInfo( fid, nommaa, jn, csit, &_numdt, &_numit, &ncor);
      EXIT_IF(ret < 0,"Erreur a la lecture des valeurs (numdt,numit) dans les joints",
	    NULL);
      if ( (_numdt != MED_NO_DT) || (_numit != MED_NO_IT) ) {
	printf("Séquence de calcul (numdt,numit) : ("IFORMAT","IFORMAT")\n",_numdt,_numit);
      }
      corit=1;
      while ( corit <= ncor ) {

	ret = MEDsubdomainCorrespondenceSizeInfo(fid,nommaa,jn,_numdt,_numit,corit,
						 (med_entity_type *) &typ_ent_local, (med_geometry_type *) &typ_geo_local,
						 (med_entity_type *) &typ_ent_distant,(med_geometry_type *)&typ_geo_distant,
						 &nent);
	EXIT_IF(ret < 0,"Erreur a la lecture des infos sur le nombre d'entite en regard",
		NULL);
	if (nent > 0) {
	  if (typ_ent_local == MED_NODE) strcpy(nom_geo_ent_local,"MED_NOEUD");
	  else  ret = _MEDgetInternalGeometryTypeName(fid,nom_geo_ent_local,typ_geo_local);
	  EXIT_IF(ret < 0,"Erreur à l'appel de _MEDgetInternalGeometryTypeName", NULL);
	  if (typ_ent_distant == MED_NODE) strcpy(nom_geo_ent_distant,"MED_NOEUD");
	  else ret = _MEDgetInternalGeometryTypeName(fid,nom_geo_ent_distant,typ_geo_distant);
	  EXIT_IF(ret < 0,"Erreur à l'appel de _MEDgetInternalGeometryTypeName", NULL);
	  fprintf(stdout,"\n\t\t- nb de couples d'entites en regard (local,distant)=(%s,%s) : "IFORMAT" \n",
		  nom_geo_ent_local,nom_geo_ent_distant,  nent);
	  /*TODO : Supprimer la ligne suivante*/
/* 	  fprintf(stdout,"  %d \n",nent); */
	  cortab = (med_int*) malloc(sizeof(med_int)*nent*2);
	  if ( (ret=MEDsubdomainCorrespondenceRd(fid,nommaa,jn,_numdt,_numit,
						 typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,
						 cortab)) < 0) {
	    fprintf(stdout,"\n\t\t- Erreur a la lecture des correspondances sur (%s,%s,%s,%s)",
		    MED23MESH_GET_ENTITY_TYPENAME[typ_ent_local+1],nom_geo_ent_local,
		    MED23MESH_GET_ENTITY_TYPENAME[typ_ent_distant+1],nom_geo_ent_distant);
	  } else {
	    if (!structure) {
	      for (k=0;k<nent;k++)
		fprintf(stdout,"\n\t\t- Correspondance "IFORMAT" : "IFORMAT" et "IFORMAT" ",k+1,
			*(cortab+2*k),*(cortab+2*k+1));
	    }
	  }
	  free(cortab);
	}

	corit++;
      }
    }
  }

  return;
}


med_int lecture_nombre_noeuds_maillage_non_structure(const med_idt fid,
						     const char *  nommaa,
						     const med_int numdt,
						     const med_int numit)
{


  med_bool chgt=MED_FALSE,trsf=MED_FALSE;

  med_int nnoe = MEDmeshnEntity(fid,nommaa,numdt,numit,
				MED_NODE,MED_NO_GEOTYPE,
				MED_COORDINATE,MED_NODAL,&chgt,&trsf);
  EXIT_IF(nnoe < 0,"lors de la lecture du nombre de noeuds",NULL);
  fprintf(stdout,"- Nombre de noeuds : "IFORMAT" \n",nnoe);

  return nnoe;
}


void lecture_noeuds_maillage_non_structure(const med_idt fid,
					   const char * const nommaa,
					   const med_int numdt,
					   const med_int numit,
					   const med_int mdim,
					   const med_int edim,
					   const med_int nnoe,
					   const med_switch_mode mode_coo,
					   const char * const nomcoo,
					   const char * const unicoo,
					   const med_axis_type *const rep)
{
  med_float *coo;
  char *nomnoe;
  med_int *numnoe;
  med_int *nufano;
  med_bool inonoe,inunoe,ifano;
  med_err ret = 0;
  med_int i;
  char str[MED_SNAME_SIZE+1];


  /* Allocations memoires */
  /* table des coordonnees
     profil : (dimension * nombre de noeuds ) */
  coo = (med_float*) malloc(sizeof(med_float)*nnoe*edim);
  EXIT_IF(coo == NULL,NULL,NULL);
  /* table  des numeros, des numeros de familles des noeuds
     profil : (nombre de noeuds) */
  numnoe = (med_int*) malloc(sizeof(med_int)*nnoe);
  EXIT_IF(numnoe == NULL,NULL,NULL);
  nufano = (med_int*) malloc(sizeof(med_int)*nnoe);
  EXIT_IF(nufano == NULL,NULL,NULL);
  /* table des noms des noeuds
     profil : (nnoe*MED_SNAME_SIZE+1) */
  nomnoe = (char*) malloc(MED_SNAME_SIZE*nnoe+1);
  EXIT_IF(nomnoe == NULL,NULL,NULL);

  /* lecture des noeuds :
     - coordonnees
     - noms (optionnel dans un fichier MED)
     - numeros (optionnel dans un fichier MED)
     - numeros des familles */
  ret = MEDmeshNodeRd(fid,nommaa,numdt,numit, mode_coo, coo,
		      &inonoe,nomnoe,&inunoe,numnoe,&ifano,nufano);


  EXIT_IF(ret < 0,"lors de la lecture des noeuds du maillage \n",NULL);

  /* affichage des resultats */
  if (nnoe) {
   fprintf(stdout,"\n(************************)\n");
   fprintf(stdout,"(* NOEUDS DU MAILLAGE : *)\n");
   fprintf(stdout,"(************************)\n\n");
  }
  if (!structure) {
    fprintf(stdout,"- Type de repere des coordonnees : %d \n",*rep);
    fprintf(stdout,"- Nom des coordonnees : \n");
    for (i=0;i<edim;i++) {
      strncpy(str,nomcoo+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
      str[MED_SNAME_SIZE] = '\0';
      fprintf(stdout," %s ",str);
    }
    fprintf(stdout,"\n- Unites des coordonnees : \n");
    for (i=0;i<edim;i++) {
      strncpy(str,unicoo+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
      str[MED_SNAME_SIZE] = '\0';
      fprintf(stdout," %s ",str);
    }
    fprintf(stdout,"\n- Coordonnees des noeuds : ");
    for (i=0;i<nnoe*edim;i++) {
      if (mode_coo == MED_FULL_INTERLACE && !(i % edim))
         fprintf(stdout,"\n [ %5"MED_IFORMAT" ] : ", (i/edim + 1) );
      if (mode_coo == MED_NO_INTERLACE && ! (i % nnoe))
	fprintf(stdout,"\n\n ");
      fprintf(stdout," %-+9.6f ",*(coo+i));
    }

  if (inonoe) {
    fprintf(stdout,"\n- Noms des noeuds : \n");
    for (i=0;i<nnoe;i++) {
      strncpy(str,nomnoe+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
      str[MED_SNAME_SIZE] = '\0';
      fprintf(stdout," %s ",str);
    }
  }
  if (inunoe) {
    fprintf(stdout,"\n- Numeros des noeuds : \n");
    for (i=0;i<nnoe;i++)
      fprintf(stdout," "IFORMAT" ",*(numnoe+i));
  }

  fprintf(stdout,"\n- Numeros des familles des noeuds : \n");
  for (i=0;i<nnoe;i++) {
    if (ifano)
      fprintf(stdout," "IFORMAT" ",*(nufano+i));
    else
      fprintf(stdout," %d ",0);
  }
  fprintf(stdout,"\n");
  }


  /* liberation memoire */
  free(coo);
  free(nomnoe);
  free(numnoe);
  free(nufano);

  return;
}


med_int lecture_nombre_mailles_standards(const med_idt fid,
					 const char * const nommaa,
					 const med_int numdt,
					 const med_int numit,
					 const med_geometry_type typ_geo,
					 const med_connectivity_mode typ_con,
					 const int indice)
{

  med_bool chgt=MED_FALSE,trsf=MED_FALSE;

  med_int nmailles = MEDmeshnEntity(fid,nommaa,numdt,numit,
				    MED_CELL,typ_geo,
				    MED_CONNECTIVITY,typ_con,&chgt,&trsf);
  EXIT_IF(nmailles < 0," lors de la lecture du nombre de mailles",NULL);

  if ( (indice < (MED_N_CELL_GEO_FIXED_CON-5) ) ||
       (indice >= (MED_N_CELL_GEO_FIXED_CON-5) && (nmailles > 0) ) )
    if (nmailles)
        fprintf (stdout,"- Nombre de mailles de type %s : "IFORMAT" \n",nommai[indice],
		 nmailles);

  return nmailles;
}

med_int lecture_nombre_et_type_mailles_elstruct(const med_idt fid,
						const char * const nommaa,
						const med_int numdt,
						const med_int numit,
						const int indice,
						med_geometry_type* geotype,
						char*              geotypename
						)
{
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;
  med_err _ret=-1;
  med_int _nmailles=0;

  _ret = MEDmeshEntityInfo(fid,nommaa,numdt,numit, MED_STRUCT_ELEMENT,
			   indice+1,geotypename,geotype );
  EXIT_IF(_ret<0,
	  "Erreur à la demande d'informations pour le type d'entités MED_STRUCT_ELEMENT",NULL);

  _nmailles = MEDmeshnEntity(fid,nommaa,numdt,numit,
			     MED_STRUCT_ELEMENT,*geotype,
			     MED_CONNECTIVITY,MED_NODAL,&chgt,&trsf);

  EXIT_IF(_nmailles < 0," lors de la lecture du nombre de mailles",NULL);

  if (_nmailles)
    fprintf (stdout,"- Nombre de mailles de type %s : "IFORMAT" \n",geotypename, _nmailles);

  return _nmailles;
}

void lecture_mailles_elstruct(const med_idt fid,
			      const char * const nommaa,
			      const med_int numdt,
			      const med_int numit,
			      const med_int nmodels,
			      const med_geometry_type* const geotype,
			      const char*              const geotypename,
			      const med_int *          const nmailles,
			      const med_switch_mode mode_coo)
{
  med_err _ret=-1;
  med_int  taille=0;
  char     str[MED_SNAME_SIZE+1];

  med_int  *connectivite;
  char     *nomele;
  med_int  *numele;
  med_int  *nufael;
  med_bool  inoele=MED_FALSE, inuele=MED_FALSE, inufael=MED_FALSE;

  med_geometry_type _geotype=MED_NONE;
  med_int           _elementdim=0;
  char              _supportmeshname[MED_NAME_SIZE+1]="";
  med_entity_type   _entitytype=MED_UNDEF_ENTITY_TYPE;
  med_int           _nnode=0;
  med_int           _ncell=0;
  med_geometry_type _geocelltype=MED_NONE;
  med_int           _nconstantatribute=0;
  med_bool          _anyprofile=MED_FALSE;
  med_int           _nvariableattribute=0;

  char               _attname[MED_NAME_SIZE+1]="";
  med_attribute_type _atttype;
  med_int            _atttypesize=0;
  med_int            _attvaluesize=0;
  med_int            _nattcomp=0;
  void              *_attvalue=NULL;
  void             (*_printf)(const void*);
  int i=0,j=0,k=0;
  med_int dispbanner=MED_FALSE;

  for (i=0; i<nmodels; i++ ) {

    _ret = MEDstructElementInfoByName(fid, &geotypename[i*(MED_NAME_SIZE+1)],
				      &_geotype,&_elementdim,
				      _supportmeshname,&_entitytype,&_nnode,&_ncell,
				      &_geocelltype,&_nconstantatribute,&_anyprofile,&_nvariableattribute);

    if (_ncell > 0 )
      taille=_ncell*_geocelltype%100;
    else
      taille = _nnode;

/*     SSCRUTE(&geotypename[i*(MED_NAME_SIZE+1)]); */
/*     ISCRUTE(_nnode); */
/*     ISCRUTE(_ncell); */
/*     ISCRUTE(taille); */

    /* allocation memoire */
    connectivite = (med_int*) calloc(taille*nmailles[i],sizeof(med_int));
    EXIT_IF(connectivite == NULL,NULL,NULL);
    nomele = (char*) malloc(sizeof(char)*MED_SNAME_SIZE*nmailles[i]+1);
    EXIT_IF(nomele == NULL,NULL,NULL);
    numele = (med_int*) malloc(sizeof(med_int)*nmailles[i]);
    EXIT_IF(numele == NULL,NULL,NULL);
    nufael = (med_int*) malloc(sizeof(med_int)*nmailles[i]);
    EXIT_IF(nufael == NULL,NULL,NULL);

    /* lecture des données */
    _ret = MEDmeshElementRd( fid,nommaa,numdt,numit,MED_STRUCT_ELEMENT,geotype[i],
			    MED_NODAL, mode_coo, connectivite,
			    &inoele,nomele,&inuele,numele,&inufael,nufael );

    EXIT_IF(_ret < 0,"lors de la lecture des mailles",NULL);


    if ( !dispbanner) {
     fprintf(stdout,"\n(***************************************)\n");
     fprintf(stdout,  "(* ELEMENTS DE STRUCTURE DU MAILLAGE : *)\n");
     fprintf(stdout,  "(***************************************)\n");
     dispbanner=MED_TRUE;
    }
    if (!structure) {
      /* affichage des resultats */
      fprintf(stdout,"\n- Mailles de type %s : ", &geotypename[i*(MED_NAME_SIZE+1)]);
      if (strcmp(&geotypename[i*(MED_NAME_SIZE+1)],"MED_PARTICLE") ) {
	fprintf(stdout,"\n  - Connectivité : ");
	for (j=0;j<nmailles[i]*taille;j++) {
	       if (mode_coo == MED_FULL_INTERLACE && !(j % taille))
		 fprintf(stdout,"\n [ %5"MED_IFORMAT" ] : ", (j/taille +1) );
	       if (mode_coo == MED_NO_INTERLACE && !(j % nmailles[i]))
		 fprintf(stdout,"\n");
	       fprintf(stdout," %9"MED_IFORMAT" ",*(connectivite+j));
	}
      }

      if (inoele) {
	fprintf(stdout,"\n  - Noms : \n");
	for (j=0;j<nmailles[i];j++) {
	  strncpy(str,nomele+j*MED_SNAME_SIZE,MED_SNAME_SIZE);
	  str[MED_SNAME_SIZE] = '\0';
	  fprintf(stdout," %s ",str);
	}
      }
      if (inuele) {
	fprintf(stdout,"\n  - Numeros :\n");
	for (j=0;j<nmailles[i];j++)
	  fprintf(stdout," "IFORMAT" ",*(numele+j));
      }
      fprintf(stdout,"\n  - Numéros de familles : \n");
      for (j=0;j<nmailles[i];j++)
	if (inufael)
	  fprintf(stdout," "IFORMAT" ",*(nufael+j));
	else
	  fprintf(stdout," %d ",0);
      fprintf(stdout,"\n");
    }


    /* Read variable attribute(s) */
    for (k=0; k<_nvariableattribute; k++) {

      /* read informations about the attribute */
      _ret = MEDstructElementVarAttInfo(fid, &geotypename[i*(MED_NAME_SIZE+1)], k+1,
					_attname, &_atttype, &_nattcomp);
      EXIT_IF(_ret < 0,"lors de la lecture des caractéristiques de attributs variables",NULL);


      /* Memory allocation */
      EXIT_IF(_atttype == MED_ATT_UNDEF,"à la lecture du type (valeur : MED_ATT_UNDEF) de l'attribut variable ",_attname);
      _atttypesize = MEDstructElementAttSizeof(_atttype);

     _attvaluesize = nmailles[i]*_nattcomp*_atttypesize;
      if ( _atttype == MED_ATT_NAME) ++_attvaluesize;
      _attvalue = (void *) malloc( _attvaluesize*sizeof(char));
      --_attvaluesize;

      /* read attribute values */
      _ret =MEDmeshStructElementVarAttRd(fid, nommaa, numdt, numit,
					 *(geotype+i), _attname, _attvalue );
      if (_ret < 0 ) free(_attvalue);
      EXIT_IF(_ret < 0,"lors de la lecture des attributs variables",NULL);

      _printf=MEDstructPrintFunction(_atttype);

      if (!structure) {
	fprintf(stdout,"\n  - Valeurs de l'attribut |%s| pour le type géométrique |%s| : \n",_attname,
		&geotypename[i*(MED_NAME_SIZE+1)]);
        for (j=0;j<nmailles[i]*_nattcomp;j++) {
	  if ( ( _nattcomp > 1 ) &&  !(j % _nattcomp) )
	    fprintf(stdout,"\n [ %5"MED_IFORMAT" ] : ", (j/_nattcomp +1) );
	  _printf( (void *)( (char *)(_attvalue) + j*_atttypesize) );
        }
      }
      /* free memory */
      free(_attvalue);

    }

    /* liberation memoire */
    free(connectivite);
    free(nomele);
    free(numele);
    free(nufael);
    }

   return;
}

void lecture_mailles_standards(const med_idt fid,
			       const char *nommaa,
			       const med_int numdt,
			       const med_int numit,
			       const med_int mdim,
			       const med_int * const nmailles,
			       const med_switch_mode mode_coo,
			       const med_connectivity_mode typ_con)
{
  med_int  taille;
  med_int *connectivite;
  char    *nomele;
  med_int *numele;
  med_int *nufael;
  med_bool       inoele=MED_FALSE, inuele=MED_FALSE, inufael=MED_FALSE;
  med_int entdim;
  med_int nnodes;
  med_int nndes;
  med_int i,j;
  med_err ret = 0;
  char str[MED_SNAME_SIZE+1];
  med_int dispbanner=MED_FALSE;

  /* Lecture des connectivites, noms, numeros des mailles */
  for (i=0;i<MED_N_CELL_GEO_FIXED_CON;i++)
    if (nmailles[i] > 0) {

      ret=_MEDgetGeometricParameter(MED_CELL, typmai[i],&entdim,&nnodes,&nndes);
      EXIT_IF(ret < 0,"lors de la lecture des caractéristiques des mailles",NULL);

      switch(typ_con) {
      case MED_NODAL :
	taille = nnodes;
	break;

      case MED_DESCENDING :
	taille = nndes;
	break;

      default :
	ret = -1;
      }

      /* allocation memoire */
      connectivite = (med_int*) malloc(sizeof(med_int)*taille*nmailles[i]);
      EXIT_IF(connectivite == NULL,NULL,NULL);
      nomele = (char*) malloc(sizeof(char)*MED_SNAME_SIZE*nmailles[i]+1);
      EXIT_IF(nomele == NULL,NULL,NULL);
      numele = (med_int*) malloc(sizeof(med_int)*nmailles[i]);
      EXIT_IF(numele == NULL,NULL,NULL);
      nufael = (med_int*) malloc(sizeof(med_int)*nmailles[i]);
      EXIT_IF(nufael == NULL,NULL,NULL);

      /* lecture des données */
      ret = MEDmeshElementRd( fid,nommaa,numdt,numit,MED_CELL,typmai[i],
			      typ_con, mode_coo, connectivite,
			      &inoele,nomele,&inuele,numele,&inufael,nufael );

      EXIT_IF(ret < 0,"lors de la lecture des mailles",NULL);

      if ( !dispbanner) {
       fprintf(stdout,"\n(**************************)\n");
       fprintf(stdout,"(* ELEMENTS DU MAILLAGE : *)\n");
       fprintf(stdout,"(**************************)\n");
       dispbanner=MED_TRUE;
      }
      if (!structure) {
        /* affichage des resultats */
        fprintf(stdout,"\n- Mailles de type %s : ", nommai[i]);
        fprintf(stdout,"\n  - Connectivité : ");
        for (j=0;j<nmailles[i]*taille;j++) {
  	    if (mode_coo == MED_FULL_INTERLACE && !(j % taille))
  	      fprintf(stdout,"\n [ %5"MED_IFORMAT" ] : ", (j/taille +1) );
  	    if (mode_coo == MED_NO_INTERLACE && !(j % nmailles[i]))
  	      fprintf(stdout,"\n");
  	    fprintf(stdout," %9"MED_IFORMAT" ",*(connectivite+j));
        }
  
        if (inoele) {
  	fprintf(stdout,"\n  - Noms : \n");
  	for (j=0;j<nmailles[i];j++) {
  	  strncpy(str,nomele+j*MED_SNAME_SIZE,MED_SNAME_SIZE);
  	  str[MED_SNAME_SIZE] = '\0';
  	  fprintf(stdout," %s ",str);
  	}
        }
        if (inuele) {
  	fprintf(stdout,"\n  - Numeros :\n");
  	for (j=0;j<nmailles[i];j++)
  	  fprintf(stdout," "IFORMAT" ",*(numele+j));
        }
        fprintf(stdout,"\n  - Numéros de familles : \n");
        for (j=0;j<nmailles[i];j++)
  	if (inufael)
  	  fprintf(stdout," "IFORMAT" ",*(nufael+j));
  	else
  	  fprintf(stdout," %d ",0);
        fprintf(stdout,"\n");
      }

      /* liberation memoire */
      free(connectivite);
      free(nomele);
      free(numele);
      free(nufael);
    }

  return;
}


med_int lecture_nombre_mailles_polygones(const med_idt fid,
					 const char * const nommaa,
					 const med_int numdt,
					 const med_int numit,
					 const med_geometry_type polytype,
					 const med_connectivity_mode typ_con)
{

  med_bool chgt=MED_FALSE,trsf=MED_FALSE;
  char polytypename[MED_NAME_SIZE+1]="Undefined GeoType";
  med_int nmpolygones;
  
  EXIT_IF( (( polytype != MED_POLYGON) &&
	    ( polytype != MED_POLYGON2) ),
	   MED_ERR_RANGE_MSG, MED_ERR_GEOMETRIC_MSG);


  nmpolygones = MEDmeshnEntity(fid,nommaa,numdt,numit,
                               MED_CELL,polytype,
                               MED_INDEX_NODE,typ_con,&chgt,&trsf);

  EXIT_IF(nmpolygones < 0,"lors de la lecture du nombre de mailles polygone\n",
	  NULL);
  if (nmpolygones > 0 ) nmpolygones--; else nmpolygones=0;
  if (nmpolygones) {
    MEDmeshGeotypeName(fid,polytype,polytypename);
    fprintf(stdout,"- Nombre de mailles de type %s : "IFORMAT" \n",
	    polytypename,nmpolygones);
  }
  polytypename[0]='\0';
  return nmpolygones;
}

void lecture_mailles_polygones(const med_idt fid,
			       const char * const nommaa,
			       const med_int numdt,
			       const med_int numit,
			       const med_geometry_type polytype,
			       const med_int nmpolygones,
			       const med_switch_mode mode_coo,
			       const med_connectivity_mode typ_con)
{
  med_int i,j;
  med_err ret = 0;
  med_int taille;
  med_int *connectivite;
  char *nomele;
  med_int *numele;
  med_int *nufael;
  med_int *indexp;
  int ind1,ind2;
  char tmp[MED_NAME_SIZE+1];
  med_err ret1,ret2,ret3;
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;
  char polytypename[MED_NAME_SIZE+1]="Undefined GeoType";

  EXIT_IF( (( polytype != MED_POLYGON) &&
	    ( polytype != MED_POLYGON2) ),
	   MED_ERR_RANGE_MSG, MED_ERR_GEOMETRIC_MSG);

  /* lecture des mailles de type MED_POLYGONE */

  /* quelle taille pour  le tableau des connectivites ? */
  taille=MEDmeshnEntity(fid,nommaa,numdt,numit,
			MED_CELL,polytype,MED_CONNECTIVITY,typ_con,
			&chgt,&trsf);
  EXIT_IF(taille < 0,"lors de la lecture des parametres des mailles polygones",
	  NULL);

  /* allocation memoire */
  indexp = (med_int *) malloc(sizeof(med_int)*(nmpolygones+1));
  EXIT_IF(indexp == NULL,NULL,NULL);
  connectivite = (med_int *) malloc(sizeof(med_int)*taille);
  EXIT_IF(connectivite == NULL,NULL,NULL);
  numele = (med_int *) malloc(sizeof(med_int)*nmpolygones);
  EXIT_IF(numele == NULL,NULL,NULL);
  nufael = (med_int *) malloc(sizeof(med_int)*nmpolygones);
  EXIT_IF(nufael == NULL,NULL,NULL);
  nomele = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*nmpolygones+1);
  EXIT_IF(nomele == NULL,NULL,NULL);

  /* lecture de la connectivite des mailles polygones */
  ret = MEDmeshPolygon2Rd(fid,nommaa,numdt,numit,MED_CELL,polytype,typ_con,
			 indexp,connectivite);

  EXIT_IF(ret < 0,"lors de la lecture des connectivites des mailles polygones",
	  NULL);

  /* lecture noms */
  ret1 = MEDmeshEntityNameRd(fid,nommaa,numdt,numit,
			     MED_CELL,polytype, nomele);

  /* lecture des numeros */
  ret2 = (med_int) MEDmeshEntityNumberRd(fid,nommaa,numdt,numit,
					 MED_CELL, polytype, numele);

  /* lecture des numeros de familles */
  ret3 = MEDmeshEntityFamilyNumberRd(fid, nommaa, MED_NO_DT, MED_NO_IT,
				     MED_CELL, polytype, nufael);

  if (!structure) {
  /* affichage des resultats */
  MEDmeshGeotypeName(fid,polytype,polytypename);
  fprintf(stdout,"\n\n- Mailles de type %s : ",polytypename);
    for (i=0;i<nmpolygones;i++) {
      fprintf(stdout,"\n >> Maille MED_POLYGONE "IFORMAT" : \n",i+1);
      fprintf(stdout,"\n  - Connectivité : ");
      ind1 = *(indexp+i)-1;
      ind2 = *(indexp+i+1)-1;
      for (j=ind1;j<ind2;j++)
	printf(" "IFORMAT" ",*(connectivite+j));
      if (ret1 == 0) {
	strncpy(tmp,nomele+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	tmp[MED_SNAME_SIZE] = '\0';
	fprintf(stdout,"\n  - Nom : %s \n",tmp);
      }
      if (ret2 == 0)
	fprintf(stdout,"\n  - Numero : "IFORMAT" \n",*(numele+i));

      if ( ret3 >= 0 )
	fprintf(stdout,"\n  - Numéro de famille : "IFORMAT" \n",*(nufael+i));
      else
	fprintf(stdout,"\n  - Numéro de famille : %d \n",0);
    }
    polytypename[0]='\0';
  }

    /* on libere la memoire */
    free(indexp);
    free(connectivite);
    free(numele);
    free(nufael);
    free(nomele);

    return;
}


med_int lecture_nombre_mailles_polyedres(const med_idt fid,
					 const char * const nommaa,
					 const med_int numdt,
					 const med_int numit,
					 const med_connectivity_mode typ_con)
{
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;

  med_int npolyedres = MEDmeshnEntity(fid,nommaa,numdt,numit,
				    MED_CELL,MED_POLYHEDRON,
				    MED_INDEX_FACE,typ_con,&chgt,&trsf);

  EXIT_IF(npolyedres < 0,"lors de la lecture du nombre de mailles polyedre \n",
	  NULL);
  if ( npolyedres > 0 ) npolyedres--; else npolyedres=0;
  if (npolyedres)
    fprintf(stdout,"- Nombre de mailles de type MED_POLYEDRE : "IFORMAT" \n",
	    npolyedres);

  return npolyedres;
}


void lecture_mailles_polyedres(const med_idt         fid,
			       const char * const    nommaa,
			       const med_int         numdt,
			       const med_int         numit,
			       const med_int         npolyedres,
			       const med_switch_mode       mode_coo,
			       const med_connectivity_mode typ_con)
{
  med_int i,j,k;
  med_err ret = 0;
  med_int taille;
  med_int *connectivite;
  char    *nomele;
  med_int *numele;
  med_int *nufael;
  med_int *indexf, *indexn;
  int ind1,ind2;
  char tmp[MED_SNAME_SIZE+1];
  med_err ret1,ret2,ret3;
  med_int nfa;
  med_int nnoe;
  med_int nindn;
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;


  /* lecture des parametres de base */
  taille = MEDmeshnEntity(fid,nommaa,numdt,numit,
			  MED_CELL,MED_POLYHEDRON,MED_CONNECTIVITY,typ_con,
			  &chgt,&trsf);
  EXIT_IF(taille < 0,"lors de la lecture des parametres des mailles polyedres",
	  NULL);

  nindn = MEDmeshnEntity(fid,nommaa,numdt,numit,
			 MED_CELL,MED_POLYHEDRON,MED_INDEX_NODE,typ_con,
			 &chgt,&trsf);
  EXIT_IF(nindn < 0,"lors de la lecture des parametres des mailles polyedres",
	  NULL);

  /* allocation memoire */
  /* nindf == npolyedres+1 */
  indexf = (med_int *) malloc(sizeof(med_int)*(npolyedres+1));
  EXIT_IF(indexf == NULL,NULL,NULL);
  indexn = (med_int *) malloc(sizeof(med_int)*nindn);
  EXIT_IF(indexn == NULL,NULL,NULL);
  connectivite  = (med_int *) malloc(sizeof(med_int)*taille);
  EXIT_IF(connectivite == NULL,NULL,NULL);
  numele = (med_int *) malloc(sizeof(med_int)*npolyedres);
  EXIT_IF(numele == NULL,NULL,NULL);
  nufael = (med_int *) malloc(sizeof(med_int)*npolyedres);
  EXIT_IF(nufael == NULL,NULL,NULL);
  nomele = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*npolyedres+1);
  EXIT_IF(nomele == NULL,NULL,NULL);

  ret = MEDmeshPolyhedronRd(fid,nommaa,numdt,numit,MED_CELL,typ_con,
			    indexf,indexn,connectivite);
  EXIT_IF(ret < 0,
	  "lors de la lecture de la connectivite des mailles polyedres",
	  NULL);

  /* lecture des noms */
  ret1 = MEDmeshEntityNameRd(fid,nommaa,numdt,numit,MED_CELL,MED_POLYHEDRON,nomele);

  /* lecture des numeros */
  ret2 = MEDmeshEntityNumberRd(fid,nommaa,numdt,numit,MED_CELL,MED_POLYHEDRON,numele);

  /* lecture des numeros de familles */
  ret3 = MEDmeshEntityFamilyNumberRd(fid,nommaa,numdt,numit,MED_CELL,MED_POLYHEDRON,nufael);

  if (!structure) {
  /* affichage des resultats */
  fprintf(stdout,"\n\n- Mailles de type MED_POLYEDRE : ");
  for (i=0;i<npolyedres;i++) {
    fprintf(stdout,"\n >> Maille MED_POLYEDRE "IFORMAT" : \n",i+1);
    fprintf(stdout,"\n  - Connectivité : \n");
    nfa  = *(indexf+i+1) - *(indexf+i);
    /* ind1 = indice dans "faces" pour acceder aux numeros des faces */
    ind1 = *(indexf+i) - 1;
    for (j=0;j<nfa;j++) {
      if (typ_con == MED_NODAL) {
	/* ind2 = indice dans "connectivite"
	   pour acceder au premier noeud de la face */
	ind2 = *(indexn+ind1+j) - 1;
	nnoe = *(indexn+ind1+j+1) - *(indexn+ind1+j);
	fprintf(stdout,"   - Face "IFORMAT" : [ ", j+1);
	for (k=0;k<nnoe;k++)
	  printf(" "IFORMAT" ",*(connectivite+ind2+k));
	printf(" ] \n");
      }
      else {
	nfa  = *(indexf+i+1) - *(indexf+i);
	/* ind1 = indice dans "connectivite"
	   pour acceder aux numeros des faces */
	ind1 = *(indexf+i) - 1;
	for (j=0;j<nfa;j++)
	  fprintf(stdout,"   - Face "IFORMAT" de numero : "IFORMAT" et de type "IFORMAT" \n", j+1,
		  *(connectivite+ind1+j),*(indexn+ind1+j));
      }
    }
    if (ret1 == 0) {
      strncpy(tmp,nomele+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
      tmp[MED_SNAME_SIZE] = '\0';
      fprintf(stdout,"\n  - Nom : %s \n",tmp);
    }
    if (ret2 == 0)
      fprintf(stdout,"\n  - Numero : "IFORMAT" \n",*(numele+i));
    if (ret3 >= 0)
      fprintf(stdout,"\n  - Numéro de famille : "IFORMAT" \n",*(nufael+i));
    else
      fprintf(stdout,"\n  - Numéro de famille : %d \n",0);

  }
  }

  /* on libere la memoire */
  free(indexf);
  free(indexn);
  free(connectivite);
  free(numele);
  free(nufael);
  free(nomele);

  return;
}

med_int lecture_nombre_faces_standards(const med_idt fid,
				       const char * const nommaa,
				       const med_int numdt,
				       const med_int numit,
				       const med_geometry_type typ_geo,
				       const med_int indice
				       )
{

  med_bool chgt=MED_FALSE,trsf=MED_FALSE;

  med_int nfaces = MEDmeshnEntity(fid,nommaa,numdt,numit,
				    MED_DESCENDING_FACE,typ_geo,
				    MED_CONNECTIVITY,MED_DESCENDING,&chgt,&trsf);
  EXIT_IF(nfaces < 0,"lors de la lecture du nombre de faces",NULL);

  if ( (indice < (MED_N_FACE_GEO_FIXED_CON-2) ) ||
       (indice >= (MED_N_FACE_GEO_FIXED_CON-2) && (nfaces > 0) ) )
    if (nfaces)
      fprintf (stdout,"- Nombre de faces de type %s : "IFORMAT" \n",
	       nomfac[indice],nfaces);

  return nfaces;
}

void lecture_faces_standard(const med_idt fid,
			    const char * const nommaa,
			    const med_int numdt,
			    const med_int numit,
			    const med_int mdim,
			    const med_int *const nfaces,
			    const med_switch_mode mode_coo)
{
  med_int taille;
  med_int *connectivite;
  char *nomele;
  med_int *numele;
  med_int *nufael;
  med_bool inoele,inuele,inufael;
  med_int i,j;
  med_err ret = 0;
  char str[MED_SNAME_SIZE+1];
  med_int entdim;
  med_int nnodes;

  for (i=0;i<MED_N_FACE_GEO_FIXED_CON;i++)
    if (nfaces[i] > 0 ) {

      /*  taille de la description : nombre d'aretes */
        ret=_MEDgetGeometricParameter(MED_DESCENDING_FACE, typfac[i],&entdim,&nnodes,&taille);
      EXIT_IF(ret < 0,"lors de la lecture des caractéristiques des mailles",NULL);

      /* allocation memoire */
      connectivite = (med_int*)malloc(sizeof(med_int)*taille*nfaces[i]);
      EXIT_IF(connectivite == NULL,NULL,NULL);
      nomele = (char*)malloc(sizeof(char)*MED_SNAME_SIZE*nfaces[i]+1);
      EXIT_IF(nomele == NULL,NULL,NULL);
      numele = (med_int*)malloc(sizeof(med_int)*nfaces[i]);
      EXIT_IF(numele == NULL,NULL,NULL);
      nufael = (med_int*)malloc(sizeof(med_int)*nfaces[i]);
      EXIT_IF(nufael == NULL,NULL,NULL);

      /* lecture des données */
     ret = MEDmeshElementRd( fid,nommaa,numdt,numit,MED_DESCENDING_FACE,typmai[i],
			      MED_DESCENDING, mode_coo, connectivite,
			      &inoele,nomele,&inuele,numele,&inufael,nufael );
      EXIT_IF(ret < 0,"lors de la lecture des faces",NULL);

      if (!structure) {
      /* affichage des resultats */
      fprintf(stdout,"\n- Faces de type %s : ", nomfac[i]);
      fprintf(stdout,"\n  - Connectivité : ");
      for (j=0;j<nfaces[i]*taille;j++) {
	    if (mode_coo == MED_FULL_INTERLACE && !(j % taille))
	      fprintf(stdout,"\n [ %5"MED_IFORMAT" ] : ", (j/taille+1) );
	    if (mode_coo == MED_NO_INTERLACE && !(j % nfaces[i]))
	      fprintf(stdout,"\n");
	    fprintf(stdout," %9"MED_IFORMAT" ",*(connectivite+j));
	  }

      if (inoele) {
	fprintf(stdout,"\n  - Noms : \n");
	for (j=0;j<nfaces[i];j++) {
	  strncpy(str,nomele+j*MED_SNAME_SIZE,MED_SNAME_SIZE);
	  str[MED_SNAME_SIZE] = '\0';
	  fprintf(stdout," %s ",str);
	}
      }
      if (inuele) {
	fprintf(stdout,"\n  - Numeros :\n");
	for (j=0;j<nfaces[i];j++)
	  fprintf(stdout," "IFORMAT" ",*(numele+j));
      }
      fprintf(stdout,"\n  - Numéros de familles : \n");
      for (j=0;j<nfaces[i];j++)
	if ( inufael )
	  fprintf(stdout," "IFORMAT" ",*(nufael+j));
	else
	  fprintf(stdout," %d ",0);
	  }

      /* liberation memoire */
      free(connectivite);
      free(nomele);
      free(numele);
      free(nufael);
    }

  return;
}

med_int lecture_nombre_faces_polygones(const med_idt fid,
				       const char * const nommaa,
				       const med_int numdt,
				       const med_int numit)
{

  med_bool chgt=MED_FALSE,trsf=MED_FALSE;

  med_int nfpolygones = MEDmeshnEntity(fid,nommaa,numdt,numit,
				       MED_CELL,MED_POLYGON,
				       MED_INDEX_NODE,MED_DESCENDING,&chgt,&trsf);

  EXIT_IF(nfpolygones < 0,"lors de la lecture du nombre de faces polygone \n",
	  NULL);
  if (nfpolygones > 0 ) nfpolygones--; else nfpolygones=0;
  if (nfpolygones)
    fprintf(stdout,"- Nombre de faces de type MED_POLYGONE : "IFORMAT" \n",
	  nfpolygones);

  return nfpolygones;
}

void lecture_faces_polygones(const med_idt fid,
			     const char * const nommaa,
			     const med_int numdt,
			     const med_int numit,
			     const med_int nfpolygones,
			     const med_switch_mode mode_coo)
{
  med_int i,j;
  med_err ret = 0;
  char *nomele;
  med_int *numele;
  med_int *nufael;
  med_int *connectivite;
  med_int taille;
  med_int *indexp;
  int ind1,ind2;
  char tmp[MED_NAME_SIZE+1];
  med_err ret1,ret2,ret3;
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;

  /* quelle taille pour  le tableau des connectivites ? */
  taille=MEDmeshnEntity(fid,nommaa,numdt,numit,
			MED_DESCENDING_FACE,MED_POLYGON,MED_CONNECTIVITY,MED_DESCENDING,
			&chgt,&trsf);
  EXIT_IF(taille < 0,"lors de la lecture des parametres des faces polygones",
	    NULL);

  /* allocation memoire */
  indexp = (med_int *) malloc(sizeof(med_int)*(nfpolygones+1));
  EXIT_IF(indexp == NULL,NULL,NULL);
  connectivite = (med_int *) malloc(sizeof(med_int)*taille);
  EXIT_IF(connectivite == NULL,NULL,NULL);
  numele = (med_int *) malloc(sizeof(med_int)*nfpolygones);
  EXIT_IF(numele == NULL,NULL,NULL);
  nufael = (med_int *) malloc(sizeof(med_int)*nfpolygones);
  EXIT_IF(nufael == NULL,NULL,NULL);
  nomele = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*nfpolygones+1);
  EXIT_IF(nomele == NULL,NULL,NULL);

  /* lecture de la connectivite des faces polygones */
  ret = MEDmeshPolygonRd(fid,nommaa,numdt,numit,MED_DESCENDING_FACE,MED_DESCENDING,
			 indexp,connectivite);
  EXIT_IF(ret < 0,"lors de la lecture des connectivites des faces polygones", NULL);

  /* lecture noms */
  ret1 = MEDmeshEntityNameRd(fid,nommaa,numdt,numit,
			     MED_DESCENDING_FACE,MED_POLYGON, nomele);

  /* lecture des numeros */
  ret2 = (med_int) MEDmeshEntityNumberRd(fid,nommaa,numdt,numit,
					 MED_DESCENDING_FACE, MED_POLYGON, numele);

  /* lecture des numeros de familles */
  ret3 = MEDmeshEntityFamilyNumberRd(fid, nommaa, MED_NO_DT, MED_NO_IT,
				     MED_DESCENDING_FACE, MED_POLYGON, nufael);

  if (!structure) {
  /* affichage des resultats */
  fprintf(stdout,"\n\n- Faces de type MED_POLYGONE : ");
  for (i=0;i<nfpolygones;i++) {
    fprintf(stdout,"\n >> Face MED_POLYGONE "IFORMAT" : \n",i+1);
    fprintf(stdout,"\n  - Connectivité : ");
    ind1 = *(indexp+i)-1;
    ind2 = *(indexp+i+1)-1;
    for (j=ind1;j<ind2;j++)
      fprintf(stdout," "IFORMAT" ",*(connectivite+j));
    if (ret1 == 0) {
      strncpy(tmp,nomele+j*MED_SNAME_SIZE,MED_SNAME_SIZE);
      tmp[MED_SNAME_SIZE] = '\0';
      fprintf(stdout,"\n  - Nom : %s \n",tmp);
    }
    if (ret2 == 0)
      fprintf(stdout,"\n  - Numero : "IFORMAT" \n",*(numele+j));
    if ( ret3 > 0 )
      fprintf(stdout,"\n  - Numéro de famille : "IFORMAT" \n",*(nufael+i));
    else
      fprintf(stdout,"\n  - Numéro de famille : %d \n",0);
  }
  }

  /* on libere la memoire */
  free(indexp);
  free(connectivite);
  free(numele);
  free(nufael);
  free(nomele);

  return;
}


med_int lecture_nombre_aretes_standards(const med_idt fid,
					const char *const  nommaa,
					const med_int numdt,
					const med_int numit,
					const med_geometry_type typ_geo,
					const med_int indice)
{

  med_bool chgt=MED_FALSE,trsf=MED_FALSE;

  med_int naretes = MEDmeshnEntity(fid,nommaa,numdt,numit,
				   MED_DESCENDING_EDGE, typ_geo,
				   MED_CONNECTIVITY,MED_DESCENDING,&chgt,&trsf);
  EXIT_IF(naretes < 0,"lors de la lecture du nombre d'aretes",NULL);
  if ( (indice < (MED_N_EDGE_GEO_FIXED_CON-1) ) ||
       (indice >= (MED_N_EDGE_GEO_FIXED_CON-1) && (naretes > 0) ) )
    if (naretes)
      fprintf (stdout,
	       "- Nombre d'aretes de type %s : "IFORMAT" \n",nomare[indice],naretes);

  return naretes;
}

void lecture_aretes_standards(const med_idt fid,
			      const char * const nommaa,
			      const med_int numdt,
			      const med_int numit,
			      const med_int mdim,
			      const med_int * const naretes,
			      const med_switch_mode mode_coo)
{
  med_int taille;
  med_int *connectivite;
  char    *nomele;
  med_int *numele;
  med_int *nufael;
  med_bool inoele,inuele,inufael;
  med_int i,j;
  med_err ret = 0;
  char str[MED_SNAME_SIZE+1];
  med_int entdim;
  med_int nnodes;

  for (i=0;i<MED_N_EDGE_GEO_FIXED_CON;i++)
    if (naretes[i] > 0) {

     ret=_MEDgetGeometricParameter(MED_DESCENDING_EDGE, typare[i],&entdim,&nnodes,&taille);
      EXIT_IF(ret < 0,"lors de la lecture des caractéristiques des mailles",NULL);

      /* allocation memoire */
      connectivite = (med_int*)malloc(sizeof(med_int)*taille*naretes[i]);
      EXIT_IF(connectivite == NULL,NULL,NULL);
      nomele = (char*)malloc(sizeof(char)*MED_SNAME_SIZE*naretes[i]+1);
      EXIT_IF(nomele == NULL,NULL,NULL);
      numele = (med_int*)malloc(sizeof(med_int)*naretes[i]);
      EXIT_IF(numele == NULL,NULL,NULL);
      nufael = (med_int*)malloc(sizeof(med_int)*naretes[i]);
      EXIT_IF(nufael == NULL,NULL,NULL);

      /* lecture des données */
      ret = MEDmeshElementRd( fid,nommaa,numdt,numit,MED_DESCENDING_EDGE,typare[i],
			      MED_DESCENDING, mode_coo, connectivite,
			      &inoele,nomele,&inuele,numele,&inufael,nufael );
      EXIT_IF(ret < 0,"lors de la lecture des aretes",
	      NULL);

      if (!structure) {
      /* affichage des resultats */
      fprintf(stdout,"\n- Aretes de type %s : ", nomare[i]);
      fprintf(stdout,"\n  - Connectivité : ");
      for (j=0;j<naretes[i]*taille;j++) {
  	    if (mode_coo == MED_FULL_INTERLACE && !(j % taille))
	      fprintf(stdout,"\n [ %5"MED_IFORMAT" ] : ", (j/taille+1) );
	    if (mode_coo == MED_NO_INTERLACE && !(j % naretes[i]))
	      fprintf(stdout,"\n");
	    fprintf(stdout," %9"MED_IFORMAT" ",*(connectivite+j));
	  }

      if (inoele) {
	fprintf(stdout,"\n  - Noms : \n");
	for (j=0;j<naretes[i];j++) {
	  strncpy(str,nomele+j*MED_SNAME_SIZE,MED_SNAME_SIZE);
	  str[MED_SNAME_SIZE] = '\0';
	  fprintf(stdout," %s ",str);
	}
      }
      if (inuele) {
	fprintf(stdout,"\n  - Numeros :\n");
	for (j=0;j<naretes[i];j++)
	  fprintf(stdout," "IFORMAT" ",*(numele+j));
      }
      fprintf(stdout,"\n  - Numéros de familles : \n");
      for (j=0;j<naretes[i];j++)
	if ( inufael )
	  fprintf(stdout," "IFORMAT" ",*(nufael+j));
	else
	  fprintf(stdout," %d ",0);
	  }

      /* liberation memoire */
      free(connectivite);
      free(nomele);
      free(numele);
      free(nufael);
    }

  return;
}


/******************************************************************************
 * - Nom de la fonction : lecture_maillage_non_structure
 * - Description : lecture et affichage d'un maillage MED_NON_STRUCTURE.
 * - Parametres :
 *     - fid                       (IN) : ID du fichier MED.
 *     - nommaa                    (IN) : nom du maillage a lire.
 *     - mdim                      (IN) : dimension du maillage.
 *     - mode_coo                  (IN) : mode de stockage en memoire :
 *                                        MED_FULL_INTERLACE : entrelace |
 *                                        MED_NO_INTERLACE : non entrelace.
 *     - typ_con                   (IN) : mode de connectivite :
 *                                        MED_DESCENDING : descendante |
 *                                        MED_NODAL : nodale.
 *     - lecture_en_tete_seulement (IN) : mode de lecture et d'affichage.
 ******************************************************************************/

void lecture_maillage_non_structure(med_idt fid,
				    const char *nommaa,
				    const med_int numdt,
				    const med_int numit,
				    const med_int mdim,
				    const med_int edim,
				    const med_switch_mode mode_coo,
				    const med_connectivity_mode typ_con,
				    const char * const nomcoo,
				    const char * const unicoo,
				    const med_axis_type *const rep,
				    med_int*             nmodels,
				    med_geometry_type**  geotype_elst,
				    char**               geotypename_elst,
				    const int lecture_en_tete_seulement)
{
  med_int i;
  /* nombre d'objets MED : noeuds, mailles, faces, aretes , ... */
  med_int nnoe;
  med_int nmailles[MED_N_CELL_GEO_FIXED_CON];
  med_int nfaces[MED_N_FACE_GEO_FIXED_CON];
  med_int naretes[MED_N_EDGE_GEO_FIXED_CON];
  /* polygones et polyedres */
  med_int nmpolygones,nmpolygones2, npolyedres, nfpolygones;
  /* familles */
  med_int nfam;
  /* equivalences */
  med_int nequ;
  /* joints */
  med_int njnt;
  /* Eléments de structure */
  med_int _nmodels=0,*_nmailles_elst    = NULL;
  med_geometry_type    *_geotype_elst    = NULL;
  char                 *_geotypename_elst = NULL;
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;

  /* Combien de noeuds dans le maillage ? */
  nnoe = lecture_nombre_noeuds_maillage_non_structure(fid,nommaa,numdt,numit);

  /* Combien de types d'éléments de structure utilisés dans le maillage */
  _nmodels = MEDmeshnEntity(fid,nommaa,numdt,numit,
			      MED_STRUCT_ELEMENT,MED_GEO_ALL,MED_UNDEF_DATATYPE,MED_UNDEF_CONNECTIVITY_MODE,
			      &chgt,&trsf);
  EXIT_IF(_nmodels<0,
	  "Erreur à la lecture du nombre de type géométrique pour le type d'entités MED_STRUCT_ELEMENT",NULL);

  _nmailles_elst     = (med_int *)           malloc(_nmodels*sizeof(med_int));
  _geotype_elst      = (med_geometry_type *) malloc(_nmodels*sizeof(med_geometry_type));
  _geotypename_elst  = (char *)              malloc(_nmodels*sizeof(char)*(MED_NAME_SIZE+1));
  /* Combien mailles par  types d'éléments de structure */
  for (i=0; i < _nmodels; i++) {
	 _nmailles_elst[i]=lecture_nombre_et_type_mailles_elstruct(fid,nommaa,numdt,numit,i,
								  &_geotype_elst[i],&_geotypename_elst[i*(MED_NAME_SIZE+1)]);
	 }
  if (_nmodels) {
    *nmodels          = _nmodels;
    *geotype_elst     = _geotype_elst;
    *geotypename_elst = _geotypename_elst;
  } else *nmodels=0;

 /*TODO : AFFICHER DT ( DTUNIT ) */
  /* Combien de mailles, faces ou aretes pour chaque type geometrique ? */
  for (i=0;i<MED_N_CELL_GEO_FIXED_CON;i++)
    nmailles[i] = lecture_nombre_mailles_standards(fid,nommaa,numdt,numit,typmai[i],
						   typ_con,i);

  /* Combien de mailles polygones simples */
  nmpolygones = lecture_nombre_mailles_polygones(fid,nommaa,numdt,numit,MED_POLYGON,typ_con);

  /* Combien de mailles polygones quadratiques */
  nmpolygones2 = lecture_nombre_mailles_polygones(fid,nommaa,numdt,numit,MED_POLYGON2,typ_con);

  /* Combien de mailles polyedres quelconques ? */
  npolyedres = lecture_nombre_mailles_polyedres(fid,nommaa,numdt,numit,typ_con);

  /* Pour la connectivite descendante */
  if (typ_con == MED_DESCENDING) {

    /* Combien de faces : types geometriques standards ? */
    for (i=0;i<MED_N_FACE_GEO_FIXED_CON;i++)
      nfaces[i] = lecture_nombre_faces_standards(fid,nommaa,numdt,numit,typfac[i],i);

    /* Combien de faces polygones quelconques ? */
    nfpolygones = lecture_nombre_faces_polygones(fid,nommaa,numdt,numit);

    /* Combien d'aretes */
    for (i=0;i<MED_N_EDGE_GEO_FIXED_CON;i++)
      naretes[i] = lecture_nombre_aretes_standards(fid,nommaa,numdt,numit,typare[i],i);
  }

  if (lecture_en_tete_seulement != MED_LECTURE_MAILLAGE_SUPPORT_UNIQUEMENT) {
   /* combien de familles ? */
  nfam = lecture_nombre_famille(fid,nommaa);

  /* combien d'equivalences ? */
  nequ = lecture_nombre_equivalence(fid,nommaa);

  /* combien de joints ? */
  njnt = lecture_nombre_joint(fid,nommaa);
  }

  /* en fonction du mode de lecture, on continue ou non */
  if (lecture_en_tete_seulement == MED_LECTURE_ENTETE_SEULEMENT) {
/* Ces desallocations sont effectuées
   après la lecture des champs sur ces éléments de structure */
/*     free(_nmailles_elst); */
/*     free(_geotype_elst); */
/*     free(_geotypename_elst); */
    return;
  }
  /****************************************************************************
  *                       LECTURE DES NOEUDS                                  *
  ****************************************************************************/
  lecture_noeuds_maillage_non_structure(fid,nommaa,numdt,numit,mdim,edim,nnoe,mode_coo,nomcoo,unicoo,rep);
  /*ICI;_MEDobjetsOuverts(fid);*/


  /****************************************************************************
  *                       LECTURE DES ELEMENTS                                *
  * Mailles :                                                                 *
  * - Types geometriques classiques : MED_SEG2, MED_SEG3, MED_TRIA3, ...      *
  * - Elements de structure                                                   *
  * - Polygones quelconques.                                                  *
  * - Polyedres quelconques.                                                  *
  * Faces (connectivite descendante uniquement) :                             *
  * - Types geometriques classiques.                                          *
  * - Polygones quelconques.                                                  *
  ****************************************************************************/

  /* lecture et affichage des mailles */
  lecture_mailles_standards(fid,nommaa,numdt,numit,mdim,nmailles,mode_coo,typ_con);
  /*ICI;_MEDobjetsOuverts(fid);*/

  if (lecture_en_tete_seulement == MED_LECTURE_MAILLAGE_SUPPORT_UNIQUEMENT) return;

  if (_nmodels>0) {
    lecture_mailles_elstruct(fid,nommaa,numdt,numit,_nmodels,
			     _geotype_elst,_geotypename_elst,_nmailles_elst,mode_coo);
  /* Ces desallocations sont effectuées
    après la lecture des champs sur ces éléments de structure */
/*     free(_nmailles_elst); */
/*     free(_geotype_elst); */
/*     free(_geotypename_elst); */
  }
  /*ICI;_MEDobjetsOuverts(fid);*/

  if (nmpolygones > 0)
    lecture_mailles_polygones(fid,nommaa,numdt,numit,MED_POLYGON,nmpolygones,mode_coo,typ_con);
  /*ICI;_MEDobjetsOuverts(fid);*/

  if (nmpolygones2 > 0)
    lecture_mailles_polygones(fid,nommaa,numdt,numit,MED_POLYGON2,nmpolygones2,mode_coo,typ_con);
  /*ICI;_MEDobjetsOuverts(fid);*/

  if (npolyedres > 0)
    lecture_mailles_polyedres(fid,nommaa,numdt,numit,npolyedres,mode_coo,typ_con);
  /*ICI;_MEDobjetsOuverts(fid);*/

  /* lecture et affichage des faces en connectivite descendante uniquement */
  if (typ_con == MED_DESCENDING) {
    lecture_faces_standard(fid,nommaa,numdt,numit,mdim,nfaces,mode_coo);
    if (nfpolygones > 0)
      lecture_faces_polygones(fid,nommaa,numdt,numit,nfpolygones,mode_coo);
  }
  /*ICI;_MEDobjetsOuverts(fid);*/

  /* lecture et affichage des aretes en connectivite descendante uniquement */
  if (typ_con == MED_DESCENDING)
    lecture_aretes_standards(fid,nommaa,numdt,numit,mdim,naretes,mode_coo);
  /*ICI;_MEDobjetsOuverts(fid);*/

  /****************************************************************************
  *                      LECTURE DES FAMILLES                                 *
  ****************************************************************************/
  lecture_famille_maillage(fid,nommaa,nfam);
  /*ICI;_MEDobjetsOuverts(fid);*/


  /****************************************************************************
  *                       LECTURE DES EQUIVALENCES                            *
  ****************************************************************************/
  lecture_equivalence_maillage(fid,nommaa,nequ);
  /*ICI;_MEDobjetsOuverts(fid);*/


  /****************************************************************************
  *                       LECTURE DES JOINTS                                  *
  ****************************************************************************/
  lecture_joint_maillage(fid,nommaa,njnt);
  /*ICI;_MEDobjetsOuverts(fid);*/

  return;
}


void lecture_caracteristiques_grille(const med_idt fid,
				     const char * const nommaa,
				     const med_int numdt,
				     const med_int numit,
				     const med_int mdim,
				     med_int *nind,
				     med_int *nnoe,
				     med_int *nmai,
				     med_grid_type *type)
{
  med_err ret = 0;
  med_int axe;
  med_int *structure_grille;
  med_data_type quoi;
  med_int j;
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;

  /* lecture de la nature du maillage structure : MED_GRILLE_CARTESIENNE ,...*/
  ret = MEDmeshGridTypeRd(fid,nommaa,type);
  EXIT_IF(ret < 0,"a la lecture du type d'une grille ",NULL);

  switch(*type) {

  case MED_CARTESIAN_GRID :
  case MED_POLAR_GRID :
    if (*type == MED_CARTESIAN_GRID)
      fprintf(stdout,"- Type de grille : MED_GRILLE_CARTESIENNE \n");
    else
      fprintf(stdout,"- Type de grille : MED_GRILLE_POLAIRE \n");
    for (axe=1;axe<=mdim;axe++) {
      switch(axe) {

      case 1:
	quoi = MED_COORDINATE_AXIS1;
	break;

      case 2:
	quoi = MED_COORDINATE_AXIS2;
	break;

      case 3:
	quoi = MED_COORDINATE_AXIS3;
	break;
      }
      nind[axe - 1] = MEDmeshnEntity(fid,nommaa, numdt, numit,
				     MED_NODE, MED_NONE, quoi, MED_NO_CMODE, &chgt, &trsf);

      EXIT_IF(nind[axe - 1] < 0,
	      "lors de la lecture de la taille d'un indice d'une grille",
	      NULL);
      *nnoe = nind[axe - 1] * (*nnoe);
      *nmai = (nind[axe - 1] - 1) * (*nmai);
      fprintf(stdout,
	      "- Taille de l'indice de l'axe "IFORMAT" des coordonnees : "IFORMAT" \n",
	      axe,nind[axe - 1]);
    }
    break;

  case MED_CURVILINEAR_GRID:
    fprintf(stdout,"- Type de grille : MED_GRILLE_DESTRUCTUREE \n");
    *nnoe = MEDmeshnEntity(fid, nommaa, numdt, numit, 
			       MED_NODE, MED_NONE, MED_COORDINATE, MED_NO_CMODE, &chgt, &trsf);
    EXIT_IF(*nnoe < 0,"lors de la lecture du nombre de noeuds du maillage "
	    ,nommaa);

    /* on alloue la memoire */
    structure_grille = (med_int *) malloc(sizeof(med_int)*mdim);
    EXIT_IF(structure_grille == NULL,NULL,NULL);
    /* on lit la structure de la grille
       et on affiche le resultat */
    ret = MEDmeshGridStructRd(fid,nommaa,numdt, numit, structure_grille);
    EXIT_IF(ret < 0,"lors de la lecture de la structure de la grille",
	    NULL);
    fprintf(stdout,"- Structure de la grille : [ ");
    for (j=0;j<mdim;j++) {
      *nmai = (*(structure_grille+j) - 1) * (*nmai);
      fprintf(stdout," "IFORMAT" ",*(structure_grille+j));
    }
    fprintf(stdout," ] \n");
    /* on nettoie la memoire */
    free(structure_grille);
    break;

  case MED_UNDEF_GRID_TYPE:
  default:
    EXIT_IF(-1,"Type de grille non reconnu.",nommaa);

  }

  fprintf(stdout,"- Nombre de noeuds : "IFORMAT" \n",*nnoe);
  fprintf(stdout,"- Nombre de mailles : "IFORMAT" \n",*nmai);

  return;
}


void lecture_noeuds_maillage_structure(const med_idt fid,
				       const char * const nommaa,
				       const med_int numdt,
				       const med_int numit,
				       const med_int mdim,
				       const med_int edim,
				       const med_int * const nind,
				       const med_int nnoe,
				       const char * const comp,
				       const char * const unit,
				       const med_grid_type type,
				       const med_switch_mode mode_coo)
{
  med_err ret = 0;
  med_int axe,i,j;
  char str[MED_SNAME_SIZE+1];
  med_float *coo     = NULL;
  med_float *indices = NULL;
  med_int   *nufano  = NULL;
  med_int   *numnoe  = NULL;
  char      *nomnoe  = NULL;
  med_bool inufael=MED_FALSE;

  fprintf(stdout,"\n(*************************)\n");
  fprintf(stdout,"(* NOEUDS DE LA GRILLE : *)\n");
  fprintf(stdout,"(*************************)\n");

  switch(type) {

  case MED_CARTESIAN_GRID :
  case MED_POLAR_GRID :
    /* on affiche les coordonnees de chacun des axes */
    for (axe = 1; axe<=mdim; axe++) {
      /* on alloue la memoire */
      indices = (med_float *) malloc(sizeof(med_float)*nind[axe - 1]);
      EXIT_IF(indices == NULL,NULL,NULL);
      /* on lit le tableau des indices de coordonnees
         et on affiche le resultat */
      ret = MEDmeshGridIndexCoordinateRd(fid,nommaa,numdt,numit, axe,indices);
      EXIT_IF(ret < 0,"lors de la lecture d'un tableau d'indice",
	      NULL);
      fprintf(stdout,"\n - Axe %." xstr(MED_SNAME_SIZE) "s [%." xstr(MED_SNAME_SIZE) "s] : [ ",
	      &comp[MED_SNAME_SIZE*(axe-1)],&unit[MED_SNAME_SIZE*(axe-1)]);
      for (j=0;j<nind[axe - 1];j++)
	fprintf(stdout," %f ",*(indices+j));
      printf(" ] \n");
      /* on nettoie la memoire */
      free(indices);
    }
    break;

  case MED_CURVILINEAR_GRID:
    /* on alloue la memoire */
    coo = (med_float *) malloc(sizeof(med_float)*nnoe*edim);
    EXIT_IF(coo == NULL,NULL,NULL);
    /* on va lire les coordonnees des noeuds */
    ret = MEDmeshNodeCoordinateRd(fid,nommaa,numdt,numit, MED_FULL_INTERLACE,coo);

    EXIT_IF(ret < 0,"lors de la lecture des noeuds du maillage",NULL);
    /* on affiche le resultat */
    fprintf(stdout,"- Nom des coordonnees : \n");
    for (i=0;i<edim;i++) {
      strncpy(str,comp+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
      str[MED_SNAME_SIZE] = '\0';
      fprintf(stdout," %s ",str);
    }
    fprintf(stdout,"\n- Unites des coordonnees : \n");
    for (i=0;i<edim;i++) {
      strncpy(str,unit+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
      str[MED_SNAME_SIZE] = '\0';
      fprintf(stdout," %s ",str);
    }
    if (!structure) {
    fprintf(stdout,"\n - Coordonnees des noeuds : [ ");
    for (j=0;j<nnoe*edim;j++)
      fprintf(stdout," %f ",*(coo+j));
    fprintf(stdout," ] \n");
    }

    /* on nettoie la memoire */
    free(coo);
    break;

  case MED_UNDEF_GRID_TYPE:
  default:
    EXIT_IF(-1,"Type de grille non reconnu.",nommaa);

  }

  /* lecture et affichage des :
     - numeros de familles des noeuds
     - noms des noeuds (optionnel)
     - numeros des noeuds (optionnel) */

  /* on alloue la memoire */
  numnoe = (med_int *) malloc(sizeof(med_int)*nnoe);
  EXIT_IF(numnoe == NULL,NULL,NULL);
  nomnoe = (char*) malloc(MED_SNAME_SIZE*nnoe+1);
  EXIT_IF(nomnoe == NULL,NULL,NULL);
  nufano = (med_int *) malloc(sizeof(med_int)*nnoe);
  EXIT_IF(nufano == NULL,NULL,NULL);

  /* on va lire les numeros de familles des noeuds */
  ret = MEDmeshEntityFamilyNumberRd(fid,nommaa,numdt,numit,MED_NODE,MED_NO_GEOTYPE,nufano);
  if (ret < 0) ret=0; else inufael=MED_TRUE;

  EXIT_IF(ret < 0,"lors de la lecture des numeros de familles des noeuds",
	  NULL);
  if (!structure) {
  /* on affiche le resultat */
  fprintf(stdout,"\n- Numeros des familles des noeuds : \n");
  for (i=0;i<nnoe;i++)
    if (inufael)
      fprintf(stdout," "IFORMAT" ",*(nufano+i));
    else
      fprintf(stdout," %d ",0);
  fprintf(stdout,"\n");
  }

  /* on va lire et afficher les noms des noeuds */
  if (MEDmeshEntityNameRd(fid,nommaa,numdt,numit,MED_NODE,MED_NO_GEOTYPE,nomnoe) == 0) {
    if (!structure) {
    fprintf(stdout,"\n- Noms des noeuds : \n");
    for (i=0;i<nnoe;i++) {
      strncpy(str,nomnoe+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
      str[MED_SNAME_SIZE] = '\0';
      fprintf(stdout," %s ",str);
    }
    }
  }

  /* on va lire et afficher les numeros des noeuds */
  if (MEDmeshEntityNumberRd(fid,nommaa,numdt,numit,MED_NODE,MED_NO_GEOTYPE,numnoe) == 0) {
    if (!structure) {
    fprintf(stdout,"\n- Numeros des noeuds : \n");
    for (i=0;i<nnoe;i++)
      fprintf(stdout," "IFORMAT" ",*(numnoe+i));
    }
  }

  /* on nettoie la memoire */
  free(nufano);
  free(numnoe);
  free(nomnoe);

  return;
}


void lecture_mailles_maillage_structure(const med_idt fid,
					const char * const nommaa,
					const med_int numdt,
					const med_int numit,
					const med_int mdim,
					const med_int nmai)

{
  med_err ret = 0;
  med_int i;
  med_int *nufael = NULL;
  char    *nomele = NULL;
  med_int *numele = NULL;
  char str[MED_SNAME_SIZE+1];
  /* type geometrique des elements */
  med_geometry_type typgeo;

  fprintf(stdout,"\n(***************************)\n");
  fprintf(stdout,"(* ELEMENTS DE LA GRILLE : *)\n");
  fprintf(stdout,"(***************************)\n");

  /* type des mailles */
  switch(mdim) {
  case 0 :
    typgeo = MED_POINT1;
    break;
  case 1 :
    typgeo = MED_SEG2;
    break;
  case 2 :
    typgeo = MED_QUAD4;
    break;
  default :
    typgeo = MED_HEXA8;
  }

  /* On va lire et afficher :
   * - Les numeros de familles
   * - Les noms (optionnel)
   * - Les numeros (optionnel)
   */

  /* on alloue la memoire */
  numele = (med_int *) malloc(sizeof(med_int)*nmai);
  EXIT_IF(numele == NULL,NULL,NULL);
  nomele = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*nmai+1);
  EXIT_IF(nomele == NULL,NULL,NULL);
  nufael = (med_int *) malloc(sizeof(med_int)*nmai);
  EXIT_IF(nufael == NULL,NULL,NULL);

  /* lecture des numeros de famille */
  ret = MEDmeshEntityFamilyNumberRd(fid,nommaa,numdt,numit,MED_CELL,typgeo,nufael);
  if (ret < 0)
    for (i=0;i<nmai;i++)
      *(nufael+i) = 0;

  if (!structure) {
  /* on affiche le resultat */
  fprintf(stdout,"\n- Numeros des familles des mailles : \n");
  for (i=0;i<nmai;i++)
    fprintf(stdout," "IFORMAT" ",*(nufael+i));
  fprintf(stdout,"\n");
  }

  /* on va lire et afficher les noms des mailles */
  if (MEDmeshEntityNameRd(fid,nommaa,numdt,numit,MED_CELL,typgeo,nomele) == 0) {
    if (!structure) {
    fprintf(stdout,"\n  - Noms : \n");
    for (i=0;i<nmai;i++) {
	  strncpy(str,nomele+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	  str[MED_SNAME_SIZE] = '\0';
	  fprintf(stdout," %s ",str);
	}
	}
  }

  /* on va lire et afficher les numeros des mailles */
  if (MEDmeshEntityNumberRd(fid,nommaa,numdt,numit,MED_CELL,typgeo,numele) == 0) {
    if (!structure) {
    fprintf(stdout,"\n  - Numeros :\n");
    for (i=0;i<nmai;i++)
      fprintf(stdout," "IFORMAT" ",*(numele+i));
    }
  }

  /* on libere la memoire */
  free(nufael);
  free(nomele);
  free(numele);

  return;
}

void lecture_maillage_structure(const med_idt fid,
				const char * const nommaa,
				const med_int numdt,
				const med_int numit,
				const med_int mdim,
				const med_int edim,
				const med_switch_mode mode_coo,
				const char * const comp,
				const char * const unit,
				const int lecture_en_tete_seulement)
{
  /* nombre de valeurs selon les axes du repere */
  med_int nind[3];
  med_int nnoe = 1;
  med_int nmai = 1;
  /* type de la grille */
  med_grid_type type;
  /* nombre de familles */
  med_int nfam;
  /* nombre d'equivalences */
  med_int nequ;
  /* nombre de joints */
  med_int njnt;

  /* lecture selon la nature de la grille des caracteristiques
     du maillage :
     - nombre de noeuds
     - nombre de mailles
  */
  lecture_caracteristiques_grille(fid,nommaa,numdt,numit,mdim,nind,&nnoe,&nmai,&type);

  /* nombre de familles */
  nfam = lecture_nombre_famille(fid,nommaa);

  /* nombre d'equivalences  */
  nequ = lecture_nombre_equivalence(fid,nommaa);

  /* combien de joints */
  njnt = lecture_nombre_joint(fid,nommaa);

  if (lecture_en_tete_seulement)
    return ;

  /****************************************************************************
  *                      LECTURE DES NOEUDS                                   *
  ****************************************************************************/
  lecture_noeuds_maillage_structure(fid,nommaa,numdt,numit,mdim,edim,nind,nnoe,comp,unit,type,mode_coo);

  /****************************************************************************
  *                      LECTURE DES ELEMENTS                                 *
  ****************************************************************************/
  lecture_mailles_maillage_structure(fid,nommaa,numdt,numit,mdim,nmai);

  /****************************************************************************
  *                      LECTURE DES FAMILLES                                 *
  ****************************************************************************/
  lecture_famille_maillage(fid,nommaa,nfam);

  /****************************************************************************
  *                       LECTURE DES EQUIVALENCES                            *
  ****************************************************************************/
  lecture_equivalence_maillage(fid,nommaa,nequ);

  /****************************************************************************
  *                       LECTURE DES JOINTS                                  *
  ****************************************************************************/
  lecture_joint_maillage(fid,nommaa,njnt);

  return ;
}

med_err getFieldsOn(const med_idt fid,
		    const char * const maillage,
		    const med_int mnumdt,
		    const med_int mnumit,
		    const med_int             nmodels,
		    const med_geometry_type* const  geotype_elst,
		    const char*              const  geotypename_elst,
		    const char * const nomcha,
		    const char * const dtunit,
		    const med_field_type typcha,
		    const med_int ncomp,
		    const char * const comp,
		    const char * const unit,
		    const med_entity_type entite,
		    const med_switch_mode stockage,
		    const med_int ncstp) {

  int i,j,k,l,m,n,nb_geo=0;
  med_int nbpdtnor=0,pflsize,*pflval,ngauss=0,ngroup,nval;
  unsigned char *val = NULL; 
  med_int numdt=0,numo=0,_nprofile;
  med_int meshnumdt=0,meshnumit=0;
  med_size medtype_size=0;
  med_float dt=0.0;
  med_err   ret=0;
  char pflname [MED_NAME_SIZE+1]="";
  char locname [MED_NAME_SIZE+1]="";
  char meshname [MED_NAME_SIZE+1]="";
  char maa_ass [MED_NAME_SIZE+1]="";
  char * lien = NULL;
  med_bool localmesh;
  med_int nmesh=0;
  med_int lnsize=0;
  med_geometry_type * type_geo;

  size_t       _bannerlen=255;
  size_t       _bannerlen1=0,_bannershift1=0;
  char         _temp1[MAXBANNERLEN+1]="";
  char *       _bannerstr1=NULL;
        size_t _bannerlen2=0,_bannershift2=0;
  char         _temp2[MAXBANNERLEN+1]="";
  char *       _bannerstr2=NULL;

  const char * const * AFF;
  const char * const * AFF_ENT=MED23FIELD_GET_ENTITY_TYPENAME+1;
  const char * * AFF_STRUCT = NULL;

  switch (entite) {
  case MED_NODE :
    type_geo = MED23FIELD_GET_NODE_GEOMETRY_TYPE;
    nb_geo   = MED_N_NODE_FIXED_GEO;
    AFF      = MED23FIELD_GET_NODE_GEOMETRY_TYPENAME;
    break;
  case  MED_CELL :
  case  MED_NODE_ELEMENT :
    type_geo = MED23FIELD_GET_CELL_GEOMETRY_TYPE;
    nb_geo   = MED_N_CELL_FIXED_GEO;
    AFF      = MED23FIELD_GET_CELL_GEOMETRY_TYPENAME;
    break;
  case  MED_DESCENDING_FACE :
    type_geo = MED23FIELD_GET_FACE_GEOMETRY_TYPE;
    nb_geo   = MED_N_FACE_FIXED_GEO;
    AFF      = MED23FIELD_GET_FACE_GEOMETRY_TYPENAME;
    break;
  case  MED_DESCENDING_EDGE :
    type_geo = MED23FIELD_GET_EDGE_GEOMETRY_TYPE;
    nb_geo   = MED_N_EDGE_FIXED_GEO;
    AFF      = MED23FIELD_GET_EDGE_GEOMETRY_TYPENAME;
    break;
  case  MED_STRUCT_ELEMENT :
    AFF_STRUCT = (const char * *) calloc(sizeof(const char * ),nmodels+1);
    for(i=0;i<nmodels;++i) AFF_STRUCT[i+1]= &geotypename_elst[(MED_NAME_SIZE+1)*i];
    type_geo = (med_geometry_type*)(geotype_elst)-1;
    nb_geo   = nmodels;
    AFF      = AFF_STRUCT;
    break;
  }

  /*On itère sur les types géométriques ds la boucle externe à cause de la compatibilité 23 :
    En V2.3.6 :
    /CHA/<fieldname>/<entitytype>[.<geotype>]/" ....<numdt> ...<numit>"/<meshname>/MED_NOM_CO
        {MED_NOM_TYP,                        {MED_NOM_NDT,MED_NOM_PDT,  {MED_NOM_NBR,
         MED_NOM_NCO                          MED_NOM_NOR,MED_NOM_MAI    MED_NOM_PFL,
         MED_NOM_NOM,                         MED_NOM_UNI}               MED_NOM_GAU,
         MED_NOM_UNI}                                                    MED_NOM_NGA}
   V3.0 :
    /CHA/<fieldname>/" ....<numdt> ...<numit>"/<entitytype>[.<geotype>]/<pflname>/MED_NOM_CO
        {MED_NOM_TYP, {MED_NOM_PDT,                   {MED_NOM_PFL(2),   {MED_NOM_NBR(1),
        MED_NOM_MAI,   MED_NOM_NDT,MED_NOM_NOR,        MED_NOM_GAUSS(2)}  MED_NOM_GAU(2),
        MED_NOM_NCO,   MED_NOM_RDT,MED_NOM_ROR}        MED_NOM_NGA}
        MED_NOM_NOM,
        MED_NOM_UNI,
        MED_NOM_UNT}

   */
  /*
    TODO : Versionner getFieldsOn en 3.0 pour rendre l'algorithme plus efficace en fonction de la version de fichier lu.
  */
  
  for (k=1;k<=nb_geo;k++) {

    /* Combien de (PDT,NOR) a lire */
    nbpdtnor = ncstp;
    if (nbpdtnor < 1 ) continue;

    for (j=0;j<nbpdtnor;j++) {

      if ( MEDfield23ComputingStepMeshInfo(fid, nomcha,j+1, &numdt, &numo, &dt,
					   &nmesh, maa_ass, &localmesh, &meshnumdt, &meshnumit ) <0) {
	MESSAGE("Erreur a la demande d'information sur (pdt,nor) : ");
	ISCRUTE(numdt); ISCRUTE(numo);ISCRUTE(nmesh);SSCRUTE(meshname);ISCRUTE_int(localmesh);
	ISCRUTE(meshnumdt);ISCRUTE(meshnumit);
	ret = -1; continue;
      }

      for (i=0;i< nmesh;++i) {

	if ( (_nprofile = MEDfield23nProfile(fid,nomcha,numdt,numo,entite,type_geo[k],i+1,
						meshname,pflname,locname   ) ) < 0 ) {
	  MESSAGE("Erreur a la demande du nombre de profils referencés par le champ : ");
	  SSCRUTE(nomcha); ISCRUTE(numdt); ISCRUTE(numo);SSCRUTE(meshname);
	  ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);SSCRUTE(pflname);SSCRUTE(locname);
	  SSCRUTE(AFF_ENT[(int)entite]);SSCRUTE(AFF[k]);
	  ret = -1; continue;
	};

	for (l=0;l<_nprofile;l++) {

	  if ( (nval = MEDfield23nValueWithProfile(fid, nomcha, numdt, numo,  entite, type_geo[k],meshname,
						   l+1,  USER_MODE, pflname,&pflsize,
						   locname, &ngauss) ) < 0 ) {
	    MESSAGE("Erreur a la lecture du nombre de valeurs du champ : ");
	    SSCRUTE(nomcha);ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(meshname);
	    ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	    ISCRUTE_int(USER_MODE);
	    ret = -1; continue;
	  };

	  /* Affiche uniquement les informations du champ pour le maillage demandé <maillage,numit,nmumdt> 
	    Si le maillage demandé n'est pas précisé maillage == "", affiche les informations du champ 
	    pour tous les maillages */
	  if ( ( (!strcmp(meshname,maillage)) && (meshnumdt == mnumdt) && (meshnumit == mnumit)) ||
	       (  !strlen(maillage) )
	     ) {

	    /*4 caractères spéciaux*/
	    _bannerstr1 = "(* CHAMP |%s| A L'ÉTAPE DE CALCUL (n°dt,n°it)="
	      "(% 2.2"MED_IFORMAT",% 2.2"MED_IFORMAT") , %.*s*)\n";
	    /*4 caractères spéciaux */
	    _bannerstr2 = "(* SUR LE MAILLAGE |%s| A L'ÉTAPE DE CALCUL (n°dt,n°it)="
	      "(% 2.2"MED_IFORMAT",% 2.2"MED_IFORMAT") : %.*s*)\n";
	    snprintf(_temp1,MAXBANNERLEN+1,_bannerstr1,nomcha,numdt,numo,0,"");
	    snprintf(_temp2,MAXBANNERLEN+1,_bannerstr2,meshname,meshnumdt,meshnumit,0,"");
	    _bannerlen1 = strlen(_temp1);
	    _bannerlen2 = strlen(_temp2);
	    _bannerlen=MAX(_bannerlen1,_bannerlen2);
	    if (_bannerlen1>_bannerlen2) {
	      _bannershift1 = 0; _bannershift2 = _bannerlen1-_bannerlen2;
	    } else {
	      _bannershift2 = 0; _bannershift1 = _bannerlen2-_bannerlen1;
	    }
	    fprintf(stdout,"\n(");
	    for (i=0;i< _bannerlen-6; i++) fprintf(stdout,"*");
	    fprintf(stdout,")\n");
	    fprintf(stdout,_bannerstr1,nomcha,numdt,numo,_bannershift1,MED_NAME_BLANK);
	    fprintf(stdout,_bannerstr2,meshname,meshnumdt,meshnumit,_bannershift2,MED_NAME_BLANK);
	    fprintf(stdout,"(");
	    for (i=0;i< _bannerlen-6; i++) fprintf(stdout,"*");
	    fprintf(stdout,")\n");



/*   fprintf(stdout,"\n(********************************************************************************)\n"); */
/*   fprintf(stdout,"(* CHAMP |%s| A L'ÉTAPE DE CALCUL (n°dt,n°it)=" */
/* 	  "(% 2.2"MED_IFORMAT",% 2.2"MED_IFORMAT") : *)\n", */
/* 	  nomcha,numdt,numo); */
/*   fprintf(stdout,"(* SUR LE MAILLAGE |%s| A L'ÉTAPE DE CALCUL (n°dt,n°it)=" */
/* 	  "(% 2.2"MED_IFORMAT",% 2.2"MED_IFORMAT") : *)\n", */
/* 	  meshname,meshnumdt,meshnumit); */
/*   fprintf(stdout,"(********************************************************************************)\n\n"); */
  fprintf(stdout,"- Valeur de la date du champ %f [%s] : \n",dt,dtunit);
  fprintf(stdout,"- Type des composantes du champ : %d\n",typcha);
  fprintf(stdout,"- Nombre de composantes par valeur : "IFORMAT"\n",ncomp);
  fprintf(stdout,"- Unité des composantes : |%s|\n",unit);
  fprintf(stdout,"- Nom des composantes : |%s|\n",comp);

	    printf("\t- Il y a "IFORMAT" entités qui portent des valeurs en mode %i."
                   "\n\t  Chaque entite %s de type geometrique %s associes au profil |%s| a "IFORMAT" point(s) d'intégration \n",
		     nval,USER_MODE,AFF_ENT[(int)entite],AFF[k],pflname,ngauss);

	    /* Le maillage reference est-il porte par un autre fichier */
	    if ( !localmesh ) {

	      if ( (lnsize=MEDlinkInfoByName(fid,maa_ass) ) < 0 )  {
	      MESSAGE("Erreur a la lecture de la taille du lien : ");
	      SSCRUTE(maa_ass);
	      ret = -1;
	    } else {
	      lien = (char *)malloc(lnsize*sizeof(char) + 1);
	      EXIT_IF(lien == NULL,NULL,NULL);

	      if ( MEDlinkRd(fid, maa_ass, lien) < 0 )  {
		MESSAGE("Erreur a la lecture du lien : ");
		SSCRUTE(maa_ass);SSCRUTE(lien);
		ret = -1;
	      } else {
		lien[lnsize] = '\0';
		printf("\tLe maillage |%s| est porte par un fichier distant |%s|\n",maa_ass,lien);
	      }
	      free(lien);
	      }
	    }

	    /*Lecture des valeurs du champ */
	    switch(typcha)  {
	    case MED_FLOAT64: medtype_size=sizeof(med_float64); break;
	    case MED_FLOAT32: medtype_size=sizeof(med_float32); break;
	    case MED_INT32  : medtype_size=sizeof(med_int32  ); break;
	    case MED_INT64  : medtype_size=sizeof(med_int64  ); break;
	    case MED_INT    : medtype_size=sizeof(med_int)    ; break;
	    default:
	      MESSAGE("Erreur a la lecture du type de champ : ");
	      ISCRUTE_int(typcha);
              EXIT_IF(NULL == NULL,NULL,NULL);
	    }
	    
	    val = (unsigned char*) calloc(ncomp*nval*ngauss,medtype_size);
	    EXIT_IF(val == NULL,NULL,NULL);
	     

	    if (MEDfield23ValueWithProfileRd(fid, nomcha, numdt,numo, entite,type_geo[k],meshname,
					       USER_MODE, pflname, stockage,MED_ALL_CONSTITUENT,
					     (unsigned char*) val) < 0 ) {
	      MESSAGE("Erreur a la lecture des valeurs du champ : ");
	      SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	      ISCRUTE(numdt);ISCRUTE(numo);
	      ret = -1;
	    }

	    if ( strlen(locname) && (_nprofile > 1) )
	      printf("\t- Modèle de localisation des points de Gauss de nom |%s|\n",locname);

	    if (entite == MED_NODE_ELEMENT)
	      ngroup = (type_geo[k] % 100);
	    else
	      ngroup = ngauss;

	    switch (stockage) {
      
	    case MED_FULL_INTERLACE :
	      if (!structure) {
		printf("\t- Valeurs :\n\t");
		for (m=0;m<(nval*ngauss)/ngroup;m++) {
		  printf("|");
		  for (n=0;n<ngroup*ncomp;n++)
		    switch(typcha)  {
		    case MED_FLOAT64:
		      printf(" %f ",*(((med_double*)val)+(m*ngroup*ncomp)+n  ) );
		      /* printf(" %f ",  ((med_double*)val)[(m*ngroup*ncomp)+n]    ); */
		      /* printf(" %f ", *( val+medtype_size*((m*ngroup*ncomp)+n))  ); */
		      break;
		    case MED_FLOAT32:
		      printf(" %f ",*(((med_float32*)val)+((m*ngroup*ncomp)+n)));
		      break;
		    case MED_INT32  :
		      printf(" %d ",*(((med_int32*)val)+(m*ngroup*ncomp)+n));
		      break;
		    case MED_INT64  :
		      printf(" %ld ",*(((med_int64*)val)+(m*ngroup*ncomp)+n));
		      break;
		    case MED_INT    :
		      printf(" "IFORMAT" ",*(((med_int*)val)+(m*ngroup*ncomp)+n));
		      break;
		    default:
		      break;
		    }
		}
	      }
	      break;

	      
	      /*??? Affichage en fonction du profil à traiter ???*/
            case MED_NO_INTERLACE :
	      if (!structure) {
		printf("\t- Valeurs :\n\t");
		for (m=0;m<ncomp;m++) {
		  printf("|");
		  for (n=0;n<(nval*ngauss);n++)
		    switch(typcha)  {
		    case MED_FLOAT64:
		      printf(" %f ",*(((med_double*)val)+(m*nval*ngauss)+n  ) );
		      /* printf("  %f ",  ((med_double*)val)[(m*nval)+n]    ); */
		      /* printf("  %f ", *( val+medtype_size*((m*nval)+n))  ); */
		      break;
		    case MED_FLOAT32:
		      printf(" %f ",*(((med_float32*)val)+((m*nval*ngauss)+n)));
		      break;
		    case MED_INT32  :
		      printf(" %d ",*(((med_int32*)val)+(m*nval*ngauss)+n));
		      break;
		    case MED_INT64  :
		      printf(" %ld ",*(((med_int64*)val)+(m*nval*ngauss)+n));
		      break;
		    case MED_INT    :
		      printf(" "IFORMAT" ",*(((med_int*)val)+(m*nval*ngauss)+n));
		      break;
		    default:
		      break;
		    }
		}
	      }
	      break;
	    }

	    if (!structure) {
	      printf("|\n");
	    }

	    if ( val ) {free(val);val = NULL;}
	   
	    /*Lecture du profil associe */
	    if (strcmp(pflname,MED_NO_PROFILE) == 0 ) {
	      printf("\t- Profil : MED_NOPFL\n");
	    } else {
	      if ( (pflsize = MEDprofileSizeByName(fid,pflname)) <0 )  {
		MESSAGE("Erreur a la lecture du nombre de valeurs du profil : ");
		SSCRUTE(pflname);
		ret = -1; continue;
	    }

	      printf("\t- Profil : |%s| de taille "IFORMAT"\n",pflname,pflsize);

	      pflval = (med_int*) malloc(sizeof(med_int)*pflsize);
	      EXIT_IF(pflval == NULL,NULL,NULL);
	      if ( MEDprofileRd(fid,pflname,pflval) <0) {
		MESSAGE("Erreur a la lecture des valeurs du profil : ");
		SSCRUTE(pflname);
		ret = -1;
	      }
	      if (!structure) {
		printf("\t");
		for (m=0;m<pflsize;m++) printf(" "IFORMAT" ",*(pflval+m));
		printf("\n");
	      }
	      free(pflval);
	    }
	  }
	}
      }
    }
  } /* fin for sur les mailles*/

  free(AFF_STRUCT);
  return ret;
}

/******************************************************************************
 *
 * - Nom de la fonction : lecture_resultats
 * - Description : lecture et affichage des champs de resultats
 *                 associe a un  maillage MED.
 * - Parametres :
 *     - fid                       (IN) : ID du fichier MED.
 *     - maillage                  (IN) : nom du maillage maillage.
 *     - mode_coo                  (IN) : mode de stockage en memoire :
 *                                        MED_FULL_INTERLACE |
 *                                        MED_NO_INTERLACE.
 *     - lecture_en_tete_seulement (IN) : mode de lecture.
 ******************************************************************************/

void lecture_resultats(const med_idt fid,
		       const char * const    maillage,
		       const med_int mnumdt,
		       const med_int mnumit,
		       const med_switch_mode mode_coo,
		       const med_int             nmodels,
		       const med_geometry_type*  geotype_elst,
		       const char*               geotypename_elst,
		       const int lecture_en_tete_seulement)
{
  med_err ret,lret;
  char *comp, *unit;
  char nomcha  [MED_NAME_SIZE+1]="";
  med_int ncomp,ncha;
  med_field_type typcha;
  int i;

  char     nommaa[MED_NAME_SIZE+1]="";
  med_bool localmaa = MED_FALSE;
  char     dtunit[MED_SNAME_SIZE+1]="";
  med_int  ncstp=0;


  /* combien de champs dans le fichier */
  ncha = MEDnField(fid);
  EXIT_IF(ncha < 0,"lors de la lecture du nombre de champs",NULL);

  if ( !strlen(maillage) || lecture_en_tete_seulement ) {
    fprintf(stdout,"\n(************************)\n");
    fprintf(stdout,"(* CHAMPS RESULTATS   : *)\n");
    fprintf(stdout,"(************************)\n");
    fprintf(stdout,"- Nombre de champs : "IFORMAT" \n",ncha);
  }

  /****************************************************************************
  *                       LECTURE DES CHAMPS                                  *
  ****************************************************************************/
  ret = 0;

  /* lecture de tous les champs pour le maillage selectionne */
  for (i =0;i<ncha;i++) {
    lret = 0;

    /* Lecture du nombre de composantes */
    if ((ncomp = MEDfieldnComponent(fid,i+1)) < 0) {
      MESSAGE("Erreur à la lecture du nombre de composantes : ");
      ISCRUTE(ncomp);
      ret = -1; continue;
    }

    /* Lecture du type du champ, des noms des composantes et du nom de
       l'unité*/
    comp = (char*) malloc(ncomp*MED_SNAME_SIZE+1);
    EXIT_IF(comp == NULL,NULL,NULL);
    unit = (char*) malloc(ncomp*MED_SNAME_SIZE+1);
    EXIT_IF(unit == NULL,NULL,NULL);

    if ( MEDfieldInfo(fid, i+1, nomcha, nommaa, &localmaa,
		     &typcha, comp, unit, dtunit, &ncstp) < 0 ) {
      MESSAGE("Erreur à la demande d'information sur les champs : ");
      ret = -1; continue;
    }


    if ( !strlen(maillage) || lecture_en_tete_seulement ) {
     printf("\nChamp numero : |%d| \n",i+1);
     printf("Nom du champ : |%s| de type |%d|\n",nomcha,typcha);
     printf("Nom des composantes : |%s|\n",comp);
     printf("Unites des composantes : |%s| \n",unit);
     if (strlen(dtunit))
       printf("Unité des dates : |%s|\n",dtunit);
     if ( ncstp > 1 )
       printf("Nombre d'étapes de calcul : |"IFORMAT"| \n",ncstp);
    }


    if (lecture_en_tete_seulement) {
      free(comp);
      free(unit);
      continue;
    }

    /* champs aux noeuds */
    lret = getFieldsOn(fid, maillage, mnumdt, mnumit, nmodels,geotype_elst,geotypename_elst,
		       nomcha, dtunit, typcha, ncomp, comp, unit,  MED_NODE,mode_coo, ncstp);

    /* champs sur les elements et aux points de Gauss */
    if (lret == 0) lret = getFieldsOn(fid, maillage, mnumdt, mnumit , nmodels,geotype_elst,geotypename_elst,
				      nomcha, dtunit, typcha, ncomp, comp, unit,  MED_CELL,mode_coo, ncstp);
    else { MESSAGE("Erreur à la lecture des champs aux noeuds "); ret = -1; continue;}
   
    if (lret == 0) lret = getFieldsOn(fid, maillage, mnumdt, mnumit , nmodels,geotype_elst,geotypename_elst,
				      nomcha, dtunit, typcha, ncomp, comp, unit,  MED_DESCENDING_FACE,mode_coo, ncstp);
    else { MESSAGE("Erreur à la lecture des champs aux mailles "); ret = -1; continue;}
   
    if (lret == 0) lret = getFieldsOn(fid, maillage, mnumdt, mnumit , nmodels,geotype_elst,geotypename_elst,
				      nomcha, dtunit, typcha, ncomp, comp, unit,  MED_DESCENDING_EDGE,mode_coo, ncstp);
    else {MESSAGE("Erreur à la lecture des champs aux faces "); ret = -1; continue;}
    
    if (lret == 0) lret = getFieldsOn(fid, maillage, mnumdt, mnumit , nmodels,geotype_elst,geotypename_elst,
				      nomcha, dtunit, typcha, ncomp, comp, unit,  MED_NODE_ELEMENT,mode_coo, ncstp);
    else {MESSAGE("Erreur a la lecture des champs aux aretes "); ret = -1; continue;}

    if  (lret != 0) {MESSAGE("Erreur a la lecture des champs aux noeuds des mailles "); ret = -1;};

    if (nmodels)
      lret = getFieldsOn(fid, maillage, mnumdt, mnumit , nmodels, geotype_elst,geotypename_elst,
			 nomcha, dtunit, typcha, ncomp, comp, unit,  MED_STRUCT_ELEMENT,mode_coo, ncstp);
    if  (lret != 0) {MESSAGE("Erreur a la lecture des champs aux éléments de sructure "); ret = -1;};
 
    free(comp);
    free(unit);

  }

  return;
}

/******************************************************************************
 *
 * - Nom de la fonction : lecture_parametres_scalaires
 * - Description : lecture des parametres scalaires definis
 *                 hors champs et maillages.
 * - Parametres :
 *     - fid                    (IN) : ID du fichier MED.
 *     - lecture_en_tete_seule  (IN) : mode de lecture.
 *
 ******************************************************************************/

void lecture_parametres_scalaires(med_idt fid,
				  int lecture_en_tete_seulement)
{
  med_err ret = 0;
  char nom_scalaire[MED_NAME_SIZE+1];
  char description[MED_COMMENT_SIZE+1];
  med_int vali;
  med_float valr;
  med_int i,n,npdt,j;
  med_parameter_type  type;
  med_int numdt,numo;
  med_float dt;
  char dt_unit[MED_SNAME_SIZE+1];


  /* Combien de variables scalaire ? */
  n = MEDnParameter(fid);
  EXIT_IF(n < 0,"lors de la lecture du nombre de scalaires",NULL);

  if (n) {
   fprintf(stdout,"\n(*******************************)\n");
   fprintf(stdout,"(* VARIABLES SCALAIRES       : *)\n");
   fprintf(stdout,"(*******************************)\n\n");
   fprintf(stdout,"- Nombre de variables scalaires : "IFORMAT"\n",n);
  }
  if (lecture_en_tete_seulement)
    return ;

  for (i=1;i<=n;i++) {

    /* Lecture des infos (type,description) */
    ret = MEDparameterInfo( fid,i,nom_scalaire,&type,description,
			    dt_unit, &npdt );
    EXIT_IF(ret < 0,"lors de la lecture des parametres d'un scalaire",NULL);
    fprintf(stdout,"- Scalaire n°"IFORMAT" de nom %s \n",i,nom_scalaire);
    if (type == MED_FLOAT64)
      fprintf(stdout,"  Type flottant. \n");
    else
      fprintf(stdout,"  Type entier. \n");
    printf("  Description associee : [%s] \n",description);

    /* Pour chaque scalaire on regarde les valeurs associees
       eventuellement a des pas de temps et des numeros d'ordre */
    EXIT_IF(npdt < 0,
	    "lors de la lecture du nombre de pas de temps d'un scalaire"
	    ,NULL);
    fprintf(stdout,"   Nombre de valeurs stockees : "IFORMAT" \n",npdt);

    for (j=1;j<=npdt;j++) {

      ret = MEDparameterComputationStepInfo(fid,nom_scalaire,j, &numdt,&numo,&dt);
      EXIT_IF(ret < 0,
	      "lors de la lecture des parametres d'un pas de temps d'un scalaire",
	      NULL);

      if (numdt == MED_NO_DT)
	fprintf(stdout,"   - Aucun de pas de temps \n");
      else
	fprintf(stdout,
		"   - Pas de de temps de numero "IFORMAT" de valeur %f [%s] \n",numdt,
	       dt,dt_unit);
      if (numo == MED_NO_IT)
	fprintf(stdout,"   - Aucun numero d'ordre \n");
      else
	fprintf(stdout,"   - Numero d'ordre : "IFORMAT" \n",numo);

      if (type == MED_FLOAT64) {
	ret = MEDparameterValueRd(fid,nom_scalaire,numdt,numo,(unsigned char * ) &valr);
	fprintf(stdout,"   - Valeur : %f \n",valr);
      }
      else {
	ret = MEDparameterValueRd(fid,nom_scalaire,numdt,numo,(unsigned char * ) &vali);
	fprintf(stdout,"   - Valeur : "IFORMAT" \n",vali);
      }
      EXIT_IF(ret < 0,"lors de la lecture de la valeur d'un scalaire",NULL);

    }
  }

  return ;
}

/******************************************************************************
 *
 * - Nom de la fonction : lecture_profils
 * - Description : lecture des différents profils
 *                 hors champs et maillages.
 * - Parametres :
 *     - fid                    (IN) : ID du fichier MED.
 *     - lecture_en_tete_seule  (IN) : mode de lecture.
 *
 ******************************************************************************/

void lecture_profils(med_idt fid,
		     int lecture_en_tete_seulement)
{
  med_err ret;
  char pflname[MED_NAME_SIZE+1]="";
  med_int npro,*pflval,nval;
  int i,j;


  /* Interrogation des profils */
  npro = MEDnProfile(fid);
  EXIT_IF(npro < 0,"lors de la lecture du nombre de profils",NULL);

  if (npro) {
    fprintf(stdout,"\n(*************)\n");
    fprintf(stdout,  "(* PROFILS : *)\n");
    fprintf(stdout,  "(*************)\n");
    printf("\nNombre de profils stockes : "IFORMAT"\n\n",npro);
  }

  for (i=1 ; i <= npro ; i++ ) {
    if ( MEDprofileInfo(fid, i, pflname, &nval) < 0)  {
      MESSAGE("Erreur a la demande d'information sur le profil n° : "); ISCRUTE_int(i);
      ret = -1;continue;
    }
    printf("\t- Profil n°%i de nom |%s| et de taille "IFORMAT"\n",i,pflname,nval);
    pflval = (med_int*) malloc(sizeof(med_int)*nval);
    if ( MEDprofileRd(fid, pflname, pflval) < 0) {
      MESSAGE("Erreur a la lecture des valeurs du profil : ");
      SSCRUTE(pflname);
      ret = -1;
    } else {
      if (!structure) {
        printf("\t");
        for (j=0;j<nval;j++) printf(" "IFORMAT" ",*(pflval+j));
      }
      printf("\n\n");
    }
    free(pflval);
  }
    return;
}

/******************************************************************************
 *
 * - Nom de la fonction : lecture_modeles_elstruct
 * - Description : lecture des différents modèles d'éléments de structure
 *                 hors champs et maillages.
 * - Parametres :
 *     - fid                    (IN) : ID du fichier MED.
 *     - lecture_en_tete_seule  (IN) : mode de lecture.
 *
 ******************************************************************************/

void lecture_modeles_elstruct(med_idt fid,
			      int lecture_en_tete_seulement)
{
  med_err           _ret=0;
  int               _i  =0,_j=0,_k=0, _n=0,_nvalue=0;
  med_int           _nstructelement=0;

  med_geometry_type _geotype=MED_NONE;

  char              _elementname[MED_NAME_SIZE+1]="";
  med_int           _elementdim=0;
  char              _supportmeshname[MED_NAME_SIZE+1]="";
  med_entity_type   _entitytype=MED_UNDEF_ENTITY_TYPE;
  med_int           _nnode=0;
  med_int           _ncell=0;
  med_geometry_type _geocelltype=MED_NONE;
  char              _geocelltypename[MED_SNAME_SIZE+1]="";
  med_int           _nconstantattribute=0;
  med_bool          _anyprofile=MED_FALSE;
  med_int           _nvariableattribute=0;

  char               _constattname[MED_NAME_SIZE+1]="";
  med_attribute_type _constatttype=MED_ATT_UNDEF;
  char               _varattname[MED_NAME_SIZE+1]="";
  med_attribute_type _varatttype=MED_ATT_UNDEF;
  med_int            _ncomponent=0;
  med_entity_type    _attentitytype=MED_UNDEF_ENTITY_TYPE;
  char               _profilename[MED_NAME_SIZE+1]="";
  med_int            _profilesize=0;

  unsigned char *    _value=NULL;
  void             (*_printf)(const void*);
  med_int            _atttypesize=0;

  _nstructelement = MEDnStructElement(fid);
  EXIT_IF(_nstructelement < 0,"lors de la lecture du nombre d'éléments de structure",NULL);

  if(_nstructelement) {
    fprintf(stdout,"\n(*************************************)\n");
    fprintf(stdout,  "(* MODELES D'ELEMENTS DE STRUCTURE : *)\n");
    fprintf(stdout,  "(*************************************)\n");
    printf("\nNombre d'éléments de structure : "IFORMAT"\n\n",_nstructelement);
  }

  for ( _i=1; _i<= _nstructelement; ++_i) {

    _ret= MEDstructElementInfo(fid,_i,_elementname,&_geotype,&_elementdim,_supportmeshname,
			     &_entitytype,&_nnode,&_ncell,&_geocelltype,&_nconstantattribute,&_anyprofile,
			     &_nvariableattribute );
    EXIT_IF(_ret < 0,"lors de la demande d'information sur les éléments de structure",NULL);

    fprintf(stdout,"\nElément de structure n°%d |%s| de type géométrique n°%d et de dimension "IFORMAT"\n",
	    _i,_elementname,_geotype,_elementdim);
    if ( strlen(_supportmeshname) ) {
      fprintf(stdout,"\t Maillage support de nom |%s|",_supportmeshname);
      if (_ncell) {
	MEDmeshGeotypeName(fid,_geocelltype,_geocelltypename);
	fprintf(stdout," avec "IFORMAT" maille(s) de type |%s| et",_ncell,_geocelltypename);
      }
      if (_nnode)
	fprintf(stdout," avec "IFORMAT" noeud(s)\n",_nnode);
      else {
	fprintf(stderr,"\n Erreur : les noeuds doivent être définis s'il existe un maillage support\n");
      }
    } else
      fprintf(stdout,"\t Maillage support implicite sur noeud\n");

    fprintf(stdout,"\t Nombre d'attribut(s) constant(s) : "IFORMAT"",_nconstantattribute);
    if (_anyprofile) fprintf(stdout,", avec profil.\n"); else fprintf(stdout,", sans profil.\n");

    if ( _nconstantattribute ) {
      for (_j=1;_j<=_nconstantattribute;++_j) {
	_ret= MEDstructElementConstAttInfo(fid, _elementname,_j,
					   _constattname, &_constatttype, &_ncomponent,
					   &_attentitytype, _profilename, &_profilesize );
	  EXIT_IF(_ret < 0,"lors de la demande d'information sur les attributs constants des éléments de structure",NULL);

	fprintf(stdout,"\t\t Attribut constant de nom |%s| de type %d à "IFORMAT" composantes\n",
		_constattname,_constatttype,_ncomponent);

        if (!_profilesize) {
	  if (_attentitytype == MED_NODE) _nvalue = _nnode; else _nvalue=_ncell;
	} else {
	  _nvalue = _profilesize;
	}
	_n     = _ncomponent*_nvalue;
	if ( _constatttype == MED_ATT_NAME) ++_n;
        _atttypesize = MEDstructElementAttSizeof(_constatttype);
	_value = (unsigned char *) malloc(_n*_atttypesize);
	if ( _constatttype == MED_ATT_NAME) --_n;

        _ret= MEDstructElementConstAttRd(fid, _elementname,_constattname, _value );
        if (_ret < 0 ) free(_value);
        EXIT_IF(_ret < 0,"lors de la lecture des valeurs des attributs constants",NULL);
        _printf=MEDstructPrintFunction(_constatttype);

        if (!structure) {
	  fprintf(stdout,"\t\t  - Valeurs de l'attribut sur les d'entité |%s|",
		  MED23MESH_GET_ENTITY_TYPENAME[_attentitytype+1]);
	  if ( _profilesize)
	    fprintf(stdout," avec un profil |%s| de taille "IFORMAT": ",_profilename,_profilesize);
	  else
	    fprintf(stdout," sans profil : ");

          for (_k=0;_k<_nvalue*_ncomponent;_k++) {
/* 	    if ( ( _ncomponent > 1 ) &&  !(_k % _ncomponent) ) */
	    if (  !(_k % _ncomponent) )
	      fprintf(stdout,"\n [ %5"MED_IFORMAT" ] : ", (_k/_ncomponent +1) );
	    _printf( (void *)( (char *)(_value) + _k*_atttypesize) );
          }
        printf("\n");
        }
        /* free memory */
        free(_value);
        printf("\n");
      } /*fin boucle sur constatt*/
    }   /*fin if _nconstantattribute */

    fprintf(stdout,"\t Nombre d'attribut(s) variable(s) : "IFORMAT"\n",_nvariableattribute);
    if ( _nvariableattribute ) {
      for (_j=1;_j<=_nvariableattribute;++_j) {
	_ret = MEDstructElementVarAttInfo(fid, _elementname,_j,_varattname,&_varatttype,&_ncomponent );
        EXIT_IF(_ret < 0,"lors de la lecture des valeurs des attributs variables",NULL);
        fprintf(stdout,"\t\t Attribut variable de nom |%s| de type %d à "IFORMAT" composantes\n",
      	  _varattname,_varatttype,_ncomponent);
      }
    }

  }

 return;
}

/******************************************************************************
 *
 * - Nom de la fonction : lecture_fonctions_interpolation
 * - Description : lecture des différentes fonctions d'interpolation
 *                 hors champs et maillages.
 * - Parametres :
 *     - fid                    (IN) : ID du fichier MED.
 *     - lecture_en_tete_seule  (IN) : mode de lecture.
 *
 ******************************************************************************/

void lecture_fonctions_interpolation(med_idt fid,
				     int lecture_en_tete_seulement)
{

  med_err           _ret=-1;
  med_int           _ninterp=0;
  int               _interpit                   =0;
  char              _interpname[MED_NAME_SIZE+1]="";
  med_geometry_type _geotype                    =MED_NONE;
  char              _geotypename[MED_SNAME_SIZE+1]="";
  med_int           _geodim=0,_geonnodes=0;
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


  _ninterp = MEDnInterp(fid);
  if (_ninterp) {
    fprintf(stdout,"\n(********************************)\n");
    fprintf(stdout,  "(* FONCTIONS D'INTERPOLATION  : *)\n");
    fprintf(stdout,  "(********************************)\n");
    printf("\nNombre de fonctions d'interpolation : "IFORMAT"\n\n",_ninterp);
  }

  for ( _interpit=1; _interpit<= _ninterp; ++_interpit) {

    if (MEDinterpInfo(fid,_interpit,_interpname,&_geotype,&_cellnodes,&_nbasisfunc,
		      &_nvariable,&_maxdegree,&_nmaxcoefficient) < 0 ) {
      MESSAGE("Erreur à la demande d'information de la fonction d'interpolation n°");
      ISCRUTE_int(_interpit);
      _ret = -1;continue;
    }

    MEDmeshGeotypeName(fid,_geotype,_geotypename);
    fprintf(stdout,"Fonction d'interpolation n° %d |%s| sur le type géométrique |%s|\n",
	    _interpit,_interpname, _geotypename);

    if ( MEDmeshGeotypeParameter(fid,_geotype,&_geodim,&_geonnodes) <0 ) {
      MESSAGE("Erreur à la lecture du nom associé au typegeo : "); ISCRUTE_int(_geotype);
      _ret = -1;continue;
    }

    if ( _cellnodes ) {
      if ( _nbasisfunc == _geonnodes )
	fprintf(stdout,"\t Les noeuds de construction sont les noeuds de la maille de référence.\n");
      else {
	MESSAGE("Erreur : le nombre de noeuds de construction "\
		"est différent du nombre de noeuds de la maille de référence.\n");
	ISCRUTE(_nbasisfunc); ISCRUTE(_geonnodes);
	_ret = -1;continue;
      }
    }

/*     if ( _nvariable != _geodim ) { */
/* 	MESSAGE("Erreur : le nombre de variables  "\ */
/* 		"est différent de la dimension de l'espace de la maille de référence.\n"); */
/* 	ISCRUTE(_nvariable); ISCRUTE (_geodim); */
/* 	_ret = -1;continue; */
/*     } else */
      fprintf(stdout,"\t Il y a  "IFORMAT" fonctions de base avec "IFORMAT" variables\n ",_nbasisfunc,_nvariable);
    fprintf(stdout,"\t Le degré maximum des fonctions de base est "IFORMAT" et possèdent au maximum "IFORMAT" coefficients\n"
	    ,_maxdegree,_nmaxcoefficient);


    _coefficient = (med_float*) calloc(sizeof(med_float),_nmaxcoefficient);
    _power       = (med_int*)   calloc(sizeof(med_int),_nvariable*_nmaxcoefficient);

    for ( _basisfuncit=1; _basisfuncit<= _nbasisfunc; ++_basisfuncit) {


      if ( (_ret = MEDinterpBaseFunctionRd( fid,_interpname,_basisfuncit,&_ncoefficient,_power,_coefficient) <0) ) {
	MESSAGE("Erreur à la lecture de la fonction de base n°");ISCRUTE_int(_basisfuncit);
	_ret=-1;continue;
      } else {
        if (!structure) {

  	fprintf(stdout,"\n\t Tableau de coefficients de la fonctions de base n° %d :\n\t",_basisfuncit);
  	for ( _coefficientit=1; _coefficientit<= _ncoefficient; ++_coefficientit)
  	  fprintf(stdout," %4f ",_coefficient[_coefficientit]);
  
  	fprintf(stdout,"\n\t Tableau de puissances de la fonctions de base n° %d :\n\t",_basisfuncit);
  	for ( _powerit=1; _powerit<= _nvariable*_ncoefficient; ++_powerit)
  	  fprintf(stdout," %4"MED_IFORMAT" ",_power[_powerit]);
        }
      }
    }
    fprintf(stdout,"\n");
    free(_coefficient);
    free(_power);

  }

  return;
}

/******************************************************************************
 *
 * - Nom de la fonction : lecture_liens
 * - Description : lecture des différents liens
 *                 hors champs et maillages.
 * - Parametres :
 *     - fid                    (IN) : ID du fichier MED.
 *     - lecture_en_tete_seule  (IN) : mode de lecture.
 *
 ******************************************************************************/

void lecture_liens(med_idt fid,med_bool montage,
		   int lecture_en_tete_seulement)
{
  med_err ret=0;
  char nomlien[MED_NAME_SIZE+1]="";
  char *lien = NULL;
  med_int nln=0,nval=0;
  int i;


  /* Interrogation des liens */
  nln = MEDnLink(fid);

  if (nln) {
    fprintf(stdout,"\n(***********)\n");
    fprintf(stdout,  "(* LIENS : *)\n");
    fprintf(stdout,  "(***********)\n");
    printf("\nNombre de liens : "IFORMAT"\n\n",nln);
  }

  for (i=1 ; i <= nln ; i++ ) {
    if ( MEDlinkInfo(fid, i, nomlien, &nval) < 0)  {
      MESSAGE("Erreur a la demande d'information sur le lien n° : "); ISCRUTE_int(i);
      ret = -1;continue;
    }
    printf("\t- Lien n°%i de nom |%s| et de taille "IFORMAT"\n",i,nomlien,nval);

    lien = (char * ) malloc((nval+1)*sizeof(char));
    EXIT_IF(lien == NULL,NULL,NULL);

    if ( MEDlinkRd(fid, nomlien, lien ) < 0 )  {
      MESSAGE("Erreur a la lecture du lien : ");
      SSCRUTE(nomlien);SSCRUTE(lien);
      ret = -1;
    } else {
      lien[nval] = '\0'; /*On s'assure qu'il y a un terminateur de chaîne */
      printf("\t\t|%s|\n\n",lien);

      /*Si le montage des liens est demandé : montage les liens */
      if (montage)
	if (( FIDS.array[FIDS.n++]=MEDfileObjectsMount(fid,  lien,MED_MESH)) < 0 ) {
	  printf("Erreur au montage du lien : |%s|\n",lien);
	  FIDS.array[FIDS.n--]=0;
	  ret=-1;
	}
    }
    free(lien);
  }
  return;
}

/******************************************************************************
 *
 * - Nom de la fonction : lecture_localisation
 * - Description : lecture des différentes localisations
 *                 hors champs et maillages.
 * - Parametres :
 *     - fid                    (IN) : ID du fichier MED.
 *     - mode_coo               (IN) : mode de stockage en memoire :
 *                                        MED_FULL_INTERLACE |
 *                                        MED_NO_INTERLACE.
 *     - lecture_en_tete_seule  (IN) : mode de lecture.
 *
 ******************************************************************************/

void lecture_localisation(med_idt fid,
			  const med_switch_mode mode_coo,
			  int lecture_en_tete_seulement)
{
  med_err ret = 0;
  med_int nloc=0,locsdim=0,ngauss=0;
  med_geometry_type type_geo;
  med_float         *refcoo=NULL, *gscoo=NULL, *wg=NULL;
  char locname [MED_NAME_SIZE+1]="";
  char    geointerpname       [MED_NAME_SIZE+1]="";
  char    ipointstructmeshname[MED_NAME_SIZE+1]="";
  med_int nsectionmeshcell = 0;
  med_geometry_type sectiongeotype;
  char              sectiongeotypename[MED_NAME_SIZE+1]="";
  med_int locentdim=0;
  med_int locnnodes=0;
  char    _locgeotypename[MED_NAME_SIZE+1]="";
  int     t1=0,t2=0,t3=0;
  int     i=0,j=0;


  /* Interrogation des localisations des points de GAUSS */
  nloc = MEDnLocalization(fid);
  if (nloc) {
    fprintf(stdout,"\n(********************************************)\n");
    fprintf(stdout,  "(* LOCALISATIONS DES POINTS D'INTEGRATION : *)\n");
    fprintf(stdout,  "(********************************************)\n");
    printf("\nNombre de localisations stockees : "IFORMAT"\n\n",nloc);
  }

  for (i=1 ; i <= nloc ; i++ ) {
    if ( MEDlocalizationInfo(fid, i, locname, &type_geo, &locsdim, &ngauss,
			     geointerpname, ipointstructmeshname, &nsectionmeshcell,
			     &sectiongeotype) < 0)  {
      MESSAGE("Erreur a la demande d'information sur la localisation n° : "); ISCRUTE_int(i);
      ret = -1;continue;
    }
    printf("\t- Loc. n°%i de nom |%s| de dimension "IFORMAT" avec "IFORMAT" pts de GAUSS \n",i,locname,locsdim,ngauss);

    if ( MEDmeshGeotypeName(fid, type_geo,_locgeotypename) <0 ) {
	MESSAGE("Erreur à la lecture du nom associé au typegeo : "); ISCRUTE_int(type_geo);
	ret = -1;continue;
    }

    if ( MEDmeshGeotypeParameter(fid, type_geo,&locentdim,&locnnodes) <0 ) {
	MESSAGE("Erreur à la lecture du nom associé au typegeo : "); ISCRUTE_int(type_geo);
	ret = -1;continue;
    }

    if (strlen(ipointstructmeshname)) {
      if ( MEDmeshGeotypeName(fid, sectiongeotype,sectiongeotypename) <0 ) {
	MESSAGE("Erreur à la lecture du nom associé au sectiongeotype : "); ISCRUTE_int(sectiongeotype);
	SSCRUTE(ipointstructmeshname);
	ret = -1;continue;
      }
    }

    t1 = locnnodes*locsdim;
    t2 = ngauss*locsdim;
    t3 = ngauss;
    refcoo = (med_float *) malloc(sizeof(med_float)*t1 );
    gscoo  = (med_float *) malloc(sizeof(med_float)*t2 );
    wg     = (med_float *) malloc(sizeof(med_float)*t3 );

    if ( MEDlocalizationRd(fid, locname, mode_coo, refcoo, gscoo, wg  ) < 0) {
      MESSAGE("Erreur a la lecture des valeurs de la localisation : ");
      SSCRUTE(locname);
      ret = -1;
    } else {
      if (!structure) {
        printf("\t  Coordonnees de l'element de reference de type |%s| :\n\t\t",_locgeotypename);
/*         for (j=0;j<t1;j++) printf(" %f ",*(refcoo+j)); */
        for (j=0;j<locnnodes*locsdim;j++) {
         if (mode_coo == MED_FULL_INTERLACE && !(j % locsdim))
           fprintf(stdout,"\n [ %5"MED_IFORMAT" ] : ", (j/locsdim + 1) );
         if (mode_coo == MED_NO_INTERLACE && ! (j % locnnodes))
	   fprintf(stdout,"\n\n ");
         fprintf(stdout," %-+9.6f ",*(refcoo+j));
        }
        printf("\n");
        printf("\t  Localisation des points de GAUSS : \n\t\t");
/*         for (j=0;j<t2;j++) printf(" %f ",*(gscoo+j)); */
        for (j=0;j<ngauss*locsdim;j++) {
         if (mode_coo == MED_FULL_INTERLACE && !(j % locsdim))
           fprintf(stdout,"\n [ %5"MED_IFORMAT" ] : ", (j/locsdim + 1) );
         if (mode_coo == MED_NO_INTERLACE && ! (j % ngauss))
	   fprintf(stdout,"\n\n ");
         fprintf(stdout," %-+9.6f ",*(gscoo+j));
	}
        printf("\n");
        printf("\t  Poids associes aux points de GAUSS :\n\t\t");
        for (j=0;j<t3;j++) printf(" %f ",*(wg+j));
        printf("\n");
      }
      if (strlen(ipointstructmeshname)) {
	printf("\t  Maillage support de section d'élément de structure |%s|.\n",ipointstructmeshname);
	printf("\t  Mailles des sections d'élément de structure de type |%s|.\n",sectiongeotypename);
	printf("\t  Nombre de mailles par section d'élément de structure : "IFORMAT".\n",nsectionmeshcell);
      }
      if (strlen(geointerpname)) {
	printf("\t  Tranformation géométrique associée à la localisation |%s|.\n",geointerpname);
      }
      printf("\n\n");
    }
    free(refcoo);
    free(gscoo);
    free(wg);
  }

  return;
}


med_idt ouverture_fichier_MED(char *fichier)
{
  med_idt fid;
  med_err ret = 0;
  med_int majeur,mineur,release;
  med_bool    hdfok;
  med_bool    medok;

  /* on regarde si le fichier existe */
  ret = (int) access(fichier,F_OK);
  if (ret <0) { SSCRUTE(fichier);}
  EXIT_IF(ret < 0,"Le fichier n'est pas accessible ou n'existe pas ",
	  fichier);

  /* on regarde s'il s'agit d'un fichier au format HDF 5 */
  ret = MEDfileCompatibility (fichier,&hdfok, &medok );
  EXIT_IF(ret < 0,"Impossible de déterminer la compatibilité de format. ",
	  fichier);

  EXIT_IF(!hdfok,"Le fichier n'est pas dans un format HDF compatible ", fichier);
  EXIT_IF(!medok,"Le fichier n'est pas dans un format MED compatible ", fichier);

  /* Quelle version de MED est utilise par mdump ? */
  MEDlibraryNumVersion(&majeur,&mineur,&release);
  fprintf(stdout,
	  "- Lecture du fichier à l'aide de la bibliotheque MED V"IFORMAT"."IFORMAT"."IFORMAT" \n",
	  majeur,mineur,release);

  /* Ouverture du fichier MED en lecture seule */
  /* Le mode lecture seul ne permet pas le montage de fichier distants*/
  fid = MEDfileOpen(fichier,MED_ACC_RDONLY);
  EXIT_IF( fid < 0,"Ouverture du du fichier ",fichier);

  MEDfileNumVersionRd(fid, &majeur, &mineur, &release);
  EXIT_IF(( (majeur < 2) || ( (majeur == 2) && (mineur < 2)) ), "Le fichier est antérieur à la version 2.2", NULL);

  return fid;
}

void lecture_en_tete(med_idt fid,char* fichier)
{
  char fichier_en_tete[MED_COMMENT_SIZE+1];
  med_err ret = 0;

  /* lecture de l'en-tete du fichier (optionnel) */
  /* on va lire dans le fichier */
  ret = MEDfileCommentRd(fid,fichier_en_tete);

  /* on affiche */
  if (ret >= 0)
    fprintf(stdout,"- En-tete du fichier : %s \n",fichier_en_tete);

  return;
}

void parametrage(med_switch_mode *mode_coo,
		 med_connectivity_mode *typ_con)
{
  int reponse;
  char _temp[256]="";

  fprintf(stdout,"(*****************)\n");
  fprintf(stdout,"(* PARAMETRAGE : *)\n");
  fprintf(stdout,"(*****************)\n");
  fprintf(stdout,"- Mode d'affichage des coordonnées des noeuds ? \n");
  fprintf(stdout,"  1. Mode entrelacé : taper 1 \n");
  fprintf(stdout,"  2. Mode non entrelacé : taper 2 \n");
  reponse = 0;
  do {
    fprintf(stdout,"  Reponse : ");
    if (!scanf("%d",&reponse)) fgets(_temp, 256, stdin) ;
  } while (reponse != 1 && reponse != 2);
  if (reponse == 1)
    *mode_coo = MED_FULL_INTERLACE;
  else
    *mode_coo = MED_NO_INTERLACE;

  fprintf(stdout,"- Connectivité des éléments ? \n");
  fprintf(stdout,"  1. Nodale : taper 1 \n");
  fprintf(stdout,"  2. Descendante : taper 2 \n");
  reponse = 0;
  do {
    fprintf(stdout,"  Reponse : ");
    if (!scanf("%d",&reponse)) fgets(_temp, 256, stdin) ;
  } while (reponse != 1 && reponse != 2);
  if (reponse == 1)
    *typ_con = MED_NODAL;
  else
    *typ_con = MED_DESCENDING;

  return;
}


void lecture_information_maillage(const med_idt fid,
				  const int numero,
				  char *          nommaa,
				  med_int * const mdim,
				  med_int * const edim,
				  med_mesh_type * const type_maillage,
				  char * const maillage_description,
				  med_int * const nstep,
				  char    * const dtunit,
				  char    * const nomcoo,
				  char    * const unicoo,
				  med_axis_type *const rep)
{
  char nom_universel[MED_LNAME_SIZE+1];
  med_err ret = 0;
  med_sorting_type sortingtype;

  fprintf(stdout,"\n(**********************************************************)\n");
  fprintf(stdout,"(* INFORMATIONS GENERALES SUR LE MAILLAGE DE CALCUL N°%2.2d: *)\n",numero);
  fprintf(stdout,"(**********************************************************)\n\n");

  /* lecture du nom et de la dimension du maillage */
  ret = MEDmeshInfo(fid, numero,nommaa,edim,mdim,type_maillage,maillage_description,
		    dtunit,&sortingtype,nstep,rep,nomcoo,unicoo);
  EXIT_IF(ret < 0,"Lecture des informations sur le maillage",NULL);

  /* affichage des donnees lues */
  fprintf(stdout,"- Nom du maillage : <<%s>>\n",nommaa);
  fprintf(stdout,"- Dimension du maillage : "IFORMAT"\n",*mdim);
  if (*edim > *mdim)
    fprintf(stdout,"- La dimension de l'espace est "IFORMAT" \n",*edim);
  if (*type_maillage == MED_UNSTRUCTURED_MESH)
    fprintf(stdout,"- Type du maillage : MED_NON_STRUCTURE \n");
  else
    fprintf(stdout,"- Type du maillage : MED_STRUCTURE \n");
  fprintf(stdout,"- Description associee au maillage : %s\n",
	  maillage_description);

  if ( *nstep > 1 )
    fprintf(stdout,"- Nombre d'étapes de calcul associées au maillage : "IFORMAT"\n",
	    *nstep);
  if (strlen(dtunit))
    fprintf(stdout,"- Unité des dates d'étapes de calcul associées au maillage : %s\n",
	    dtunit);

 /* lecture du nom universel (presence optionnelle) */
  ret = MEDmeshUniversalNameRd(fid,nommaa,nom_universel);
  if (ret == 0)
   fprintf(stdout,"- Nom universel du maillage : %s \n",nom_universel);

 return;
}

/******************************************************************************
 *
 * - Nom de la fonction : main
 * - Description : fonction "main" de l'outil de DUMP d'un fichier MED.
 * - Parametres :
 *     - argc  (IN) : nombre d'arguments sur la ligne de commande.
 *     - argv  (IN) : liste des arguments.
 *
 ******************************************************************************/

int main (int argc, char **argv)
{
  med_err ret = 0;
  med_idt fid;
  int     i=0,numero=0,firstmesh=0,lastmesh=0,meshit=0;
  med_switch_mode       mode_coo = MED_UNDEF_INTERLACE;
  med_connectivity_mode typ_con  = MED_UNDEF_CONNECTIVITY_MODE;
  int           lecture_en_tete_seulement = 0;
  med_int       mdim=0,nmaa=0,nmaasup=0;
  char          nommaa[MED_NAME_SIZE+1];
  char          maillage_description[MED_COMMENT_SIZE+1];
  med_mesh_type type_maillage;
  med_int       edim;
  int           decalage;
  char          nomcoo[3*MED_SNAME_SIZE+1]="";
  char          unicoo[3*MED_SNAME_SIZE+1]="";
  char          dtunit[MED_SNAME_SIZE+1]="";
  med_int       nstep=0,numdt=MED_NO_DT,numit=MED_NO_IT;
  int           csit=0;
  med_float     dt=0.0;
  med_axis_type rep;

  /*Gestion des paramètres de la ligne de commande*/
  char          *filename=NULL,*typ_con_param=NULL,*mode_coo_param=NULL;
  size_t        _bannerlen=0;
  char          _temp[MAXBANNERLEN+1]="";
  char *        _bannerstr=NULL;
  /* Focntionnalité non encore activée.*/
  med_bool      _montage=MED_FALSE;

  /*Modèles d'élements de structure utilisés par le maillage spécifié*/
  /*Celà permet de demander les champs uniquement sur ces modèles*/
  med_int               _nmodels=0;
  med_geometry_type    *_geotype_elst     = NULL;
  char                 *_geotypename_elst = NULL;


  /****************************************************************************
  *                  TEST DU NOMBRE D'ARGUMENTS                               *
  ****************************************************************************/

  structure = 0;
  decalage  = 0;
  
  if (argc > 2 && strcmp(argv[1], "--structure") == 0) {
    --argc;++decalage;
    structure = 1;
  }

  /*S'il y a deux arguments nous sommes en interactif, sinon il en faut 5*/
  if ( (argc !=  2) && (argc !=  5) ) {
/*     fprintf(stderr,"Utilisation mdump [--structure] monfichier.med\n"); */
    fprintf(stderr,"Utilisation mdump [--structure] monfichier.med [ NODALE|DESCENDANTE "
	    "NO_INTERLACE|FULL_INTERLACE|LECTURE_EN_TETE_SEULEMENT N°MAILLAGE|0 pour tous ] \n");
    fprintf(stderr,
	    "\t--structure               : Lis l'ensemble des données sans afficher les données volumineuses\n"
	    "\tNODALE                    : Scrute la connectivité nodale      des maillages\n"
	    "\tDESCENDANTE               : Scrute la connectivité descendante des maillages\n"
            "\tFULL_INTERLACE            : Affiche les connectivités en mode      entrelacé x1y1x2y2\n"
            "\tNO_INTERLACE              : Affiche les connectivités en mode  non entrelacé x1x2y1y2\n"
            "\tLECTURE_EN_TETE_SEULEMENT : Affiche uniquement les entêtes, désactive la lecture et l'affichage des données volumineuses\n"
	    "\tN°MAILLAGE ==  i          : Affiche le maillage n°i et ses champs associés\n"
	    "\tN°MAILLAGE ==  0          : Affiche l'ensemble des maillages et leurs champs associés\n"
	    "\tN°MAILLAGE == -1          : Affiche l'ensemble des champs qu'ils soient associés ou non à un maillage local\n");
  }
  EXIT_IF( (argc !=  2) && (argc !=  5),"nombre de parametres incorrect\n",NULL);


  /****************************************************************************
  *                      OUVERTURE DU FICHIER EN LECTURE                      *
  ****************************************************************************/
  filename = argv[1+decalage];
  fid = ouverture_fichier_MED(filename);
/*   ICI;_MEDobjetsOuverts(fid); */

  /****************************************************************************
   *                     QUESTIONS PRELIMINAIRES                              *
   *    1. Mode d'affichage des coordonnees (entrelace ou non) ?              *
   *    2. Connectivite des elements (nodale ou descendante) ?                *
   ***************************************************************************/
  fprintf(stdout,"\n >>>>>> DUMP DU FICHIER %s >>>>>>\n",filename);

  /* lecture et affichage de l'en-tete du fichier */
  lecture_en_tete(fid,filename);
/*   ICI;_MEDobjetsOuverts(fid); */

  if (argc == 2)
    parametrage(&mode_coo,&typ_con);
  else {
    typ_con_param=argv[2 + decalage];
/*     SSCRUTE(typ_con_param); */
    if (! strcmp(typ_con_param,"NODALE"))       typ_con = MED_NODAL;
    if (! strcmp(typ_con_param,"DESCENDANTE"))  typ_con = MED_DESCENDING;
    EXIT_IF( typ_con==MED_UNDEF_CONNECTIVITY_MODE,"Le paramètre de connectivité doit être soit NODALE|DESCENDANTE",NULL);
    mode_coo_param=argv[3 + decalage];
/*     SSCRUTE(mode_coo_param); */
    if (!strcmp(mode_coo_param,"NO_INTERLACE"))               mode_coo = MED_NO_INTERLACE;
    if (!strcmp(mode_coo_param,"FULL_INTERLACE"))             mode_coo = MED_FULL_INTERLACE;
    if (!strcmp(mode_coo_param,"LECTURE_EN_TETE_SEULEMENT")) { lecture_en_tete_seulement = MED_LECTURE_ENTETE_SEULEMENT;
							       mode_coo = MED_FULL_INTERLACE;}
    EXIT_IF( (mode_coo==MED_UNDEF_INTERLACE) ,
	     "Le paramètre d'entrelacement doit être soit "
	     "NO_INTERLACE|FULL_INTERLACE|LECTURE_EN_TETE_SEULEMENT",NULL);
  }


   /****************************************************************************
   *                       LIENS                                      *
   ****************************************************************************/
  
  /*On s'informe sur les liens avant de vérifier les paramètres de la ligne de commande
    afin de proposer le montage des maillages distants et de les comptabiliser dans les maillages disponibles */
  /* Cette option n'est pas encore activée */
  lecture_liens(fid, _montage, lecture_en_tete_seulement);
/*   _MEDobjetsOuverts(fid); */


  /****************************************************************************
   *                      QUEL MAILLAGE LIRE ?                                *
   ***************************************************************************/
  nmaa = MEDnMesh(fid);
/*   ICI;_MEDobjetsOuverts(fid); */

  /* Quel maillage lire ? */
  if (argc == 2) {
    if (nmaa >0) {
      fprintf(stdout,"- Il y a "IFORMAT" maillage(s) de type local dans ce fichier \n",nmaa);
      fprintf(stdout,"  Lequel voulez-vous lire (0 pour tous|1|2|3|...|n) ?\n");
      do {
	fprintf(stdout,"  Reponse : ");
	if (!scanf("%d",&numero)) fgets(_temp, 256, stdin) ;
      } while ( (numero > nmaa) || (numero < -1) );
    } else {
      fprintf(stdout,"- Il n'y a pas de maillage local dans ce fichier \n");
    }
  } else {
      if ( argc == 5 ) {
      numero = atoi(argv[4 + decalage]);
      EXIT_IF(numero > nmaa || numero < -1,"ce numero de maillage n'existe pas", NULL);
    }
  }

  /****************************************************************************
   *                       MAILLAGES SUPPORTS                                 *
   ****************************************************************************/

    nmaasup= MEDnSupportMesh(fid);
    if (nmaasup ) {
      fprintf(stdout,   "\n(*****************************************************)\n");
      fprintf(stdout,   "(* INFORMATIONS GENERALES SUR LES MAILLAGES SUPPORT: *)\n");
      fprintf(stdout, "(*****************************************************)\n");
    }
    for (meshit=1;meshit <= nmaasup;++meshit) {


	MEDsupportMeshInfo(fid, meshit, nommaa, &edim, &mdim,
			   maillage_description,  &rep,  nomcoo,  unicoo);
        fprintf(stdout,"\n(*******************************************)\n");
	fprintf(stdout,"(******** Maillage support n°%2.2d/%2.2"MED_IFORMAT" : *******)\n",meshit,nmaasup);
        fprintf(stdout,"(*******************************************)\n");

        fprintf(stdout,"- Nom du maillage support : <<%s>>\n",nommaa);
	fprintf(stdout,"- Dimension du maillage : "IFORMAT"\n",mdim);
	if (edim > mdim)
	   fprintf(stdout,"- La dimension de l'espace est "IFORMAT" \n",edim);

	  lecture_maillage_non_structure(fid,nommaa,MED_NO_DT,MED_NO_IT,mdim,edim,mode_coo,typ_con,
					 nomcoo,unicoo,&rep, &_nmodels, &_geotype_elst,&_geotypename_elst,
					 MED_LECTURE_MAILLAGE_SUPPORT_UNIQUEMENT);

     }


  /****************************************************************************
   *                       PARAMETRES SCALAIRES                               *
   ****************************************************************************/

  /* on va lire l'ensemble des parametres scalaire */
  lecture_parametres_scalaires(fid,lecture_en_tete_seulement);
/*   _MEDobjetsOuverts(fid); */

  /****************************************************************************
   *                       LOCALISATIONS                                      *
   ****************************************************************************/
  lecture_localisation(fid,mode_coo,lecture_en_tete_seulement);
/*   _MEDobjetsOuverts(fid); */

  /****************************************************************************
   *                       PROFILS                                      *
   ****************************************************************************/
  lecture_profils(fid,lecture_en_tete_seulement);
/*   _MEDobjetsOuverts(fid); */


  /****************************************************************************
   *                       MODELES D'ELEMENT DE STRUCTURE                     *
   ****************************************************************************/
    lecture_modeles_elstruct( fid, lecture_en_tete_seulement);
/*   _MEDobjetsOuverts(fid); */

  /****************************************************************************
   *                       FONCTIONS D'INTERPOLATION                          *
   ****************************************************************************/
    lecture_fonctions_interpolation( fid, lecture_en_tete_seulement);
/*   _MEDobjetsOuverts(fid); */


    /**********************************************************************************
   *            INFOS GENERALES SUR LE MAILLAGE, PUIS MAILLAGE+CHAMPS SUR CE MAILLAGE *
   ***********************************************************************************/
  if (numero > 0) {
   firstmesh=numero;lastmesh=numero;
  } else if (numero == 0) {
   firstmesh=1;lastmesh=nmaa;
  } else {
    firstmesh = nmaa +1;
  }

  for (meshit=firstmesh;meshit<=lastmesh;++meshit) {

    lecture_information_maillage(fid,meshit,nommaa,&mdim,&edim,&type_maillage,
				 maillage_description,&nstep,dtunit,nomcoo,unicoo,&rep);
    /*   _MEDobjetsOuverts(fid); */

    if ( nstep == 0 ) {
      fprintf(stderr,"Warning : Ce maillage n'a aucune étape de calcul, ceci est anormal..."
	      "\n\t  Recherche des champs résultats associés à ce maillage.\n");
      csit=0;}
    else csit =1;
    for (; csit <= nstep; ++csit) {

      if (csit) {
	
	ret = MEDmeshComputationStepInfo(fid, nommaa, csit, &numdt, &numit, &dt);
	EXIT_IF(ret < 0,"lors de l'appel à MEDmeshComputationStepInfo",NULL);

	/*       fprintf(stdout,"\n(*********************************************************************************)\n"); */
	/*       fprintf(stdout,  "(* MAILLAGE DE CALCUL |%s| N°%2.2d À L'ÉTAPE DE CALCUL (n°dt,n°it)=" */
	/* 	      "(% 2.2"MED_IFORMAT",% 2.2"MED_IFORMAT"): *)\n",nommaa,meshit,numdt,numit); */
	/*       fprintf(stdout,  "(*********************************************************************************)\n\n"); */

	/*les caractères ° sont comptabilisés comme deux caractères en locale "C" ? */
	_bannerstr ="(* MAILLAGE DE CALCUL |%s| n°%2.2d A L'ETAPE DE CALCUL (n°dt,n°it)="
	  "(% 2.2"MED_IFORMAT",% 2.2"MED_IFORMAT"): %.*s*)\n";
	snprintf(_temp,MAXBANNERLEN+1,_bannerstr,nommaa,meshit,numdt,numit,0,"");
	_bannerlen =strlen(_temp);
	fprintf(stdout,"\n(");
	for (i=0;i< _bannerlen-6; i++) fprintf(stdout,"*");
	fprintf(stdout,")\n%s(",_temp);
	for (i=0;i< _bannerlen-6; i++) fprintf(stdout,"*");
	fprintf(stdout,")\n");

	/****************************************************************************
	 *                      LECTURE DU MAILLAGE ET DES RESULTATS ASSOCIES        *
	 ****************************************************************************/
	/*   _MEDobjetsOuverts(fid); */

	if (type_maillage == MED_UNSTRUCTURED_MESH)
	  lecture_maillage_non_structure(fid,nommaa,numdt,numit,mdim,edim,mode_coo,typ_con,
					 nomcoo,unicoo,&rep,
					 &_nmodels,&_geotype_elst,&_geotypename_elst,
					 lecture_en_tete_seulement);
	else {
	  lecture_maillage_structure(fid,nommaa,numdt,numit,mdim,edim,mode_coo,
				     nomcoo,unicoo,lecture_en_tete_seulement);
	}
	/*   _MEDobjetsOuverts(fid); */

      }
      /* on lit ensuite les resultats associes au 
	 maillage selectionné à une étape de calcul de maillage donnée */
      lecture_resultats(fid,nommaa,numdt,numit,mode_coo,
			_nmodels,_geotype_elst,_geotypename_elst,
			lecture_en_tete_seulement);

      free(_geotype_elst);
      free(_geotypename_elst);
    }
  }


 /****************************************************************************
  *          LECTURE DES CHAMPS RESULTATS QLQ SOIENT LES MAILLAGES           *
  ****************************************************************************/

  if (numero == -1) {
    lecture_resultats(fid,"",numdt,numit,mode_coo,
		      _nmodels,_geotype_elst,_geotypename_elst,
		      lecture_en_tete_seulement);
    
    free(_geotype_elst);
    free(_geotypename_elst);
  }
  
  /****************************************************************************
 *                      FERMETURE DU FICHIER                                 *
 ****************************************************************************/
  for (i=1;i < FIDS.n ; ++i)
    if ( MEDfileObjectsUnmount(fid, FIDS.array[i], MED_MESH) < 0 ) {
      printf("Erreur de démontage du fichier n°%d\n",i);
    }


 ret = MEDfileClose(fid);
 EXIT_IF(ret < 0,"lors de la fermeture du fichier",argv[1 + decalage]);

 fprintf(stdout,"\n >>>>>> FIN DU DUMP DU FICHIER %s >>>>>>\n",argv[1 + decalage]);

 return EXIT_SUCCESS;
}
