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
 * - Nom du fichier : test26.c
 *
 * - Description : lecture de mailles de type MED_POLYEDRE
 *                 dans le maillage MED du fichier test25.med
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include "med_utils.h"
#include <string.h>

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

#define MAXDIM 3

int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid;
  char maa[MED_NAME_SIZE+1];
  med_int nmaa,i,j,mdim;
  char desc[MED_COMMENT_SIZE+1];
  med_int taille,nindf,nindn,npoly;
  med_int taille2,nindf2,nindn2;
  med_int *conn, *conn2, *indexn, *indexn2, *num, *fam;
  med_int *indexf, *indexf2;
  char *nom;
  char tmp[MED_SNAME_SIZE+1];
  int ind1, ind2,k,nfaces,nnoeuds,l;
  char dtunit[MED_SNAME_SIZE+1]="";
  char nomcoo[MAXDIM*MED_SNAME_SIZE+1];
  char unicoo[MAXDIM*MED_SNAME_SIZE+1];
  med_mesh_type type;
  med_sorting_type sort;
  med_axis_type rep;
  med_int nstep=0,sdim=0;
  med_int inoele=0,inuele=0,ifaele=0;
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;;

  /* Ouverture du fichier test25.med en lecture seule */
  fid = MEDfileOpen("test25.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test25.med");
    return -1;
  }
  printf("Ouverture du fichier test25.med \n");

  /* Lecture du nombre de maillages */
  nmaa = MEDnMesh(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur a lecture du nombre de maillage");
    return -1;
  }
  printf("Nombre de maillages = "IFORMAT"\n",nmaa);

  for (i=0;i<nmaa;i++) {

    /* Lecture des infos sur le maillage */
    if ( MEDmeshInfo( fid, i+1,  maa, &sdim, &mdim, &type, desc, dtunit, &sort,
		      &nstep,  &rep, nomcoo, unicoo) < 0 ) {
      MESSAGE("Erreur a lecture des infos sur le maillage");
      return -1;
    } else {
      printf("maillage "IFORMAT" de nom [%s] et de dimension : "IFORMAT" , et de type %d\n",i+1,maa,mdim,type);
      printf("\t -Dimension de l'espace : "IFORMAT"\n",sdim);
      printf("\t -Description du maillage : |%s|\n",desc);
      printf("\t -Noms des axes : |%s|\n",nomcoo);
      printf("\t -Unités des axes : |%s|\n",unicoo);
      printf("\t -Type de repère : %d\n",rep);
      printf("\t -Nombre d'étapes de calcul : "IFORMAT"\n",nstep);
      printf("\t -Unité des dates : |%s|\n",dtunit);
    }

    printf("maillage "IFORMAT" de nom [%s] et de dimension : "IFORMAT" \n",i+1,maa,mdim);

    /* Taille du tableau d'index des faces constituant chacun des polyedres */
    if ((nindf = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
				MED_CELL,MED_POLYHEDRON,MED_INDEX_FACE,MED_NODAL,
				&chgt,&trsf)) < 0) {
      MESSAGE("Erreur a lecture du nombre de maille MED_POLYEDRE en mode nodal");
      return -1;
    }
    npoly = nindf-1;
    printf("Nombre de mailles polyedres : "IFORMAT" \n",npoly);

    /* Taille du tableau d'index des noeuds constituant chacune des faces en mode nodal */
    if ((nindn = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
				MED_CELL,MED_POLYHEDRON,MED_INDEX_NODE,MED_NODAL,
				&chgt,&trsf)) < 0) {
      MESSAGE("Erreur a lecture des infos sur les polyedres");
      return -1;
    }
    printf("Taille a allouer pour le tableau des faces (d'indexation des noeuds),(nodal) : "IFORMAT" \n",nindn);

    /* Taille du tableau d'index des type constituant chacune des faces en mode descendant */
    if ((nindn2 = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
				 MED_CELL,MED_POLYHEDRON,MED_INDEX_NODE,MED_DESCENDING,
				 &chgt,&trsf)) < 0) {
      MESSAGE("Erreur a lecture des infos sur les polyedres");
      return -1;
    }
    printf("Taille a allouer pour le tableau des types de faces (descendant) : "IFORMAT" \n",nindn2);

    /* Taille du tableau de connectivité en mode MED_NODAL */
    if ((taille = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
				 MED_CELL,MED_POLYHEDRON,MED_CONNECTIVITY,MED_NODAL,
				 &chgt,&trsf)) < 0) {
      MESSAGE("Erreur a la lecture des infos sur les polyedres");
      return -1;
    }
    printf("Taille a allouer pour le tableau des noeuds des polyedres (connectivite),(nodal) : "IFORMAT" \n",taille);

    /* Taille du tableau de connectivité en mode MED_DESCENDING */
    if ((taille2 = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
				  MED_CELL,MED_POLYHEDRON,MED_CONNECTIVITY,MED_DESCENDING,
				  &chgt,&trsf)) < 0) {
      MESSAGE("Erreur a la lecture des infos sur les polyedres");
      return -1;
    }
    printf("Taille a allouer pour le tableau des noeuds des polyedres (connectivite),(descendant) : "IFORMAT" \n",taille2);


    /* Allocation memoire :
     *  - tableau indexf et indexf2 : nindf
     *  - tableau indexn et indexn2 : nindn et nindn2
     *  - tableau des connectivites : consize
     *  - tableaux numeros et numeros de familles : npoly
     *  - tableau des noms : MED_SNAME_SIZE*npoly + 1
     */
    indexf   = (med_int *) malloc(sizeof(med_int)*nindf);
    indexf2  = (med_int *) malloc(sizeof(med_int)*nindf);
    indexn   = (med_int *) malloc(sizeof(med_int)*nindn);
    indexn2  = (med_int *) malloc(sizeof(med_int)*nindn2);
    conn    = (med_int *) malloc(sizeof(med_int)*taille);
    conn2   = (med_int *) malloc(sizeof(med_int)*taille2);
    num     = (med_int *) malloc(sizeof(med_int)*npoly);
    fam     = (med_int *) calloc(sizeof(med_int),npoly);
    nom     = (char *)    malloc(sizeof(char)*MED_SNAME_SIZE*npoly+1);

    /* Lecture de la connectivite des mailles polyedres en mode nodal */
    if (MEDmeshPolyhedronRd(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_NODAL,
			    indexf,indexn,conn) < 0) {
      MESSAGE("Erreur a lecture de la connectivite nodale des polyedres");
      return -1;
    }

    printf("Lecture de la connectivite des mailles MED_POLYEDRE en mode nodal \n");

    /* Lecture de la connectivite des mailles polyedres en mode descendant */
    if (MEDmeshPolyhedronRd(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_DESCENDING,
			    indexf2,indexn2,conn2) < 0) {
      MESSAGE("Erreur a lecture de la connectivite descendante des polyedres");
      return -1;
    }
    printf("Lecture de la connectivite des mailles MED_POLYEDRE en mode descendant \n");

    /* Lecture noms */
    if ( (inoele = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
				 MED_CELL,MED_POLYHEDRON,MED_NAME,MED_DESCENDING,
				 &chgt,&trsf)) < 0) {
      MESSAGE("Erreur de detection de la présence de noms optionnels des polyedres");
      return -1;
    }
    if ( inoele ) {
      if (MEDmeshEntityNameRd(fid,maa,MED_NO_DT,MED_NO_IT,
			      MED_CELL,MED_POLYHEDRON,nom) < 0) {
	MESSAGE("Erreur à la lecture des noms optionnels des polyedres");
      } else {
	printf("Lecture des noms des mailles MED_POLYEDRE \n");
      }
    }

    /* Lecture des numeros */
    if ( (inuele = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
				  MED_CELL,MED_POLYHEDRON,MED_NUMBER,MED_DESCENDING,
				  &chgt,&trsf)) < 0) {
      MESSAGE("Erreur de detection de la présence de noms optionnels des polyedres");
      return -1;
    }
    if ( inuele) {
      if (MEDmeshEntityNumberRd(fid,maa,MED_NO_DT,MED_NO_IT,
				MED_CELL,MED_POLYHEDRON,num) < 0) {
	MESSAGE("Erreur à la lecture des numeros optionnels des polyedres");
      } else {
	printf("Lecture des numeros des mailles MED_POLYEDRE \n");
      }
    }

    /* Lecture des numeros de familles */
    if ( (ifaele = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
				  MED_CELL,MED_POLYHEDRON,MED_FAMILY_NUMBER,MED_DESCENDING,
				  &chgt,&trsf)) < 0) {
      MESSAGE("Erreur de detection de la présence des numéros de familles des polyedres");
      return -1;
    }
    if ( ifaele )
      if (MEDmeshEntityFamilyNumberRd(fid,maa,MED_NO_DT,MED_NO_IT,
				      MED_CELL,MED_POLYHEDRON,fam) < 0) {
	MESSAGE("Erreur a lecture des numeros de famille des polyedres");
	/*TODO : Considérer famille 0 */
	return -1;
      }
    printf("Lecture des numeros de familles des mailles MED_POLYEDRE \n");

    printf("Affichage des resultats \n");
    for (j=0;j<npoly;j++) {

      printf(">> Maille MED_POLYEDRE "IFORMAT" : \n",j+1);
      printf("---- Connectivite nodale      ----- : \n");
      nfaces  = *(indexf+j+1) - *(indexf+j);
      ISCRUTE_int(nfaces);
      /* ind1 = indice dans "faces" pour acceder aux numeros des faces */
      ind1 = *(indexf+j) - 1;
      for (k=0;k<nfaces;k++) {
	/* ind2 = indice dans "conn" pour acceder au premier noeud de la face */
	ind2 = *(indexn+ind1+k) - 1;
	nnoeuds = *(indexn+ind1+k+1) - *(indexn+ind1+k);
	printf("   - Face %d : [ ", k+1);
	for (l=0;l<nnoeuds;l++)
	  printf(" "IFORMAT" ",*(conn+ind2+l));
	printf(" ] \n");
      }
      printf("---- Connectivite descendante ----- : \n");
      nfaces  = *(indexf2+j+1) - *(indexf2+j);
      /* ind1 = indice dans "conn2" pour acceder aux numeros des faces */
      ind1 = *(indexf2+j) - 1;
      for (k=0;k<nfaces;k++)
	printf("   - Face %d de numero : "IFORMAT" et de type "IFORMAT" \n", k+1,*(conn2+ind1+k),*(indexn2+ind1+k));
      strncpy(tmp,nom+j*MED_SNAME_SIZE,MED_SNAME_SIZE);
      tmp[MED_SNAME_SIZE] = '\0';
      if (inoele) printf("---- Nom                      ----- : %s \n",tmp);
      if (inuele) printf("---- Numero                   ----- : "IFORMAT" \n",*(num+j));
      printf("---- Numero de famille        ----- : "IFORMAT" \n",*(fam+j));
    }


    /* liberation de la memoire */
    free(indexf);
    free(indexf2);
    free(indexn);
    free(indexn2);
    free(conn);
    free(conn2);
    free(num);
    free(fam);
    free(nom);
  }

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a fermeture du fichier");
    return -1;
  }
    printf("Fermeture du fichier \n");

    return ret;
}
