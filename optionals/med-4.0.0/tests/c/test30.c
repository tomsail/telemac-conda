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
 * - Nom du fichier : test30.c
 *
 * - Description : lecture des joints d'un maillage MED.
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

int afficheCorres(med_idt fid, char *maa, char *jnt,
                 med_entity_type typ_ent_local,   med_geometry_type typ_geo_local,
                 med_entity_type typ_ent_distant, med_geometry_type typ_geo_distant,
                 char *type);

int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid;
  char maa[MED_NAME_SIZE+1],maa_dist[MED_NAME_SIZE+1];
  med_int mdim,sdim;
  med_int njnt,ncor,ndom,nc;
  char jnt[MED_NAME_SIZE+1],corr[MED_NAME_SIZE+1];
  char des[MED_COMMENT_SIZE+1];
  char dtunit[MED_SNAME_SIZE+1]="";
  char nomcoo[3*MED_SNAME_SIZE+1];
  char unicoo[3*MED_SNAME_SIZE+1];
  med_axis_type rep;
  med_mesh_type type;
  med_sorting_type sort;
  med_int nstep=0,njstep=0,nodtitncor=0,nentity=0;
  med_entity_type typ_ent_local,typ_ent_distant;
  med_geometry_type typ_geo_local,typ_geo_distant;
  int i,j,k;

  if (argc != 2) {
    MESSAGE("Il faut passer un fichier MED en paramètre");
    return -1;
  }

  /* Ouverture du fichier passe en argument en lecture seule */
  if ((fid = MEDfileOpen(argv[1],MED_ACC_RDONLY)) < 0) {
    MESSAGE("Erreur à l'ouverture du fichier : "); SSCRUTE(argv[1]);
    return -1;
  }

  if ((sdim=MEDmeshnAxis(fid, 1)) <0) {
    MESSAGE("Erreur à la lecture de la dimension de l'espace du maillage :");
    SSCRUTE(maa);
    return -1;
  }

  /* Lecture des infos concernant le premier maillage */
  if ( MEDmeshInfo( fid, 1,  maa, &sdim, &mdim, &type, des, dtunit, &sort,
		    &nstep,  &rep, nomcoo,unicoo) < 0 ) {
    MESSAGE("Erreur a la lecture des informations sur le maillage : ");SSCRUTE(maa);
    return -1;
  } else {
    printf("Maillage de nom : |%s| , de dimension : "IFORMAT" , et de type %d\n",maa,mdim,type);
    printf("\t -Dimension de l'espace : "IFORMAT"\n",sdim);
    printf("\t -Description du maillage : |%s|\n",des);
    printf("\t -Noms des axes : |%s|\n",nomcoo);
    printf("\t -Unités des axes : |%s|\n",unicoo);
    printf("\t -Type de repère : %d\n",rep);
    printf("\t -Nombre d'étapes de calcul : "IFORMAT"\n",nstep);
    printf("\t -Unité des dates : |%s|\n\n",dtunit);
  }

  /* Lecture du nombre de joints */
  if ((njnt = MEDnSubdomainJoint(fid,maa)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de joints");
    return -1;
  }
  printf("Nombre de joints : "IFORMAT" \n",njnt);

  /* Lecture de tous les joints du maillage */
  for (i = 0;i<njnt;i++) {
    printf("Joint numero : %d \n",i+1);

    /* Lecture des infos sur le joints */
    if (MEDsubdomainJointInfo(fid,maa,i+1,jnt,des,&ndom,maa_dist,&njstep,&nodtitncor) < 0) {
      MESSAGE("Erreur a la lecture du joint d'indice");
      ISCRUTE_int(i+1);
      return -1;
    }
    printf("Nom du joint: |%s| \n",jnt);
    printf("Description du joint      : |%s| \n",des);
    printf("Domaine en regard         : "IFORMAT" \n",ndom);
    printf("Maillage distant          : |%s| \n",maa_dist);
    printf("Nombre d'étapes de calcul : "IFORMAT" \n",njstep);
    printf("Nombre de correspondance pour (NO_DT,NO_IT) : "IFORMAT" \n",nodtitncor);



    /* lecture des correspondances une par une
       en connaissant leur type a priori */

    /* Lecture de la correspondance Noeud Noeud */
    afficheCorres(fid,maa,jnt,MED_NODE,0,MED_NODE,0,"noeud/noeud");

    /* Lecture de la correspondance Noeud Maille */
    afficheCorres(fid,maa,jnt,MED_NODE,0,MED_CELL,MED_TRIA3,"noeud/TRIA3");


    /* lecture des correspondances une par une
       sans connaitre leur type a priori
       -> utilisation de la fonction MEDjointTypeCorres */

    ncor=1;

    while ( ncor <= nodtitncor ) {
      ISCRUTE(ncor);
      if ( MEDsubdomainCorrespondenceSizeInfo(fid,maa,jnt,MED_NO_DT,MED_NO_IT,ncor,
					   &typ_ent_local,&typ_geo_local,&typ_ent_distant,&typ_geo_distant,
					      &nentity) < 0 ) {
	MESSAGE("Erreur a la lecture des infos sur le nombre d'entite en regard");
	return -1;
      }

      /* Lecture de la correspondance Noeud Noeud */
      afficheCorres(fid,maa,jnt,typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,"------");

      ncor++;
    }

  }

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier ");
    return -1;
  }

  return ret;
}




int afficheCorres(med_idt fid, char *maa, char *jnt,
		 med_entity_type typ_ent_local,   med_geometry_type typ_geo_local,
		 med_entity_type typ_ent_distant, med_geometry_type typ_geo_distant,
		 char *type)
{
  med_int nc;
  med_int *cortab;
  int k,ncor,ret=0;

  ISCRUTE_int(typ_ent_local);ISCRUTE_int(typ_geo_local);
  ISCRUTE_int(typ_ent_distant);ISCRUTE_int(typ_geo_distant);
  if ( MEDsubdomainCorrespondenceSize(fid,maa,jnt,MED_NO_DT,MED_NO_IT,
				      typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,
				      &nc) < 0) {
    MESSAGE("Erreur a la lecture des infos sur le nombre d'entite en regard de type");
    SSCRUTE(type);
    return -1;
  }

  printf("nb de couples d'entites en regard |%s|: "IFORMAT" \n",type,nc);

  if (nc > 0) {
    cortab = (med_int*) malloc(sizeof(med_int)*nc*2);
    if ((ret=MEDsubdomainCorrespondenceRd(fid,maa,jnt,MED_NO_DT,MED_NO_IT,
					  typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,
					  cortab)) < 0) {
      MESSAGE("Erreur a la lecture des correspondances sur ");
      SSCRUTE(type);
      ret = -1;
    }
    if (ret == 0)
      for (k=0;k<nc;k++)
	printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",k+1,*(cortab+2*k),
	       *(cortab+2*k+1));
    free(cortab);
  }
  return ret;
}

