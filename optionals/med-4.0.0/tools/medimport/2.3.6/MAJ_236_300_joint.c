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


#include "med_config.h"
#include "med_outils.h"
#include "med_versioned.h"
#include <string.h>


#include "MAJ_236_300.h"
#include "MAJ_version.h"


int afficheCorres(med_idt fid, const char * const maa, char *jnt,
                 med_entity_type typ_ent_local,   med_geometry_type typ_geo_local,
                 med_entity_type typ_ent_distant, med_geometry_type typ_geo_distant,
                 char *type);

med_err MAJ_236_300_joint(med_idt fid, const char * const maa)


{
  med_err ret = 0;
  char    maa_dist[MED_NAME_SIZE+1];
  med_int mdim,sdim;
  med_int njnt,ncor,ndom,nc;
  char jnt[MED_NAME_SIZE+1],corr[MED_NAME_SIZE+1];
  char des[MED_COMMENT_SIZE+1];
  med_int           njstep=0,nodtitncor=0,nentity=0;
  med_entity_type   typ_ent_local,typ_ent_distant;
  med_geometry_type typ_geo_local,typ_geo_distant;
  int i,j,k;

  MAJ_version_num(fid,2,3,6);

  /* Lecture du nombre de joints */
  if ((njnt = MEDnSubdomainJoint(fid,maa)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de joints");
    return -1;
  }
/*   printf("Nombre de joints : "IFORMAT" \n",njnt); */

  /* Lecture de tous les joints du maillage */
  for (i = 0;i<njnt;i++) {
/*     printf("Joint numero : %d \n",i+1); */

    /* Lecture des infos sur le joints */
    if (MEDsubdomainJointInfo(fid,maa,i+1,jnt,des,&ndom,maa_dist,&njstep,&nodtitncor) < 0) {
      MESSAGE("Erreur a la lecture du joint d'indice");
      ISCRUTE_int(i+1);
      return -1;
    }
/*     printf("Nom du joint: |%s| \n",jnt); */
/*     printf("Description du joint      : |%s| \n",des); */
/*     printf("Domaine en regard         : "IFORMAT" \n",ndom); */
/*     printf("Maillage distant          : |%s| \n",maa_dist); */
/*     printf("Nombre d'étapes de calcul : "IFORMAT" \n",njstep); */
/*     printf("Nombre de correspondance pour (NO_DT,NO_IT) : "IFORMAT" \n",nodtitncor); */

    fprintf(stdout,"  >>> Normalisation du joint [%s] du maillage [%s] \n",
	    jnt,maa);

    /* Creation du joint */
    MAJ_version_num(fid,3,0,8);
    if (MEDsubdomainJointCr(fid,maa,jnt,des,ndom,maa_dist) < 0) {
      MESSAGE("Erreur a la creation du joint");
      return -1;
    }
    MAJ_version_num(fid,2,3,6);

    /* lecture des correspondances une par une
       sans connaitre leur type a priori
       -> utilisation de la fonction MEDjointTypeCorres */

    ncor=1;

    while ( ncor <= nodtitncor ) {
/*       ISCRUTE(ncor); */
      if ( MEDsubdomainCorrespondenceSizeInfo(fid,maa,jnt,MED_NO_DT,MED_NO_IT,ncor,
					      &typ_ent_local,&typ_geo_local,&typ_ent_distant,&typ_geo_distant,
					      &nentity) < 0 ) {
	MESSAGE("Erreur a la lecture des infos sur le nombre d'entite en regard");
	return -1;
      }

      /* Lecture de la correspondance Noeud Noeud */
      if ( afficheCorres(fid,maa,jnt,typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,"------") < 0)
	return -1;

      ncor++;
    }

  }
  MAJ_version_num(fid,3,0,8);

  return ret;
}




int afficheCorres(med_idt fid, const char * const maa, char *jnt,
		 med_entity_type typ_ent_local,   med_geometry_type typ_geo_local,
		 med_entity_type typ_ent_distant, med_geometry_type typ_geo_distant,
		 char *type)
{
  med_int nc;
  med_int *cortab;
  int k,ncor,ret=0;

  if ( MEDsubdomainCorrespondenceSize(fid,maa,jnt,MED_NO_DT,MED_NO_IT,
				      typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,
				      &nc) < 0) {
    MESSAGE("Erreur a la lecture des infos sur le nombre d'entite en regard de type");
    SSCRUTE(type);
    return -1;
  }

/*   printf("nb de couples d'entites en regard |%s|: "IFORMAT" \n",type,nc); */

  if (nc > 0) {
    cortab = (med_int*) malloc(sizeof(med_int)*nc*2);
    if ((ret=MEDsubdomainCorrespondenceRd(fid,maa,jnt,MED_NO_DT,MED_NO_IT,
					  typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,
					  cortab)) < 0) {
      MESSAGE("Erreur a la lecture des correspondances sur ");
      SSCRUTE(type);
      ret = -1;
    }
    if (ret == 0) {
      MAJ_version_num(fid,3,0,8);
      for (k=0;k<nc;k++)
/* 	printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",k+1,*(cortab+2*k), */
/* 	       *(cortab+2*k+1)); */
      /* Ecriture de la correspondance Noeud Maille */
      if (MEDsubdomainCorrespondenceWr(fid,maa,jnt,MED_NO_DT,MED_NO_IT,
				       typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,
				       nc,cortab) < 0) {
	MESSAGE("Erreur a l'ecriture du tableau des correspondances :");
	ret = -1;
      }
      MAJ_version_num(fid,2,3,6);

    }
    free(cortab);
  }
  return ret;
}

