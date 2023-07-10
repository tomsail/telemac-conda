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
#define MODE_ACCES MED_LECTURE_ECRITURE
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_LECTURE_AJOUT
#else
#define MODE_ACCES MED_CREATION
#endif

int afficheCorres(med_idt fid, char *maa, char *jnt,
                 med_entite_maillage typ_ent_local,   med_geometrie_element typ_geo_local,
                 med_entite_maillage typ_ent_distant, med_geometrie_element typ_geo_distant,
                 char *type);

int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid;
  char maa[MED_TAILLE_NOM+1],maa_dist[MED_TAILLE_NOM+1];
  med_int mdim;
  med_int njnt,ncor,ndom,nc;
  char jnt[MED_TAILLE_NOM+1],corr[MED_TAILLE_NOM+1];
  char des[MED_TAILLE_DESC+1];
  med_entite_maillage typ_ent_local,typ_ent_distant;
  med_geometrie_element typ_geo_local,typ_geo_distant;

  int i,j,k;
  med_maillage type;

  if (argc != 2) {
    MESSAGE("Il faut passer un fichier MED en paramètre");
    return -1;
  }

  /* Ouverture du fichier passe en argument en lecture seule */
  if ((fid = MEDouvrir(argv[1],MED_LECTURE)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier : "); SSCRUTE(argv[1]);
    return -1;
  }
  
  /* Lecture des infos sur le premier maillage */
  if (MEDmaaInfo(fid,1,maa,&mdim,&type,des) < 0) {
    MESSAGE("Erreur a lecture des infos sur le 1er maillage"); 
    return -1;
  }
  printf("Maillage de nom %s et de dimension "IFORMAT" \n",maa,mdim);

  /* Lecture du nombre de joints */
  if ((njnt = MEDnJoint(fid,maa)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de joints"); 
    return -1;
  }
  printf("Nombre de joints : "IFORMAT" \n",njnt);

  /* Lecture de tous les joints du maillage */
  if (njnt > 0)
    for (i = 0;i<njnt;i++) {
      printf("Joint numero : %d \n",i+1);

      /* Lecture des infos sur le joints */
      if (MEDjointInfo(fid,maa,i+1,jnt,des,&ndom,maa_dist) < 0) {
	MESSAGE("Erreur a la lecture du joint d'indice");
	ISCRUTE_int(i+1);
	return -1;
      }
      printf("Nom du joint: %s \n",jnt);
      printf("Description du joint      : %s \n",des);
      printf("Domaine en regard         : "IFORMAT" \n",ndom);
      printf("Maillage distant          : %s \n",maa_dist);



      /* lecture des correspondances une par une 
         en connaissant leur type a priori */

      /* Lecture de la correspondance Noeud Noeud */
      afficheCorres(fid,maa,jnt,MED_NOEUD,0,MED_NOEUD,0,"noeud/noeud");
  
      /* Lecture de la correspondance Noeud Maille */
      afficheCorres(fid,maa,jnt,MED_NOEUD,0,MED_MAILLE,MED_TRIA3,"noeud/TRIA3");


      /* lecture des correspondances une par une 
         sans connaitre leur type a priori 
         -> utilisation de la fonction MEDjointTypeCorres */

      ncor=1;

      while (MEDjointTypeCorres(fid,maa,jnt,ncor,
				&typ_ent_local,&typ_geo_local,&typ_ent_distant,&typ_geo_distant)>=0) {

	/* Lecture de la correspondance Noeud Noeud */
	afficheCorres(fid,maa,jnt,typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,"noeud/noeud");
	
	ncor++;
      }


	    
    }			    

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier ");
    return -1;
  }

  return ret;
}




int afficheCorres(med_idt fid, char *maa, char *jnt,
		 med_entite_maillage typ_ent_local,   med_geometrie_element typ_geo_local,
		 med_entite_maillage typ_ent_distant, med_geometrie_element typ_geo_distant,
		 char *type)
{
  med_int nc;
  med_int *cortab;
  int k,ncor,ret=0;

  if ((nc = MEDjointnCorres(fid,maa,jnt,typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant)) < 0) {
    MESSAGE("Erreur a la lecture des infos sur le nombre d'entite en regard de type");
    SSCRUTE(type);
    return -1;
  }
	
  printf("nb de couples d'entites en regard %s: "IFORMAT" \n",type,nc);
  
  if (nc > 0) {
    cortab = (med_int*) malloc(sizeof(med_int)*nc*2);
    if ((ret=MEDjointLire(fid,maa,jnt,cortab,nc*2,
			  typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant)) < 0) {
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

