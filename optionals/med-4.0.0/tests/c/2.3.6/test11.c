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
 * - Nom du fichier : test11.c
 *
 * - Description : lecture de champs de resultats MED 
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include "med_utils.h"
#include <string.h>

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_LECTURE_ECRITURE
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_LECTURE_AJOUT
#else
#define MODE_ACCES MED_CREATION
#endif

#ifndef USER_INTERLACE
#define USER_INTERLACE MED_FULL_INTERLACE
#endif

#define USER_MODE MED_COMPACT

med_err getFieldsOn(med_idt fid, char * nomcha, med_type_champ typcha, med_int ncomp,
                    med_entite_maillage entite, med_mode_switch stockage);

int main (int argc, char **argv)


{
  med_err ret,lret;
  med_idt fid;
  char * fichier = NULL;
  char maa[MED_TAILLE_NOM+1]="";
  char desc[MED_TAILLE_DESC+1]="";
  char pflname[MED_TAILLE_NOM+1]="",nomlien[MED_TAILLE_NOM+1]="";
  char locname[MED_TAILLE_NOM+1]="";
  char * lien = NULL;
  char *comp= NULL, *unit= NULL;
  char nomcha  [MED_TAILLE_NOM+1]="";
  med_int mdim,ncomp,ncha,npro,nln,pflsize,*pflval,nval;
  med_int ngauss,nloc;
  int t1,t2,t3;
  med_type_champ typcha;
  med_maillage type;
  med_geometrie_element type_geo;
  med_float *refcoo, *gscoo, *wg;
  int i,j;
  
  if (argc != 2) {
    MESSAGE("Aucun nom de fichier precise, fichier test10.med utilise ");
    fichier = "test10.med";
  } else {
    fichier = argv[1];
  };


  /* Ouverture du fichier med */
  if ((fid = MEDouvrir(fichier,MED_LECTURE)) < 0){
    MESSAGE("Erreur a l'ouverture du fichier : ");SSCRUTE(fichier);
    return -1;
  }
   
  ret = 0;
  
  /* infos sur le premier maillage */
  if (  MEDmaaInfo(fid,1,maa,&mdim,&type,desc) < 0 ) {
    MESSAGE("Erreur a la lecture des informations sur le maillage : ");
    SSCRUTE(maa);ISCRUTE(mdim);ISCRUTE_int(type);SSCRUTE(desc);
    return -1;
  }
  
  printf("Maillage de nom |%s| et de dimension "IFORMAT" \n",maa,mdim);

  /* combien de champs dans le fichier */
  if ((ncha = MEDnChamp(fid,0)) < 0) {
    MESSAGE("Impossible de lire le nombre de champs : ");ISCRUTE(ncha);
    return ncha;
  }
  
  printf("Nombre de champs : "IFORMAT" \n",ncha);
  
  /* lecture de tous les champs  */
  for (i =0;i<ncha;i++) {
    lret = 0;
    printf("\nChamp numero : %d \n",i+1);
    
    /* Lecture du nombre de composantes */
    if ((ncomp = MEDnChamp(fid,i+1)) < 0) {
      MESSAGE("Erreur a la lecture du nombre de composantes : "); ISCRUTE(ncomp); 
      ret = -1; continue;
    }
    
    /* Lecture du type du champ, des noms des composantes et du nom de l'unite*/
    comp = (char*) malloc(ncomp*MED_TAILLE_PNOM+1);
    EXIT_IF(comp == NULL,NULL,NULL);
    unit = (char*) malloc(ncomp*MED_TAILLE_PNOM+1);
    EXIT_IF(unit == NULL,NULL,NULL);
      
    if ( MEDchampInfo(fid,i+1,nomcha,&typcha,comp,unit,ncomp) < 0 ) {
      MESSAGE("Erreur a la demande d'information sur les champs : "); 
      ISCRUTE_int(i+1);SSCRUTE(nomcha);ISCRUTE_int(typcha);SSCRUTE(comp);SSCRUTE(unit);
      ISCRUTE(ncomp);
      ret = -1; continue;
    }
      
    printf("Nom du champ : |%s| de type %d\n",nomcha,typcha);
    printf("Nom des composantes : |%s|\n",comp);
    printf("Unites des composantes : |%s| \n",unit);
    
    free(comp);
    free(unit);
    
      
    lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_NOEUD, USER_INTERLACE );
    
    if (lret == 0) lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_MAILLE, USER_INTERLACE );
    else { MESSAGE("Erreur a la lecture des champs aux noeuds "); ret = -1; continue;}
   
    if (lret == 0) lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_FACE,USER_INTERLACE);
    else { MESSAGE("Erreur a la lecture des champs aux mailles "); ret = -1; continue;}
   
    if (lret == 0) lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_ARETE,USER_INTERLACE);
    else {MESSAGE("Erreur a la lecture des champs aux faces "); ret = -1; continue;}
    
    if (lret == 0) lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_NOEUD_MAILLE,USER_INTERLACE);
    else {MESSAGE("Erreur a la lecture des champs aux aretes"); ret = -1; continue;}
    
    if  (lret != 0) {MESSAGE("Erreur a la lecture des champs aux noeuds des mailles "); ret = -1;};
  } 
  

  /* Interrogation des profils */
  npro = MEDnProfil(fid);
  
  printf("\nNombre de profils stockes : "IFORMAT"\n\n",npro);
  for (i=1 ; i <= npro ; i++ ) {
    if ( MEDprofilInfo(fid, i, pflname, &nval) < 0)  {
      MESSAGE("Erreur a la demande d'information sur le profil n° : "); ISCRUTE_int(i);
      ret = -1;continue;
    }
    printf("\t- Profil n°%i de nom |%s| et de taille "IFORMAT"\n",i,pflname,nval);
    pflval = (med_int*) malloc(sizeof(med_int)*nval);
    if ( MEDprofilLire(fid, pflval, pflname) < 0) {
      MESSAGE("Erreur a la lecture des valeurs du profil : "); 
      SSCRUTE(pflname);
      ret = -1;
    } else {
      printf("\t");
      for (j=0;j<nval;j++) printf(" "IFORMAT" ",*(pflval+j));
      printf("\n\n");
    }
    free(pflval);
  }
  
  /* Interrogation des liens */
  nln = MEDnLien(fid);
  
  printf("\nNombre de liens stockes : "IFORMAT"\n\n",nln);
  for (i=1 ; i <= nln ; i++ ) {
    if ( MEDlienInfo(fid, i, nomlien, &nval) < 0)  {
      MESSAGE("Erreur a la demande d'information sur le lien n° : "); ISCRUTE_int(i);
      ret = -1;continue;
    }
    printf("\t- Lien n°%i de nom |%s| et de taille "IFORMAT"\n",i,nomlien,nval);

    lien = malloc((nval+1)*sizeof(char));
    EXIT_IF(lien == NULL,NULL,NULL);

    if ( MEDlienLire(fid, lien, nomlien) < 0 )  {
      MESSAGE("Erreur a la lecture du lien : "); 
      SSCRUTE(nomlien);SSCRUTE(lien);
      ret = -1; 
    } else {
      lien[nval] = '\0';
      printf("\t\t|%s|\n\n",lien);
    }
    free(lien);
  }
  
  /* Interrogation des localisations des points de GAUSS */
  nloc = MEDnGauss(fid);
  
  printf("\nNombre de localisations stockees : "IFORMAT"\n\n",nloc);
  for (i=1 ; i <= nloc ; i++ ) {
    if ( MEDgaussInfo(fid, i, locname, &type_geo, &ngauss) < 0)  {
      MESSAGE("Erreur a la demande d'information sur la localisation n° : "); ISCRUTE_int(i);
      ret = -1;continue;
    }
    printf("\t- Loc. n°%i de nom |%s| et nbr. de pts de GAUSS "IFORMAT"\n",i,locname,ngauss);
    t1 = (type_geo%100)*(type_geo/100);
    t2 = ngauss*(type_geo/100);
    t3 = ngauss;
    refcoo = (med_float *) malloc(sizeof(med_float)*t1 );
    gscoo  = (med_float *) malloc(sizeof(med_float)*t2 );
    wg     = (med_float *) malloc(sizeof(med_float)*t3 );
    
    if ( MEDgaussLire(fid, refcoo, gscoo, wg, USER_INTERLACE, locname ) < 0) {
      MESSAGE("Erreur a la lecture des valeurs de la localisation : "); 
      SSCRUTE(locname);
      ret = -1;
    } else {
      printf("\t  Coordonnees de l'element de reference de type %i :\n\t\t",type_geo);
      for (j=0;j<t1;j++) printf(" %f ",*(refcoo+j));
      printf("\n");
      printf("\t  Localisation des points de GAUSS : \n\t\t");
      for (j=0;j<t2;j++) printf(" %f ",*(gscoo+j)); 
      printf("\n");
      printf("\t  Poids associes aux points de GAUSS :\n\t\t");
      for (j=0;j<t3;j++) printf(" %f ",*(wg+j));    
      printf("\n\n");
    }
    free(refcoo);
    free(gscoo);
    free(wg);
  }


  /* fermeture du fichier */
  if ( MEDfermer(fid) < 0) return -1;
  
  return ret;
}
	
med_err getFieldsOn(med_idt fid, char * nomcha, med_type_champ typcha, med_int ncomp,
		    med_entite_maillage entite, med_mode_switch stockage) {
	      
  int j,k,l,m,n,nb_geo;
  med_int nbpdtnor=0,pflsize,*pflval,ngauss=0,ngroup,*vale=NULL,nval;
  med_int numdt=0,numo=0,lnsize,nbrefmaa;
  med_float *valr=NULL,dt=0.0;
  med_err ret=0;
  med_booleen local;
  char pflname [MED_TAILLE_NOM+1]="";
  char locname [MED_TAILLE_NOM+1]="";
  char * lien = NULL;
  char maa_ass [MED_TAILLE_NOM+1]="";
  char dt_unit [MED_TAILLE_PNOM+1]="";


  med_geometrie_element * type_geo;
  med_geometrie_element typ_noeud[1] = { MED_NONE };
  med_geometrie_element typmai[MED_NBR_GEOMETRIE_MAILLE+2] = {MED_POINT1, MED_SEG2, MED_SEG3, MED_TRIA3,
							      MED_QUAD4, MED_TRIA6,MED_QUAD8, MED_TETRA4,
							      MED_PYRA5, MED_PENTA6, MED_HEXA8, MED_TETRA10, 
							      MED_PYRA13, MED_PENTA15, MED_HEXA20, 
							      MED_POLYGONE, MED_POLYEDRE};
  med_geometrie_element typfac[MED_NBR_GEOMETRIE_FACE+1] = {MED_TRIA3,MED_TRIA6,
							    MED_QUAD4,MED_QUAD8,
							    MED_POLYGONE};
  med_geometrie_element typare[MED_NBR_GEOMETRIE_ARETE] = {MED_SEG2,MED_SEG3};  

  char ** AFF; 

  switch (entite) {
  case MED_NOEUD : 
    type_geo = typ_noeud;
    nb_geo   = 1;
    AFF      = MED_GEOMETRIE_NOEUD_AFF;
    break;
  case  MED_MAILLE : 
  case  MED_NOEUD_MAILLE :
    type_geo = typmai;
    nb_geo   = MED_NBR_GEOMETRIE_MAILLE+2;
    AFF      = MED_GEOMETRIE_MAILLE_AFF;
    break;
  case  MED_FACE : 
    type_geo = typfac;
    nb_geo   = MED_NBR_GEOMETRIE_FACE+1;
    AFF      = MED_GEOMETRIE_FACE_AFF;
    break;
  case  MED_ARETE :
    type_geo = typare;
    nb_geo   = MED_NBR_GEOMETRIE_ARETE;
    AFF      = MED_GEOMETRIE_ARETE_AFF;
    break;
  }
	
      
  for (k=0;k<nb_geo;k++) {
    
    /* Combien de (PDT,NOR) a lire */
    nbpdtnor = MEDnPasdetemps(fid,nomcha,entite,type_geo[k]); 
    if (nbpdtnor < 1 ) continue;

    for (j=0;j<nbpdtnor;j++) {
		
      if ( MEDpasdetempsInfo(fid,nomcha,entite,type_geo[k],
			     j+1, &ngauss, &numdt, &numo, dt_unit,
			     &dt, maa_ass, &local, &nbrefmaa) <0) {
	MESSAGE("Erreur a la demande d'information sur (pdt,nor) : "); 
	ISCRUTE(numdt); ISCRUTE(numo);
	ret = -1; continue;
      };
      
      printf("\n  +Pas de Temps n."IFORMAT" (%f) [%s], n. d'ordre "IFORMAT", avec "IFORMAT" pts de gauss sur le maillage par defaut.\n",numdt,dt,dt_unit,numo,ngauss);
      
      printf("\tLe maillage par defaut est : |%s|, sur un total de : "IFORMAT" maillages associes\n",
	     maa_ass, nbrefmaa);

      /* Le maillage reference est-il porte par un autre fichier */
      if ( !local ) {
	
	if ( (lnsize=MEDnValLien(fid,maa_ass) ) < 0 )  {
	  MESSAGE("Erreur a la lecture de la taille du lien : "); 
	  SSCRUTE(maa_ass);
	  ret = -1;
	} else {

	  lien = malloc(lnsize*sizeof(char)+1);
	  EXIT_IF(lien == NULL,NULL,NULL);
	  if ( MEDlienLire(fid, lien, maa_ass) < 0 )  {
	    MESSAGE("Erreur a la lecture du lien : "); 
	    SSCRUTE(maa_ass);SSCRUTE(lien);
	    ret = -1; 
	  } else {
	    printf("\tLe maillage |%s| est porte par un fichier distant |%s|\n",maa_ass,lien);
	  }
	  free(lien);
	}
      }

      /* Combien de maillages lies aux (nomcha,ent,geo,numdt,numo)  */
      /* Notons que cette information est egalement disponible a partir de MEDpasdetempsInfo */
      if ( (nbrefmaa = MEDnChampRef(fid,nomcha,entite,type_geo[k],numdt,numo) ) < 0 ) {
	MESSAGE("Erreur a la demande du nombre de maillages references par le champ : "); 
	SSCRUTE(nomcha);
	ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	SSCRUTE(MED_ENTITE_MAILLAGE_AFF[(int)entite]);SSCRUTE(AFF[k]);
	ISCRUTE(numdt); ISCRUTE(numo);
	ret = -1; continue;
      };

      for (l=0;l<nbrefmaa;l++) {
	
	if ( MEDchampRefInfo(fid,nomcha,entite,type_geo[k],
			     l+1,numdt, numo, maa_ass, &local, &ngauss) <0 ) {
	  MESSAGE("Erreur a la demande d'information sur le maillage utilise par le champ n° : "); 
	  SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
          ISCRUTE_int(l+1);ISCRUTE(numdt); ISCRUTE(numo);SSCRUTE(maa_ass); 
	  ret = -1; continue;
	};		  
		  
	/* Prend en compte le nbre de pt de gauss automatiquement */
	if ((nval = MEDnVal(fid,nomcha,entite,type_geo[k],numdt,numo,maa_ass,USER_MODE)) <= 0)   {
	  MESSAGE("Erreur a la lecture du nombre de valeurs du champ : "); 
	  SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	  ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(maa_ass);ISCRUTE_int(USER_MODE);
	  ret = -1; continue;
	};	
	  
	  
	printf("\t- Il y a "IFORMAT" valeurs en mode %i. Chaque entite %s\
 de type geometrique %s associes au maillage |%s| a "IFORMAT" pts de gauss \n",
	       nval,USER_MODE,MED_ENTITE_MAILLAGE_AFF[(int)entite],AFF[k],maa_ass,ngauss);                    
	
	/* Le maillage reference est-il porte par un autre fichier */
	if ( !local ) {
	
	  if ( (lnsize=MEDnValLien(fid,maa_ass) ) < 0 )  {
	    MESSAGE("Erreur a la lecture de la taille du lien : "); 
	    SSCRUTE(maa_ass);
	    ret = -1;
	  } else {
	    
	    lien = malloc(lnsize*sizeof(char) + 1);
	    EXIT_IF(lien == NULL,NULL,NULL);

	    if ( MEDlienLire(fid, lien, maa_ass) < 0 )  {
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
	if (typcha == MED_FLOAT64) {
	  
	  valr = (med_float*) calloc(ncomp*nval,sizeof(med_float));
	  EXIT_IF(valr == NULL,NULL,NULL);
	  if ( MEDchampLire(fid,maa_ass,nomcha,(unsigned char*)valr,stockage,MED_ALL,locname,
			    pflname,USER_MODE,entite,type_geo[k],numdt,numo) < 0 ) {
	    MESSAGE("Erreur a la lecture du nombre de valeurs du champ : "); 
	    SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	    ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(maa_ass);
	    ret = -1;
	  };		  

	} else {
	  
	  vale = (med_int*) calloc(ncomp*nval,sizeof(med_int));
	  EXIT_IF(vale == NULL,NULL,NULL);
	  if ( MEDchampLire(fid,maa_ass,nomcha,(unsigned char*)vale,stockage,MED_ALL,locname,
			    pflname,USER_MODE,entite,type_geo[k],numdt,numo) < 0 ) {
	    MESSAGE("Erreur a la lecture des valeurs du champ : "); 
	    SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	    ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(maa_ass);
	    ret = -1;
	  };	
	  
	}

	if ( ngauss > 1 )
	  printf("\t- Modèle de localisation des points de Gauss de nom |%s|\n",locname);

	if (entite == MED_NOEUD_MAILLE)
	  ngroup = type_geo[k] % 100;
	else
	  ngroup = ngauss;

	switch (stockage) {
      
	case MED_FULL_INTERLACE : 
	  printf("\t- Valeurs :\n\t");
	  for (m=0;m<nval/ngroup;m++) {
	    printf("|");
	    for (n=0;n<ngroup*ncomp;n++)
	      if (typcha == MED_FLOAT64) 
		printf(" %f ",*(valr+(m*ngroup*ncomp)+n));
	      else
		printf(" "IFORMAT" ",*(vale+(m*ngroup*ncomp)+n));

	  }
	  break;
	
	  /*Affichage en fonction du profil à traiter*/
	case MED_NO_INTERLACE :
	  printf("\t- Valeurs :\n\t");
	  for (m=0;m<ncomp;m++) {
	    printf("|");
	    for (n=0;n<nval;n++) 
	      if (typcha == MED_FLOAT64) 
		printf(" %f ",*(valr+(m*nval)+n));
	      else
		printf(" "IFORMAT" ",*(vale+(m*nval)+n));
	  }
	  break;
	}
      
	printf("|\n");
	if (typcha == MED_FLOAT64) {
	  if ( valr ) {free(valr);valr = NULL;}}
	else
	  if (vale) { free(vale);vale = NULL; }
      
	/*Lecture du profil associe */
	if (strcmp(pflname,MED_NOPFL) == 0 )
	  printf("\t- Profil : MED_NOPFL\n");
	else {
	
	  if ( (pflsize = MEDnValProfil(fid,pflname)) <0 )  {
	    MESSAGE("Erreur a la lecture du nombre de valeurs du profil : "); 
	    SSCRUTE(pflname);
	    ret = -1; continue;
	  }
		  
	  printf("\t- Profil : |%s| de taille "IFORMAT"\n",pflname,pflsize);
	  
	  pflval = (med_int*) malloc(sizeof(med_int)*pflsize);
	  EXIT_IF(pflval == NULL,NULL,NULL);
	  if ( MEDprofilLire(fid,pflval,pflname) <0) {
	    MESSAGE("Erreur a la lecture des valeurs du profil : "); 
	    SSCRUTE(pflname);
	    ret = -1;
	  }
	  printf("\t");
	  for (m=0;m<pflsize;m++) printf(" "IFORMAT" ",*(pflval+m));
	  printf("\n");
	  free(pflval);
	  
	}
	
      }
    } 
  } /* fin for sur les mailles*/
  
  return ret;
}

