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

#include <assert.h>

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

#ifndef USER_INTERLACE
#define USER_INTERLACE MED_FULL_INTERLACE
#endif

#define USER_MODE MED_COMPACT_STMODE

med_err getFieldsOn(med_idt fid, char * nomcha, med_field_type typcha, med_int ncomp,
		    med_entity_type entite, med_switch_mode stockage, med_int ncstp);

int main (int argc, char **argv)


{
  med_err ret,lret;
  med_idt fid;
  char * fichier = NULL;
  char maa[MED_NAME_SIZE+1]="";
  char desc[MED_COMMENT_SIZE+1]="";
  char pflname[MED_NAME_SIZE+1]="",nomlien[MED_NAME_SIZE+1]="";
  char _meshname [MED_NAME_SIZE+1]="";
  char _dtunit [MED_SNAME_SIZE+1]="";
  char locname[MED_NAME_SIZE+1]="";
  char * lien = NULL;
  char *comp= NULL, *unit= NULL;
  char nomcha  [MED_NAME_SIZE+1]="";
  med_int mdim=0,sdim=0,ncomp,ncha,npro,nln,pflsize,*pflval,nval;
  med_int _ncstp=0,ngauss=0,nloc=0,locsdim=0,lnsize=0;
  int t1,t2,t3;
  med_field_type    typcha;
  med_geometry_type type_geo;
  med_float *refcoo, *gscoo, *wg;
  int i,j;
  med_bool _local;

  char dtunit[MED_SNAME_SIZE+1]="";
  char nomcoo[3*MED_SNAME_SIZE+1]="";
  char unicoo[3*MED_SNAME_SIZE+1]="";
  char geointerpname[MED_NAME_SIZE+1]="";
  char ipointstructmeshname[MED_NAME_SIZE+1]="";
  med_mesh_type type;
  med_sorting_type sort;
  med_int nstep=0;
  med_axis_type rep;
  med_int nsectionmeshcell;
  med_geometry_type sectiongeotype;

  if (argc != 2) {
    MESSAGE("Aucun nom de fichier precise, fichier test10.med utilise ");
    fichier = "test10.med";
  } else {
    fichier = argv[1];
  };


  /* Ouverture du fichier med */
  if ((fid = MEDfileOpen(fichier,MED_ACC_RDONLY)) < 0){
    MESSAGE("Erreur a l'ouverture du fichier : ");SSCRUTE(fichier);
    return -1;
  }

  ret = 0;


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


  /* combien de champs dans le fichier */
  if ((ncha = MEDnField(fid)) < 0) {
    MESSAGE("Impossible de lire le nombre de champs : ");ISCRUTE(ncha);
    return ncha;
  }

  printf("Nombre de champs : "IFORMAT" \n",ncha);

  /* lecture de tous les champs  */
  for (i =0;i<ncha;i++) {
    lret = 0;
    printf("\nChamp numero : %d \n",i+1);

    /* Lecture du nombre de composantes */
    if ((ncomp = MEDfieldnComponent(fid,i+1)) < 0) {
      MESSAGE("Erreur a la lecture du nombre de composantes : "); ISCRUTE(ncomp);
      ret = -1; continue;
    }

    /* Lecture du type du champ, des noms des composantes et du nom de l'unite*/
    comp = (char*) malloc(ncomp*MED_SNAME_SIZE+1);
    EXIT_IF(comp == NULL,NULL,NULL);
    unit = (char*) malloc(ncomp*MED_SNAME_SIZE+1);
    EXIT_IF(unit == NULL,NULL,NULL);

    if ( MEDfieldInfo(fid,i+1,nomcha,_meshname,&_local,&typcha,comp,unit,_dtunit,&_ncstp) < 0 ) {
      MESSAGE("Erreur a la demande d'information sur les champs : ");
      ISCRUTE_int(i+1);SSCRUTE(nomcha);ISCRUTE_int(typcha);SSCRUTE(comp);SSCRUTE(unit);
      ISCRUTE(ncomp);
      ret = -1; continue;
    }


    printf("Nom du champ : |%s| de type %d\n",nomcha,typcha);
    printf("Nombre de composantes : |"IFORMAT"|\n",ncomp);
    printf("Nom des composantes : |%s|\n",comp);
    printf("Unites des composantes : |%s| \n",unit);
    printf("Unites des dates  : |%s| \n",_dtunit);
    printf("Le maillage associé est |%s|\n",_meshname);
    printf("Nombre de séquences de calcul |"IFORMAT"|\n",_ncstp);

      /* Le maillage reference est-il porte par un autre fichier */
    if ( !_local ) {

      if ( (lnsize=MEDlinkInfoByName(fid,_meshname) ) < 0 )  {
	MESSAGE("Erreur a la lecture de la taille du lien : ");
	SSCRUTE(_meshname);
	ret = -1;
      } else {

	lien = malloc((lnsize+1)*sizeof(char));
	  EXIT_IF(lien == NULL,NULL,NULL);

	  if ( MEDlinkRd(fid,_meshname, lien) < 0 )  {
	    MESSAGE("Erreur a la lecture du lien : ");
	    SSCRUTE(_meshname);SSCRUTE(lien);
	    ret = -1;
	  } else {
	    printf("\tLe maillage |%s| est porte par un fichier distant |%s|\n",_meshname,lien);
	  }
	  free(lien);
	}
      }
    
    free(comp);
    free(unit);
    
      
    lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_NODE, USER_INTERLACE,_ncstp );
    
    if (lret == 0) lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_CELL, USER_INTERLACE,_ncstp );
    else { MESSAGE("Erreur a la lecture des champs aux noeuds "); ret = -1; continue;}
   
    if (lret == 0) lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_DESCENDING_FACE,USER_INTERLACE,_ncstp);
    else { MESSAGE("Erreur a la lecture des champs aux mailles "); ret = -1; continue;}
   
    if (lret == 0) lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_DESCENDING_EDGE,USER_INTERLACE,_ncstp);
    else {MESSAGE("Erreur a la lecture des champs aux faces "); ret = -1; continue;}
    
    if (lret == 0) lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_NODE_ELEMENT,USER_INTERLACE,_ncstp);
    else {MESSAGE("Erreur a la lecture des champs aux aretes"); ret = -1; continue;}
    
    /*TODO */
/*     if (lret == 0) lret = getFieldsOn(fid, nomcha, typcha, ncomp, MED_STRUCT_ELEMENT,USER_INTERLACE,_ncstp); */
/*     else {MESSAGE("Erreur a la lecture des champs aux éléments de structure"); ret = -1; continue;} */
    
    if  (lret != 0) {MESSAGE("Erreur a la lecture des champs aux noeuds des mailles "); ret = -1;};
  }


  /* Interrogation des profils */
  npro = MEDnProfile(fid);

  printf("\nNombre de profils stockes : "IFORMAT"\n\n",npro);
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
      printf("\t");
      for (j=0;j<nval;j++) printf(" "IFORMAT" ",*(pflval+j));
      printf("\n\n");
    }
    free(pflval);
  }

  /* Interrogation des liens */
  nln = MEDnLink(fid);

  printf("\nNombre de liens stockes : "IFORMAT"\n\n",nln);
  for (i=1 ; i <= nln ; i++ ) {
    if ( MEDlinkInfo(fid, i, nomlien, &nval) < 0)  {
      MESSAGE("Erreur a la demande d'information sur le lien n° : "); ISCRUTE_int(i);
      ret = -1;continue;
    }
    printf("\t- Lien n°%i de nom |%s| et de taille "IFORMAT"\n",i,nomlien,nval);

    lien = malloc((nval+1)*sizeof(char));
    EXIT_IF(lien == NULL,NULL,NULL);

    if ( MEDlinkRd(fid, nomlien, lien ) < 0 )  {
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
  nloc = MEDnLocalization(fid);

  printf("\nNombre de localisations stockees : "IFORMAT"\n\n",nloc);
  for (i=1 ; i <= nloc ; i++ ) {
    if ( MEDlocalizationInfo(fid, i, locname, &type_geo, &locsdim,&ngauss,
			     geointerpname, ipointstructmeshname,&nsectionmeshcell,
			     &sectiongeotype) < 0)  {
      MESSAGE("Erreur a la demande d'information sur la localisation n° : "); ISCRUTE_int(i);
      ret = -1;continue;
    }
    printf("\t- Loc. n°%i de nom |%s| de dimension |"IFORMAT"| avec "IFORMAT" pts de GAUSS \n",i,locname,locsdim,ngauss);
    t1 = (type_geo%100)*(type_geo/100);
    t2 = ngauss*(type_geo/100);
    t3 = ngauss;
    refcoo = (med_float *) malloc(sizeof(med_float)*t1 );
    gscoo  = (med_float *) malloc(sizeof(med_float)*t2 );
    wg     = (med_float *) malloc(sizeof(med_float)*t3 );

    if ( MEDlocalizationRd(fid, locname, USER_INTERLACE, refcoo, gscoo, wg  ) < 0) {
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
  if ( MEDfileClose(fid) < 0) return -1;

  return ret;
}

med_err getFieldsOn(med_idt fid, char * nomcha, med_field_type typcha, med_int ncomp,
		    med_entity_type entite, med_switch_mode stockage, med_int ncstp) {

  int j,k,l,m,n,nb_geo=0;
  med_int  nbpdtnor=0,pflsize,*pflval,ngauss=0,ngroup,nval;
  med_int  numdt=0,numo=0,_nprofile;
  med_size medtype_size=0;
  med_float dt=0.0;
  unsigned char *val = NULL; 
  med_err ret=0;
  char pflname [MED_NAME_SIZE+1]="";
  char locname [MED_NAME_SIZE+1]="";
  char * lien = NULL;
  char dt_unit [MED_SNAME_SIZE+1]="unknown";

  med_geometry_type * type_geo;

  const char * const * AFF;
  const char * const * AFF_ENT=MED_GET_ENTITY_TYPENAME+1;
  switch (entite) {
  case MED_NODE :
    type_geo = MED_GET_NODE_GEOMETRY_TYPE;
    nb_geo   = MED_N_NODE_FIXED_GEO;
    AFF      = MED_GET_NODE_GEOMETRY_TYPENAME;
    break;
  case  MED_CELL :
  case  MED_NODE_ELEMENT :
    type_geo = MED_GET_CELL_GEOMETRY_TYPE;
    nb_geo   = MED_N_CELL_FIXED_GEO;
    AFF      = MED_GET_CELL_GEOMETRY_TYPENAME;
    break;
  case  MED_DESCENDING_FACE :
    type_geo = MED_GET_FACE_GEOMETRY_TYPE;
    nb_geo   = MED_N_FACE_FIXED_GEO;
    AFF      = MED_GET_FACE_GEOMETRY_TYPENAME;
    break;
  case  MED_DESCENDING_EDGE :
    type_geo = MED_GET_EDGE_GEOMETRY_TYPE;
    nb_geo   = MED_N_EDGE_FIXED_GEO;
    AFF      = MED_GET_EDGE_GEOMETRY_TYPENAME;
    break;
  }

  for (k=1;k<=nb_geo;k++) {

    /* Combien de (PDT,NOR) a lire */
    nbpdtnor = ncstp;
    if (nbpdtnor < 1 ) continue;

    for (j=0;j<nbpdtnor;j++) {

      if ( MEDfieldComputingStepInfo(fid,nomcha,j+1, &numdt, &numo, &dt ) <0) {
	MESSAGE("Erreur a la demande d'information sur (pdt,nor) : ");
	ISCRUTE(numdt); ISCRUTE(numo);
	ret = -1; continue;
      }

      if ( (_nprofile = MEDfieldnProfile(fid,nomcha,numdt,numo,entite,type_geo[k],
					    pflname,locname   ) ) < 0 ) {
	MESSAGE("Erreur a la demande du nombre de profils referencés par le champ : ");
	SSCRUTE(nomcha);
	ISCRUTE(numdt); ISCRUTE(numo);
	ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	SSCRUTE(AFF_ENT[(int)entite]);SSCRUTE(AFF[k]);
	ret = -1; continue;
      };

      for (l=0;l<_nprofile;l++) {


	if ( (nval = MEDfieldnValueWithProfile(fid, nomcha, numdt, numo, entite, type_geo[k],
					       l+1,  USER_MODE, pflname,&pflsize,
					       locname, &ngauss) ) < 0 ) {
	  MESSAGE("Erreur a la lecture du nombre de valeurs du champ : ");
	  SSCRUTE(nomcha);ISCRUTE(numdt);ISCRUTE(numo);
	  ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	  ISCRUTE_int(USER_MODE);
	  ret = -1; continue;
	};

	printf("\n  +Pas de Temps n."IFORMAT" (%f) [%s], n. d'ordre "IFORMAT", avec "IFORMAT" valeur(s) par entité.\n",numdt,dt,dt_unit,numo,ngauss);
	printf("\t- Il y a "IFORMAT" entités qui portent des valeurs en mode %i. Chaque entite %s\
 de type geometrique %s associes au profile |%s| a "IFORMAT" valeurs associées \n",
	       nval,USER_MODE,AFF_ENT[(int)entite],AFF[k],pflname,ngauss);

	/*Lecture des valeurs du champ */
	switch(typcha)  {
	case MED_FLOAT64: medtype_size=sizeof(med_float64); break;
	case MED_FLOAT32: medtype_size=sizeof(med_float32); break;
	case MED_INT32  : medtype_size=sizeof(med_int32  ); break;
	case MED_INT64  : medtype_size=sizeof(med_int64  );
	  break;
	case MED_INT    : medtype_size=sizeof(med_int)    ; break;
	default:
	  MESSAGE("Erreur a la lecture du type de champ : ");
	  ISCRUTE_int(typcha);
          EXIT_IF(NULL == NULL,NULL,NULL);
	}

	val = (unsigned char*) calloc(ncomp*nval*ngauss,medtype_size);
	EXIT_IF(val == NULL,NULL,NULL);
	
	if (MEDfieldValueWithProfileRd(fid, nomcha, numdt,numo, entite,type_geo[k],
				       USER_MODE, pflname, stockage,MED_ALL_CONSTITUENT, val) < 0 ) {
	  MESSAGE("Erreur a la lecture des valeurs du champ : ");
	  SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	  ISCRUTE(numdt);ISCRUTE(numo);
	  ret = -1;
	}

	if ( strlen(locname) )
	  printf("\t- Modèle de localisation des points de Gauss de nom |%s|\n",locname);

	if (entite == MED_NODE_ELEMENT)
	  ngroup = (type_geo[k] % 100);
	else
	  ngroup = ngauss;

	switch (stockage) {
	  
	case MED_FULL_INTERLACE :
	  printf("\t- Valeurs :\n\t");
	  for (m=0;m<(nval*ngauss)/ngroup;m++) {
	    printf("|");
	    for (n=0;n<ngroup*ncomp;n++)
	      switch(typcha)  {
	      case MED_FLOAT64:
		printf("  %f ",*(((med_double*)val)+(m*ngroup*ncomp)+n  ) );
	      	/* printf("  %f ",  ((med_double*)val)[(m*ngroup*ncomp)+n]    ); */
		/* printf("  %f ", *( val+medtype_size*((m*ngroup*ncomp)+n))  ); */
		break;
	      case MED_FLOAT32:
	      	printf(" %f ",*(((med_float32*)val)+((m*ngroup*ncomp)+n)));
	      	break;
	      case MED_INT32  :
	      	printf(" %d ",*(((med_int32*)val)+(m*ngroup*ncomp)+n));
	      	break;
	      case MED_INT64  :
	      	printf(" %lld ",*(((med_int64*)val)+(m*ngroup*ncomp)+n));
	      	break;
	      case MED_INT    :
	      	printf(" "IFORMAT" ",*(((med_int*)val)+(m*ngroup*ncomp)+n));
	      	break;
	      default:
	      	break;
	      }
	  }
	  break;
	  /*Affichage en fonction du profil à traiter*/
	case MED_NO_INTERLACE :
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
	      	printf(" %lld ",*(((med_int64*)val)+(m*nval*ngauss)+n));
	      	break;
	      case MED_INT    :
	      	printf(" "IFORMAT" ",*(((med_int*)val)+(m*nval*ngauss)+n));
	      	break;
	      default:
	      	break;
	      }
	  }
	  break;
	}

	printf("|\n");
	if ( val ) {free(val);val = NULL;}

	/*Lecture du profil associe */
	if (strcmp(pflname,MED_NO_PROFILE) == 0 )
	  printf("\t- Profil : MED_NO_PROFILE\n");
	else {

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

