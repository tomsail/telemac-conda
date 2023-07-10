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
#include <string.h>

/* #ifdef __cplusplus */
/* } */
/* #endif */

#include "med_utils.h"

#include "med_hdfi21.h"
#include "med_hdfi231.h"
#include "med21.h"

#include "MAJ_21_22.h"

void MAJ_21_22_champs(med_idt fid)
{
  med_idt gid,eid,pid,mid,did,aid;
  med_err ret;
  int n,i,j,k;
  char nom[MED_TAILLE_NOM+1];
  char chemin[MED_TAILLE_CHA+MED_TAILLE_NOM+1];
  char maillage[MED_TAILLE_NOM+1];
  med_int type;
  char *comp, *unit;
  char *nouvelle_chaine;
  med_int ncomp;
  int nent, npdt;
  char noment[MED_TAILLE_NOM+1];
  char nompdt[2*MED_MAX_PARA+1];
  char oldunidt[ANCIEN_MED_TAILLE_PNOM+1];
  char unitdt[MED_TAILLE_PNOM+1];
  char tmp[MED_TAILLE_DESC+1];
  med_int att;
  char profil[MED_TAILLE_NOM+1];
  char gauss[MED_TAILLE_NOM+1];
  char modele[3];
  med_float *valr;
  med_int nval;
  hid_t hdf_type;
  med_size dimd[1];
  med_float pdt;
  

  /* Lecture du nombre de champs */
  n = 0;
  _MEDnObjets(fid,(char *)(MED_CHA),&n);
  EXIT_IF(n < 0,"Erreur a la lecture du nombre de champ",NULL);

  for (i=0;i<n;i++) {
    /* on recupere le nom du champ */
    ret = _MEDobjetIdentifier(fid,(char *) MED_CHA,i,nom);
    EXIT_IF(ret < 0,"Identification d'un champ",NULL);
    fprintf(stdout,"  >>> Normalisation du champ [%s] \n",nom);

    /* on accede au champ */
    strcpy(chemin,(char *)(MED_CHA));
    strcat(chemin,nom);
    gid = _MEDdatagroupOuvrir(fid,chemin); 
    EXIT_IF(gid < 0,"Accès au champ",nom);

    /* Lecture du nombre de composantes */ 
    ret = _MEDattrEntierLire(gid,(char *)(MED_NOM_NCO),&ncomp);
    EXIT_IF(ret < 0,"Lecture du nombre de composante",NULL);

    /* Lecture du type du champ */
    ret = _MEDattrEntierLire(gid,(char *)(MED_NOM_TYP),&type);
    EXIT_IF(ret < 0,"Lecture du type du champ",NULL);

    /* Normalisation des nom et unites des composantes */
    comp = (char *) malloc(ncomp*ANCIEN_MED_TAILLE_PNOM+1);
    EXIT_IF(comp == NULL,NULL,NULL);
    unit = (char *) malloc(ncomp*ANCIEN_MED_TAILLE_PNOM+1);
    EXIT_IF(unit == NULL,NULL,NULL);
    nouvelle_chaine = (char *) malloc(ncomp*MED_TAILLE_PNOM+1);
    EXIT_IF(nouvelle_chaine == NULL,NULL,NULL);

    ret = _MEDattrStringLire(gid,(char *)(MED_NOM_NOM),ncomp*ANCIEN_MED_TAILLE_PNOM,comp);
    EXIT_IF(ret < 0,"Lecture des noms des composantes du champ",NULL);
    _MED23v30stringConvert(nouvelle_chaine, MED_TAILLE_PNOM,
			   comp, ANCIEN_MED_TAILLE_PNOM, ncomp );
/*     MAJ_21_22_chaine(comp,nouvelle_chaine,ncomp); */
    H5Adelete(gid,(char *)(MED_NOM_NOM));
    ret = _MEDattrStringEcrire(gid,(char *)(MED_NOM_NOM),MED_TAILLE_PNOM*ncomp,nouvelle_chaine);
    EXIT_IF(ret < 0,"Ecriture des nouveaux noms des composantes du champ",NULL);  
    free(comp);

    ret = _MEDattrStringLire(gid,(char *)(MED_NOM_UNI),ncomp*ANCIEN_MED_TAILLE_PNOM,unit);
    EXIT_IF(ret < 0,"Lecture des unités des composantes du champ",NULL);
    _MED23v30stringConvert(nouvelle_chaine, MED_TAILLE_PNOM,
			   unit, ANCIEN_MED_TAILLE_PNOM, ncomp );
/*     MAJ_21_22_chaine(unit,nouvelle_chaine,ncomp); */
    H5Adelete(gid,(char *)(MED_NOM_UNI));
    ret = _MEDattrStringEcrire(gid,(char *)(MED_NOM_UNI),MED_TAILLE_PNOM*ncomp,nouvelle_chaine);
    EXIT_IF(ret < 0,"Ecriture des nouvelles unités des composantes du champ",NULL);  
    free(unit);

    free(nouvelle_chaine);
    fprintf(stdout,"  ... Normalisation des noms et unités des composantes effectuée...\n");

    /* On ajoute le groupe HDF correspondant au maillage */
    /* On recupere le nom du maillage par defaut */
    nent = 0;
    _MEDnObjets(gid,(char *)("./"),&nent);
    EXIT_IF(nent < 0,"Erreur a la lecture du nombre d'entité dans un champ",NULL);
    for (j=0;j<nent;j++) {
      /* on recupere le nom du groupe HDF correspondant a l'entite */ 
      ret = _MEDobjetIdentifier(gid,(char *)("./"),j,noment);
      EXIT_IF(ret < 0,"Identification des objets de niveau 1 dans le champ",NULL);

      /* On ouvre le groupe HDF */
      eid = _MEDdatagroupOuvrir(gid,noment);
      EXIT_IF(eid < 0,"Ouverture du groupe HDF de niveau 1",NULL);

      /* On recupere le nombre de pas de temps */
      npdt = 0;
      _MEDnObjets(eid,(char *)("./"),&npdt);
      EXIT_IF(nent < 0,"Erreur a la lecture du nombre de pas de temps",NULL);
      /* On traite chaque pas de temps */
      for (k=0;k<npdt;k++) {
	/* On recupere le nom du groupe HDF correspondant au pas de temps */ 
	ret = _MEDobjetIdentifier(eid,(char *)("./"),k,nompdt);
	EXIT_IF(ret < 0,"Identification des objets de niveau 2 dans le champ",NULL);
	/* On ouvre le groupe HDF correspondant au pas de temps */
	pid = _MEDdatagroupOuvrir(eid,nompdt);
	EXIT_IF(pid < 0,"Ouverture du groupe HDF de niveau 2",NULL);

	/* On lit le pas de temps */
	aid = H5Aopen_name(pid,(char *)(MED_NOM_PDT));
	EXIT_IF(aid < 0,"Ouverture de l'attribut HDF MED_NOM_PDT",NULL);
	hdf_type = H5Aget_type(aid);
	EXIT_IF(hdf_type < 0,"Lecture du type de l'attribut HDF MED_NOM_PDT",NULL);
	ret = H5Aclose(aid);
	EXIT_IF(ret < 0,"Fermeture de l'attribut MED_NOM_PDT",NULL);
	ret = _MED21attrNumLire(pid,MED_FLOAT64,(char *)(MED_NOM_PDT),(unsigned char*)(&pdt),hdf_type);
	EXIT_IF(ret < 0,"Lecture du pas de temps dans MED_NOM_PDT",NULL);
	dimd[0] = 1;
	/* 	printf("EGALITE des types : %d \n",H5Tequal(hdf_type,H5T_NATIVE_DOUBLE)); */
	/* 	printf("Avant conversion : [%f]\n",pdt); */
	if (! H5Tequal(hdf_type,H5T_NATIVE_DOUBLE)) {
	  ret = H5Tconvert(hdf_type,H5T_NATIVE_DOUBLE,(hsize_t)*dimd,(void *)(&pdt),NULL,0);
	  /* 	printf("Après conversion éventuelle : [%f] \n",pdt); */
	  EXIT_IF(ret < 0,"Conversion du pas de temps",NULL);
	}
	H5Tclose(hdf_type);
	H5Adelete(pid,(char *)(MED_NOM_PDT));
	ret = _MEDattrFloatEcrire(pid,(char *)(MED_NOM_PDT),&pdt);
	EXIT_IF(ret < 0,"Ecriture du nouveau pas de temps",NULL);

	/* On met a jour l'unité du pas de temps : 8 -> 16 caractères 
           Si on rencontre "        " => on ecrit "" 
        */
	ret = _MEDattrStringLire(pid,(char *)(MED_NOM_UNI),ANCIEN_MED_TAILLE_PNOM,oldunidt);
	EXIT_IF(ret < 0,"Lecture de l'unite du pas de temps",NULL);
	if (! strcmp(oldunidt,ANCIEN_MED_BLANC_PNOM))
	  strcpy(unitdt,"");
	else {
/* 	  MAJ_21_22_chaine(oldunidt,unitdt,1); */
	  _MED23v30stringConvert(unitdt, MED_TAILLE_PNOM,
				 oldunidt, ANCIEN_MED_TAILLE_PNOM, 1 );

	}
	H5Adelete(pid,(char *)(MED_NOM_UNI));
	ret = _MEDattrStringEcrire(pid,(char *)(MED_NOM_UNI),MED_TAILLE_PNOM,unitdt);
	EXIT_IF(ret < 0,"Ecriture de la nouvelle unité du pas de temps",NULL);  
	/* On recupere le nom du maillage */
	ret = _MEDattrStringLire(pid,(char *)(MED_NOM_MAI),MED_TAILLE_NOM,maillage);
	EXIT_IF(ret < 0,"Lecture du nom du maillage du pas de temps",NULL);
	/* On cree le groupe HDF de niveau 3 qui porte le nom du maillage */
	mid = _MEDdatagroupCreer(pid,maillage);
	EXIT_IF(mid < 0,"Creation du groupe HDF de niveau 3",NULL);
	/* Déplacement de l'attribut MED_NOM_NBR */
	ret = _MEDattrEntierLire(pid,(char *)(MED_NOM_NBR),&nval);
	EXIT_IF(ret < 0,"Lecture de l'attribut MED_NOM_NBR",NULL);
	ret = H5Adelete(pid,(char *)(MED_NOM_NBR));
	EXIT_IF(ret < 0,"Destruction de l'attribut MED_NOM_NBR",NULL);
	ret = _MEDattrEntierEcrire(mid,(char *)(MED_NOM_NBR),&nval);
	EXIT_IF(ret < 0,"Ecriture de l'attribut MED_NOM_NBR au niveau 3",NULL);

	/* Deplacement de l'attribut MED_NOM_NGA */
	ret = _MEDattrEntierLire(pid,(char *)(MED_NOM_NGA),&att);
	EXIT_IF(ret < 0,"Lecture de l'attribut MED_NOM_NGA",NULL);
	if ( (att > 100) || (att < 1)) {
	  MESSAGE("Detection d'un nombre incorrect de nombre de points de gauss, réinitialisation à 1");
	  att=1;
	}
	ret = H5Adelete(pid,(char *)(MED_NOM_NGA));
	EXIT_IF(ret < 0,"Destruction de l'attribut MED_NOM_NGA",NULL);
	ret = _MEDattrEntierEcrire(mid,(char *)(MED_NOM_NGA),&att);
	EXIT_IF(ret < 0,"Ecriture de l'attribut MED_NOM_NGA au niveau 3",NULL);

	/* Ecriture de l'attribut MED_NOM_GAU : localisation des points de Gauss */
	if (att < 2)
	  strcpy(gauss,(char *)(MED_NOM_BLANC));
	else {
	  /* on prend en compte que les 3 dernières lettres de noment + nb de points de Gauss : 
	     Exemple : "MAI.QU4" et 4 points => gauss="QU4_04_LocalisationDefaut_IMPORT */
	  strcpy(gauss,noment+4);
	  /* on recupere dans le nom, le nombre de points de Gauss */
	  /*	  sprintf(modele,"%0*d",2,att);*/
	  sprintf(modele, "%02d"  ,(int) att);
	  modele[2] = '\0';
	  strcat(gauss,"_");
	  strcat(gauss,modele);
	  strcat(gauss,"_LocalisationDefaut_IMPORT");
	  /* On ecrit en plus la localisation bidon des points de Gauss */
	  MAJ_21_22_localisation_Gauss(fid,gauss,att);
	}
	ret = _MEDattrStringEcrire(mid,(char *)(MED_NOM_GAU),MED_TAILLE_NOM,gauss);
	EXIT_IF(ret < 0,"Ecriture de la localisation des points de Gauss",NULL);

	/* Deplacement de l'attribut MED_NOM_PFL */
	ret = _MEDattrStringLire(pid,(char *)(MED_NOM_PFL),MED_TAILLE_NOM,profil);
	EXIT_IF(ret < 0,"Lecture de l'attribut MED_NOM_PFL",NULL);
	ret = H5Adelete(pid,(char *)(MED_NOM_PFL));
	EXIT_IF(ret < 0,"Desctruction de l'attribut MED_NOM_PFL",NULL);
	ret = _MEDattrStringEcrire(mid,(char *)(MED_NOM_PFL),MED_TAILLE_NOM,profil);
	EXIT_IF(ret < 0,"Ecriture de l'attribut MED_NOM_PFL au niveau 3",NULL);

	/* On ferme le groupe HDF de niveau 3 */
	ret = _MEDdatagroupFermer(mid);
	EXIT_IF(ret < 0,"fermeture du groupe HDF de niveau 3",NULL);

	/* On deplace du niveau 2 -> 3, le dataset MED_NOM_CO 
           avec si type==MED_FLOAT_64 une conversion au passage */
	strcpy(tmp,maillage);
	strcat(tmp,"/");
	strcat(tmp,(char *)(MED_NOM_CO));

	if (type != MED_FLOAT64) {
	  ret = H5Gmove(pid,(char *)(MED_NOM_CO),tmp);
	  EXIT_IF(ret < 0,"Transfert du dataset MED_NOM_CO",NULL);
	} else {
	  did = _MEDdatasetOuvrir(pid,(char *)(MED_NOM_CO));
	  EXIT_IF(did < 0,"Ouverture du dataset HDF des valeurs",NULL);
	  hdf_type = H5Dget_type(did);
	  EXIT_IF(hdf_type < 0,"Lecture du type HDF de stockage des valeurs",NULL);
	  ret = _MEDdatasetFermer(did);
	  EXIT_IF(ret < 0,"Fermeture du dataset HDF des valeurs",NULL);
	  valr = (med_float *) malloc(sizeof(med_float)*ncomp*nval);
	  EXIT_IF(valr == NULL,NULL,NULL);
	  ret = _MED21datasetNumLire(pid,(char *)(MED_NOM_CO),MED_FLOAT64,
				      MED_FULL_INTERLACE,ncomp,MED_ALL,
				      0,NULL,MED_NOPG,
				      (unsigned char*) valr,hdf_type);
	  EXIT_IF(ret < 0,"Lecture des valeurs du champ",NULL);
	  H5Gunlink(pid,(char *)(MED_NOM_CO));
	  dimd[0] = ncomp*nval;
	  ret = H5Tconvert(hdf_type,H5T_NATIVE_DOUBLE,(hsize_t)*dimd,(void *)valr,NULL,0);
	  EXIT_IF(ret < 0,"Conversion des valeurs",NULL);
	  H5Tclose(hdf_type);
	  /* On reporte les valeurs dans le groupe HDF TMP */
	  ret = _MED231datasetNumEcrire(pid,tmp,MED_FLOAT64,MED_FULL_INTERLACE,
					ncomp,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,MED_NOPG,dimd,
					(unsigned char*) valr);
	  EXIT_IF(ret < 0,"Ecriture des valeurs après conversion",NULL);
	  free(valr);
	}
	  
	/* On ferme le groupe HDF de niveau 2*/
	ret = _MEDdatagroupFermer(pid);
	EXIT_IF(ret < 0,"Fermeture du groupe HDF de niveau 2",NULL);
      }

      /* On ferme le groupe HDF */
      ret = _MEDdatagroupFermer(eid);
      EXIT_IF(ret < 0,"Fermeture du groupe HDF de niveau 1",NULL);
    }
    fprintf(stdout,"  ... Normalisation de tous les pas de temps effectuée...\n");

    /* On ferme tout */
    ret = _MEDdatagroupFermer(gid);
    EXIT_IF(ret < 0,"Fermeture de l'accès au champ",NULL);
    
    fprintf(stdout,"  >>> Normalisation du champ [%s] : ... OK ... \n",nom);
  }
}
