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
#define _a 0.446948490915965
#define _b 0.091576213509771
#define _p1 0.11169079483905
#define _p2 0.0549758718227661

/******************************************************************************
 * - Nom du fichier : test10.c
 *
 * - Description : ecriture de champs de resultats MED 
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

#define USER_INTERLACE MED_FULL_INTERLACE
#define USER_MODE MED_COMPACT
 
int main (int argc, char **argv)


{
  med_err ret=0;
  med_idt fid;
  


  /* Maillage support aux champs*/
  /* Ces maillages sont vides*/
  char maa1[MED_TAILLE_NOM+1]= "maa1";
  char maa2[MED_TAILLE_NOM+1]= "maa2";
  char * lien_maa2 = "./testfoo.med";
  char maa3[MED_TAILLE_NOM+1]= "maa3";


  /* Caractéristiques du champ n° 1 sur TRIA6 */
  char nomcha1[MED_TAILLE_NOM+1]  = "champ reel";
  char comp1[2*MED_TAILLE_PNOM+1] = "comp1           comp2           ";
                                   /*12345678901234561234567890123456*/
  char unit1[2*MED_TAILLE_PNOM+1] = "unit1           unit2           ";
  med_int ncomp1  = 2; 
  /* Caractéristiques du model n° 1 de localisation des points de gauss pour le champ n°1*/
  med_int ngauss1_1 = 6;
  char gauss1_1[MED_TAILLE_NOM+1]  = "Model n1";
  med_float refcoo1[12] = { -1.0,1.0, -1.0,-1.0, 1.0,-1.0, -1.0,0.0, 0.0,-1.0, 0.0,0.0 }; 

  /* Constantes */

  med_float gscoo1_1[12] = { 2*_b-1, 1-4*_b, 2*_b-1, 2*_b-1, 1-4*_b, 
                             2*_b-1, 1-4*_a, 2*_a-1, 2*_a-1, 1-4*_a, 2*_a-1, 2*_a-1 }; 
  med_float wg1_1[6] = { 4*_p2, 4*_p2, 4*_p2, 4*_p1, 4*_p1, 4*_p1 }; 

  med_int   nval1_1= 1*6; /*1 valeurs et 6 points de gauss par valeur */
  med_float valr1_1[1*6*2]  = {0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0, 20.0,21.0, 22.0,23.0};  /* 2 composantes*/
  /* Caractéristiques du model n° 2 de localisation des points de gauss pour le champ n°1*/
  med_int ngauss1_2 = 3;
  char gauss1_2[MED_TAILLE_NOM+1]  = "Model n2";
  med_float gscoo1_2[6] = { -2.0/3,1.0/3, -2.0/3,-2.0/3, 1.0/3,-2.0/3  }; 
  med_float wg1_2[3] = { 2.0/3, 2.0/3, 2.0/3 }; 
  med_int   nval1_2= 2*3; /*2 valeurs et 3 points de gauss par valeur */
  med_float valr1_2[2*3*2]  = {0.0,1.0, 2.0,3.0, 10.0,11.0,   12.0,13.0, 20.0,21.0, 22.0,23.0};  /* 2 composantes*/
  med_float valr1_2p[2*3*2]  = {                              12.0,13.0, 20.0,21.0, 22.0,23.0};  /* 2 composantes*/
 /* Caractéristiques du model n° 3 sans points de gauss pour le champ n°1*/
  med_int   nval1_3= 6; /*6 valeurs et pas de points de gauss */
  med_float valr1_3[2*3*2]  = {0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0, 20.0,21.0, 22.0,23.0};  /* 2 composantes*/
  med_float valr1_3p[2*2*2] = {         2.0,3.0, 10.0,11.0                                 };  /* 2 composantes profil1 */
  
  /* Caractéristiques du champ n° 2 */
  char nomcha2[MED_TAILLE_NOM+1]  = "champ entier";
  char comp2[3*MED_TAILLE_PNOM+1] = "comp1           comp2           comp3           ";
                                   /*123456789012345612345678901234561234567890123456*/
  char unit2[3*MED_TAILLE_PNOM+1] = "unit1           unit2           unit3           ";
  med_int ncomp2  = 3; 
  med_int nval2 = 5;   /*5 valeurs */
  med_int   valr2[5*3  ]  = {0,1,2, 10,11,12, 20,21,22, 30,31,32, 40,41,42};              /* 3 composantes*/
  med_int   valr2p[3*3  ] = {0,1,2,           20,21,22,           40,41,42};              /* 3 composantes*/

  /* Profils utilisés */
  char nomprofil1[MED_TAILLE_NOM+1]  = "PROFIL(champ(1))";
  char nomprofil2[MED_TAILLE_NOM+1]  = "PROFIL(champ2)";
  med_int profil1[2] = { 2, 3 };
  med_int profil2[3] = { 1, 3, 5 };

  /* ouverture du fichier */
  if ((fid = MEDouvrir("test10.med",MODE_ACCES)) < 0){
    MESSAGE("Erreur à l'ouverture du fichier : ");
    return -1;
  }
  
  /* creation de maa1 de dimension 3*/
  if ( MEDmaaCr(fid,maa1,3,MED_NON_STRUCTURE,"Maillage vide") < 0) {
    MESSAGE("Erreur à la création du maillage : ");SSCRUTE(maa1);
    ret = -1;
  };

  /* creation de maa3 de dimension 3*/
  if ( MEDmaaCr(fid,maa3,3,MED_NON_STRUCTURE,"Maillage vide") < 0) {
    MESSAGE("Erreur à la création du maillage : ");SSCRUTE(maa3);
    ret = -1;
  };
    
  /* creation du champ réel n°1 */
  if ( MEDchampCr(fid,nomcha1,MED_FLOAT64,comp1,unit1,ncomp1) < 0) {
    MESSAGE("Erreur à la création du champ : ");SSCRUTE(nomcha1);
    ret = -1;
  };
  
  /* creation du champ entier n°2 */
  if ( MEDchampCr(fid,nomcha2,MED_INT32,comp2,unit2,ncomp2) < 0) {
    MESSAGE("Erreur à la création du champ : ");SSCRUTE(nomcha2);
    ret = -1;
  };
  
  /* creation du lien au fichier distant contenant maa2 */
  if (MEDlienEcr(fid,lien_maa2,maa2) < 0) {
    MESSAGE("Erreur à la création du lien : ");SSCRUTE(lien_maa2);
    ret = -1;
  };

  /* creation de la localisation des points de Gauss modèle n°1 */
  if (MEDgaussEcr(fid, MED_TRIA6, refcoo1, USER_INTERLACE,
		  ngauss1_1, gscoo1_1, wg1_1, gauss1_1) < 0) {
    MESSAGE("Erreur à la création du modèle n°1 : ");SSCRUTE(gauss1_1);
    ret = -1;
  };

   /* creation de la localisation des points de Gauss modèle n°2 */
  if (MEDgaussEcr(fid, MED_TRIA6, refcoo1, USER_INTERLACE,
		  ngauss1_2, gscoo1_2, wg1_2, gauss1_2) < 0) {
    MESSAGE("Erreur à la création du modèle n°1 : ");SSCRUTE(gauss1_2);
    ret = -1;
  };
 
  /* ecriture du champ n°1*/
  /* enregistre uniquement les composantes n°2 de valr1_1, et n'utilise ni pas de temps ni n° d'ordre*/
  if ( MEDchampEcr(fid,maa1,nomcha1,(unsigned char*)valr1_1,USER_INTERLACE,nval1_1,gauss1_1,
		   2,MED_NOPFL,MED_NO_PFLMOD,MED_MAILLE,MED_TRIA6,MED_NOPDT,"",0.0,MED_NONOR ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE(MED_NOPDT);ISCRUTE(MED_NONOR);SSCRUTE(MED_NOPFL);
    SSCRUTE(maa1);
    ret = -1;
  };
  

  /* enregistre uniquement les composantes n°1 de valr1_1, et n'utilise ni pas de temps ni n° d'ordre */
  if ( MEDchampEcr(fid,maa1,nomcha1,(unsigned char*)valr1_1,USER_INTERLACE,nval1_1,gauss1_1,
		      1,MED_NOPFL,MED_NO_PFLMOD,MED_MAILLE,MED_TRIA6,MED_NOPDT,"",0.0,MED_NONOR) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE(MED_NOPDT);ISCRUTE(MED_NONOR);SSCRUTE(MED_NOPFL);
    SSCRUTE(maa1);
    ret = -1;
  };


  /* enregistre uniquement les composantes n°1 de valr1_2, au pas de temps n°1(5.5), n'utilise pas de n°d'ordre*/
  /* ce champ repose sur le maillage maa2 qui est distant */
  if ( MEDchampEcr(fid,maa2,nomcha1,(unsigned char*)valr1_2,USER_INTERLACE,nval1_2,gauss1_2,
		      1,MED_NOPFL,MED_NO_PFLMOD,MED_MAILLE,MED_TRIA6,1,"ms",5.5,MED_NONOR) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE(1);ISCRUTE(MED_NONOR);SSCRUTE(MED_NOPFL);
    SSCRUTE(maa2);
    ret = -1;
  };

  /* enregistre uniquement les composantes n°2 de valr1_2, au pas de temps n°1(5.5), n'utilise pas de n°d'ordre*/
  /* ce champ repose sur le maillage maa1 qui est local */
  if ( MEDchampEcr(fid,maa1,nomcha1,(unsigned char*)valr1_1,USER_INTERLACE,nval1_1,gauss1_1,
		      2,MED_NOPFL,MED_NO_PFLMOD,MED_MAILLE,MED_TRIA6,1,"ms",5.5,MED_NONOR) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE(1);ISCRUTE(MED_NONOR);SSCRUTE(MED_NOPFL);
    SSCRUTE(maa1);
    ret = -1;
  };

    /* enregistre uniquement les composantes n°1 de valr1_1, au pas de temps n°1(5.5), et n°d'itération n°2*/
    /* ce champ repose sur le maillage maa3 qui est local */
  if ( MEDchampEcr(fid,maa3,nomcha1,(unsigned char*)valr1_2,USER_INTERLACE,nval1_2,gauss1_2,
		   1,MED_NOPFL,MED_NO_PFLMOD,MED_MAILLE,MED_TRIA6,1,"ms",5.5,2) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE(1);ISCRUTE(2);SSCRUTE(MED_NOPFL);
    SSCRUTE(maa3);
    ret = -1;
  };
  
  /* Creation d'un profil (selection  du deuxieme élément de valr1_1) */
  /* On n'utilise que la première valeur (2) du profil */
  if ( MEDprofilEcr(fid,profil1,1,nomprofil1) < 0) {
    MESSAGE("Erreur à l'écriture du profil : ");
    SSCRUTE(nomprofil1);
    ret = -1;
  };


  /* enregistre toutes les composantes du deuxième élément de valr1_1 (premier élément en stockage compact de valr1p),
     au pas de temps n°2(5.6), et n°d'itération n°2*/
  if ( MEDchampEcr(fid,maa1, nomcha1,(unsigned char*)valr1_3p,USER_INTERLACE,nval1_3,MED_NOGAUSS,
		   MED_ALL,nomprofil1,USER_MODE,MED_MAILLE,MED_TRIA6,2,"ms",5.6,2) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE(2);ISCRUTE(2);SSCRUTE(nomprofil1);
    SSCRUTE(maa1);SSCRUTE(MED_NOLIEN);
    ret = -1;
  };

  /* enregistre toutes les composantes du deuxième élément de valr1_1 (premier élément en stockage compact de valr1p),
     au pas de temps n°2(5.6), et n°d'itération n°2 */
  if ( MEDchampEcr(fid,maa2, nomcha1,(unsigned char*)valr1_2p,USER_INTERLACE,nval1_2,gauss1_2,
		   MED_ALL,nomprofil1,USER_MODE,MED_MAILLE,MED_TRIA6,2,"ms",5.6,2) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE(2);ISCRUTE(2);SSCRUTE(nomprofil1);
    SSCRUTE(maa1);SSCRUTE(MED_NOLIEN);
    ret = -1;
  }; 


  /* enregistre la composante n°2 du deuxième élément de valr1, au pas de temps n°2(5.7), et n°d'itération n°2*/
  if ( MEDchampEcr(fid,maa1,nomcha1,(unsigned char*)valr1_3p,USER_INTERLACE,nval1_3,MED_NOGAUSS,
		   2,nomprofil1,USER_MODE,MED_MAILLE,MED_TRIA6,3,"ms",5.7,2) < 0)  {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE(2);ISCRUTE(2);SSCRUTE(nomprofil1);
    SSCRUTE(maa1);SSCRUTE(MED_NOLIEN);
    ret = -1;
  };
    
    
    
  /* Ecriture du champ n° 2 */
  /* TESTER EGALEMENT EN MODE FULL_INTERLACE EN MODIFIANT LA SIGNATURE DE LA ROUTINE */
    
  /* enregistre la composante n°1 des éléments de valr2, et n'utilise ni pas de temps ni n° d'ordre */
  if (MEDchampEcr(fid,maa1,nomcha2,(unsigned char*)valr2,USER_INTERLACE,nval2,MED_NOGAUSS,
		  1,MED_NOPFL,MED_NO_PFLMOD,MED_ARETE,MED_SEG2,MED_NOPDT,"",0.0,MED_NONOR) < 0)  {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha2);ISCRUTE(MED_NOPDT);ISCRUTE(MED_NONOR);SSCRUTE(MED_NOPFL);
    SSCRUTE(maa1);SSCRUTE(MED_NOLIEN);
    ret = -1;
  };


  /* enregistre la composante n°2 des éléments de valr2, et n'utilise ni pas de temps ni n° d'ordre */
  /*        pour des raisons de complétude des tests on change le type d'élément (aucun sens phys.))*/
  if (MEDchampEcr(fid,maa1,nomcha2,(unsigned char*)valr2,USER_INTERLACE,nval2,MED_NOGAUSS,
                      2,MED_NOPFL,MED_NO_PFLMOD,MED_NOEUD,0,MED_NOPDT,"",0.0,MED_NONOR) < 0)  {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha2);ISCRUTE(MED_NOPDT);ISCRUTE(MED_NONOR);SSCRUTE(MED_NOPFL);
    SSCRUTE(maa1);SSCRUTE(MED_NOLIEN);
    ret = -1;
  };

  /* enregistre la composante n°3 des éléments de valr2, et n'utilise ni pas de temps ni n° d'ordre */
  /*        pour des raisons de complétude des tests on change le type d'élément (aucun sens phys.))*/
  if ( MEDchampEcr(fid,maa1,nomcha2,(unsigned char*)valr2,USER_INTERLACE,nval2,MED_NOGAUSS,
		   3,MED_NOPFL,MED_NO_PFLMOD,MED_FACE,MED_TRIA6,MED_NOPDT,"",0.0,MED_NONOR) < 0 ) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha2);ISCRUTE(MED_NOPDT);ISCRUTE(MED_NONOR);SSCRUTE(MED_NOPFL);
    SSCRUTE(maa1);SSCRUTE(MED_NOLIEN);
    ret = -1;
  };

  /* Creation d'un profil (selection  des éléments 1,3,5 de valr2) */
  /* On utilise les trois valeurs du profil */
  if ( MEDprofilEcr(fid,profil2,3,nomprofil2) < 0) {
    MESSAGE("Erreur à l'écriture du profil : ");
    SSCRUTE(nomprofil2);
    ret = -1;
  };

  /* enregistre la composante n°3 des éléments du profil2 de valr2, et n'utilise ni pas de temps ni n° d'ordre */
  if ( MEDchampEcr(fid,maa1,nomcha2,(unsigned char*)valr2p,USER_INTERLACE,nval2,MED_NOGAUSS,
                      3,nomprofil2,USER_MODE,MED_MAILLE,MED_TRIA6,MED_NOPDT,"",0.0,MED_NONOR) < 0 ) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha2);ISCRUTE(MED_NOPDT);ISCRUTE(MED_NONOR);SSCRUTE(MED_NOPFL);
    SSCRUTE(maa1);SSCRUTE(MED_NOLIEN);
    ret = -1;
  };


  /* fermeture du fichier */
  if ( MEDfermer(fid) < 0 ) ret=-1;
  
  return ret;
}




