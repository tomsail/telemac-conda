/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2016  EDF R&D, CEA/DEN
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

int main (int argc, char **argv)


{
  med_err ret=0;
  med_idt fid;



  /* Maillage support aux champs*/
  /* Ces maillages sont vides*/
  char maa1[MED_NAME_SIZE+1]= "maa1";
  char maa2[MED_NAME_SIZE+1]= "maa2";
  char * lien_maa2 = "./testfoo.med";
  char maa3[MED_NAME_SIZE+1]= "maa3";


  /* Caractéristiques du champ n° 1 sur TRIA6 */
  char nomcha1[MED_NAME_SIZE+1]  = "champ reel";
  char comp1[2*MED_SNAME_SIZE+1] = "comp1           comp2           ";
                                   /*12345678901234561234567890123456*/
  char unit1[2*MED_SNAME_SIZE+1] = "unit1           unit2           ";
  med_int ncomp1  = 2;
  /* Caractéristiques du model n° 1 de localisation des points de gauss pour le champ n°1*/
  med_int ngauss1_1 = 6;
  char gauss1_1[MED_NAME_SIZE+1]  = "Model n1";
  med_float refcoo1[12] = { -1.0,1.0, -1.0,-1.0, 1.0,-1.0, -1.0,0.0, 0.0,-1.0, 0.0,0.0 };

  /* Constantes */

  med_float gscoo1_1[12] = { 2*_b-1, 1-4*_b, 2*_b-1, 2*_b-1, 1-4*_b,
                             2*_b-1, 1-4*_a, 2*_a-1, 2*_a-1, 1-4*_a, 2*_a-1, 2*_a-1 };
  med_float wg1_1[6] = { 4*_p2, 4*_p2, 4*_p2, 4*_p1, 4*_p1, 4*_p1 };

  med_int   nval1_1= 1*6; /*1 valeurs et 6 points de gauss par valeur */
  med_int   _nent1_1= 1; /*1 valeurs et 6 points de gauss par valeur */
  med_float  valr1_1[1*6*2]  = {0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0, 20.0,21.0, 22.0,23.0};  /* 2 composantes*/
  /* Caractéristiques du model n° 2 de localisation des points de gauss pour le champ n°1*/
  med_int ngauss1_2 = 3;
  char gauss1_2[MED_NAME_SIZE+1]  = "Model n2";
  med_float gscoo1_2[6] = { -2.0/3,1.0/3, -2.0/3,-2.0/3, 1.0/3,-2.0/3  };
  med_float wg1_2[3] = { 2.0/3, 2.0/3, 2.0/3 };
  med_int   nval1_2= 2*3; /*2 valeurs et 3 points de gauss par valeur */
  med_int   _nent1_2= 2; /*2 valeurs et 3 points de gauss par valeur */
  med_float  valr1_2[2*3*2]  = {0.0,1.0, 2.0,3.0, 10.0,11.0,   12.0,13.0, 20.0,21.0, 22.0,23.0};  /* 2 composantes*/
  med_float  valr1_2p[2*3*2]  = {                              12.0,13.0, 20.0,21.0, 22.0,23.0};  /* 2 composantes*/
 /* Caractéristiques du model n° 3 sans points de gauss pour le champ n°1*/
  med_int   nval1_3= 6; /*6 valeurs et pas de points de gauss */
  med_int   _nent1_3= 6; /*6 valeurs et pas de points de gauss */
  med_float valr1_3[2*3*2]  = {0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0, 20.0,21.0, 22.0,23.0};  /* 2 composantes*/
  med_float valr1_3p[2*2*2] = {         2.0,3.0, 10.0,11.0                                 };  /* 2 composantes profil1 */

  /* Caractéristiques du champ n° 2 */
  char nomcha2[MED_NAME_SIZE+1]  = "champ entier";
  char comp2[3*MED_SNAME_SIZE+1] = "comp1           comp2           comp3           ";
                                   /*123456789012345612345678901234561234567890123456*/
  char unit2[3*MED_SNAME_SIZE+1] = "unit1           unit2           unit3           ";
  med_int ncomp2  = 3; 
  med_int nval2 = 5;   /*5 valeurs */
  med_int   valr2[5*3  ]  = {0,1,2, 10,11,12, 20,21,22, 30,31,32, 40,41,42};              /* 3 composantes*/
  med_int   valr2p[3*3  ] = {0,1,2,           20,21,22,           40,41,42};              /* 3 composantes*/

  /* Profils utilisés */
  char nomprofil1[MED_NAME_SIZE+1]  = "PROFIL(champ(1))";
  char nomprofil1b[MED_NAME_SIZE+1] = "PROFIL(champ(1b))";
  char nomprofil2[MED_NAME_SIZE+1]  = "PROFIL(champ2)";
  med_int profil1[2] = { 2, 3 };
  med_int profil2[3] = { 1, 3, 5 };


  /* Caractéristiques du champ n° 3 */
  char nomcha3[MED_NAME_SIZE+1]  = "champ entier 3";
  char comp3[3*MED_SNAME_SIZE+1] = "comp1           comp2           ";
                                   /*123456789012345612345678901234561234567890123456*/
  char unit3[3*MED_SNAME_SIZE+1] = "unit1           unit2           ";
  char dtunit[MED_SNAME_SIZE+1] = "s";
  med_int ncomp3  = 2;
  med_int nval3 = 5*4;     /*5 valeurs et 4 noeuds par element*/
  med_int _nent3 = 5;     /*5 valeurs et 4 noeuds par element*/
  med_int valr3[5*4*2]   = {0,1, 10,11, 20,21, 30,31,
                            40,41, 50,51, 60,61, 70,71,
                            80,81, 90,91, 100,101, 110,111,
                            120,121, 130,131, 140,141, 150,151,
                            160,161, 170,171, 180,181, 190,191};    /* 2 composantes*/
  med_int valr3p[3*4*2]  = {0,1, 10,11, 20,21, 30,31,
                            80,81, 90,91, 100,101, 110,111,
                            160,161, 170,171, 180,181, 190,191};    /* 2 composantes*/


  char nomcoo[3*MED_SNAME_SIZE+1] = "x               y               z               ";
  char unicoo[3*MED_SNAME_SIZE+1] = "cm              cm              cm              ";




  /* ouverture du fichier */
  if ((fid = MEDfileVersionOpen("test10.med",MODE_ACCES,3,2,1)) < 0){
    MESSAGE("Erreur à l'ouverture du fichier : ");
    return -1;
  }

  /* creation de maa1 de dimension 3*/
  if (MEDmeshCr( fid, maa1, 3, 3, MED_UNSTRUCTURED_MESH,
		 "Maillage vide","s", MED_SORT_DTIT,
		 MED_CARTESIAN, nomcoo, unicoo) < 0) {
    MESSAGE("Erreur a la creation du maillage : "); SSCRUTE(maa1);
    ret = -1;
  }


  /* creation de maa3 de dimension 3*/
  if (MEDmeshCr( fid, maa3, 3, 3, MED_UNSTRUCTURED_MESH,
		 "Maillage vide","s", MED_SORT_DTIT,
		 MED_CARTESIAN, nomcoo, unicoo) < 0) {
    MESSAGE("Erreur a la creation du maillage : "); SSCRUTE(maa3);
    ret = -1;
  }


  /* creation du champ réel n°1 */
  if ( MEDfieldCr(fid,nomcha1,MED_FLOAT64,ncomp1,comp1,unit1,dtunit,maa1 ) < 0) {
    MESSAGE("Erreur à la création du champ : ");SSCRUTE(nomcha1);
    ret = -1;
  };

  /* creation du champ entier n°2 */
  if ( MEDfieldCr(fid,nomcha2,MED_INT32,ncomp2,comp2,unit2,dtunit,maa2 ) < 0) {
    MESSAGE("Erreur à la création du champ : ");SSCRUTE(nomcha2);
    ret = -1;
  };

  /* creation du lien au fichier distant contenant maa2 */
  if (MEDlinkWr(fid,maa2,lien_maa2) < 0) {
    MESSAGE("Erreur à la création du lien : ");SSCRUTE(lien_maa2);
    ret = -1;
  };

  /* creation de la localisation des points de Gauss modèle n°1 */
  if (MEDlocalizationWr(fid, gauss1_1, MED_TRIA6, MED_TRIA6/100, refcoo1, USER_INTERLACE,
			ngauss1_1, gscoo1_1, wg1_1,
			MED_NO_INTERPOLATION, MED_NO_MESH_SUPPORT ) < 0) {
    MESSAGE("Erreur à la création du modèle n°1 : ");SSCRUTE(gauss1_1);
    ret = -1;
  };

   /* creation de la localisation des points de Gauss modèle n°2 */
  if (MEDlocalizationWr(fid, gauss1_2, MED_TRIA6, MED_TRIA6/100, refcoo1, USER_INTERLACE,
			ngauss1_2, gscoo1_2, wg1_2,
			MED_NO_INTERPOLATION, MED_NO_MESH_SUPPORT) < 0) {
    MESSAGE("Erreur à la création du modèle n°1 : ");SSCRUTE(gauss1_2);
    ret = -1;
  };
 
  /* ecriture du champ n°1*/
  /* enregistre uniquement les composantes n°2 de valr1_1, et n'utilise ni pas de temps ni n° d'ordre*/

  if ( MEDfieldValueWithProfileWr(fid, nomcha1,MED_NO_DT,MED_NO_IT,0.0,MED_CELL,MED_TRIA6,USER_MODE,MED_ALLENTITIES_PROFILE,
		       gauss1_1,USER_INTERLACE, 2, _nent1_1, (unsigned char*)valr1_1 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };

  

  /* enregistre uniquement les composantes n°1 de valr1_1, et n'utilise ni pas de temps ni n° d'ordre */

  if ( MEDfieldValueWithProfileWr(fid, nomcha1,MED_NO_DT,MED_NO_IT,0.0,MED_CELL,MED_TRIA6,USER_MODE,MED_ALLENTITIES_PROFILE,
		       gauss1_1,USER_INTERLACE, 1, _nent1_1, (unsigned char*)valr1_1 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };

  /* enregistre uniquement les composantes n°1 de valr1_2, au pas de temps n°1(5.5), n'utilise pas de n°d'ordre*/
  /* ce champ repose sur le maillage maa2 qui est distant */

  if ( MEDfieldValueWithProfileWr(fid, nomcha1,1,MED_NO_IT,5.5,MED_CELL,MED_TRIA6,USER_MODE,MED_ALLENTITIES_PROFILE,
		       gauss1_2,USER_INTERLACE, 1, _nent1_2, (unsigned char*)valr1_2 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };

/*Ce test utilise un deuxième maillage pour un même champ, ceci n'existe plus en 3.0*/
  /* enregistre uniquement les composantes n°2 de valr1_2, au pas de temps n°1(5.5), n'utilise pas de n°d'ordre*/
  /* ce champ repose sur le maillage maa1 qui est local */

  /*Ce test utilise un deuxième maillage pour un même champ, ceci n'existe plus en 3.0*/
  if ( MEDfieldValueWithProfileWr(fid, nomcha1,1,1,5.5,MED_CELL,MED_TRIA6,USER_MODE,MED_ALLENTITIES_PROFILE,
		       gauss1_1,USER_INTERLACE, 2, _nent1_1, (unsigned char*)valr1_1 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };


    /* enregistre uniquement les composantes n°1 de valr1_1, au pas de temps n°1(5.5), et n°d'itération n°2*/
    /* ce champ repose sur le maillage maa3 qui est local */

  if ( MEDfieldValueWithProfileWr(fid, nomcha1,1,2,5.5,MED_CELL,MED_TRIA6,USER_MODE,MED_ALLENTITIES_PROFILE,
		       gauss1_2,USER_INTERLACE, 1, _nent1_2, (unsigned char*)valr1_2 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };

  /* Creation d'un profil (selection  du deuxieme élément de valr1_1) */
  /* On n'utilise que la première valeur (2) du profil */
  if ( MEDprofileWr(fid,nomprofil1,1,profil1) < 0) {
    MESSAGE("Erreur à l'écriture du profil : ");
    SSCRUTE(nomprofil1);
    ret = -1;
  };


  if ( MEDprofileWr(fid,nomprofil1b,1,profil1) < 0) {
    MESSAGE("Erreur à l'écriture du profil : ");
    SSCRUTE(nomprofil1b);
    ret = -1;
  };




  /* enregistre toutes les composantes du deuxième élément de valr1_1 (premier élément en stockage compact de valr1p),
     au pas de temps n°2(5.6), et n°d'itération n°2*/
  if ( MEDfieldValueWithProfileWr(fid, nomcha1,2,2,5.6,MED_CELL,MED_TRIA6,USER_MODE,nomprofil1,
		       MED_NO_LOCALIZATION,USER_INTERLACE, MED_ALL_CONSTITUENT, nval1_3, (unsigned char*)valr1_3p ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };


  /* enregistre toutes les composantes du deuxième élément de valr1_1 (premier élément en stockage compact de valr1p),
     au pas de temps n°2(5.6), et n°d'itération n°2 */

  if ( MEDfieldValueWithProfileWr(fid, nomcha1,2,2,5.6,MED_CELL,MED_TRIA6,USER_MODE,nomprofil1b,
		       gauss1_2,USER_INTERLACE, MED_ALL_CONSTITUENT, _nent1_2, (unsigned char*)valr1_2p ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };


  if ( MEDfieldValueWithProfileWr(fid, nomcha1,3,2,5.7,MED_CELL,MED_TRIA6,USER_MODE,nomprofil1,
		       MED_NO_LOCALIZATION,USER_INTERLACE, 2, _nent1_3, (unsigned char*)valr1_3p ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };


  /* Ecriture du champ n° 2 */
    

  if ( MEDfieldValueWr(fid, nomcha2,MED_NO_DT,MED_NO_IT,0,
				  MED_DESCENDING_EDGE,MED_SEG2,
				  USER_INTERLACE, 1, nval2, (unsigned char*)valr2 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };


  if ( MEDfieldValueWr(fid, nomcha2,MED_NO_DT,MED_NO_IT,0,MED_NODE,MED_NONE,
				  USER_INTERLACE, 2, nval2, (unsigned char*)valr2 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };


  if ( MEDfieldValueWr(fid, nomcha2,MED_NO_DT,MED_NO_IT,0,MED_DESCENDING_FACE,MED_TRIA6,
				  USER_INTERLACE, 3, nval2, (unsigned char*)valr2 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };

  /* Creation d'un profil (selection  des éléments 1,3,5 de valr2) */
  /* On utilise les trois valeurs du profil */
  if ( MEDprofileWr(fid,nomprofil2,3,profil2) < 0) {
    MESSAGE("Erreur à l'écriture du profil : ");
    SSCRUTE(nomprofil2);
    ret = -1;
  };


  if ( MEDfieldValueWithProfileWr(fid, nomcha2,MED_NO_DT,MED_NO_IT,0,MED_CELL,MED_TRIA6,USER_MODE,nomprofil2,
		       MED_NO_LOCALIZATION,USER_INTERLACE, 3, nval2, (unsigned char*)valr2p ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };

  /* creation du champ entier n°3 */
  if ( MEDfieldCr(fid,nomcha3,MED_INT32,ncomp3,comp3,unit3,dtunit,maa1) < 0) {
    MESSAGE("Erreur à la création du champ : ");SSCRUTE(nomcha3);
    ret = -1;
  };
  
  /* Ecriture du champ n° 3 */

  if ( MEDfieldValueWr(fid, nomcha3,MED_NO_DT,MED_NO_IT,0,MED_CELL,MED_QUAD4,
				  USER_INTERLACE, 1, nval3, (unsigned char*)valr3 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };

  if ( MEDfieldValueWr(fid, nomcha3,MED_NO_DT,MED_NO_IT,0,MED_NODE_ELEMENT,MED_QUAD4,
				  USER_INTERLACE, MED_ALL_CONSTITUENT, _nent3, (unsigned char*)valr3 ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };

  if ( MEDfieldValueWithProfileWr(fid, nomcha3,MED_NO_DT,MED_NO_IT,0,MED_NODE_ELEMENT,MED_QUAD4,USER_MODE,nomprofil2,
		       MED_NO_LOCALIZATION,USER_INTERLACE, MED_ALL_CONSTITUENT, _nent3, (unsigned char*)valr3p ) < 0) {
    MESSAGE("Erreur à l'écriture du champ : ");
    SSCRUTE(nomcha1);ISCRUTE_int(MED_NO_DT);ISCRUTE_int(MED_NO_IT);SSCRUTE(MED_NO_PROFILE);
    SSCRUTE(maa1);
    ret = -1;
  };


  /* fermeture du fichier */
  if ( MEDfileClose(fid) < 0 ) ret=-1;

  return ret;
}




