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

/* L'utilisation des med_utils n'est pas optionelle
   dans l'outil medimport, elle revet cependant deux
   formes :
   - Si -DWITH_EXCEPTION EXITIF renvoie une exception MEDerreur
   - Sinon exécute l'instruction return */


#ifdef HAVE_STDBOOL_H
# include <stdbool.h>
#else
# ifndef HAVE__BOOL
#  ifdef __cplusplus
typedef bool _Bool;
#  else
#   define _Bool signed char
#  endif
# endif
# define bool _Bool
# define false 0
# define true 1
# define __bool_true_false_are_defined 1
#endif

/* #ifdef __cplusplus */
/* extern "C" { */
/* #endif */

/* Pour faciliter la detection autotools */
int HAVE_MEDimport=1;

/*Inclusion des .h de la version de la bibliothèque courante*/
#include <med_config.h>
#include <med.h>
#include <med_outils.h>

#include <stdio.h>
#include <string.h>

/*TODO : En attendant une conversion totale de l'outil libmedimport en 3.0*/
#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

#ifdef PPRO_NT
# include <stdlib.h>
#else
# include <libgen.h>
#endif

/* #ifdef __cplusplus */
/* } */
/* #endif */

/*Inclusion des .h spécifiques à medimport */

/* Interface publique de l'outil medimport */
/* MEDimport.h == MEDimport.hxx */
#include "MEDimport.h"
#include "MAJ_version.h"

/* Interfaces privées à l'outil medimport  */
#include "MAJ_21_22.h"
#include "MAJ_231_232.h"
#include "MAJ_236_300.h"
#include "MAJ_300_310.h"
#include "MAJ_310_320.h"
#include "MAJ_320_330.h"


#ifdef __cplusplus
extern "C" void _MEDmodeErreurVerrouiller(void);
#endif



int MEDimport(char * filein, char *  fileout) {

  med_idt fid, gid;
  med_err ret;
  med_int majeur, mineur, release;
  med_bool hdfok=MED_FALSE;
  med_bool medok=MED_FALSE;
  char *_fileout,*tmp=NULL;
  int   _fileoutsize;
  bool  hasfileout=false;
  char *commande;
  med_int nprofil;
  char chemin_profils[MED_PROFILE_GRP_SIZE+1];
  char chemin_liens[MED_TAILLE_LIENS+1];
  char version[9];
  int MAJ_21_22 = 0, MAJ_231_232 = 0, MAJ_236_300 = 0, MAJ_300_310 = 0, MAJ_310_320 = 0, MAJ_320_330 = 0 ;
#ifdef PPRO_NT
  char *drive, *dir, *ext;
#endif
  unsigned char reponse='o';
  med_bool      _noversion=MED_FALSE;

  EXIT_IF(filein == NULL,"Le nom du fichier d'entrée est vide : ", filein);

  hasfileout = strcmp(fileout,"");
  if ( hasfileout ) {
    _fileoutsize = strlen(fileout);
    _fileout     = fileout;
  } else {
    _fileoutsize = strlen(filein)+strlen(PACKAGE_VERSION);
    tmp          = (char *) malloc(sizeof(char)*(_fileoutsize+1));
    strcpy(tmp,filein);
    strcat(tmp,PACKAGE_VERSION);
#ifdef PPRO_NT
    _splitpath( tmp, drive, dir, _fileout, ext );
#else
    _fileout     = basename(tmp);
#endif
    _fileoutsize = strlen(_fileout);

  }

  /* Test du format du fichier */

  ret = MEDfileCompatibility(filein,&hdfok,&medok);

  if (ret < 0 ) {
    fprintf(stdout,">>> Attention le fichier %s ne contient pas de numéro de version. \n",filein);
    fprintf(stdout,">>> Le fichier  %s est supposé être en version 2.1.1. \n",filein);
    /* Pas d'interactif dans une bibliothèque !*/
/*     fprintf(stdout,">>> Voulez-vous essayer une conversion d'un fichier  < 2.2 (o/n) ? "); */
/*     scanf("%c",&reponse); */
    if ( (reponse != 'o') && (reponse != 'O') && (reponse != 'y') && (reponse != 'Y') ) {
      EXIT_IF(MEDfileCompatibility(filein,&hdfok,&medok) < 0,
	      "Erreur d'appel de  MEDfileCompatibility : ", filein);
    }
    _noversion = MED_TRUE;
  }
  EXIT_IF( !hdfok ,
	  "Le fichier d'entrée n'est pas dans un format HDF compatible : ", filein);

/*   EXIT_IF( !medok , */
/* 	  "MEDimport ne gère pas le format  MED de ce fichier : ", filein); */

  /* Creation et ouverture du fichier que l'on va convertir au format MED actuel */
  commande = (char *) malloc(sizeof(char)*(strlen("cp ")+strlen(filein)+
					   strlen(" ")+_fileoutsize + 4 +1  ) );
  EXIT_IF(commande == NULL,NULL,NULL);
  strcpy(commande,"cp \"");
  strcat(commande,filein);
  strcat(commande,"\" \"");
  strcat(commande,_fileout);
  strcat(commande,"\"");
  fprintf(stdout,">>> Creation du fichier %s : %s \n",_fileout,commande);
  system(commande);
  free(commande);
  commande = (char *) malloc(sizeof(char)*(strlen("chmod u+w \"") + _fileoutsize +1 +1  ) );
  EXIT_IF(commande == NULL,NULL,NULL);
  strcpy(commande,"chmod u+w \"");
  strcat(commande,_fileout);
  strcat(commande,"\"");
  fprintf(stdout,">>> Chmod +w du fichier %s : %s \n",_fileout,commande);
  system(commande);
  free(commande);

  fid = MEDfileOpen(_fileout,MED_ACC_RDWR);
  EXIT_IF(fid < 0,"Ouverture du fichier : ", _fileout);

  /* Verification du numero de version */
  if (! _noversion)
    ret = MEDfileNumVersionRd(fid,&majeur,&mineur,&release);
  else {
    ret=0;
    majeur=2;
    mineur=1;
    release=1;
  }
  sprintf(version, IFORMAT"_"IFORMAT"_"IFORMAT, majeur, mineur, release);
  EXIT_IF(ret < 0,"Lecture du numero de version de MED-fichier",NULL);
  if (strcmp(version, "2_2_0") < 0)
    MAJ_21_22 = 1;
  if (strcmp(version, "2_3_2") < 0)
    MAJ_231_232 = 1;
  if (strcmp(version, "3_0_0") < 0)
    MAJ_236_300 = 1;
  if (strcmp(version, "3_1_0") < 0)
    MAJ_300_310 = 1;
  if (strcmp(version, "3_2_0") < 0)
    MAJ_310_320 = 1;
  if (strcmp(version, "3_3_0") < 0)
    MAJ_320_330 = 1;

  /* Ne pas oublier que la version cible du fichier à convertir est celui de la bibliothèque. */
  if (MAJ_320_330 == 0) {
    fprintf(stdout,"Le fichier %s est déjà au bon format !!! \n",_fileout);
    ret = MEDfileClose(fid);
    EXIT_IF(ret < 0,"Fermeture du fichier",filein);
    return 0;
  }

  /* On avertit qu'on commence la conversion */
  fprintf(stdout,">>> Lancement de la normalisation du fichier selon le format " PACKAGE_VERSION " ...\n");

  /* On inhibe le gestionnaire d'erreur HDF5 */
  _MEDmodeErreurVerrouiller();

  /* Mise a jour du numero de version */
  fprintf(stdout,"- Lancement de la mise à jour du numéro de version ... \n");
  /*   La mise à jour MAJ_version(fid); doit être différée pour que les majs des fichiers anciens
       fonctionnent correctement*/
  /*   MAJ_version(fid); */
  MAJ_write_version_num(fid,2,3,6);
  fprintf(stdout,"  Numéro de version : ... OK ... \n");

  if (MAJ_21_22) {

    /* Mise a jour des maillages : type = MED_NON_STRUCTURE, description, ... */
    fprintf(stdout,"- Lancement de la mise à jour des maillages (21_22)... \n");
    MAJ_21_22_maillages(fid);
    fprintf(stdout,"  Maillage(s) : ... OK ...\n");

    /* Mise a jour des champs */
    fprintf(stdout,"- Lancement de la mise à jour des champs de résultats (21_22)... \n");
    MAJ_21_22_champs(fid);
    fprintf(stdout,"  Champs(s) : ... OK ...\n");

    /* Mise a jour des profils eventuels */
    nprofil = MEDnProfil(fid);
    if (nprofil > 0) {
      fprintf(stdout,"- Lancement de la mise à jour des profils (21_22)... \n");
      MAJ_21_22_profils(fid,nprofil);
      fprintf(stdout,"  Profils(s) : ... OK ...\n");
    } else {
      strncpy(chemin_profils,MED_PROFILE_GRP,MED_PROFILE_GRP_SIZE-1);
      chemin_profils[MED_PROFILE_GRP_SIZE-1] = '\0';
      gid = _MEDdatagroupCreer(fid,chemin_profils);
      EXIT_IF(gid < 0,"Creation du groupe HDF sur les profils",chemin_profils);
      ret = _MEDdatagroupFermer(gid);
      EXIT_IF(ret < 0,"Fermeture du groupe HDF sur les profils",chemin_profils);
    }

    /* On cree le groupe HDF pour les liens */
    strncpy(chemin_liens,MED_LIENS,MED_TAILLE_LIENS-1);
    chemin_liens[MED_TAILLE_LIENS-1] = '\0';
    gid = _MEDdatagroupCreer(fid,chemin_liens);
    EXIT_IF(gid < 0,"Creation du groupe HDF sur les liens",chemin_liens);
    ret = _MEDdatagroupFermer(gid);
    EXIT_IF(ret < 0,"Fermeture du groupe HDF sur les liens",chemin_liens);
  }

  if (MAJ_231_232) {
    /* Mise a jour des champs */
    fprintf(stdout,"- Lancement de la mise à jour des champs de résultats (231_232)... \n");
    MAJ_231_232_champs(fid);
    fprintf(stdout,"  Champs(s) : ... OK ...\n");
    fprintf(stdout,"- Lancement de la mise à jour des noms de maillages (231_232)... \n");
    MAJ_231_232_maillages(fid);
    fprintf(stdout,"  Noms(s) de maillage(s): ... OK ...\n");
  }

  if (MAJ_236_300) {
    /* Le système de cache de version a été developpé à partir de la 3.0*/
    /* Initialise le cache sur une 2.3.6 (cas d'absence d'INFO)*/
    _MEDfileVersion(fid);

    /* Mise a jour des champs */
    fprintf(stdout,"- Lancement de la mise à jour des champs de résultats (236_300)... \n");
    MAJ_236_300_champs(fid);
    fprintf(stdout,"  Champs(s) : ... OK ...\n");

    /* MAJ_version(fid); */

    fprintf(stdout,"- Lancement de la mise à jour des maillages (236_300)... \n");
    MAJ_236_300_maillages(fid);
    fprintf(stdout,"  Maillage(s): ... OK ...\n");

    /* MAJ_version(fid);  */

  }

  if (MAJ_300_310) {
    /* Le système de cache de version a été developpé à partir de la 3.0*/
    /* Initialise le cache sur une 3.0.8 (cas d'absence d'INFO)*/
    /* s'il n'a pas déjà été instanciée ds les MAJ précédentes */
    MAJ_write_version_num(fid,3,0,8);
    _MEDfileVersion(fid);
    /* Si le cache était dèjà instancié, met à jour le cache */
    MAJ_version_num(fid,3,0,8);

    /* Mise a jour des champs */
    fprintf(stdout,"- Lancement de la mise à jour des champs de résultats (300_310) ... \n");
    MAJ_300_310_champs(fid);
    fprintf(stdout,"  Champs(s) : ... OK ...\n");


  }

  if (MAJ_310_320) {
    /* Le système de cache de version a été developpé à partir de la 3.0*/
    /* Initialise le cache sur une 3.1.0 (cas d'absence d'INFO)*/
    /* s'il n'a pas été déjà été instanciée ds les MAJ_ précédentes */
    MAJ_write_version_num(fid,3,1,0);
    _MEDfileVersion(fid);
    /* Si le cache était dèjà instancié, met à jour le cache */
    MAJ_version_num(fid,3,1,0);

    /* Mise a jour des familles/groupes */
    fprintf(stdout,"- Lancement de la mise à jour des familles/groupes (310_320) ... \n");
    MAJ_310_320_familles(fid);
    fprintf(stdout,"  Famille(s)/Groupe(s) : ... OK ...\n");
  }

  if (MAJ_320_330) {
    /* Le système de cache de version a été developpé à partir de la 3.0*/
    /* Initialise le cache sur une 3.2.1 (cas d'absence d'INFO)*/
    /* s'il n'a pas été déjà été instanciée ds les MAJ_ précédentes */
    MAJ_write_version_num(fid,3,2,1);
    _MEDfileVersion(fid);
    /* Si le cache était dèjà instancié, met à jour le cache */
    MAJ_version_num(fid,3,2,1);

    /* Mise a jour des familles/groupes */
    fprintf(stdout,"- Lancement de la mise à jour des champs entiers (321_330) ... \n");
    MAJ_320_330_champs(fid);
    fprintf(stdout,"  Champs entiers : ... OK ...\n");
  }

  /* A l'écriture d'une nouvelle version de MAJ ex 310_320,
   il est necessaire de revisiter les appels à MAJ_version(fid) pour
   les remplacer par les appels MAJ_version(fid,3,1,Lastest31z) */

  MAJ_version(fid);  
  MAJ_write_version_num(fid,MED_NUM_MAJEUR,MED_NUM_MINEUR,MED_NUM_RELEASE);

  /* Fermeture du fichier */
  ret = MEDfileClose(fid);
  EXIT_IF(ret < 0,"Fermeture du fichier",_fileout);

  /* On avertit que c'est fini */
  fprintf(stdout,">>> Conversion du fichier %s au format MED V" PACKAGE_VERSION " terminée\n",
	  _fileout);

  /* On libere la memoire */
  if (!hasfileout) free(tmp);

  return 0;
}

