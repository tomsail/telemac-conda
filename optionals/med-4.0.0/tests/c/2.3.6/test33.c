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
 * - Nom du fichier : test33.c
 *
 * - Description : lecture d'une numerotation globale inexistante dans un maillage MED 
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

int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid;
  /* la dimension du maillage */
  med_int mdim;
  /* nom du maillage de longueur maxi MED_TAILLE_NOM */
  char maa[MED_TAILLE_NOM+1];
  /* le nombre de maiuds */
  med_int narr = 0;
  /* table des numeros global */
  med_int *numglobalmai;

  /* variable de stockage pour reperer le maillage */
  med_int i;
  char des[MED_TAILLE_DESC+1];
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


  /* Lecture du nombre de arretes */
  if ((narr = MEDnEntMaa(fid,maa,MED_COOR,MED_ARETE,MED_SEG2,0)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de arrete ");
    return -1;
  }
  printf("Nombre d'arretes : "IFORMAT" \n",narr);

  /* Allocations memoires */

  /* table de la numerotation globale
     profil : (nombre de arretes +1) pour avoir une table
     meme s'il n'y a pas d'entite concernées*/
  numglobalmai = (med_int*) malloc(sizeof(med_int)*(narr+1));



  /* lecture de la numerotation globale attachee aux arrete Tria3*/
  /* elle n'existe pas le code doit gerer les erreurs */
  if ((ret=MEDglobalNumLire(fid,maa,numglobalmai,narr,MED_ARETE,MED_TRIA3))<0) {
    MESSAGE("Erreur a la lecture de de la numerotation globale pour les arretes");
    MESSAGE("ce qui etait attendu puisqu'il n'y a pas de numerotation globale sur les arretes!");
  }


  free(numglobalmai);

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier ");
    return -1;
  }

  if (ret<0) {
    /* le test a reporte une erreur, ce qui est attendu 
       --> PASS */
    return 0;
  }
  else {
    /* le test n'a pas reporte une erreur, ce qui etait attendu 
       --> FAIL */
    return -1;
  }
}

