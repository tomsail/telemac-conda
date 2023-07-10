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
 * - Nom du fichier : test31.c
 *
 * - Description : ecriture d'une numerotation globale dans un maillage MED 
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
  /* le nombre de noeuds */
  med_int nnoe = 0;
  /* table des numeros global */
  med_int *numglobalnoe;

  /* variable de stockage pour reperer le maillage */
  med_int i;
  char desc[MED_TAILLE_DESC+1];
  med_maillage type;



  if (argc != 2) {
    MESSAGE("Il faut passer un fichier MED en paramètre");
    return -1;
  }

  /* Ouverture du fichier passe en argument */
  if ((fid = MEDouvrir(argv[1],MED_LECTURE_ECRITURE)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier : "); SSCRUTE(argv[1]);
    return -1;
  }
  

  /* Lecture des infos concernant le premier maillage */
  if (MEDmaaInfo(fid,1,maa,&mdim,&type,desc) < 0) {
    MESSAGE("Erreur a la lecture des informations du 1er maillage");
    return -1;
  }
  printf("Maillage de nom : %s et de dimension : "IFORMAT" \n",maa,mdim);

  /* Lecture du nombre de noeuds */
  if ((nnoe = MEDnEntMaa(fid,maa,MED_COOR,MED_NOEUD,0,0)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de noeuds ");
    return -1;
  }
  printf("Nombre de noeuds : "IFORMAT" \n",nnoe);

  /* Allocations memoires */

  /* table de la numerotation globale
     profil : (nombre de noeuds ) */
  if (nnoe > 0) {
    numglobalnoe = (med_int*) malloc(sizeof(med_int)*nnoe);
    for (i=0;i<nnoe;i++) {
      numglobalnoe[i]=i+100;
    }
  }

  /* ecriture de la numerotation globale */
  
  if (MEDglobalNumEcr(fid,maa,numglobalnoe,nnoe,MED_NOEUD,0)<0) {
    MESSAGE("Erreur a l''ecriture de la numerotation globale");
    return -1;
  }

  free(numglobalnoe);

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }

  return 0;
}




