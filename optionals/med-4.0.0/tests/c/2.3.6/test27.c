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
 * - Nom du fichier : test27.c
 *
 * - Description : creation de maillages structures (grille cartesienne |
 *                 grille standard ) dans le fichier test27.med
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

int main (int argc, char **argv)


{
  med_idt fid;
  /* la dimension du maillage */
  med_int mdim,axe;
  med_float indice[4];
  med_float coo[8] = {0.0,0.0,1.0,0.0,0.0,1.0,1.0,1.0};
  med_int nnoeuds = 4;
  med_int structure_grille[2] = {2,2};
  med_int nind;
  /* nom du maillage de longueur maxi MED_TAILLE_NOM */
  char maa[MED_TAILLE_NOM+1];
  /* composantes et unites */
  char comp[MED_TAILLE_PNOM+1];
  char unit[MED_TAILLE_PNOM+1];
  /*                               12345678901234561234567890123456 */
  char comp2[2*MED_TAILLE_PNOM+1] = "X               Y               ";
  char unit2[2*MED_TAILLE_PNOM+1] = "cm              cm              ";

  /* Creation du fichier test27.med */
  fid = MEDouvrir("test27.med",MODE_ACCES);
  if (fid < 0) {
    MESSAGE("Erreur a la creation du fichier test27.med");
    return -1;
  }
  printf("Creation du fichier test27.med \n");

  /* Creation d'un maillage MED_NON_STRUCTURE */
  if (MEDmaaCr(fid,"maillage vide",3,MED_NON_STRUCTURE,"un maillage vide") < 0) {
    MESSAGE("Erreur a la creation du maillage MED_NON_STRUCTURE");
    return -1;
  }

  /* creation d'une grille cartesienne de dimension 2 */
  strcpy(maa,"grille_cartesienne");
  mdim = 2;
  /* on commence par definir un maillage MED_STRUCTURE 
     de dimension 2 */
  if (MEDmaaCr(fid,maa,mdim,MED_STRUCTURE,
		   "un exemple de grille cartesienne") < 0) {
    MESSAGE("Erreur a la creation de la grille");
    return -1;
  }
  printf("Creation d'un maillage structure MED_STRUCTURE \n");

  /* On specifie la nature du maillage structure : MED_GRILLE_CARTESIENNE */
  if (MEDnatureGrilleEcr(fid,maa,MED_GRILLE_CARTESIENNE) < 0) {
    MESSAGE("Erreur a l'ecriture de la nature de la grille");
    return -1;
  }
  printf("On definit la nature du maillage structure : MED_GRILLE_CARTESIENNE \n");

  /* on definit les indices des coordonnees de la grille selon chaque dimension  */
  /* axe des "X" */
  indice[0] = 1.1;
  indice[1] = 1.2;
  indice[2] = 1.3;
  indice[3] = 1.4;
  nind = 4;
  axe = 1;
  /* Les deux chaines suivantes ont une longueur incorrecte*/
  strcpy(comp,"X");
  strcpy(unit,"cm");
  if (MEDindicesCoordEcr(fid,maa,mdim,indice,nind,axe,comp,unit) < 0) {
    MESSAGE("Erreur a l'ecriture de l'axe X");
    return -1;
  }
  printf("Ecriture des indices des coordonnees selon l'axe des X \n");

  /* axe des "Y" */
  indice[0] = 2.1;
  indice[1] = 2.2;
  indice[2] = 2.3;
  indice[3] = 2.4;
  nind = 4;
  /* Les deux chaines suivantes ont une longueur incorrecte*/
  strcpy(comp,"Y");
  strcpy(unit,"cm");
  axe = 2;
  if (MEDindicesCoordEcr(fid,maa,mdim,indice,nind,axe,comp,unit) < 0) {
    MESSAGE("Erreur a l'ecriture de l'axe Y");
    return -1;
  }
  printf("Ecriture des indices des coordonnees selon l'axe des Y \n");

  /* Creation d'une grille MED_STANDARD de dimension 2 */
  strcpy(maa,"grille_standard");
  mdim = 2;
  /* on commence par definir un maillage MED_STRUCTURE 
     de dimension 2 */
  if (MEDmaaCr(fid,maa,mdim,MED_STRUCTURE,
		   "un exemple de grille standard") < 0) {
    MESSAGE("Erreur a la creation de la 2e grille");
    return -1;
  }
  printf("Creation d'un maillage structure MED_STRUCTURE \n");

  /* On specifie la nature du maillage structure : MED_GRILLE_STANDARD */
  if (MEDnatureGrilleEcr(fid,maa,MED_GRILLE_STANDARD) < 0) {
    MESSAGE("Erreur a l'ecriture du type de la grille");
    return -1;
  }
  printf("On definit la nature du maillage structure : MED_GRILLE_STANDARD \n");

  /* On ecrit les coordonnees de la grille */
  if (MEDcoordEcr(fid,maa,mdim,coo,MED_FULL_INTERLACE,nnoeuds,MED_CART,comp2,unit2) < 0) {
    MESSAGE("Erreur a l'ecriture des noeuds de la grille MED_GRILLE_STANDARD");
    return -1;
  }
  printf("Ecriture des coordonnees des noeuds \n");

  /* On definit la structure de la grille */
  if (MEDstructureCoordEcr(fid,maa,mdim,structure_grille) < 0) {
    MESSAGE("Erreur a l'ecriture de la structure de la grille");
    return -1;
  }
  printf("Ecriture de la structure de la grille : / 2,2 / \n");
  
  /* On ferme le fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier \n");
  
  return 0;
}
