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
 * - Nom du fichier : test19.c
 *
 * - Description : conversion groupes => famille 
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
  char maa[MED_TAILLE_NOM+1] = "maillage_test19";
  char desc[MED_TAILLE_DESC+1]="un maillage pour test19.med";
  med_int mdim=2;
  /* Donnees de tests pour MEDgro2FamCr() 
     Les noeuds/mailles sont numerotes de 1 a 5 et les
     groupes de 1 a 3.
     Au depart, on a :
     - G1 : 1,2
     - G2 : 3,4,6
     - G3 : 1,4
     Au retour, on foit avoir 4 familles de noeuds + 4 familles de mailles 
     + la famille 0 dans le fichier :
     - F0 : 5       - groupes : aucun groupe par defaut (convention habituelle).
     - F1 : 1       - groupes : G1,G3  
     - F2 : 2       - groupes : G1
     - F3 : 3,6     - groupes : G2
     - F4 : 4       - groupes : G2,G3
  */
  med_int ngroup = 3;
  med_int nent = 6;
  char nom_groupes[MED_TAILLE_LNOM*3+1];
  /*                     0 1  2 3 4  5 6 */
  med_int entites[7] = { 1,2, 3,4,6, 1,4};
  med_int index[4] =   { 1,   3,     6,   8};
  int i;
  char nom_famille0[MED_TAILLE_NOM+1] = "FAMILLE0";
  /* on fait la meme distribution pour des mailles */
  med_int ngeo = 3;
  med_geometrie_element geo[3] = {MED_SEG2,MED_TRIA3,MED_TETRA4};
  /* MED_SEG2 : M1,M2,M3 - MED_TRI3 : M4,M5 - MED_TETRA4 : M6 */
  med_int index_geo[4] = {1,4,6,7};

  /* Creation du fichier test19.med */
  if ((fid = MEDouvrir("test19.med",MODE_ACCES)) < 0) {
    MESSAGE("Erreur a la creation du fichier test19.med");
    return -1;
  }
  printf("Creation du fichier test19.med \n");

  /* Creation du maillage */
  if (MEDmaaCr(fid,maa,mdim,MED_NON_STRUCTURE,desc) < 0) {
    MESSAGE("Erreur a la creation du maillage");
    return -1;
  }
  printf("Creation du maillage \n");

  /* on teste la fonction MEDgro2fam() */
  /* on definit les noms des groupes */
  strcpy(nom_groupes,"GROUPE 1");
  for (i=8;i<MED_TAILLE_LNOM;i++)
    nom_groupes[i] = ' ';
  nom_groupes[MED_TAILLE_LNOM] = '\0';
  strcat(nom_groupes,"GROUPE 2");
  for (i=8;i<MED_TAILLE_LNOM;i++)
    nom_groupes[MED_TAILLE_LNOM+i] = ' ';
  nom_groupes[2*MED_TAILLE_LNOM] = '\0';
  strcat(nom_groupes,"GROUPE 3");
  for (i=8;i<MED_TAILLE_LNOM;i++)
    nom_groupes[2*MED_TAILLE_LNOM+i] = ' ';
  nom_groupes[3*MED_TAILLE_LNOM] = '\0';

  /* On cree la famille 0 */
  if (MEDfamCr(fid,maa,nom_famille0,0,NULL,NULL,NULL,0,NULL,0) < 0) {
    MESSAGE("Erreur a la creation de la famille 0");
    return -1;
  }
  printf("Creation de la famille 0 \n");

  /* 
   * On definit et on archive les familles de noeuds dans test.19.med 
   */
  if (MEDgro2famCr(fid,maa,nom_groupes,index,ngroup,entites,nent,
		   MED_NOEUD,NULL,NULL,0) < 0) {
    MESSAGE("Erreur a la creation des familles de noeuds ");
    return -1;
  }
  printf("On constuit les familles de noeuds et on les stocke dans test19.med \n");

  /* 
   * On fait la meme chose pour des mailles de differents types 
   */
  if (MEDgro2famCr(fid,maa,nom_groupes,index,ngroup,entites,nent,
		   MED_MAILLE,geo,index_geo,ngeo) < 0) {
    MESSAGE("Erreur a la creation des familles d'elements ");
    return -1;
  }
  printf("On constuit les familles d'elements et on les stocke dans test19.med \n");  

  /* Fermeture du fichier */
  if (MEDfermer(fid) <0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier \n");
  
  return 0;
}
