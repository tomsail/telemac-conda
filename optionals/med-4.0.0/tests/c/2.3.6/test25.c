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
 * - Nom du fichier : test25.c
 *
 * - Description : ecriture de mailles de type MED_POLYEDRE
 *                 dans un maillage MED
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
  med_idt fid;
  char maa[MED_TAILLE_NOM+1] = "maa1";
  med_int mdim = 3;
  med_int n=2;
  /* connectivite nodale */
  med_int indexp[3] = {1,5,9};
  med_int np = 3;
  med_int indexf[9] = {1,4,7,10,13,16,19,22,25};
  med_int nf = 9;
  med_int conn[24] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
		      20,21,22,23,24};
  /* connectivite descendante */
  med_int indexp2[3] = {1,5,9};
  med_int np2 = 3;
  med_int conn2[8] = {1,2,3,4,5,6,7,8};
  med_int indexf2[8] = {MED_TRIA3,MED_TRIA3,MED_TRIA3,MED_TRIA3,
			MED_TRIA3,MED_TRIA3,MED_TRIA3,MED_TRIA3};
  med_int nf2=8;
  /*                             12345678901234561234567890123456*/
  char nom[MED_TAILLE_PNOM*2+1]="polyedre1       polyedre2       ";
  med_int num[2] = {1,2};
  med_int fam[2] = {0,-1};

  /* Creation du fichier test25.med */
  if ((fid = MEDouvrir("test25.med",MODE_ACCES)) < 0) {
    MESSAGE("Erreur a la creation du fichier test25.med");
    return -1;
  }
  printf("Creation du fichier test25.med \n");
 
  /* Creation du maillage */
  if (MEDmaaCr(fid,maa,mdim,MED_NON_STRUCTURE,
		   "un maillage pour test25") < 0) {
    MESSAGE("Erreur a la creation du maillage");
    return -1;
   }
  printf("Creation du maillage \n");

  /* Ecriture des connectivites des mailles polyedres en mode nodal */
  if (MEDpolyedreConnEcr(fid,maa,indexp,np,indexf,nf,conn,MED_NOD) < 0) {
    MESSAGE("Erreur a l'ecriture de la connectivite des mailles MED_POLYEDRE");
    return -1;
  }
  printf("Ecriture des connectivites de mailles de type MED_POLYEDRE en mode nodal \n"); 

  /* Ecriture des connectivites des mailles polyedres en mode descendant */
  if (MEDpolyedreConnEcr(fid,maa,indexp2,np2,indexf2,nf2,conn2,MED_DESC) < 0) {
    MESSAGE("Erreur a l'ecriture des connectivites des mailles MED_POLYEDRE en mode descendant");
    return -1;
  }
  printf("Ecriture des connectivites de mailles de type MED_POLYEDRE en mode descendant \n"); 

  /* Ecriture des noms des polyedres */
  if (MEDnomEcr(fid,maa,nom,n,MED_MAILLE,MED_POLYEDRE) < 0) {
    MESSAGE("Erreur a l'ecriture des noms des mailles MED_POLYEDRE");
    return -1;
  }
  printf("Ecriture des noms des polyedres \n");

  /* Ecriture des numeros des polyedres */
  if (MEDnumEcr(fid,maa,num,n,MED_MAILLE,MED_POLYEDRE) < 0) {
    MESSAGE("Erreur a l'ecriture des numeros des mailles MED_POLYEDRE");
    return -1;
  }
  printf("Ecriture des numeros des polyedres \n");

  /* Ecriture des numeros des familles des polyedres */
  if (MEDfamEcr(fid,maa,fam,n,MED_MAILLE,MED_POLYEDRE) < 0) {
    MESSAGE("Erreur a l'ecriture des familles des mailles MED_POLYEDRE");
    return -1;
  }
  printf("Ecriture des numeros des familles des polyedres \n");
  
  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier test25.med \n");

  return 0; 
}
