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
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

int main (int argc, char **argv)


{
  med_idt fid;
  char maa[MED_NAME_SIZE+1] = "maa1";
  med_int mdim = 3;
  med_int n=2;
  /* connectivite nodale */
  med_int indexf[3] = {1,5,9};
  med_int nf = 3;
  med_int indexn[9] = {1,4,7,10,13,16,19,22,25};
  med_int nn = 9;
  med_int conn[24] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
		      20,21,22,23,24};
  /* connectivite descendante */
  med_int indexf2[3] = {1,5,9};
  med_int nf2 = 3;
  med_int conn2[8] = {1,2,3,4,5,6,7,8};
  med_int indexn2[8] = {MED_TRIA3,MED_TRIA3,MED_TRIA3,MED_TRIA3,
			MED_TRIA3,MED_TRIA3,MED_TRIA3,MED_TRIA3};
  med_int nn2=8;
  /*                             12345678901234561234567890123456*/
  char nom[MED_SNAME_SIZE*2+1]="polyedre1       polyedre2       ";
  med_int num[2] = {1,2};
  med_int fam[2] = {0,-1};
  char nomcoo[3*MED_SNAME_SIZE+1] = "x               y               z               ";
  char unicoo[3*MED_SNAME_SIZE+1] = "cm              cm              cm              ";

  /* Creation du fichier test25.med */
  if ((fid = MEDfileOpen("test25.med",MODE_ACCES)) < 0) {
    MESSAGE("Erreur a la creation du fichier test25.med");
    return -1;
  }
  printf("Creation du fichier test25.med \n");

  /* Creation du maillage */
 if (MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,
		 "un maillage pour test25","s", MED_SORT_DTIT,
		 MED_CARTESIAN, nomcoo, unicoo) < 0) {
    MESSAGE("Erreur a la creation du maillage");
    return -1;
   }
  printf("Creation du maillage \n");

  /* Ecriture de la connectivite des mailles polyedres en mode nodal */
  if (MEDmeshPolyhedronWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MED_CELL,MED_NODAL,
			  nf,indexf,nn,indexn,conn) < 0) {
    MESSAGE("Erreur a l'ecriture de la connectivite des mailles MED_POLYGONE");
    return -1;
  }
  printf("Ecriture des connectivites de mailles de type MED_POLYEDRE en mode nodal \n");

  /* Ecriture des connectivites des mailles polyedres en mode descendant */
  if (MEDmeshPolyhedronWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MED_CELL,MED_DESCENDING,
			  nf2,indexf2,nn2,indexn2,conn2) < 0) {
    MESSAGE("Erreur a l'ecriture des connectivites des mailles MED_POLYEDRE en mode descendant");
    return -1;
  }
  printf("Ecriture des connectivites de mailles de type MED_POLYEDRE en mode descendant \n"); 


  /* Ecriture des noms des polyedres */
  /* ecriture (optionnelle) des noms des polyedres */
  if (MEDmeshEntityNameWr(fid,maa,MED_NO_DT,MED_NO_IT,
			  MED_CELL,MED_POLYHEDRON,n,nom) < 0) {
    MESSAGE("Erreur a l'ecriture des noms des polyedres");
    return -1;
  }
  printf("Ecriture des noms des polyedres \n");

  /* ecriture (optionnelle) des numeros des polyedres */
  if (MEDmeshEntityNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,
			    MED_CELL,MED_POLYHEDRON,n,num) < 0) {
    MESSAGE("Erreur a l'ecriture des numeros des polyedres");
    return -1;
  }
  printf("Ecriture des numeros des polyedres \n");

  /* ecriture des numeros des familles des polyedres */
  if (MEDmeshEntityFamilyNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,
				  MED_CELL,MED_POLYHEDRON,n,fam) < 0) {
    MESSAGE("Erreur a l'ecriture des numeros de familles des polyedres");
    return -1;
  }
  printf("Ecriture des numeros des familles des polyedres \n");

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier test25.med \n");

  return 0; 
}
