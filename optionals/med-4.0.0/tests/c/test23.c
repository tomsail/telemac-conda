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
 * - Nom du fichier : test23.c
 *
 * - Description : ecriture de mailles/faces de type MED_POLYGONE
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
  med_int index[4] = {1,6,12,17};
  med_int con[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  med_int n=3,ni = 4;
  /*                            123456789012345612345678901234561234567890123456 */
  char nom[MED_SNAME_SIZE*3+1]="poly1           poly2           poly3           ";
  med_int num[3] = {1,2,3};
  med_int fam[3] = {0,-1,-2};
  char nomcoo[3*MED_SNAME_SIZE+1] = "x               y               z               ";
  char unicoo[3*MED_SNAME_SIZE+1] = "cm              cm              cm              ";

  /* Creation du fichier test23.med */
  fid = MEDfileOpen("test23.med",MODE_ACCES);
  if (fid == -1) {
    MESSAGE("Erreur a la creation du fichier test23.med");
    return -1;
  }
  printf("Creation du fichier test23.med \n");

  /* Creation du maillage */
  if (MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,
		 "un maillage pour test23","s", MED_SORT_DTIT,
		 MED_CARTESIAN, nomcoo, unicoo) < 0) {
    MESSAGE("Erreur a la creation du maillage");
    return -1;
  }
  printf("Creation du maillage \n");


  /* Ecriture de la connectivite des mailles polygones en mode nodal */
  if (MEDmeshPolygonWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MED_CELL,MED_NODAL,
		       ni,index,con) < 0) {
    MESSAGE("Erreur a l'ecriture de la connectivite des mailles MED_POLYGONE");
    return -1;
  }
  printf("Ecriture des connectivites de mailles de type MED_POLYGONE en mode nodal \n");

  /* Ecriture des noms des polygones */
  /* ecriture (optionnelle) des noms des polygones */
  if (MEDmeshEntityNameWr(fid,maa,MED_NO_DT,MED_NO_IT,
			  MED_CELL,MED_POLYGON,n,nom) < 0) {
    MESSAGE("Erreur a l'ecriture des noms des polygones");
    return -1;
  }
  printf("Ecriture des noms des polygones \n");

  /* ecriture (optionnelle) des numeros des polygones */
  if (MEDmeshEntityNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,
			    MED_CELL,MED_POLYGON,n,num) < 0) {
    MESSAGE("Erreur a l'ecriture des numeros des polygones");
    return -1;
  }
  printf("Ecriture des numeros des polygones \n");

  /* ecriture des numeros des familles des polygones */
  if (MEDmeshEntityFamilyNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,
				  MED_CELL,MED_POLYGON,n,fam) < 0) {
    MESSAGE("Erreur a l'ecriture des numeros de familles des polygones");
    return -1;
  }
  printf("Ecriture des numeros des familles des polygones \n");

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier test23.med \n");

  return 0;
}
