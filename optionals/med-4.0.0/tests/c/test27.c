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
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

int main (int argc, char **argv)

/*TODO : Tester l'écriture des attributs famille,numéros optionnels, noms optionnels*/
{
  med_idt   fid=0;
  med_int   mdim=2,axe=0,nind=0;
  med_float indiceX[4] = {1.0,1.1,1.2,1.3};
  med_float indiceY[4] = {2.0,2.1,2.2,2.3};
  med_float coo[8]     = {0.0,0.0,1.0,0.0,0.0,1.0,1.0,1.0};
  med_int   nnoeuds    = 4;
  med_int   structure_grille[2] = {2,2};
  char      maa [MED_NAME_SIZE+1]= "grille_cartesian";
  char      maa2[MED_NAME_SIZE+1]= "grille_curvilinear";
  /* composantes et unites */
  /*                               12345678901234561234567890123456 */
  char comp[2*MED_SNAME_SIZE+1] = "X               Y               ";
  char unit[2*MED_SNAME_SIZE+1] = "cm              cm              ";

  /* Creation du fichier test27.med */
  fid = MEDfileOpen("test27.med",MODE_ACCES);
  if (fid < 0) {
    MESSAGE("Erreur a la creation du fichier test27.med");
    return -1;
  }
  printf("Creation du fichier test27.med \n");

  /* Creation du maillage "maa" de type MED_NON_STRUCURE  et de dimension 2 */
  if (MEDmeshCr( fid, "maillage vide",2, 2, MED_UNSTRUCTURED_MESH,
		 "un maillage vide","s", MED_SORT_DTIT,
		 MED_CARTESIAN, comp, unit) < 0) {
    MESSAGE("Erreur a la creation du maillage MED_UNSTRUCTURED_MESH : "); SSCRUTE(maa);
    return -1;
  }
  /* creation d'une grille cartesienne de dimension 2 */
  /* on commence par definir un maillage MED_STRUCTURED_MESH
     de dimension 2 */
  if (MEDmeshCr( fid, maa,mdim, mdim, MED_STRUCTURED_MESH,
		 "un exemple de grille cartesienne","s", MED_SORT_DTIT,
		 MED_CARTESIAN, comp, unit) < 0) {
    MESSAGE("Erreur a la creation de la grille");
    return -1;
  }
  printf("Creation d'un maillage structure MED_STRUCTURED_MESH \n");

  /* On specifie la nature du maillage structure : MED_GRILLE_CARTESIENNE */
  if (MEDmeshGridTypeWr(fid,maa, MED_CARTESIAN_GRID) < 0) {
    MESSAGE("Erreur a l'ecriture de la nature de la grille");
    return -1;
  }

  printf("On definit la nature du maillage structure : MED_GRILLE_CARTESIENNE \n");

  /* on definit les indices des coordonnees de la grille selon chaque dimension  */
  /* axe des "X" */
  nind = 4;
  axe = 1;
  if (MEDmeshGridIndexCoordinateWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
				   axe,nind,indiceX) < 0) {
    MESSAGE("Erreur a l'ecriture de l'axe X");
    return -1;
  }
  printf("Ecriture des indices des coordonnees selon l'axe des X \n");

  /* axe des "Y" */
  nind = 4;
  axe = 2;
  if (MEDmeshGridIndexCoordinateWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
				   axe,nind,indiceY) < 0) {
    MESSAGE("Erreur a l'ecriture de l'axe Y");
    return -1;
  }
  printf("Ecriture des indices des coordonnees selon l'axe des Y \n");

  /* Creation d'une grille MED_CURVILINEAR_GRID de dimension 2 */
  /* on commence par definir un maillage MED_STRUCTURED_MESH
     de dimension 2 */
  if (MEDmeshCr( fid, maa2,mdim, mdim, MED_STRUCTURED_MESH,
		 "un exemple de grille standard","s", MED_SORT_DTIT,
		 MED_CARTESIAN, comp, unit) < 0) {
    MESSAGE("Erreur a la creation de la 2e grille");
    return -1;
  }
  printf("Creation d'un maillage structure MED_STRUCTURED_MESH \n");

 /* On specifie la nature du maillage structure : MED_CURVILINEAR_GRID */
  if (MEDmeshGridTypeWr(fid,maa2, MED_CURVILINEAR_GRID) < 0) {
    MESSAGE("Erreur a l'ecriture de la nature de la grille");
    return -1;
  }
  printf("On definit la nature du maillage structure : MED_CURVILINEAR_GRID \n");


  if (MEDmeshNodeCoordinateWr(fid,maa2,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
			      MED_FULL_INTERLACE,nnoeuds, coo) < 0) {
    MESSAGE("Erreur a l'ecriture des noeuds de la grille MED_CURVILINEAR_GRID");
    return -1;
  }
  printf("Ecriture des coordonnees des noeuds \n");

  /* On definit la structure de la grille */
  if ( MEDmeshGridStructWr(fid,maa2,MED_NO_DT,MED_NO_IT, MED_UNDEF_DT, structure_grille ) < 0) {
    MESSAGE("Erreur a l'ecriture de la structure de la grille");
    return -1;
  }
  printf("Ecriture de la structure de la grille : / 2,2 / \n");

  /* On ferme le fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier \n");

  return 0;
}
