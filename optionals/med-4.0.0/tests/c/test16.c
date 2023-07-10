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
 * - Nom du fichier : test16.c
 *
 * - Description : ecriture d'elements de maillages MED
 *                 via les routines de niveau 2
 *                 - equivalent a test6.c
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
  med_idt fid  = 0;
  med_int nse2 = 5;
  med_int se2[10] = {1,2,1,3,2,4,3,4,2,3};
  /*                               12345678901234561234567890123456123456789012345612345678901234561234567890123456 */
  char nomse2[MED_SNAME_SIZE*5+1]="se1             se2             se3             se4             se5             ";
  med_int numse2[5] = {1,2,3,4,5};
  med_int nufase2[5] = {-1,-1,0,-2,-3};
  med_int ntr3 = 2;
  med_int tr3[6] = {1,2,-5,-5,3,-4};
  /*                                 1234567890123456123456789012345612345678901234561234567890123456 */
  char nomtr3[MED_SNAME_SIZE*2+1] = "tr1             tr2             ";
  med_int numtr3[2] = {4,5};
  med_int nufatr3[2] = {0,-1};
  char maa[MED_NAME_SIZE+1] = "maa1";
  med_int mdim = 2;
  /*                                 12345678901234561234567890123456 */
  char nomcoo[2*MED_SNAME_SIZE+1] = "x               y               ";
  char unicoo[2*MED_SNAME_SIZE+1] = "cm              cm              ";

  /* Creation du fichier test16.med */
  if ((fid = MEDfileOpen("test16.med",MODE_ACCES)) < 0) {
    MESSAGE("Erreur a la creation du fichier test16.med");
    return -1;
  }


  /* Creation du maillage */
  if (MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,
		 "un maillage pour test16","s", MED_SORT_DTIT,
		 MED_CARTESIAN, nomcoo, unicoo) < 0) {
    MESSAGE("Erreur a la creation du maillage : "); SSCRUTE(maa);
    return -1;
  }


  /* Ecriture des aretes segments MED_SEG2 :
     - connectivite
     - noms (optionnel)
     - numeros (optionnel)
     - numeros des familles */
  if ( MEDmeshElementWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
			MED_DESCENDING_EDGE,MED_SEG2,MED_DESCENDING,MED_NO_INTERLACE,
			nse2,se2,MED_TRUE,nomse2,MED_TRUE,numse2,MED_TRUE,nufase2)< 0 ) {
    MESSAGE("Impossible d'ecrire la connectivit� des aretes : ");
    return -1;
  }

  /* Ecriture des mailles MED_TRIA3 :
     - Connectivite
     - Noms (optionnel)
     - Numeros (optionnel)
     - Numeros des familles */
  if ( MEDmeshElementWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
			MED_CELL,MED_TRIA3,MED_DESCENDING,MED_NO_INTERLACE,
			ntr3,tr3,MED_TRUE,nomtr3,MED_TRUE,numtr3,MED_TRUE,nufatr3)< 0 ) {
    MESSAGE("Impossible d'ecrire les �l�ments triangles : ");
    return -1;
  }

  /* Fermeture du fichier */
  if (  MEDfileClose(fid) < 0) {
    MESSAGE("Impossible de fermerle fichier : ");
    return -1;
  }

  return 0;
}






