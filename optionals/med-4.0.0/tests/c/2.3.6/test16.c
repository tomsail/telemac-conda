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
#define MODE_ACCES MED_LECTURE_ECRITURE
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_LECTURE_AJOUT
#else
#define MODE_ACCES MED_CREATION
#endif

int main (int argc, char **argv)


{
  med_idt fid;
  med_int nse2 = 5;
  med_int se2[10] = {1,2,1,3,2,4,3,4,2,3};
  /*                                12345678901234561234567890123456123456789012345612345678901234561234567890123456 */
  char nomse2[MED_TAILLE_PNOM*5+1]="se1             se2             se3             se4             se5             ";
  med_int numse2[5] = {1,2,3,4,5};
  med_int nufase2[5] = {-1,-1,0,-2,-3}; 
  med_int ntr3 = 2;
  med_int tr3[6] = {1,2,-5,-5,3,-4};
  /*                                  1234567890123456123456789012345612345678901234561234567890123456 */
  /* Erreur sur la taille de tr1, il manque deux blancs.*/
  char nomtr3[MED_TAILLE_PNOM*2+1] = "tr1           tr2             ";
  med_int numtr3[2] = {4,5};
  med_int nufatr3[2] = {0,-1};
  char maa[MED_TAILLE_NOM+1] = "maa1";
  med_int mdim = 2;

  /* Creation du fichier test16.med */
  if ( (fid = MEDouvrir("test16.med",MODE_ACCES) ) < 0) {
    MESSAGE("Impossible de creer le fichier test16.med : ");
    return -1;
  }

  /* Creation du maillage */
  if ( MEDmaaCr(fid,maa,mdim,MED_NON_STRUCTURE,
		   "un maillage pour test16") < 0 ) {
    MESSAGE("Impossible de creer le maillage : ");
    return -1;
  }

  /* Ecriture des aretes segments MED_SEG2 :
     - connectivite
     - noms (optionnel) 
     - numeros (optionnel)
     - numeros des familles */
  if ( MEDelementsEcr(fid,maa,mdim,se2,MED_NO_INTERLACE,nomse2,MED_VRAI,numse2,MED_VRAI,
			 nufase2,nse2,MED_ARETE,MED_SEG2,MED_DESC)< 0 ) {
    MESSAGE("Impossible d'ecrire la connectivité des aretes : ");
    return -1;
  }

  /* Ecriture des mailles MED_TRIA3 :
     - Connectivite
     - Noms (optionnel) 
     - Numeros (optionnel)
     - Numeros des familles */
  if ( MEDelementsEcr(fid,maa,mdim,tr3,MED_NO_INTERLACE,nomtr3,MED_VRAI,numtr3,MED_VRAI,
			 nufatr3,ntr3,MED_MAILLE,MED_TRIA3,MED_DESC) < 0 ) {
    MESSAGE("Impossible d'ecrire les éléments triangles : ");
    return -1;
  }

  /* Fermeture du fichier */
  if (  MEDfermer(fid) < 0) {
    MESSAGE("Impossible de fermerle fichier : ");
    return -1;
  }

  return 0;
  
}






