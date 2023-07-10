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
 * - Nom du fichier : test3.c
 *
 * - Description : lecture des informations sur les maillages d'un fichier MED.
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
  med_err ret = 0;
  med_idt fid;
  med_int nmaa,i,mdim,edim;
  char maa[MED_TAILLE_NOM+1];
  char nomu[MED_TAILLE_LNOM+1];
  char desc[MED_TAILLE_DESC+1];
  med_maillage type;
  med_err inomu;

  /* Ouverture du fichier "test2.med" en lecture seule */
  fid = MEDouvrir("test2.med",MED_LECTURE);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test2.med");
    return -1;
  }

  /* Lecture du nombre de maillage dans le fichier */
  nmaa = MEDnMaa(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur a la lecture du nombre de maillage");
    ret = -1;
  } 
  if (ret == 0)
    printf("- Nombre de maillage dans test2.med = "IFORMAT"\n",nmaa);

  /* Boucle sur tous les maillages, pour chaque maillage, on lit :
     - Le nom.
     - Le type
     - La dimension
     - La description
     - La dimension de l'espace si elle existe
     - Le nom universel s'il existe 
     */
  if (ret == 0)
    for (i=0;i<nmaa;i++) {
      /* lecture des informations */
      if (MEDmaaInfo(fid,i+1,maa,&mdim,&type,desc) < 0) {
	MESSAGE("Erreur a la lecture des informations du maillage :"); SSCRUTE(maa);
	ret = -1;
      }
      /* lecture de la dimension de l'espace */
      edim = MEDdimEspaceLire(fid,maa);
      /* lecture du nom universel */
      inomu = MEDunvLire(fid,maa,nomu);
      /* affichage des donnees lues */
      if (inomu < 0)
	printf("maillage "IFORMAT" de nom %s, de dimension "IFORMAT" \n",i+1,maa,mdim);
      else
	printf("maillage "IFORMAT" de nom %s, de dimension "IFORMAT" et de nom univ. %s\n",i+1,maa,mdim,nomu);	
      if (edim > 0)
	printf("La dimension de l'espace est "IFORMAT" \n",edim);
      else
	printf("La dimension de l'espace est "IFORMAT" \n",mdim);
      if (type == MED_STRUCTURE)
	printf("Il s'agit d'un maillage structure \n");
      else
	printf("Il s'agit d'un maillage non structure \n");
      printf("Description associee au maillage : %s \n\n",desc);
    }

  /* Fermeture du fichier */
  ret = MEDfermer(fid);
  if (ret < 0) {
    MESSAGE("Erreur a la fermeture du fichier test2.med");
    return -1;
  }
  
  return ret;
}




