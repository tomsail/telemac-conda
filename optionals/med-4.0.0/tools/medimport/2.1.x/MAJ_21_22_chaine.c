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


#include "med_hdfi21.h"
#include "med21.h"
#include "MAJ_21_22.h"
#include <string.h>

void MAJ_21_22_chaine(char *ancienne_chaine,char *nouvelle_chaine,med_int n)
{
  int i;
  int j;
  char tmp[MED_TAILLE_PNOM+1];

  for (i=0;i<n;i++) {
    if (i == n-1) {
      strcpy(tmp,ancienne_chaine+i*ANCIEN_MED_TAILLE_PNOM);
      if (strlen(tmp) < ANCIEN_MED_TAILLE_PNOM)
	for(j=strlen(tmp);j<ANCIEN_MED_TAILLE_PNOM;j++) 
	  tmp[j] = ' ';
    }
    else
      strncpy(tmp,ancienne_chaine+i*ANCIEN_MED_TAILLE_PNOM,ANCIEN_MED_TAILLE_PNOM);
    tmp[ANCIEN_MED_TAILLE_PNOM] = '\0';
    /*          12345678 */
    strcat(tmp,"        ");
    /*     printf("[%s]\n",tmp); */
    if (i == 0)
      strcpy(nouvelle_chaine,tmp);
    else
      strcat(nouvelle_chaine,tmp);
  }
  *(nouvelle_chaine+MED_TAILLE_PNOM*n) = '\0';
} 
