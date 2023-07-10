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



#include "med_config.h"
#include "med_outils.h"
#include <string.h>

/* #ifdef __cplusplus */
/* } */
/* #endif */

#include "MAJ_236_300.h"

/*Supression des blancs terminaux 
  Si ils existent --> ret 1
  Si ils n'existent pas --> ret 0
*/
int MAJ_236_300_chaine(char * nomi, char * nomf) {
  int i,n;

  n=strlen(nomi);
  i=n-1;
  if ( nomi[i] == ' ') {
    for ( ; nomi[i-1] == ' '; ) {--i;};
    strncpy(nomf,nomi,i);
    nomf[i] = '\0';
    return 1;
  }
  return 0;
}
