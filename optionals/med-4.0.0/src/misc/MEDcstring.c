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


#include <med.h>
#include "med_config.h"
#include "med_outils.h"
#include <stdlib.h>
#include <string.h>

/*
 * - Nom de la fonction _MEDcstring
 * - Description : convertit une chaine de caracteres C en 
 *                 nouvelle chaine de caracteres sans blancs terminaux
 *                 Cette opération est réalisée à partir du buffer source
 *                 dans le buffer destination déjà alloué de taille supposée
 *                 suffisante.
 * - Parametres :
 *     - source (IN)          : la chaine C
 *     - dest (OUT)           : la chaine C
 * - Resultat : la nouvelle chaine C en cas de succes, NULL sinon
 */
med_err _MEDcstring(char *source, char *dest)
{
  char *temoin;
  int longueur_source,long_reelle;
  int i;

  longueur_source = strlen(source);
  long_reelle     = longueur_source;

  if ( longueur_source <= 0 ) return -1;

  temoin = source+longueur_source-1;
  while (*temoin == ' ' && (temoin > source) )
    {
      temoin --;
      long_reelle--;
    }
  if ( *temoin == ' ') long_reelle = 0;
/*   ISCRUTE_int(long_reelle); */

  for (i=0;i<long_reelle+1;i++)
    *(dest+i) = *(source+i);
  *(dest+long_reelle) = '\0';
  
  return 0;
}
