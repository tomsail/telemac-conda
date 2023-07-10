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


#include <stdlib.h>
#include "med_config.h"
#include "med_outils.h"

/*
 * - Nom de la fonction _MED2cstring
 * - Description : convertit une chaine de caracteres FORTRAN en
 *                 nouvelle chaine de caracteres C
 *                 (crée une copie de chaine C en supprimant
 *                   les blancs terminaux et
 *                   terminant par le caractère \0)
 * - Parametres :
 *     - chaine (IN)   : la chaine FORTRAN
 *     - longueur (IN) : longueur de la chaine FORTRAN
 * - Resultat : la nouvelle chaine C en cas de succes, NULL sinon
 */
char *_MED2cstring(char *chaine, int longueur)
{
  char *nouvelle;
  char *temoin;
  int long_reelle = longueur;
  int i;
  med_err _ret =0;

  if ( longueur < 0 ) return NULL;

  temoin = chaine+longueur-1;
  while (*temoin == ' ' && (temoin > chaine) )
    {
      temoin --;
      long_reelle--;
    }
  if ( *temoin == ' ') long_reelle = 0;

  if ((nouvelle = (char *) malloc(sizeof(char)*(long_reelle+1))) == NULL) {
    MED_ERR_(_ret,MED_ERR_NOTNULL,MED_ERR_API,MED_ERR_NAME_MSG"malloc");
    return NULL;
  }

  /* +1 en trop mais caractère écrasé par le caractère null */
  for (i=0;i<long_reelle+1 ;i++)
    *(nouvelle+i) = *(chaine+i);
  *(nouvelle+long_reelle) = '\0';

  return nouvelle;
}
