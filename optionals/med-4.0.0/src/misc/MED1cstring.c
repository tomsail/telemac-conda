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
 * - Nom de la fonction : _MED1cstring
 * - Description : convertit une chaine de caracteres FORTRAN
 *                 en une nouvelle chaine de caracteres C
 *                 dont la longueur est passee en parametre.
 *                 Les caracteres completes sont des blancs
 * Cette routine est utile au les paramètres chaînes f77 au format chaînes de taille fixe de l'API C.
 * - Parametres :
 *     - chaine (IN)          : la chaine FORTRAN
 *     - longueur_reelle (IN) : la longueur de la chaine FORTRAN
 *     - longueur_fixee (IN)  : longueur de la chaine C a construire
 * - Resultat : la nouvelle chaine C en cas de succes, NULL sinon
 */
char *_MED1cstring(char *chaine,int longueur_reelle,int longueur_fixee)
{
  char *nouvelle;
  int i;
  med_err _ret = 0;

  if (longueur_reelle > longueur_fixee) {
    fprintf(stderr,"Erreur n°1 ds _MED1cstring\n");
    return NULL;
  }
  if ((nouvelle = (char *) malloc(sizeof(char)*(longueur_fixee+1))) == NULL) {
    fprintf(stderr,"Erreur n°2 ds _MED1cstring : longueur_reelle %d, longueur_fixee %d\n",longueur_reelle,longueur_fixee);
    MED_ERR_(_ret,MED_ERR_NOTNULL,MED_ERR_API,MED_ERR_NAME_MSG"malloc");
    return NULL;
  }

  for (i=0;i<longueur_reelle;i++)
    *(nouvelle+i) = *(chaine+i);

  for (i=longueur_reelle;i<longueur_fixee;i++)
    *(nouvelle+i) = ' ';

  *(nouvelle+longueur_fixee) = '\0';

  return nouvelle;
}

