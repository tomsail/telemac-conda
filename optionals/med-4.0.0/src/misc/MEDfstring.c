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
#include <med_config.h>
#include <med_outils.h>
#include <string.h>

/*
 *  Chaine C -> chaine FORTRAN completee par des blancs
 */

med_err _MEDfstring(char *chaine, med_int longueur_fixee)
{
  int longueur_reelle, i;

  if (longueur_fixee == 0 ) return 0;

  longueur_reelle = strlen(chaine);
  if (longueur_fixee < longueur_reelle)
    return -1;

  /* on supprime le caractere de fin de chaine C '\0'
     et complete par des blancs */
  for (i=longueur_reelle;i<longueur_fixee;i++)
    *(chaine+i) = ' ';

  return 0;
}
