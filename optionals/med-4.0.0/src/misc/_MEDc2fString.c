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

med_err _MEDc2fString(const char *   const chainec,
		            char *   const chainef,
		      med_int        longueur_buffer77)
{
  int _longueur_chainec=0, i;

  if ( longueur_buffer77 == 0 ) return 0;

  _longueur_chainec = strlen(chainec);

  if (longueur_buffer77 < _longueur_chainec)
    return -1;

  /*Recopie la chaine C dans la chaine F77 sans le caractère terminal C */
  strncpy(chainef,chainec,_longueur_chainec);

  /* on  complete par des blancs */
  for (i=_longueur_chainec;i<longueur_buffer77;i++)
    *(chainef+i) = ' ';

  return 0;
}
