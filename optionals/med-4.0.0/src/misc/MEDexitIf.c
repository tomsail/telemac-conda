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


#include <stdio.h>
#include <stdlib.h>
#include <med_exit_if.h>


/******************************************************************************
 *
 * - Nom de la fonction : exit_if
 * - Description : gestion et affichage des erreurs.
 * - Parametres :
 *     - fichier    (IN) : nom du fichier.
 *     - ligne      (IN) : ligne ou s'est produite l'erreur.
 *     - condition  (IN) : condition a evaluer pour voir s'il y a une erreur.
 *     - message    (IN) : message a afficher sur le "stderr" en cas d'erreur.
 *     - arg        (IN) : parametre en complement au message.
 *
 ******************************************************************************/
void exit_if(const char * const fichier,int ligne, int condition,const char * const message, const char * const arg) 
{
  if (condition == 0)
    return;
  if (message != NULL)
    if (arg == NULL)
      fprintf(stderr,"%s : %d : >>>> ERREUR : %s  \n",
	      fichier,ligne,message);
    else
      fprintf(stderr,"%s : %d : >>>> ERREUR : %s %s \n",
	      fichier,ligne,message,arg);
  else
    fprintf(stderr,"%s : %d : >>>> ERREUR d'allocation memoire \n",
	    fichier,ligne);
  exit(condition);
} 
