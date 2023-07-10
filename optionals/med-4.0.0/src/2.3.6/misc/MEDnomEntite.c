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
 * - Nom de la fonction : _MEDnomEntite
 * - Description : fournit le nom associe a un type d'entite MED
 * - Parametres :
 *     - nom_ent (OUT) : le nom de l'entite
 *     - type_ent (IN) : le type de l'entite
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 
med_err _MEDnomEntite(char *nom_ent,med_entite_maillage type_ent)
{
   switch(type_ent)
     {
     case MED_NOEUD :
       strcpy(nom_ent,MED_NOM_NOE);
       break;

       /* MED_NOEUD_...ne devrait pas intervenir à ce niveau, 
	  mais évite une modification sélective
	  dans les API de gestion des champs */
     case MED_NOEUD_MAILLE :
       strcpy(nom_ent,MED_NOM_NOE);
       break;

     case MED_MAILLE :
       strcpy(nom_ent,MED_NOM_MAI);
       break;

     case MED_FACE :
       strcpy(nom_ent,MED_NOM_FAC);
       break;

     case MED_ARETE :
       strcpy(nom_ent,MED_NOM_ARE);
       break;

     default :
       return -1;
     }
   return 0;
}
