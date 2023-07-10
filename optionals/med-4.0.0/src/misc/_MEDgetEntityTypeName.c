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

med_err _MEDgetEntityTypeName(char * const entitytypename,const med_entity_type entitytype)
{
   switch(entitytype)
     {
     case MED_NODE :
       strcpy(entitytypename,MED_NOM_NOE);
       break;

       /* MED_NOEUD_...ne devrait pas intervenir � ce niveau, 
	  mais �vite une modification s�lective
	  dans les API de gestion des champs */
     case MED_NODE_ELEMENT :
       strcpy(entitytypename,MED_NOM_NOE);
       break;

     case MED_CELL :
       strcpy(entitytypename,MED_NOM_MAI);
       break;

     case MED_DESCENDING_FACE :
       strcpy(entitytypename,MED_NOM_FAC);
       break;

     case MED_DESCENDING_EDGE :
       strcpy(entitytypename,MED_NOM_ARE);
       break;

     case MED_STRUCT_ELEMENT :
       strcpy(entitytypename,MED_NOM_STR);
       break;

     default :
       return -1;
     }
   return 0;
}
