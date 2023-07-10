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
 * - Nom de la fonction : _MEDnomGeometrie
 * - Description : fournit le nom de l'element geometrique associe
 *                 au type geometrique MED
 * - Parametres :
 *     - nom_geo (OUT) : le nom de l'element
 *     - type_geo (IN) : le type de l'element
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 
med_err _MEDnomGeometrie(char *nom_geo,med_geometrie_element type_geo)
{
   switch (type_geo)
     {
     case MED_POINT1 :
       strcpy(nom_geo,MED_NOM_PO1);
       break;
       
     case MED_SEG2 :
       strcpy(nom_geo,MED_NOM_SE2);
       break;
	   
     case MED_SEG3 :
       strcpy(nom_geo,MED_NOM_SE3);
       break;
	   
     case MED_TRIA3 :
       strcpy(nom_geo,MED_NOM_TR3);
       break;
	   
     case MED_TRIA6 :
       strcpy(nom_geo,MED_NOM_TR6);
       break;
	   
     case MED_QUAD4 :
       strcpy(nom_geo,MED_NOM_QU4);
       break;
       
     case MED_QUAD8 :
       strcpy(nom_geo,MED_NOM_QU8);
       break;
       
     case MED_TETRA4 :
       strcpy(nom_geo,MED_NOM_TE4);
       break;
       
     case MED_TETRA10 :
       strcpy(nom_geo,MED_NOM_T10);
       break;
       
     case MED_HEXA8 :
       strcpy(nom_geo,MED_NOM_HE8);
       break;
       
     case MED_HEXA20 :
       strcpy(nom_geo,MED_NOM_H20);
       break;
       
     case MED_PENTA6 :
       strcpy(nom_geo,MED_NOM_PE6);
       break;
       
     case MED_PENTA15 :
       strcpy(nom_geo,MED_NOM_P15);
       break;
       
     case MED_PYRA5 :
       strcpy(nom_geo,MED_NOM_PY5);
       break;
       
     case MED_PYRA13 :
       strcpy(nom_geo,MED_NOM_P13);
       break;

     case MED_POLYGONE :
       strcpy(nom_geo,MED_NOM_POG);
       break;

     case MED_POLYEDRE :
       strcpy(nom_geo,MED_NOM_POE);
       break;

     default :
       return -1;
	 }
   
   return 0;
} 
