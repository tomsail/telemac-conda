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
 * - Nom de la fonction : _MEDnomDataset
 * - Description : fournit un nom de dataset
 * - Parametres :
 *     - nom_dataset (OUT) : le nom du data set
 *     - quoi (IN)         : le type de table MED
 *     - type_conn (IN)    : le type de connectivite
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 
med_err _MEDnomDataset(char *nom_dataset,med_table quoi,med_connectivite type_conn)
{
  switch(quoi)
    {
    case MED_COOR :
      strcpy(nom_dataset,MED_NOM_COO);
      break;

    case MED_COOR_IND1 : 
      strcpy(nom_dataset,MED_NOM_IN1);
      break;

    case MED_COOR_IND2 : 
      strcpy(nom_dataset,MED_NOM_IN2);
      break;

    case MED_COOR_IND3 : 
      strcpy(nom_dataset,MED_NOM_IN3);
      break;

    case MED_CONN :
      switch(type_conn)
	{
	case MED_NOD :
	  strcpy(nom_dataset,MED_NOM_NOD);
	  break;

	case MED_DESC :
	  strcpy(nom_dataset,MED_NOM_DES);
	  break;

	default :
	  return -1;
	}
      break;

    case MED_NOM :
      strcpy(nom_dataset,MED_NOM_NOM);
      break;

    case MED_NUM :
      strcpy(nom_dataset,MED_NOM_NUM);
      break;

    case MED_FAM :
      strcpy(nom_dataset,MED_NOM_FAM);
      break;

    default :
      return -1;
    }

  return 0;
}

