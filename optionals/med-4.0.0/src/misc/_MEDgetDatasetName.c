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

med_err _MEDgetDatasetName(char * const datasetname,
			   const med_data_type meddatatype,
			   med_connectivity_mode cmode )
{
  med_err _ret=-1;

  /*TODO : Remplacer par un tableau global const de pointeur const ! (plus de strcpy) */
  switch(meddatatype)
    {
    case MED_COORDINATE :
      strcpy(datasetname,MED_NOM_COO);
      break;

    case MED_COORDINATE_AXIS1 :
      strcpy(datasetname,MED_NOM_IN1);
      break;

    case MED_COORDINATE_AXIS2 :
      strcpy(datasetname,MED_NOM_IN2);
      break;

    case MED_COORDINATE_AXIS3 :
      strcpy(datasetname,MED_NOM_IN3);
      break;

    case MED_CONNECTIVITY :
    case MED_INDEX_FACE :
    case MED_INDEX_NODE :
      switch(cmode)
	{
	case MED_NODAL :
	  switch(meddatatype)
	    {
	    case MED_CONNECTIVITY :
	      strcpy(datasetname,MED_NOM_NOD);
	      break;
	    case MED_INDEX_FACE :
	      strcpy(datasetname,MED_NOM_IFN);
	      break;
	    case MED_INDEX_NODE :
	      strcpy(datasetname,MED_NOM_INN);
	      break;
	    }
	  break;

	case MED_DESCENDING :
	  switch(meddatatype)
	    {
	    case MED_CONNECTIVITY :
	      strcpy(datasetname,MED_NOM_DES);
	      break;
	    case MED_INDEX_FACE :
	      strcpy(datasetname,MED_NOM_IFD);
	      break;
	    case MED_INDEX_NODE :
	      strcpy(datasetname,MED_NOM_IND);
	      break;
	    }
	  break;

	default :
	  MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_CONNECTIVITYMODE,MED_ERR_VALUE_MSG);
	  ISCRUTE_int(cmode);goto ERROR;
	}
      break;

    case MED_NAME :
      strcpy(datasetname,MED_NOM_NOM);
      break;

    case MED_NUMBER :
      strcpy(datasetname,MED_NOM_NUM);
      break;

    case MED_GLOBAL_NUMBER :
      strcpy(datasetname,MED_NOM_GLB);
      break;

    case MED_FAMILY_NUMBER :
      strcpy(datasetname,MED_NOM_FAM);
      break;

    case MED_COORDINATE_TRSF :
      strcpy(datasetname,MED_NOM_TRF);
      break;

    default :
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_MEDDATATYPE,MED_ERR_VALUE_MSG);
      ISCRUTE_int(meddatatype);goto ERROR;
    }

  _ret = 0;
 ERROR:
  return _ret;
}

