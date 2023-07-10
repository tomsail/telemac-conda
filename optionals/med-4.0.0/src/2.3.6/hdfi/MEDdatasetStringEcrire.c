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
#include <hdf5.h>

/*
 * - Nom de la fonction : _MEDdatasetStringEcrire
 * - Description : ecriture d'un dataset tableau de caracteres
 * - Parametres :
 *     - pere (IN)     : l'ID de l'objet HDF pere ou placer l'attribut
 *     - nom  (IN)     : le nom de l'attribut 
 *     - dimd (IN)     : profil du tableau
 *     - val  (IN)     : valeurs du tableau
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 
med_err _MEDdatasetStringEcrire(med_idt pere, char *nom, med_size *dimd, char *val)
{
  med_idt dataset;
  med_idt datatype = 0;
  med_idt dataspace = 0;
  med_err ret;
  med_mode_acces MED_MODE_ACCES;

  if ( (MED_MODE_ACCES = _MEDmodeAcces(pere) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier ");
    return -1;
  }

  if ((dataset = H5Dopen(pere,nom)) < 0)
    {
      if ((dataspace = H5Screate_simple(1,dimd,NULL)) < 0)
	return -1;
      if((datatype = H5Tcopy(H5T_C_S1)) < 0)
	return -1;
      if((ret = H5Tset_size(datatype,1)) < 0)
	return -1;
      if ((dataset = H5Dcreate(pere,nom,datatype,dataspace,
			     H5P_DEFAULT)) < 0)
	return -1;    
    }
  else
    if ( MED_MODE_ACCES == MED_LECTURE_AJOUT)
      {
	H5Dclose(dataset);
	return -1;
      }
    else
      {
      if ((dataspace = H5Screate_simple(1,dimd,NULL)) < 0)
	return -1;
      if((datatype = H5Tcopy(H5T_C_S1)) < 0)
	return -1;
      if((ret = H5Tset_size(datatype,1)) < 0)
	return -1;
      }
  if ((ret = H5Dwrite(dataset,datatype,H5S_ALL,H5S_ALL,
		      H5P_DEFAULT, val)) < 0)
    return -1;
  if (dataspace)
    if((ret = H5Sclose(dataspace)) < 0)
      return -1;
  if (datatype)
    if ((ret = H5Tclose(datatype)) < 0)
      return -1;
  if ((ret = H5Dclose(dataset)) < 0)
    return -1;

  return 0;
}
