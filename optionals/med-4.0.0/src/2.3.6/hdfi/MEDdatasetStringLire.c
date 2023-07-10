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
 * - Nom de la fonction : _MEDdatasetStringLire
 * - Description : lecture d'un dataset tableau de caracteres
 * - Parametres :
 *     - pere (IN)     : l'ID de l'objet HDF pere ou placer l'attribut
 *     - nom  (IN)     : le nom de l'attribut 
 *     - val  (IN)     : valeurs du tableau
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 
med_err _MEDdatasetStringLire(med_idt pere,char *nom,char *val)
{
  med_idt dataset,datatype;
  med_err ret;

  if ((dataset = H5Dopen(pere,nom)) < 0)
    return -1;
  if ((datatype = H5Tcopy(H5T_C_S1)) < 0)
    return -1;
  if ((ret = H5Tset_size(datatype,1)) < 0)
    return -1;
  if ((ret = H5Dread(dataset,datatype,H5S_ALL,H5S_ALL,H5P_DEFAULT,val)) < 0)
     return -1;
  if ((ret = H5Tclose(datatype)) < 0)
    return -1;
  if ((ret = H5Dclose(dataset)) < 0)
    return -1;

  return 0;
}
