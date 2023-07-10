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
 * - Nom de la fonction : _MEDfichierDemonter
 * - Description : Démontage d'un fichier HDF externe
 * - Parametres :
 *     - pid (IN)    : ID du pere (fichier ou groupe HDF)
 *     - nom (IN)    : nom du groupe HDF dans lequel est monte le fichier
 * - Resultat : O en cas de succes, -1 sinon
 */ 

extern
med_err _MEDfichierDemonter(med_idt pid, const char *nom)
{
  med_err ret = 0;

  ret = H5Funmount(pid,nom);

  return ret;
}
