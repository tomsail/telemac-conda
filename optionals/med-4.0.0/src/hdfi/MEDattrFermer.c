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
 * - Nom de la fonction : _MEDattrFermer
 * - Description : fermeture de l'acces a l'attribut dont l'ID est passe en 
 *                 parametre
 * - Parametres :
 *     - pid (IN)  : l'ID de l'objet HDF pere ou placer l'attribut
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 
med_err _MEDattrFermer(med_idt id)
{
  med_err ret;

  if ((ret = H5Aclose(id)) < 0)
    return -1;

  return 0;
}
