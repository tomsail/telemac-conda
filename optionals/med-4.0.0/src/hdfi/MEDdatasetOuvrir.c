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
 * - Nom de la fonction : _MEDdatasetOuvrir
 * - Description : ouverture d'un objet HDF dataset
 * - Parametres :
 *     - pid  (IN)     : l'ID de l'objet HDF pere 
 *     - nom  (IN)     : le nom du dataset
 * - Resultat : ID du dataset en cas de succes, -1 sinon
 */ 
med_idt
_MEDdatasetOuvrir(med_idt pid,char *nom)
{
  med_idt id=0;

  if ( (id = H5Dopen(pid,nom)) < 0)
    return -1;

  return id;
}
