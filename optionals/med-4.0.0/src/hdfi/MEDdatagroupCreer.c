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
 * - Nom de la fonction : _MEDdatagroupCreer
 * - Description : creation et ouverture d'un Datagroup HDF
 * - Parametres :
 *     - pid     (IN)    : l'ID de l'objet pere
 *     - nom     (IN)    : le nom de l'objet fils
 * - Resultat : l'ID du fils en cas de succes, -1 sinon
 */ 
med_idt
_MEDdatagroupCreer(med_idt pid, const char * const nom)
{
  med_idt id;
  med_err ret=0;
  med_access_mode MED_ACCESS_MODE;

  if ( (MED_ACCESS_MODE = _MEDmodeAcces(pid) ) == MED_ACC_UNDEF ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier.");
    return -1;
  }

  if ( MED_ACCESS_MODE == MED_ACC_RDONLY) {
    MESSAGE("Impossible de créer un datagroup en mode MED_ACC_RDONLY.");
    return -1;
  };


  id = _MEDdatagroupOuvrir(pid,nom);
  if (id > 0)
    if (MED_ACCESS_MODE == MED_ACC_RDEXT) {
      MESSAGE("Création d'un datagroup existant impossible en mode MED_ACC_RDEXT.");
      return -1;
    };
  /* sinon on le crée */
  if (id <= 0)
    if ((id = H5Gcreate(pid,nom,0)) < 0) {
      MESSAGE("Impossible de créer le datagroup : ");
      SSCRUTE(nom);
      return -1;
    }

  return id;
}
