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
 * - Nom de la fonction : _MEDfileName
 * - Description : Renvoi le nom du fichier contenant l'objet
 *                 identifiés par id
 * - Parametres :
 *     - id               (IN)  : identificateur de l'objet
 *     - filenamesize     (IN)  : taille 
 *     - filename         (OUT) : nom de fichier contenant l'objet id
 * - Resultat : numéro hdf du fichier en cas de succes, -1 sinon
 */
med_int _MEDfileName(med_idt id, char * const filename, const med_int filenamesize )
{
  ssize_t _size=-1;
  
  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  if ( (_size = H5Fget_name(id, filename,  filenamesize+1 ) ) < 0) {
    MED_ERR_(_size,MED_ERR_READ,MED_ERR_FILE,MED_ERR_NAME_MSG);
    SSCRUTE(filename);ISCRUTE_id(id);ISCRUTE_long(_size);
    goto ERROR;
  }

 ERROR:
  return _size;
}
