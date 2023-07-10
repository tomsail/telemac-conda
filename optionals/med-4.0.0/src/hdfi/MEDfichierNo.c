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
 * - Nom de la fonction : _MEDfichierNo
 * - Description : Renvoi le numéro de fichier contenant l'objet
 *                 identifiés par id1
 * - Parametres :
 *     - id     (IN)  : identificateur de l'objet1
 *     - fileno (OUT) : numéro de fichier unique
 * - Resultat : numéro hdf du fichier en cas de succes, -1 sinon
 */
med_err _MEDfichierNo(med_idt id, unsigned long * fileno )
{

  H5G_stat_t statbuf;

  if ( H5Gget_objinfo(id, "/", 1 , &statbuf ) < 0 ) {
    MESSAGE("Impossible d'identifier un numéro de fichier correct à partir de l'identifiant :");
    ISCRUTE_id(id);
    return -1;
  };

  *fileno = statbuf.fileno[0];
/*   ISCRUTE_id(id); */
/*   ISCRUTE(statbuf.fileno[0]); */
/*   ISCRUTE(statbuf.fileno[1]); */

  return 0;
}
