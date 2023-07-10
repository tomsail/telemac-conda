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
 * - Nom de la fonction : _MEDdatagroupLienSupprimer
 * - Description : suppression d'un lien SOFT HDF dans un datagroup
 * - Parametres :
 *     - id       (IN)    : l'ID du datagroup
 *     - nom_lien (IN)    : nom du lien a supprimer 
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

med_err 
_MEDdatagroupLienSupprimer(med_idt id,const char *nom_lien) {
  med_err ret;

  ret = H5Gunlink(id,nom_lien);
   
  return ret;
}
