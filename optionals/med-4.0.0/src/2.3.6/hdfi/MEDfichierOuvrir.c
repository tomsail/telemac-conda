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
 * - Nom de la fonction : _MEDfichierOuvrir
 * - Description : ouverture d'un fichier HDF en fonction du mode passe
 *                 en parametre
 * - Parametres :
 *     - nom  (IN) : le nom du fichier
 *     - mode (IN) : mode d'ouverture  
 * - Resultat : ID du fichier en cas de succes, -1 sinon
 */ 
med_idt _MEDfichierOuvrir(char *nom,med_mode_acces mode)
{ 
  med_idt fid;
  int hdf_mode;

  switch(mode)
    {
    case MED_LECTURE_ECRITURE :
    case MED_LECTURE_AJOUT    :
      hdf_mode = H5F_ACC_RDWR; 
      break;

    case MED_LECTURE :
      hdf_mode = H5F_ACC_RDONLY;
      break;

    default :
      MESSAGE("Le mode d'accès demandé pour le fichier :");
      SSCRUTE(nom);
      MESSAGE("n'est pas compris");
      return -1;
    }  

  if ((fid = H5Fopen(nom,hdf_mode,H5P_DEFAULT)) < 0) {
    MESSAGE("Impossible d'ouvrir le fichier :");
    SSCRUTE(nom);
    MESSAGE("en mode :");
    ISCRUTE_int(mode);
    return -1;
  }

  _MEDsetModeAcces(fid,mode);

  return fid;
}
