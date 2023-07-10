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
 * - Nom de la fonction : _MEDdatagroupLienCreer
 * - Description : création d'un lien SOFT HDF dans un datagroup
 * - Parametres :
 *     - id       (IN)    : l'ID du datagroup
 *     - nom      (IN)    : nom de l'objet source
 *     - nom_lien (IN)    : nom du lien a creer
 * - Resultat : 0 en cas de succes, -1 sinon
 */

med_err
_MEDdatagroupLienCreer(med_idt id,const char *nom, const char *nom_lien)
{
  med_err _ret=-1;

/*   if( (_ret = H5Glink(id,H5G_LINK_SOFT,nom,nom_lien)) < 0 ) { */
/*     MESSAGE("Impossible de créer le lien : "); */
/*     SSCRUTE(nom_lien); */
/*   } */

  if ( H5Lcreate_soft( nom, id, nom_lien, H5P_DEFAULT, H5P_DEFAULT ) < 0) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_LINK,nom_lien);
    SSCRUTE(nom);
    H5Eprint1(stderr);
    goto ERROR;
  }

/*interfile hard links are not allowed !*/
/*   if ( H5Lcreate_hard( id, nom, id, nom_lien, H5P_DEFAULT, H5P_DEFAULT ) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_LINK,nom_lien); */
/*     SSCRUTE(nom); */
/*     H5Eprint1(stderr); */
/*     goto ERROR; */
/*   } */

  _ret = 0;
 ERROR:
  return _ret;
}
