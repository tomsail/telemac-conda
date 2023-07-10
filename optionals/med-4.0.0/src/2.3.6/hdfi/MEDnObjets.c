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
 * - Nom de la fonction : _MEDnObjets
 * - Description : indique le nombre d'objets HDF contenu dans le
 *                 datagroup passe en argument
 * - Parametres :
 *     - fid     (IN)     : l'ID du fichier HDF
 *     - chemin  (IN)     : chemin d'acces au datagroup
 *     - n       (OUT)    : le nombre recherche
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 
med_err 
_MEDnObjets(med_idt fid,char *chemin,int *n)
{
  /*int idx;*/
  /*   if ((idx  = H5Giterate(fid,chemin,NULL,_MEDindiceNum,(void *)n)) < 0) */
  /*     return -1; */

  /* EF */
  med_err ret=-1;
  med_idt id=0;
  herr_t  err;
  hsize_t num_obj=0;

  if ( (id = H5Gopen(fid,chemin)) < 0   ) goto ERROR;

  if ( (err =H5Gget_num_objs(id,&num_obj) ) < 0 ) goto ERROR; 

  ret = 0;
  *n = (med_err) num_obj;

 ERROR:
  if (id) H5Gclose(id);

  return ret;
}
