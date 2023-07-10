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
 * - Nom de la fonction : _MEDobjetIdentifier
 * - Description : retrouve le nom de l'objet de rang "indice" 
 *                 se trouvant dans le datagroup "chemin"
 * - Parametres :
 *     - fid     (IN)     : l'ID du fichier ou se trouve le datagroup
 *     - chemin  (IN)     : chemin d'acces au datagroup
 *     - indice  (IN)     : indice de l'objet du datagroup dont on veut
 *                          le nom
 *     - nom     (OUT)    : le nom 
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 
med_err
_MEDobjetIdentifier(med_idt fid,char *chemin,int indice,void *nom)
{
  int idx;

  if ((idx = H5Giterate(fid,chemin,&indice,_MEDindiceInfo,
			nom)) < 0)
    return -1;
    /* {  H5Eprint1(stderr);return -1;} */
  ;
  return 0;
}

