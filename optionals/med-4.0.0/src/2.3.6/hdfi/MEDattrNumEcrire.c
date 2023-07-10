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
 * - Nom de la fonction : _MEDattrNumEcrire
 * - Description : ecriture d'un attribut entier
 * - Parametres :
 *     - pere (IN) : l'ID de l'objet HDF pere ou placer l'attribut
 *     - type (IN) : le type du champ {MED_FLOAT64,MED_INT}
 *     - nom  (IN) : le nom de l'attribut
 *     - val  (IN) : la valeur de l'attribut
 * - Resultat : 0 en cas de succes, -1 sinon
 */

med_err _MEDattrNumEcrire(med_idt pere,med_type_champ type,char *nom,unsigned char *val)
{
  med_idt aid,attr;
  med_err ret;
  med_idt type_hdf;
  med_mode_acces MED_MODE_ACCES;

  if ( (MED_MODE_ACCES = _MEDmodeAcces(pere) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier ");
    return -1;
  }

  switch(type)
    {
    case MED_FLOAT64 :
      type_hdf = H5T_NATIVE_DOUBLE;
      break;
      
    case MED_INT :
#if defined(HAVE_F77INT64)
      type_hdf = H5T_NATIVE_LONG; 
#else
      type_hdf = H5T_NATIVE_INT;
#endif
      break;

    default :
      return -1;
    }

  if ((aid = H5Screate(H5S_SCALAR)) < 0)
    return -1;

  if ( ((attr = H5Aopen_name(pere,nom)) > 0) && ( MED_MODE_ACCES == MED_LECTURE_AJOUT ) )
    return -1;
  else
    if ( attr < 0)
      if ((attr = H5Acreate(pere,nom,type_hdf,aid,H5P_DEFAULT)) < 0) return -1;  

  if ((ret = H5Awrite(attr,type_hdf,val)) < 0)
    return -1;


  if ((ret = H5Sclose(aid)) < 0)
    return -1;
  if ((ret = H5Aclose(attr)) < 0)
    return -1;

  return 0;
}
