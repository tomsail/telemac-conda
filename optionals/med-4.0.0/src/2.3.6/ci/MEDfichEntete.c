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

#include <string.h>

med_err 
MEDfichEntete(med_idt fid, med_fich_info quoi, char str[])
{
  med_idt atid, root;
  med_err ret;
  char locale[MED_TAILLE_DESC+1];
  char chemin[MED_TAILLE_MAA+1];

  switch (quoi)
    {
    case MED_HDF_VERSION : 
      strcpy(str,HDF_VERSION_ACTUELLE);
      break;

    case MED_VERSION :
      strcpy(str,PACKAGE_VERSION);
      break;

    case MED_FICH_DES :
      /*
       * On inhibe le gestionnaire d'erreur HDF
       */
      _MEDmodeErreurVerrouiller();
/* Ajout de la ligne suivante par un script malencontreux ?*/
/* if (MEDcheckVersion(fid) < 0) return -1; */

      /*
       * On ouvre le Data Group racine
       */
      strncpy(chemin,MED_MAA,strlen(MED_MAA)-1);
      chemin[MED_TAILLE_MAA-1] = '\0';
      if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
	return -1;

      /*
       * On regarde si l'attribut existe
       * Si non => erreur
       * Si oui => on le copie dans str
       */
      if ((ret = _MEDattrStringLire(root,MED_NOM_DESCRIPTEUR,
				    MED_TAILLE_DESC,locale)) < 0) {
	_MEDdatagroupFermer(root);
	return -1;
      }

      strcpy(str,locale);
      
      if ( _MEDdatagroupFermer(root) < 0)
	return -1;

      break;
      
    default :
      return -1;
    }
  return 0;
}
