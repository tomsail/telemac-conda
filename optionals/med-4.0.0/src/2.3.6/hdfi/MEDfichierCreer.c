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

/*
 * - Nom de la fonction : _MEDfichierCreer
 * - Description : creation d'un fichier HDF
 * - Parametres :
 *     - nom (IN) : le nom du fichier
 * - Resultat : ID du fichier en cas de succes, -1 sinon
 */ 
med_idt _MEDfichierCreer(char *nom, med_mode_acces mode)
{
  med_idt fid,gid;
  med_err ret;
  med_int majeur = 2;
  med_int mineur = 3; 
  med_int release = 6;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  if ((fid = H5Fcreate(nom,H5F_ACC_TRUNC,
			       H5P_DEFAULT,H5P_DEFAULT)) < 0)
    return -1;

  _MEDsetModeAcces(fid,MED_LECTURE_ECRITURE);

  if ((gid = _MEDdatagroupCreer(fid,MED_INFOS)) < 0)
    return -1;

  /* Numero de versions de MED */
  if ((ret = _MEDattrEntierEcrire(gid,MED_NOM_MAJEUR,&majeur)) < 0)
    return -1;

  if ((ret = _MEDattrEntierEcrire(gid,MED_NOM_MINEUR,&mineur)) < 0)
    return -1;

  if ((ret = _MEDattrEntierEcrire(gid,MED_NOM_RELEASE,&release)) < 0)
    return -1;

  /* On ferme tout */
  if ((ret = _MEDdatagroupFermer(gid)) < 0)
    return -1;

  _MEDsetModeAcces(fid,mode);

  return fid;
}
