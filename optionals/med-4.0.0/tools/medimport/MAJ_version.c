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

#include "med_config.h"
#include <med.h>
#include <med_outils.h>
#include "MAJ_version.h"

void MAJ_version_num(med_idt fid, const int majeur, const int mineur, const int release )
{

  static med_file_version _fileversion           = MED_FILE_VERSION_INIT;

  _fileversion.majeur  = majeur ;
  _fileversion.mineur  = mineur ;
  _fileversion.release = release;

  _MEDfileVersionSetCache(fid,_fileversion);

}

void MAJ_write_version_num(med_idt fid, const int majeur, const int mineur, const int release )
{
  med_err ret;
  med_idt gid;
  med_int _majeur=majeur, _mineur=mineur, _release=release;


  gid = _MEDdatagroupOuvrir(fid,MED_INFOS);
  if (gid < 0)
    gid = _MEDdatagroupCreer(fid,MED_INFOS);

  ret = _MEDattributeIntWr(gid,MED_NOM_MAJEUR,&_majeur);
  EXIT_IF(ret < 0,"Ecriture du numéro majeur",NULL);
  ret = _MEDattributeIntWr(gid,MED_NOM_MINEUR,&_mineur);
  EXIT_IF(ret < 0,"Ecriture du numéro mineur",NULL);
  ret = _MEDattributeIntWr(gid,MED_NOM_RELEASE,&_release);
  EXIT_IF(ret < 0,"Ecrriture du numéro de release",NULL);
  ret = _MEDdatagroupFermer(gid);
  EXIT_IF(ret < 0,"Fermeture du groupe HDF MED_INFOS",NULL);

}



void MAJ_version(med_idt fid)
{
  med_err ret;
  med_idt gid;
  med_int majeur, mineur, release;

  /* On ecrit le bon numero de version */
  majeur  = MED_NUM_MAJEUR ;
  mineur  = MED_NUM_MINEUR ;
  release = MED_NUM_RELEASE;

  MAJ_version_num(fid,  majeur, mineur, release );

}
