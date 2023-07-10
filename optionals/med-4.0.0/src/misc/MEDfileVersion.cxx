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
#include "MEDfileVersion.hxx"
#include <med_config.h>
#include <med_outils.h>

using namespace std;

static MED_FILE_VERSION & MedfileVersion=MED_FILE_VERSION::Instance();

MED_FILE_VERSION& MED_FILE_VERSION::Instance() {
  static MED_FILE_VERSION obj;
  return obj;
}

MED_FILE_VERSION::~MED_FILE_VERSION() {};

extern "C" valueType  _MEDfileVersion(const med_idt oid) {

  map<keyType, valueType >::const_iterator it;
  fileNo key=0;
  static med_file_version       _fileversion           = MED_FILE_VERSION_INIT;
  static const med_file_version _med_file_version_null = MED_FILE_VERSION_INIT;
  med_idt           gid=0;

  if ( _MEDfichierNo(oid,&key) < 0 ) return _med_file_version_null;

  it = MedfileVersion.find(key);

  if (it != MedfileVersion.end() ) return (*it).second;

  /* On ouvre le group ou se trouvent les infos à la racine du fichier,
     puis au niveau de l'objet courant */
  if ((gid = _MEDdatagroupOuvrir(oid,MED_INFOS)) < 0) {
    if ((gid = _MEDdatagroupOuvrir(oid,&MED_INFOS[1])) < 0) {
      //  Ne stocke pas la clé du fichier dans le cache car la structure MED_INFOS n'a pas été
      //  trouvée. Elle pourrait l'être par un autre appel avec un autre oid (fichiers HDF contenants une structure MED)
      return _med_file_version_null;
    }
  } else {
    if ( _MEDattrEntierLire(gid,MED_NOM_MAJEUR ,&_fileversion.majeur ) < 0) return _med_file_version_null;
    if ( _MEDattrEntierLire(gid,MED_NOM_MINEUR ,&_fileversion.mineur ) < 0) return _med_file_version_null;
    if ( _MEDattrEntierLire(gid,MED_NOM_RELEASE,&_fileversion.release) < 0) return _med_file_version_null;
    if ( _MEDdatagroupFermer(gid) < 0) return _med_file_version_null;
  }

//   ISCRUTE(_fileversion.majeur  );
//   ISCRUTE(_fileversion.mineur  );
//   ISCRUTE(_fileversion.release );

  return (MedfileVersion[key]=_fileversion);

}

extern "C" med_err _MEDfileVersionSetCache(const med_idt oid,const valueType v) {

  med_err _ret=-1;

  map<keyType, valueType >::const_iterator it;
  fileNo                                   key=0;
  /* static med_file_version                  _fileversion           = MED_FILE_VERSION_INIT;*/

  if ( _MEDfichierNo(oid,&key) < 0 ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_FILE,"");
    ISCRUTE_int(oid);
    goto ERROR;
  }

  it = MedfileVersion.find(key);

  if ( it != MedfileVersion.end() )
    MedfileVersion[key]=v;
  else {
    MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILE,"");
    ISCRUTE_int(oid);
    goto ERROR;
  }

  _ret=0;
 ERROR:
  return _ret;
}
