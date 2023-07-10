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
#include "MEDfieldChecked.hxx"
#include "med_config.h"
#include "med_outils.h"
#include <iostream>

using namespace std;

static MED_FIELD_CHECKED & MedFieldChecked=MED_FIELD_CHECKED::Instance();

MED_FIELD_CHECKED& MED_FIELD_CHECKED::Instance() {
  static MED_FIELD_CHECKED obj;
  return obj;
}

MED_FIELD_CHECKED::~MED_FIELD_CHECKED() {};

extern "C" med_bool _MEDfieldChecked(const med_idt oid,const char * const fieldname) {

  med_err _ret=-1;
  map<keyType, valueType >::const_iterator it;
  fileNo  key1=0;
  keyType key;

 if ( _MEDfichierNo(oid,&key1) < 0 ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_FILE,"");
    ISCRUTE_int(oid);
    return MED_FALSE;
 }

 key.first =key1;
 key.second=fieldname;;

 it = MedFieldChecked.find(key);

 if (it != MedFieldChecked.end() ) return (*it).second;

 return MED_FALSE;

}

extern "C" med_err _MEDfieldCheckedSetCache(const med_idt      oid,
					    const char * const fieldname,
					    const med_bool     ischecked) {

  med_err _ret=-1;

  map<keyType, valueType >::const_iterator it;
  fileNo  key1=0;
  keyType key;

  if ( _MEDfichierNo(oid,&key1) < 0 ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_FILE,"");
    ISCRUTE_int(oid);
    goto ERROR;
  }
  key.first =key1;
  key.second=fieldname;

  MedFieldChecked[key]=ischecked;
//   std::cout << "key.first : " << key.first << " key.second : " << key.second << std::endl;
  _ret=0;
 ERROR:
  return _ret;
}
