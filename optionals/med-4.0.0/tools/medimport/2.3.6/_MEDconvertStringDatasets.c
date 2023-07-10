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


#include <med_config.h>
#include <med.h>
#include <med_outils.h>
#include <hdf5.h>
#include <string.h>

#include "MAJ_236_300.h"

/* #include <2.3.6/med23v30.h> */
/* #include <2.3.6/med23v30_proto.h> */
/* #include "2.3.6/med23v30_misc.h" */

#define MED_TAILLE_LNOM 80
#define MAX_LEN_PATH 255

med_err _MEDconvertStringDatasets(med_idt id, const char *lname, const H5L_info_t *linfo, visitordatas *data) {

  med_err  _ret=-1,_err=-1;
  med_idt  _gid=0;
  H5O_info_t oinfo;
  char     _tmpbuff[MAX_LEN_PATH+1]="";
  int      _tmpbuffsize=0;
  med_int  _nbratt=0;

#ifdef _DEBUG_
  SSCRUTE(lname);
#endif

  if (!strcmp(lname,".")) return 0;

  switch ( (*linfo).type ) {

  case H5L_TYPE_SOFT:
    oinfo.type=(H5O_type_t) H5G_LINK;
    break;
  case H5L_TYPE_HARD:
    if ( H5Oget_info_by_name( id, lname, &oinfo, H5P_DEFAULT ) <0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"H5Oget_info_by_name");
      SSCRUTE(lname);
    }
    break;
  case H5L_TYPE_EXTERNAL:
  case H5L_TYPE_ERROR:
  default:
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_HDFTYPE,lname);
    ISCRUTE_int((*linfo).type);
    goto ERROR;
    break;
 }

  switch ( oinfo.type ) {

  case H5G_GROUP:
  case H5G_LINK:
    break;

  case H5G_DATASET:

    strcat(_tmpbuff,lname);
    _tmpbuffsize=strlen(_tmpbuff);

    /*
      Ce traitement est spécifique aux datatsets présents dans les familles
    */
/*     SSCRUTE(_tmpbuff); */
/*     SSCRUTE(&_tmpbuff[_tmpbuffsize-3]); */
    if ( strcmp(&_tmpbuff[_tmpbuffsize-3],MED_NOM_NOM) ) break;

    _tmpbuff[_tmpbuffsize-4]='\0';
/*     SSCRUTE(_tmpbuff); */

    /*
     * Lecture de l'attribut MED_NOM_NBR sur le dataset
     */
    if ( _MEDattributeNumRdByName(id,_tmpbuff,MED_NOM_NBR,MED_INTERNAL_INT,
				  ( unsigned char * const) &_nbratt ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NBR);
      goto ERROR;
    }

/*     ISCRUTE(_nbratt); */
    _err=MAJ_236_300_string_datasets( data->gid2, _tmpbuff, MED_NOM_NOM, MED_TAILLE_LNOM,
				      MED_LNAME_SIZE,_nbratt);

    MED_ERR_EXIT_IF( _err < 0, MED_ERR_CALL, MED_ERR_API, "MAJ_236_300_string_datasets");

    _tmpbuff[_tmpbuffsize-4]='/';
    _err = H5Adelete_by_name( data->gid2, _tmpbuff, MED_NOM_NBR, H5P_DEFAULT  );

    MED_ERR_EXIT_IF( _err < 0, MED_ERR_DELETE, MED_ERR_ATTRIBUTE, "MED_NOM_NBR");


  break;

  case H5G_TYPE:
  default:
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_HDFTYPE,lname);
    goto ERROR;
  }
  _ret = 0;

 ERROR:
  return _ret;
}

