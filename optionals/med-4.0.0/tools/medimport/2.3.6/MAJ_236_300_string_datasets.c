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
#include "med_outils.h"
#include <string.h>

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

#include "MAJ_236_300.h"

extern MEDC_EXPORT
med_err _MEDdatasetStringLire(med_idt pere,char *nom,char *val);

int MAJ_236_300_string_datasets(med_idt fid,
				const char * const absdatagrouppath,
				const char * const datasetname,
				int           isubstringsize,
				int           fsubstringsize,
				int           nsubstring) {

  med_err     _ret       = -1;
  char *      _itmpbuff  = NULL;
  char *      _ftmpbuff  = NULL;
  med_idt     _datagroup = 0;
  med_filter  _filter    = MED_FILTER_INIT;
  char        _pathi[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1+2*MED_TAILLE_NOM_ENTITE+1+3+1]="";
  int         _itmplen   = 0;
  med_internal_type _medinternaltype=MED_INTERNAL_UNDEF;

  strcpy(_pathi,absdatagrouppath);
  _itmplen=strlen(_pathi);


  /* Ouverture du datagroup */
  if ((_datagroup = _MEDdatagroupOuvrir(fid,_pathi)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_pathi);
    goto ERROR;
  }

  _pathi[_itmplen]='/';
  strcpy(&_pathi[_itmplen+1],datasetname);

/*   SSCRUTE(_pathi); */


  _itmpbuff = (char *) malloc(nsubstring*sizeof(char)*isubstringsize+1);

  if ( _MEDdatasetStringLire(_datagroup,(char*)datasetname,_itmpbuff) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,_pathi);
    goto ERROR;

  };
/*   SSCRUTE(_itmpbuff); */

  if ( _MEDselectAllEntitiesNoI(fid, nsubstring, 1, 1,
				MED_ALL_CONSTITUENT, &_filter) < 0) {
    MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
    MESSAGE("MED_NO_INTERLACE, MED_NO_PROFILE, MED_NO_FILTER");
    goto ERROR;
  };

  if ( isubstringsize != fsubstringsize ) {
    _ftmpbuff = (char *) malloc(nsubstring*sizeof(char)*fsubstringsize+1);
    _MED23v30stringConvert(_ftmpbuff,fsubstringsize,_itmpbuff,isubstringsize,nsubstring);
  } else
    _ftmpbuff  = _itmpbuff;


  switch(fsubstringsize)
    {
    case MED_NAME_SIZE:
      _medinternaltype=MED_INTERNAL_NAME;
      break;
    case MED_SNAME_SIZE:
      _medinternaltype=MED_INTERNAL_SNAME;
      break;
    case MED_LNAME_SIZE:
      _medinternaltype=MED_INTERNAL_LNAME;
      break;
    case MED_IDENT_SIZE:
      _medinternaltype=MED_INTERNAL_IDENT;
      break;
    default:
      MED_ERR_EXIT_IF(1,MED_ERR_RANGE,MED_ERR_PARAMETER,"fsubstringsize");

    }
/*   SSCRUTE(_ftmpbuff); */

/*   if ( H5Ldelete(_datagroup,datasetname,H5P_DEFAULT) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_DELETE,MED_ERR_LINK,datasetname); */
/*     H5Eprint1(stderr); */
/*     goto ERROR; */
/*   } */

  if ( _MEDdatasetWr(_datagroup,datasetname,_medinternaltype,&_filter,_ftmpbuff) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,datasetname);
    goto ERROR;
  }

  MEDfilterClose(&_filter);

  if (_datagroup>0) {
    if (_MEDdatagroupFermer(_datagroup) < 0) {
      _pathi[_itmplen]='\0';
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_pathi);
      goto ERROR;
    } else {
      _datagroup=0;
    }
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_NBR sur le dataset
   */

  if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				(const unsigned char * const) &nsubstring ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_NBR);
    SSCRUTE(_pathi);
    goto ERROR;
  }

  _ret=0;
 ERROR:

  if (_datagroup>0)
    if (_MEDdatagroupFermer(_datagroup) < 0) {
      _pathi[_itmplen]='\0';
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_pathi);
    }

  if (_itmpbuff ) {free(_itmpbuff);}

  if ( isubstringsize != fsubstringsize ) {
    if (_ftmpbuff ) { free(_ftmpbuff);};
  }

  return (int) _ret;
}
