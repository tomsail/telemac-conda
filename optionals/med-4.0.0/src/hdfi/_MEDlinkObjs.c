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
#include <string.h>

med_err _MEDlinkobjs(med_idt id,const char *lname, const H5L_info_t *linfo, visitordatas *data) {

  med_err  _ret=-1;
  med_idt  _gid=0,_gid1=0,_gid2=0;
  med_idt  _attid_src=0,_attid_dst=0,_attspace=0;
  med_idt  _atttype=0;
  char     _attname[MED_TAILLE_NOM_ENTITE+1];
  char     _attvalue[MED_COMMENT_SIZE+1]=""; /*Plus grande taille d'attributs possible*/
  char    *_attvalueptr=_attvalue;
  med_size _attsize;
  med_int  _attfalse=0;
  char *   _tmppath=0;
  hsize_t  _it;
  H5O_info_t oinfo;

/*   hid_t   _ocp_plist_id = H5Pcreate( H5P_OBJECT_COPY ); */
/*   hid_t   _lcp_plist_id = H5Pcreate( H5P_LINK_CREATE ); */
/*   _ret = H5Pset_copy_object( ocp_plist_id, H5O_COPY_EXPAND_SOFT_LINK_FLAG ); */
/*   H5Pset_create_intermediate_group(_lcp_plist_id, -1); */
/*   H5Pset_copy_object( _ocp_plist_id, H5O_COPY_SHALLOW_HIERARCHY_FLAG); */

#ifdef _DEBUG_
  SSCRUTE(lname);
#endif

  if (!strcmp(lname,".")) return 0;

  switch ( (*linfo).type ) {

  case H5L_TYPE_SOFT:
    /* Rem : On affecte un H5G (deprecated) a un H5O pour pouvoir tester
     de façon uniforme dans le switch */
    oinfo.type=H5G_LINK;
    break;
  case H5L_TYPE_HARD:
    /* Le pb de cette routine c'est qu'elle renvoie seulement :
       H5O_TYPE_GROUP, H5O_TYPE_DATASET, H5O_TYPE_NAMED_DATATYPE, H5O_TYPE_NTYPES
       sur un lien hard
       Sur un lien soft H5O_TYPE_UNKNOWN
    */
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

/*Copie le groupe avec ses attributs (optionnel) et ses datasets (obligatoire) ! (même si SHALLOW COPY )*/
/*     if ( H5Ocopy(id,oname,data->gid2,oname,_ocp_plist_id,_lcp_plist_id) < 0 ) { */
/*       MED_ERR_(_ret,MED_ERR_COPY,MED_ERR_DATAGROUP,oname); */
/*       goto ERROR; */
/*     } */

    if ((_gid = H5Gcreate(data->gid2,lname,0)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,lname);
      goto ERROR;
    }

    if (_gid>0)     if (H5Gclose(_gid) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,lname);
      ISCRUTE_id(_gid);
    }
#ifdef _DEBUG_
    ISCRUTE_size(oinfo.num_attrs);
#endif

    if ((_gid1 = _MEDdatagroupOuvrir(id,lname)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,lname);
      ISCRUTE_id(_gid1);
      goto ERROR;
    }

    if ((_gid2 = _MEDdatagroupOuvrir(data->gid2,lname)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,lname);
      ISCRUTE_id(_gid2);
      goto ERROR;
    }

      for ( _it=0; _it< oinfo.num_attrs;++_it ) {

	if ( H5Aget_name_by_idx( id, lname, H5_INDEX_NAME, H5_ITER_NATIVE,
				 _it, _attname, MED_TAILLE_NOM_ENTITE+1, H5P_DEFAULT ) <0) {
	  MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_ATTRIBUTE,"");
	  ISCRUTE_size(_it);SSCRUTE(_attname);
	  goto ERROR;

	}
#ifdef _DEBUG_
	SSCRUTE(_attname);
#endif
	if ( (_attid_src = H5Aopen( _gid1, _attname, H5P_DEFAULT ) ) < 0) {
	  MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_ATTRIBUTE,_attname);
	  ISCRUTE_id(_gid1);goto ERROR;
	}

	_atttype  = H5Aget_type(_attid_src);
	_attspace = H5Aget_space(_attid_src);
	_attsize  = H5Aget_storage_size(_attid_src);

	if (_attsize > MED_COMMENT_SIZE+1 ) {
	  MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_ATTRIBUTE,_attname);
	  ISCRUTE_size(_attsize);goto ERROR;
	}

	if ( strcmp(_attname,MED_NOM_CGT) && strcmp(_attname,MED_NOM_CGS) ) {
	  if ( H5Aread(_attid_src, _atttype, _attvalue ) < 0 ) {
	    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_attname);
	    ISCRUTE_id(_gid1); H5Eprint1(stderr);
	    goto ERROR;
	  }
#ifdef _DEBUG_
	  SSCRUTE(_attvalue);
#endif
	} else _attvalueptr=(char*)&_attfalse;


	if ( (_attid_dst = H5Acreate2( _gid2, _attname,
				      _atttype,_attspace,
				      H5P_DEFAULT, H5P_DEFAULT ) ) < 0) {
	  MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_ATTRIBUTE,_attname);
	  ISCRUTE_id(_gid2);
	  H5Eprint1(stderr);
	  goto ERROR;
	}
#ifdef _DEBUG_
	SSCRUTE(_attvalueptr);
#endif
	if ( H5Awrite(_attid_dst, _atttype, _attvalueptr ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_attname);
	  ISCRUTE_id(_gid2); H5Eprint1(stderr);
	  goto ERROR;
	}

	if (_attspace>0)     if (H5Sclose(_attspace) < 0) {
	  MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASPACE,"");
	  ISCRUTE_id(_atttype);
	}

	if (_atttype>0)     if (H5Tclose(_atttype) < 0) {
	  MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATATYPE,"");
	  ISCRUTE_id(_atttype);
	}

	if (_attid_src>0)     if (H5Aclose(_attid_src) < 0) {
	  MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_ATTRIBUTE,_attname);
	  ISCRUTE_id(_attid_src);
	}

	if (_attid_dst>0)     if (H5Aclose(_attid_dst) < 0) {
	  MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_ATTRIBUTE,_attname);
	  ISCRUTE_id(_attid_dst);
	}

	_attid_src=0;_attid_dst=0;_attspace=0,_atttype=0;
	_attvalueptr=_attvalue;

	/* Ne fonctionne pas avec les atributs*/
/* 	if ( H5Ocopy(_gid1,_attname,data->gid2,_attname,_ocp_plist_id,_lcp_plist_id) < 0 ) { */
/* 	  MED_ERR_(_ret,MED_ERR_COPY,MED_ERR_ATTRIBUTE,lname); */
/* 	  H5Eprint1(stderr); */
/*  	  goto ERROR; */
/* 	} */

      }

      if (_gid2>0)     if (_MEDdatagroupFermer(_gid2) < 0) {
	MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,lname);
	ISCRUTE_id(_gid2);
      }

      if (_gid1>0)     if (_MEDdatagroupFermer(_gid1) < 0) {
	MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,lname);
	ISCRUTE_id(_gid1);
      }


    break;

  case H5G_DATASET:
  case H5G_LINK:

    /*TODO : Améliorer pour éviter un malloc */
    _tmppath=(char *)malloc(strlen(data->srcpath)+strlen(lname)+1);
    strcpy(_tmppath,data->srcpath);
    strcat(_tmppath,lname);
    if ( H5Lcreate_soft( _tmppath, data->gid2, lname, H5P_DEFAULT, H5P_DEFAULT ) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_LINK,_tmppath);SSCRUTE(data->dstpath);SSCRUTE(lname);
      H5Eprint1(stderr);
      goto ERROR;
    }
    free (_tmppath);

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

