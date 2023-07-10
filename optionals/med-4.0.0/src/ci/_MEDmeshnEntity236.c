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
#include "med_config.h"
#include "med_outils.h"

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

#include "med_versioned.h"

#include <string.h>
#include <stdlib.h>

void
_MEDmeshnEntity236(int dummy, ...)
{

   med_int          _ret=-1,_n=0,_tmpn=0;
   med_size         _nn =0;
   med_data_type    _meddatatype;
   med_int          _nfaces=0;
   char             _meshpath[MED_MESH_GRP_SIZE+MED_TAILLE_NOM+1]=MED_MESH_GRP;
   med_idt          _meshid=0,_datagroup1=0;
   char             _datagroupname1[MED_TAILLE_NOM_ENTITE+1]="";
   med_err          _err =0;
   med_bool         _datagroupexist=MED_FALSE;
   med_bool         _isasoftlink   =MED_FALSE;
   int              _cmodeit=0, _ncmode=1;
   med_connectivite _cmode[2] = { MED_NO_CMODE, MED_NO_CMODE };
 
  MED_VARGS_DECL(const, med_idt                , , fid           );
  MED_VARGS_DECL(const, char * , const           , meshname      );
  MED_VARGS_DECL(const, med_int                , , numdt         );
  MED_VARGS_DECL(const, med_int                , , numit         );
  MED_VARGS_DECL(const, med_entity_type        , , entitytype    );
  MED_VARGS_DECL(const, med_geometry_type      , , geotype       );
  MED_VARGS_DECL(const, med_data_type          , , meddatatype   );
  MED_VARGS_DECL(const, med_connectivity_mode  , , cmode         );
  MED_VARGS_DECL(const, med_storage_mode       , , storagemode   );
  MED_VARGS_DECL(, char     *, const       , profilename         );
  MED_VARGS_DECL(, med_int  *, const       , profilesize         );
  MED_VARGS_DECL(, med_bool *, const       , changement          );
  MED_VARGS_DECL(, med_bool *, const       , transformation      );
  MED_VARGS_DECL(, med_int  *             ,, fret                );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt                , , fid           );
  MED_VARGS_DEF(const, char * , const           , meshname      );
  MED_VARGS_DEF(const, med_int                , , numdt         );
  MED_VARGS_DEF(const, med_int                , , numit         );
  MED_VARGS_DEF(const, med_entity_type        , , entitytype    );
  MED_VARGS_DEF(const, med_geometry_type      , , geotype       );
  MED_VARGS_DEF(const, med_data_type          , , meddatatype   );
  MED_VARGS_DEF(const, med_connectivity_mode  , , cmode         );
  MED_VARGS_DEF(const, med_storage_mode       , , storagemode   );
  MED_VARGS_DEF(, char     *, const       , profilename         );
  MED_VARGS_DEF(, med_int  *, const       , profilesize         );
  MED_VARGS_DEF(, med_bool *, const       , changement          );
  MED_VARGS_DEF(, med_bool *, const       , transformation      );
  MED_VARGS_DEF(, med_int  *             ,, fret                );

  _meddatatype=meddatatype;
   
 
  if ( (numdt != MED_NO_DT) || (numit != MED_NO_IT) ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_COMPUTINGSTEP,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numdt);ISCRUTE(numit);
    goto ERROR;
  }

  _cmode[0]= (med_connectivite) cmode;

  if ( entitytype == MED_UNDEF_ENTITY_TYPE ) goto SORTIE;

  /*
   * Gestion entitytype == MED_ALL_ENTITY_TYPE
   */
  if ( entitytype == MED_ALL_ENTITY_TYPE ) {

    strcat(_meshpath,meshname);
    if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_meshpath);
      ISCRUTE_id(_meshid);goto ERROR;
    }

    if( _MEDdatagroupExist(_meshid,MED_NOM_NOE,&_datagroupexist,&_isasoftlink) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDdatagroupExist");
      SSCRUTE(MED_NOM_NOE);goto ERROR;
    }
    if ( _datagroupexist ) _nn++;

    if( _MEDdatagroupExist(_meshid,MED_NOM_MAI,&_datagroupexist,&_isasoftlink) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDdatagroupExist");
      SSCRUTE(MED_NOM_MAI);goto ERROR;
    }
    if ( _datagroupexist ) _nn++;

    if( _MEDdatagroupExist(_meshid,MED_NOM_FAC,&_datagroupexist,&_isasoftlink) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDdatagroupExist");
      SSCRUTE(MED_NOM_FAC);goto ERROR;
    }
    if ( _datagroupexist ) _nn++;

    if( _MEDdatagroupExist(_meshid,MED_NOM_ARE,&_datagroupexist,&_isasoftlink) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDdatagroupExist");
      SSCRUTE(MED_NOM_ARE);goto ERROR;
    }
    if ( _datagroupexist ) _nn++;

/* En 2.3.6 les familles, les équivalences ... sont stockées dans le groupe <meshname>*/
/*     _err=_MEDnObjects(_meshid,".",&_nn); */
/*     if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) { */
/*       MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_meshid); */
/*       goto ERROR; */
/*     } */
    _n = _nn;
    goto SORTIE;
  }

  if ( geotype == MED_GEO_ALL ) {

      strcat(_meshpath,meshname);
      if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) {
	MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_meshpath);
	ISCRUTE_id(_meshid);goto ERROR;
      }

      if (_MEDgetEntityTypeName(_datagroupname1,entitytype) < 0) {
	MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
	ISCRUTE_int(entitytype);SSCRUTE(meshname);goto ERROR;
      }

      if ((_datagroup1 = _MEDdatagroupOuvrir(_meshid,_datagroupname1)) < 0) {
	_n=0;_datagroup1=0;
	goto SORTIE;
      }

      _err=_MEDnObjects(_datagroup1,".",&_nn);
      if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
	MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_datagroupname1);
	goto ERROR;
      }
/*       SSCRUTE(_datagroupname1);ISCRUTE_long(_nn); */
      if ( ( entitytype == MED_NODE ) && (_nn > 0) ) _nn=1;
/*       SSCRUTE(_datagroupname1);ISCRUTE_long(_nn); */
      _n = _nn;
      goto SORTIE;

  }


  switch( meddatatype )
    {
    case MED_GLOBAL_NUMBER:
      /* Cette notion n'existe pas en 2.3.6 */
      _n=0;
      break;
      
    case MED_NAME:
    case MED_NUMBER:
    case MED_FAMILY_NUMBER:

      switch (entitytype) {
      case MED_NODE:
	/*Pour connaître le nombre d'entités des attributs optionnels,
	  on se base sur le tableau de coordonnées */
	/* 	_meddatatype = MED_COORDINATE; */
	break;
      case MED_CELL:
      case MED_DESCENDING_FACE:
      case MED_DESCENDING_EDGE:
	/*Pour connaître le nombre d'entités des attributs optionnels,
	  on se base sur le tableau de connectivité */
	/* 	_meddatatype = MED_CONNECTIVITY; */

	/*
	 * Positionne un mode de connectivité _cmode si le meddatatype demandé
	 * est autre chose que des coordonnées ou des connectivités et le cmode non
	 * positionné.
	 * Cette Information est necessaire pour construire le nom du datatset.
	 */
	if ( _cmode[0] == MED_NO_CMODE ) {
	  _ncmode =2;_cmode[0]=MED_NOD;_cmode[1]=MED_DESC;
	}
	break;
      default:
	MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
	ISCRUTE_int(entitytype);goto ERROR;
      }
      goto CALL_NENT;

    case MED_COORDINATE: /*Par extension au CONNECTIVITY mais ne devrait pas être utilisé.*/
    case MED_CONNECTIVITY:

      
      if ( geotype == MED_POLYGON2 ) {_n=0; break;}
      if ( geotype == MED_POLYGON ) {
	  if ( MEDpolygoneInfo( fid,(char *)meshname, (med_entite_maillage) entitytype, (med_connectivite) _cmode[0],
				&_n) < 0) {
/* 	    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDpolygoneInfo");SSCRUTE(meshname); */
/* 	    ISCRUTE(meddatatype);ISCRUTE(entitytype);ISCRUTE(geotype);goto ERROR; */
	    _n=0;
	  }
	  break;
      }

      if ( geotype == MED_POLYHEDRON ) {
	if (    MEDpolyedreInfo(fid,(char *) meshname, (med_connectivite) _cmode[0]
				,&_nfaces,&_n) < 0) {
/* 	  MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDpolyedreInfo");SSCRUTE(meshname); */
/* 	  ISCRUTE(meddatatype);ISCRUTE(entitytype);ISCRUTE(geotype);goto ERROR; */
	  _n=0;
	}
	break;
      }
      goto CALL_NENT;

      
    case MED_INDEX_FACE:
      
      if ( geotype == MED_POLYHEDRON ) {

	_meddatatype = MED_CONNECTIVITY;
	_n=1 ;

	goto CALL_NENT;

/* 	if ( (_n = MEDnEntMaa(fid,meshname,(med_table) MED_CONNECTIVITY, (med_entite_maillage) MED_CELL, */
/* 			      (med_geometrie_element) MED_POLYHEDRON, (med_connectivite) MED_NOD)) < 0) { */
/* 	  MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDnEntMaa");SSCRUTE(meshname); */
/* 	  ISCRUTE(meddatatype);ISCRUTE(entitytype);ISCRUTE(geotype);goto ERROR; */
/* 	} */

	_n+=1;
	break;
      } else {
	MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
	ISCRUTE_int(geotype);
	goto ERROR;
      }
      
    case MED_INDEX_NODE:
      
      if ( geotype == MED_POLYHEDRON ) {
	if ( MEDpolyedreInfo(fid,(char *) meshname, (med_connectivite) _cmode[0],&_nfaces, &_n) < 0) {
/* 	  MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDpolyedreInfo");SSCRUTE(meshname); */
/* 	  ISCRUTE(meddatatype);ISCRUTE(entitytype);ISCRUTE(geotype);goto ERROR; */
	  _nfaces=0;
	}
	_n=_nfaces;
	break;
      } else
	if ( (geotype == MED_POLYGON) || (geotype == MED_POLYGON2) ) {
	  _meddatatype = MED_CONNECTIVITY;
	  _n = 1;
	} else {
	  MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
	  ISCRUTE_int(geotype);goto ERROR;
	}
      
    case MED_COORDINATE_AXIS1:
    case MED_COORDINATE_AXIS2:
    case MED_COORDINATE_AXIS3:

    CALL_NENT:

      for (_cmodeit=0; _cmodeit < _ncmode; ++_cmodeit)
	if ( (_tmpn =  MEDnEntMaa( fid, (char *) meshname,
				   (med_table)             _meddatatype,
				   (med_entite_maillage)   entitytype,
				   (med_geometrie_element) geotype,
				   (med_connectivite)      _cmode[_cmodeit]) ) > 0 )
	  break;

      if ( _tmpn < 0 ) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDnEntMaa");SSCRUTE(meshname);
	ISCRUTE_int(_meddatatype);ISCRUTE_int(entitytype);ISCRUTE_int(geotype);goto ERROR;
      }

      _n+=_tmpn;
      break;

    default:
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_MEDDATATYPE,MED_ERR_VALUE_MSG);
      ISCRUTE_int(meddatatype);goto ERROR;

    }

 SORTIE:

 *transformation = MED_FALSE;
 /*TODO : 3.0.1 : Tester la présence d'autres datasets
  que les coord/conn/index pour positionner chgt TRUE uniquement
 dans ce cas de figure.*/
 *changement     = MED_TRUE;
 *profilesize    = 0;
 profilename[0]  = '\0';

  _ret = _n;

 ERROR:
  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname1);
    ISCRUTE_id(_datagroup1);
  }

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }

  *fret = _ret;
  va_end(params);
  return;
}
