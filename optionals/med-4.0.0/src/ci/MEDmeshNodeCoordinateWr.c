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
#include <stdlib.h>

/**\ingroup MEDmesh
  \brief \MEDmeshNodeCoordinateWrBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param dt \dt
  \param switchmode \switchmode
  \param nentity \nentity
  \param coordinate \coordinate
  \retval med_err \error
  \details \MEDmeshNodeCoordinateWrDetails
  \remarks
  \MEDmeshNodeCoordinateWrgridRem1
  \see MEDmeshNodeCoordinateWithProfileWr
  \see MEDmeshNodeCoordinateAdvancedWr
  \see MEDmeshGridIndexCoordinateWr
  \see MEDmeshGridStructWr
 */

med_err MEDmeshNodeCoordinateWr(const med_idt               fid,
				const char*  const          meshname,
				const med_int               numdt,
				const med_int               numit,
				const med_float             dt,
				const med_switch_mode       switchmode,
				const med_int               nentity,
				const med_float* const      coordinate)
{
  med_idt         _meshid      = 0;
  med_idt         _datagroup1=0,_datagroup2=0,_datagroup3=0,_datagroup_trsf=0;
  med_idt         _dataset_trsf = 0;
  med_err         _ret         = -1;
  med_grid_type   _gridtype    = MED_UNDEF_GRID_TYPE;
  med_int         _intgridtype = 0;
  med_int         _intmeshtype = 0;
  med_int         _meshdim     = 0;
  char            _meshpath   [MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]="";
  char            _geotypename[MED_TAILLE_NOM_ENTITE+1]="";
  char            _datagroup_trsfname[MED_TAILLE_NOM_ENTITE+MED_TAILLE_COOTRF+1]=MED_NOM_NOE MED_COOTRF;
  int             _i=0;
  med_bool        _isasupportmesh=MED_FALSE,_isasoftlink=MED_FALSE;
  med_int         _0 = 0;
  med_int         _medintgeotype = MED_NO_GEOTYPE;

if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  /* Ouverture du maillage de calcul ou du maillage support */
  if ((_meshid=_MEDmeshDatagroupOpen(fid,meshname,_meshpath,&_isasupportmesh)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(_meshpath); goto ERROR;
  }

  /* Lecture du type de maillage (attribut MED_NOM_TYP)  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_TYP,&_intmeshtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_TYP);ISCRUTE(_intmeshtype);goto ERROR;
  }

  /*Sortir en erreur si maillage support et numdt,numit != MED_NO_PDT,MED_NO_IT*/
  /* Accès (création éventuelle) du datagroup numdt,numit */

  if ( _isasupportmesh ) {
    if (( numdt != MED_NO_DT) && (numit != MED_NO_IT) ) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numdt);ISCRUTE(numit);goto ERROR;
    }
  } else
    if ( (_datagroup1 =_MEDmeshAssociatedGroupCr(fid, MED_MESH_GRP,
						 meshname, numdt, numit, dt,MED_FALSE,
						 "." ) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAssociatedGroupCr");
      goto ERROR;
    }

  /* Création de la structure de connectivité assoiciée aux grilles MED_CURVILLINEAR_GRID*/
  if ( ( (med_mesh_type) _intmeshtype ) != MED_UNSTRUCTURED_MESH ) {

    /* Lecture de l'attribut MED_NOM_GTY  */
    if (_MEDattrEntierLire(_meshid,MED_NOM_GTY,&_intgridtype) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(MED_NOM_GTY);ISCRUTE_int(_gridtype);goto ERROR;
    }
    _gridtype= (med_grid_type) _intgridtype;

    if ( _gridtype == MED_CURVILINEAR_GRID ) {

      if ((_datagroup2 = _MEDdatagroupOuvrir(_datagroup1,MED_NOM_MAI)) < 0) {

	if ((_datagroup2 = _MEDdatagroupCreer(_datagroup1,MED_NOM_MAI)) < 0) {
	  MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_NOM_MAI);
	  SSCRUTE(meshname);ISCRUTE(numdt);ISCRUTE(numit);
	  goto ERROR;
	}

	if ( _MEDattributeIntWr(_datagroup2,MED_NOM_CGT,&_0) < 0) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(MED_NOM_CGT);
	  goto ERROR;
	}

	/* Lecture de l'attribut MED_NOM_DIM  */
	if (_MEDattrEntierLire(_meshid,MED_NOM_DIM,&_meshdim) < 0) {
	  MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(meshname);SSCRUTE(MED_NOM_DIM);ISCRUTE(_meshdim);goto ERROR;
	}

	switch ( _meshdim )  {
	case 1 :
	  strcpy(_geotypename,MED_NOM_SE2);
	  _medintgeotype = MED_SEG2;
	  break;
	case 2 :
	  strcpy(_geotypename,MED_NOM_QU4);
	  _medintgeotype = MED_QUAD4;
	  break;
	case 3 : strcpy(_geotypename,MED_NOM_HE8);
	  break;
	  _medintgeotype = MED_HEXA8;
	case 0 : strcpy(_geotypename,MED_NOM_PO1);
	  _medintgeotype = MED_POINT1;
	  break;
	default :
	  MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_PARAMETER,"");
	  SSCRUTE(meshname);ISCRUTE(numdt);ISCRUTE(numit);
	  goto ERROR;
	}

	if ((_datagroup3 = _MEDdatagroupCreer(_datagroup2,_geotypename)) < 0) {
	  MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_geotypename);
	  SSCRUTE(meshname);ISCRUTE(numdt);ISCRUTE(numit);
	  goto ERROR;
	}

	if ( _MEDattributeStringWr(_datagroup3,MED_NOM_PFL,MED_NAME_SIZE,MED_NO_PROFILE_INTERNAL) < 0) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
	  goto ERROR;
	}

	if ( _MEDattributeIntWr(_datagroup3,MED_NOM_CGT,&_0) < 0) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(MED_NOM_CGT);
	  goto ERROR;
	}

	if ( _MEDattributeIntWr(_datagroup3,MED_NOM_CGS,&_0) < 0) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(MED_NOM_CGS);
	  goto ERROR;
	}

	if (_MEDattributeIntWr(_datagroup3,MED_NOM_GEO,&_medintgeotype) < 0) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_GEO);
	  ISCRUTE(_medintgeotype);
	  goto ERROR;
	}

      }
    } else {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_GRIDTYPE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(MED_NOM_GTY);ISCRUTE_int(_gridtype);goto ERROR;

    }

  }


  /*Réinitialisation du lien MED_TRSF_NOM/MED_COORDINATE_TRSF s'il existe */
  /*Si un dataset de transformation existe une erreur est générée par
    _MEDsoftlinkDel */
  if ( !_isasupportmesh)
    if ((_datagroup_trsf = _MEDdatagroupOuvrir(_datagroup1,_datagroup_trsfname)) > 0) {

      if ( _MEDisasoftlink(_datagroup_trsf, MED_NOM_TRF,MED_FALSE, &_isasoftlink ) < 0 ) {
	MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_LINK,MED_NOM_COO);
	goto ERROR;
      }
      if (_isasoftlink ) {
	if (  _MEDsoftlinkDel(_datagroup_trsf,
			      MED_NOM_TRF,
			      MED_TRUE) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDsoftlinkDel");
	  goto ERROR;
	}
      } else {
	if ( (_dataset_trsf = _MEDdatasetOuvrir(_datagroup_trsf,MED_NOM_TRF)) >= 0 ) {
	  _MEDdatasetFermer(_dataset_trsf);
	  MED_ERR_(_ret,MED_ERR_NULL,MED_ERR_DATASET,MED_NOM_TRF);
	  goto ERROR;
	}

      }
    }

  /*Ecriture des coordonnées*/
  if (  _MEDmeshAdvancedWr(fid,
			    meshname,
			    MED_COORDINATE,
			    MED_NO_NAME,
			    MED_INTERNAL_UNDEF,
			    numdt,
			    numit,
			    dt,
			    MED_NODE,
			    MED_NONE,
			    MED_NO_CMODE,
			    MED_UNDEF_STMODE,
			    MED_NO_PROFILE,
			    switchmode,
			    MED_ALL_CONSTITUENT,
			    NULL,
			    nentity,
			   coordinate) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedWr");
    goto ERROR;

  }


  _ret = 0;

 ERROR:

  if (_datagroup_trsf>0)     if (_MEDdatagroupFermer(_datagroup_trsf) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroup_trsfname);
    ISCRUTE_id(_datagroup_trsf);
  }

  if (_datagroup3>0)     if (_MEDdatagroupFermer(_datagroup3) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_geotypename);
    ISCRUTE_id(_datagroup3);
  }

  if (_datagroup2>0)     if (_MEDdatagroupFermer(_datagroup2) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_NOM_MAI);
    ISCRUTE_id(_datagroup2);
  }

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_datagroup1);ISCRUTE(numdt);ISCRUTE(numit);
  }

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,meshname);
    ISCRUTE_id(_meshid);
  }

  return _ret;
}
