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
   \brief \MEDmeshNodeCoordinateTrsfRdBrief
   \param fid \fid
   \param meshname \meshname
   \param numdt \numdt
   \param numit \numit
   \param coordinatetrsf \coordinatetrsf
   \retval med_err \error
   \details \MEDmeshNodeCoordinateTrsfRdDetails
   \remarks
   \li \MEDmeshNodeCoordinateTrsfRm1
   \li \MEDmeshNodeCoordinateTrsfRm2
   \li \MEDmeshNodeCoordinateTrsfRm3
*/

med_err MEDmeshNodeCoordinateTrsfRd(const med_idt               fid,
				    const char*  const          meshname,
				    const med_int               numdt,
				    const med_int               numit,
				    const med_float* const      coordinatetrsf)
{
  med_err         _ret         = -1;
  med_idt         _meshid      = 0,_datagroup1=0,_dataset_coo=0;
  med_int         _intmeshtype = 0;
  char            _meshpath   [MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]="";
  char            _dataset_cooname[MED_TAILLE_NOM_ENTITE+1+3+1]=MED_NOM_NOE"/"MED_NOM_COO;
  med_bool        _isasupportmesh=MED_FALSE,_isasoftlink=MED_FALSE;

  /* Il ne peut pas y  avoir de transformation géométrique à MED_NO_DT, MED_NO_IT */
  if ( (numdt == MED_NO_DT) && (numit == MED_NO_IT) ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,MED_ERR_MESH_MSG);
    SSCRUTE(_meshpath); ISCRUTE(numdt);ISCRUTE(numit);goto ERROR;
  }

  /* Maillage de calcul ou maillage support */
  if ((_meshid=_MEDmeshDatagroupOpen(fid,meshname,_meshpath,&_isasupportmesh)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(_meshpath); goto ERROR;
  }

  /* Lecture de l'attribut du type de maillage MED_NOM_TYP  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_TYP,&_intmeshtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_TYP);ISCRUTE(_intmeshtype);goto ERROR;
  }

  /* Accès (création éventuelle non attendue) du datagroup numdt,numit */
  if ( _isasupportmesh ) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,"");
      SSCRUTE(meshname);goto ERROR;
  } else
    if ( (_datagroup1 =_MEDmeshAssociatedGroupCr(fid, MED_MESH_GRP,
						 meshname, numdt, numit, MED_UNDEF_DT, MED_TRUE,
						 "." ) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAssociatedGroupCr");
      goto ERROR;
    }

  /* Erreur si un un dataset  MED_NODE/MED_COORDINATE existe */
  if ( _MEDisasoftlink(_datagroup1, _dataset_cooname,MED_TRUE, &_isasoftlink ) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_LINK,MED_NOM_COO);
    goto ERROR;
  }

  if (!_isasoftlink) {
    MED_ERR_(_ret,MED_ERR_NULL,MED_ERR_DATASET,MED_NOM_COO);
    goto ERROR;
  }

  if ( ( (med_mesh_type) _intmeshtype ) == MED_UNSTRUCTURED_MESH ) {

    if (  _MEDmeshAdvancedRd(fid,
			     meshname,
			     MED_COORDINATE_TRSF,
			     MED_NO_NAME,
			     MED_INTERNAL_UNDEF,
			     numdt,
			     numit,
			     MED_NODE,
			     MED_NONE,
			     MED_NO_CMODE,
			     MED_UNDEF_STMODE,
			     MED_NO_PROFILE,
			     MED_NO_INTERLACE,
			     MED_ALL_CONSTITUENT,
			     NULL,
			     (unsigned char * ) coordinatetrsf) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAdvancedRd");
      goto ERROR;

    }

  } else {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_MESHTYPE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_GTY);goto ERROR;
  }

  _ret = 0;

 ERROR:
  if (_dataset_coo>0)     if (_MEDdatagroupFermer(_dataset_coo) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_dataset_cooname);
    ISCRUTE_id(_dataset_coo);
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
