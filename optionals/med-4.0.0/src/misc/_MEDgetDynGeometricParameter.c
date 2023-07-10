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

med_err _MEDgetDynGeometricParameter(const med_idt fid,
				     const med_entity_type       entitytype,
				     const med_geometry_type     geotype,
				     med_int * const             entdim,
				     med_int * const             nnodes,
				     med_int * const             ncells)
{

  med_err _ret=-1;
  med_idt _elemid=0;
  char    _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  char    _supportmeshname[MED_NAME_SIZE+1]="";
  med_bool           _chgt=MED_FALSE,_trsf=MED_FALSE;
  med_geometry_type  sgeotype;
  med_int            _medintsgeotype = MED_NONE;
/*   char *  _modelname[MED_NAME_SIZE+1]=""; */



  if( ( entitytype != MED_STRUCT_ELEMENT)) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(entitytype); goto ERROR;
  }

  if( ( geotype <= MED_STRUCT_GEO_INTERNAL) ||
      ( geotype >= MED_STRUCT_GEO_SUP_INTERNAL) ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
    ISCRUTE_int(geotype); goto ERROR;
  }

  if ( MEDstructElementName(fid,geotype,&_path[strlen(_path)]) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDstructElementName");
    SSCRUTE(_path);goto ERROR;
  }

  /*
   * Si le DataGroup /STRUCT/<modelname> n'existe pas => erreur
   */
  if ((_elemid = _MEDdatagroupOpen(fid,_path)) < 0)  {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_DIM (dimension de l'élément)
   */
  if ( _MEDattrEntierLire(_elemid,MED_NOM_DIM,entdim) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_DIM);ISCRUTE(*entdim);
    goto ERROR;
  }

  /*
   * Lecture  de l'attribut MED_NOM_NOM (nom du maillage support)
   */
  /* Chercher plutôt ds le maillage support et supprimer les attributs NBM et NBN */
  if ( _MEDattrStringLire(_elemid,MED_NOM_NOM,MED_NAME_SIZE,_supportmeshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NOM);SSCRUTE(_supportmeshname);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_GEO (type géométrique des mailles support)
   */
  if ( _MEDattrEntierLire(_elemid,MED_NOM_GEO,&_medintsgeotype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_GEO);ISCRUTE(_medintsgeotype);
    goto ERROR;
  }
  sgeotype=_medintsgeotype;

  /*
   * Lecture du nombre de noeuds support
   */
  if (strlen(_supportmeshname)) {
    if ( (*nnodes = MEDmeshnEntity(fid,_supportmeshname,MED_NO_DT,MED_NO_IT,
				   MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,
				   &_chgt,&_trsf) )  <= 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      ISCRUTE(*nnodes);goto ERROR;
    }
  } else {
    *nnodes=1;
  }

  /*
   * Lecture du nombre de mailles support
   */

  if (strlen(_supportmeshname)) {
    if ( (*ncells = MEDmeshnEntity(fid,_supportmeshname,MED_NO_DT,MED_NO_IT,
				   MED_CELL,sgeotype,MED_CONNECTIVITY,MED_NODAL,
				   &_chgt,&_trsf) )  < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      ISCRUTE(*ncells);goto ERROR;
    }
  } else {
    *ncells=0;
  }

  _ret = 0;
 ERROR:
  return _ret;
}
