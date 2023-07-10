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
#include "med_outils.h"
#include <string.h>

med_err
_MEDmeshInfoByName(const med_idt fid, const char * const root,const char * const meshname,
		   med_int *const spacedim, med_int * const meshdim,  med_mesh_type * const meshtype,
		   char * const description,  char * const dtunit,med_sorting_type * const sortingtype,
		   med_int * const nstep,  med_axis_type * const axistype,  char * const axisname,
		   char * const axisunit)
{
  med_err  _ret=-1,_err=-1;
  med_idt  _root=0,_meshid=0;
/*   char     _meshpath[MED_FIELD_GRP_SIZE+MED_TAILLE_NOM+1]=MED_FIELD_GRP; */
/*   char     _meshpath [MED_MESH_GRP_SIZE+MED_TAILLE_NOM+1]=MED_MESH_GRP; */
/*   char     _linkpath [MED_TAILLE_LIENS+MED_TAILLE_NOM+1]=MED_LIENS; */
  med_size _nstep=0;
  med_int  _intmeshtype=0;
  med_int  _intaxistype=0;
  med_int  _intsortingtype=0;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * Si le DataGroup root n'existe pas => erreur
   */
  if ((_root = _MEDdatagroupOuvrir(fid,root)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,root);
    ISCRUTE_id(_root);goto ERROR;
  }


  if ((_meshid = _MEDdatagroupOuvrir(_root,meshname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,meshname);
    SSCRUTE(root);goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_ESP */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_ESP,spacedim) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_ESP);
    SSCRUTE(root);SSCRUTE(meshname);goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_DIM */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_DIM,meshdim) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_DIM);
    SSCRUTE(root);SSCRUTE(meshname);goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_TYP */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_TYP,&_intmeshtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_TYP); goto ERROR;
  }
  *meshtype = (med_mesh_type) (_intmeshtype);


   /* Lecture de l'attribut MED_NOM_DES */
  if (_MEDattrStringLire(_meshid,MED_NOM_DES,MED_COMMENT_SIZE, description) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(root);SSCRUTE(meshname);SSCRUTE(MED_NOM_DES);SSCRUTE(description);goto ERROR;
  }

   /* Lecture de l'attribut MED_NOM_UNT */
  if (_MEDattrStringLire(_meshid,MED_NOM_UNT,MED_SNAME_SIZE, dtunit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(root);SSCRUTE(meshname);SSCRUTE(MED_NOM_UNT);
    SSCRUTE(dtunit);goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_SRT */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_SRT,&_intsortingtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(root);SSCRUTE(meshname);SSCRUTE(MED_NOM_SRT);
    ISCRUTE(_intsortingtype);goto ERROR;
  }
  *sortingtype = (med_sorting_type) (_intsortingtype);

  /* Lecture de l'attribut MED_NOM_REP */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_REP,&_intaxistype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(root);SSCRUTE(meshname);SSCRUTE(MED_NOM_REP);
    ISCRUTE(_intaxistype);goto ERROR;
  }
  *axistype = (med_mesh_type) (_intaxistype);


  /* Lecture de l'attribut MED_NOM_NOM */
  if (_MEDattrStringLire(_meshid,MED_NOM_NOM,*spacedim*MED_SNAME_SIZE, axisname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(root);SSCRUTE(meshname);SSCRUTE(MED_NOM_NOM);
    SSCRUTE(axisname);goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_UNI */
  if (_MEDattrStringLire(_meshid,MED_NOM_UNI,*spacedim*MED_SNAME_SIZE, axisunit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(root);SSCRUTE(meshname);SSCRUTE(MED_NOM_UNI);
    SSCRUTE(axisunit);goto ERROR;
  }

  /* Nombre d'étapes de calcul*/
  if ((_err=_MEDnObjects(_meshid,".",&_nstep)) <0) 
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,meshname);
      goto ERROR;
    }

  *nstep = (med_int) _nstep;

  _ret = 0;

 ERROR:


  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,meshname);
    ISCRUTE_id(_meshid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,root);
    ISCRUTE_id(_root);
  }

  return _ret;
}
