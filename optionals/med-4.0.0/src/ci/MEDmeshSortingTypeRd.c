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
  \brief \MEDmeshSortingTypeRdBrief
  \param fid \fid
  \param meshname \meshname
  \param sortingtype \sortingtype
  \retval med_err \error
  \details \MEDmeshSortingTypeRdDetails
 */

med_err
MEDmeshSortingTypeRd(const med_idt               fid,
		     const char*  const          meshname,
		     med_sorting_type * const    sortingtype )

{
  med_idt               _ret=-1;
  med_idt               _meshid=0;
  char                  _meshpath         [MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  med_int               _intsortingtype=0;


  /*
   * Si le DataGroup MED_MESH_GRP n'existe pas => erreur
   */

  strcat(_meshpath,meshname);
  if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_meshpath); goto ERROR;
  }

 /*
  * Lecture du type de stockage des séquences de calcul du maillage
  */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_SRT,&_intsortingtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_SRT);ISCRUTE(_intsortingtype);goto ERROR;
  }

  *sortingtype = (med_sorting_type) (_intsortingtype);

  _ret = 0;

 ERROR:

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }


  return _ret;

}
