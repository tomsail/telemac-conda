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

/**\ingroup MEDmesh
  \brief \MEDmeshAttributeWrBrief
  \param fid \fid
  \param meshname \meshname
  \param isolatednodes \isolatednodes
  \param verticesnodes \verticesnodes
  \param cellmaxnodes \cellmaxnodes
  \retval med_err \error
  \details \MEDmeshAttributeWrDetails
*/

#include <med.h>
#include <med_config.h>
#include <med_outils.h>
#include <string.h>

med_err
MEDmeshAttributeWr(const med_idt fid, 
		   const char * const meshname,
		   const med_int isolatednodes,
		   const med_int verticesnodes,
		   const med_int cellmaxnodes)
{
  med_access_mode _MED_ACCESS_MODE;
  med_err _ret=-1;
  med_idt _meshid=0;
  char    _path [MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;


  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
 if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  if ( _MED_ACCESS_MODE == MED_ACC_RDONLY) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    ISCRUTE_int(_MED_ACCESS_MODE);
    goto ERROR;
  }

  /*
   * Si le maillage n'existe pas => erreur
   */
  strcat(_path,meshname);
  if ((_meshid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_meshid);goto ERROR;
  }


  /*
   * Creation de l'attribut "Nombre de Noeuds Isoles"
   */
  if ( _MEDattributeIntWr(_meshid,MED_NOM_NNI,&isolatednodes) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_NNI);ISCRUTE(isolatednodes);goto ERROR;
  }

  /*
   * Creation de l'attribut "Nombre de Noeuds Sommets"
   */
  if ( _MEDattributeIntWr(_meshid,MED_NOM_NNS,&verticesnodes) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_NNS);ISCRUTE(verticesnodes);goto ERROR;
  }


  /*
   * Creation de l'attribut "Nombre de Noeuds Max par maille"
   */
  if ( _MEDattributeIntWr(_meshid,MED_NOM_NNM,&cellmaxnodes) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_NNM);ISCRUTE(cellmaxnodes);goto ERROR;
  }


  _ret = 0;
 ERROR:

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_meshid);
  }

  return _ret;
}

