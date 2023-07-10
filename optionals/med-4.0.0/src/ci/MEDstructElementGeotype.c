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


/**\ingroup MEDstructElement
  \brief \MEDstructElementGeotypeBrief
  \param fid \fid
  \param modelname \modelname
  \retval mgeotype \mgeotype
  \return \error
  \details \MEDstructElementGeotypeDetails
  \see  MEDstructElementName
 */

med_geometry_type
MEDstructElementGeotype(const med_idt                 fid,
			const char *            const modelname) {

  med_idt           _elemid=0;
  char              _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  med_geometry_type _ret=-1;
  med_int           _medintstructelementtype=MED_NONE;

#ifdef _DEBUG_
  SSCRUTE(modelname);
#endif

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
  strcat(_path,modelname);

  /*
   * Si le DataGroup /STRUCT/<modelname> n'existe pas => erreur
   */
  if ((_elemid = _MEDdatagroupOpen(fid,_path)) < 0)  {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }
  /*
   * Lecture de l'attribut MED_NOM_NEO (numéro de type géométrique associé à un élément de structure)
   */
  if ( _MEDattrEntierLire(_elemid,MED_NOM_NEO,&_medintstructelementtype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NEO);ISCRUTE(_medintstructelementtype);
    goto ERROR;
  }

  _ret = _medintstructelementtype;

 ERROR:

  if (_elemid>0)            if (_MEDdatagroupFermer(_elemid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,modelname);
    ISCRUTE_id(_elemid);
  }

  return _ret;
}

