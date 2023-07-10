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

/**\ingroup MEDinterp
  \brief \MEDinterpBaseFunctionCoefSizeBrief
  \param fid             \fid
  \param interpname      \interpname
  \param basisfuncit     \basisfuncit
  \retval \ncoef
  \return \error
  \details \MEDinterpBaseFunctionCoefSizeDetails
  \see MEDinterpBaseFunctionRd
 */

med_int
MEDinterpBaseFunctionCoefSize( const med_idt          fid,
			       const char*      const interpname,
			       const med_int          basisfuncit)

{
  med_idt           _interpid=0,_bid=0;
  med_int           _ret=-1;
  char              _path[MED_INTERPOLATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_INTERPOLATION_GRP;
  char              _basisfuncname[MED_MAX_PARA+1]="";
  med_int           _ncoef=0;
  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

  if (basisfuncit < 1 ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_PARAMETER,"");
    ISCRUTE(basisfuncit);
    goto ERROR;
  }

  NOFINALBLANK(interpname,ERROR);
  strcat(_path,interpname);
  /*
   * Ouverture du DataGroup /INTERP/<interpname>
   */
  if ((_interpid = _MEDdatagroupOpen(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,interpname);
    SSCRUTE(_path);goto ERROR;
  }

  sprintf(_basisfuncname,"%0*li",MED_MAX_PARA,(long ) basisfuncit);
  /*
   * Ouverture du DataGroup /INTERP/<interpname>/<basisfuncit>
   */
  if ((_bid = _MEDdatagroupOpen(_interpid,_basisfuncname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);SSCRUTE(_basisfuncname);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_MCF
   */
  if ( _MEDattrEntierLire(_bid,MED_NOM_MCF,&_ncoef) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);SSCRUTE(_basisfuncname);
    SSCRUTE(MED_NOM_MCF);ISCRUTE(_ncoef);
    goto ERROR;
  }

  _ret=_ncoef;

 ERROR:

  if (_interpid>0)            if (_MEDdatagroupFermer(_interpid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_interpid);
  }

  if (_bid>0)            if (_MEDdatagroupFermer(_bid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_basisfuncname);
    ISCRUTE_id(_bid);SSCRUTE(_path);
  }

  return _ret;
}
