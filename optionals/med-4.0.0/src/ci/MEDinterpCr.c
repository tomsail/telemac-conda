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
  \brief \MEDinterpCrBrief
  \param fid           \fid
  \param interpname    \interpname
  \param geotype       \geotype
  \param cellnode      \cellnode
  \param nvariable  \nvariable
  \param maxdegree     \maxdegree
  \param nmaxcoef      \nmaxcoef

  \return \error
  \details \MEDinterpCrDetails
  \see MEDinterpBaseFunctionWr
  \remarks
  \li \MEDinterpCrcellnodeCm1
  \li \MEDinterpCrcellnodeCm2
 */
med_err
MEDinterpCr(const med_idt                 fid,
	    const char*             const interpname,
	    const med_geometry_type       geotype,
	    const med_bool                cellnode,
	    const med_int                 nvariable,
	    const med_int                 maxdegree,
	    const med_int                 nmaxcoef
	    )
{
  med_access_mode   _MED_ACCESS_MODE;
  med_idt           _root=0,_interpid=0;
  med_int           _ret=-1;
  char              _path[MED_INTERPOLATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_INTERPOLATION_GRP;
  med_int           _cellnodes = cellnode;
  med_int           _geotype = geotype;
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
   * Si le DataGroup /INTERP/ n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOpen(fid,_path)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,_path)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_path);
     goto ERROR;
    }

  NOFINALBLANK(interpname,ERROR);

  /*
   * Si le DataGroup /INTERP/<interpname> n'existe pas, on le cree
   */
  if ((_interpid = _MEDdatagroupOpen(_root,interpname)) < 0)
    if ((_interpid = _MEDdatagroupCreer(_root,interpname)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,interpname);
      SSCRUTE(_path);goto ERROR;
    }

  strcat(_path,interpname);

  /*
   * Creation/Ecriture de l'attribut MED_NOM_GEO
   */
  if ( _MEDattributeIntWr(_interpid,MED_NOM_GEO,&_geotype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_GEO);ISCRUTE(_geotype);
    goto ERROR;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_NBB
   */
/*   if ( _MEDattributeIntWr(_interpid,MED_NOM_NBB,&nbasisfunc) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path); */
/*     SSCRUTE(MED_NOM_NBB);ISCRUTE(nbasisfunc); */
/*     goto ERROR; */
/*   } */

  /*
   * Creation/Ecriture de l'attribut MED_NOM_NBV
   */
  if ( _MEDattributeIntWr(_interpid,MED_NOM_NBV,&nvariable) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NBV);ISCRUTE(nvariable);
    goto ERROR;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_MDG
   */
  if ( _MEDattributeIntWr(_interpid,MED_NOM_MDG,&maxdegree) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_MDG);ISCRUTE(maxdegree);
    goto ERROR;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_MCF
   */
  if ( _MEDattributeIntWr(_interpid,MED_NOM_MCF,&nmaxcoef) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_MCF);ISCRUTE(nmaxcoef);
    goto ERROR;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_FCN
   */
  if ( _MEDattributeIntWr(_interpid,MED_NOM_FCN,&_cellnodes) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_FCN);ISCRUTE(_cellnodes);
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  if (_interpid>0)     if (_MEDdatagroupFermer(_interpid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_interpid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_INTERPOLATION_GRP);
    ISCRUTE_id(_root);
  }

  return _ret;

}
