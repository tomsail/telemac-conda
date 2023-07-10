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
  \brief \MEDinterpInfoByNameBrief
  \param fid           \fid
  \param interpname    \interpname
  \param geotype       \geotype
  \param cellnode      \cellnode
  \param nbasisfunc \nbasisfunc
  \param nvariable  \nvariable
  \param maxdegree     \maxdegree
  \param nmaxcoef      \nmaxcoef

  \return \error
  \details \MEDinterpInfoByNameDetails
  \see     MEDinterpInfo
 */
med_err
MEDinterpInfoByName(const med_idt                   fid,
		    const char*               const interpname,
		           med_geometry_type* const geotype,
		           med_bool*          const cellnode,
		           med_int*           const nbasisfunc,
		           med_int*           const nvariable,
		           med_int*           const maxdegree,
		           med_int*           const nmaxcoef
	    )
{
  med_idt           _interpid=0;
  med_int           _err=-1;
  med_err           _ret=-1;
  char              _path[MED_INTERPOLATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_INTERPOLATION_GRP;
  med_int           _cellnode = 0;
  med_size          _tmpn=0;
  med_int           _medintgeotype=MED_UNDEF_GEOTYPE;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();


  NOFINALBLANK(interpname,ERROR);
  strcat(_path,interpname);
  /*
   * Ouverture du  DataGroup /INTERP/<interpname>
   */
  if ((_interpid = _MEDdatagroupOpen(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,interpname);
    SSCRUTE(_path);goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_GEO
   */
  if ( _MEDattrEntierLire(_interpid,MED_NOM_GEO,&_medintgeotype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_GEO);ISCRUTE(_medintgeotype);
    goto ERROR;
  }
  *geotype=(med_geometry_type) _medintgeotype;

  /*
   * Lecture de l'attribut MED_NOM_NBV
   */
  if ( _MEDattrEntierLire(_interpid,MED_NOM_NBV,nvariable) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NBV);ISCRUTE(*nvariable);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_MDG
   */
  if ( _MEDattrEntierLire(_interpid,MED_NOM_MDG,maxdegree) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_MDG);ISCRUTE(*maxdegree);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_MCF
   */
  if ( _MEDattrEntierLire(_interpid,MED_NOM_MCF,nmaxcoef) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_MCF);ISCRUTE(*nmaxcoef);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_FCN
   */
  if ( _MEDattrEntierLire(_interpid,MED_NOM_FCN,&_cellnode) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_FCN);ISCRUTE(_cellnode);
    goto ERROR;
  }
  if (_cellnode) *cellnode=MED_TRUE; else *cellnode=MED_FALSE;

  /*
   * Lecture de l'attribut MED_NOM_NBB
   */
/*   if ( _MEDattrEntierLire(_interpid,MED_NOM_NBB,nbasisfunc) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path); */
/*     SSCRUTE(MED_NOM_NBB);ISCRUTE(*nbasisfunc); */
/*     goto ERROR; */
/*   } */

  /*
   *  Lecture du nombre de fonctions de base
   */
  if ((_err=_MEDnObjects(fid,_path,&_tmpn)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_INTERP,_path);
      goto ERROR;
    }
  *nbasisfunc = (med_int) _tmpn;

  _ret = 0;

 ERROR:

  if (_interpid>0)     if (_MEDdatagroupFermer(_interpid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_interpid);
  }
  return _ret;

}
