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

void _MEDnFamily30(int dummy, ...)
{


  med_int       _ret=-1;
  char          _path[MED_FAMILY_GRP_SIZE+MED_NAME_SIZE+MED_TAILLE_FAS_ENTITE+MED_NAME_SIZE+1]=MED_FAMILY_GRP;
  int           _pathreflen=0;
  med_int       _n=0;
  med_size      _tmpn=0;


  MED_VARGS_DECL(const, med_idt      , , fid      );
  MED_VARGS_DECL(const, char * , const , meshname );
  MED_VARGS_DECL(, med_int *          ,, fret     );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid      );
  MED_VARGS_DEF(const, char * , const , meshname );
  MED_VARGS_DEF(, med_int *          ,, fret     );

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();

  strcat(_path,meshname);
  _pathreflen=strlen(_path);

/*   SSCRUTE(_path); */
  if ( (_ret=_MEDnObjects(fid,_path,&_tmpn)) < 0)
    if ( _ret == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }
  _n = (med_int ) _tmpn;
/*   ISCRUTE(_n); */

  /* Comptage des familles de mailles/faces/aretes */
  strcat(_path,MED_FAS_ELEME);
/*   SSCRUTE(_path); */
  if ( (_ret=_MEDnObjects(fid,_path,&_tmpn)) < 0 ) {
    if ( _ret == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }
  } else  {
/*     ISCRUTE(_ret); */
    _n --;  _n += (med_int ) _tmpn;
  }
/*   ISCRUTE(_ret); */
/*   ISCRUTE(_n); */

  /* Comptage des familles de noeuds */
  strncpy(&_path[_pathreflen],MED_FAS_NOEUD,MED_TAILLE_FAS_ENTITE+1);
/*   SSCRUTE(_path); */
  if ( (_ret=_MEDnObjects(fid,_path,&_tmpn)) < 0 ) {
    if ( _ret == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }
  } else  {
    _n --;  _n += (med_int ) _tmpn;
  }
/*   ISCRUTE(_n); */

  _ret =  _n;
 ERROR:
  va_end(params);
  *fret = _ret;

  return;
}

