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

void _MEDnFamilyGroup32(int dummy, ... )
{


  med_int   _ret=-1,_err=-1;
  med_idt   _datagroup=0,_famid=0;
  char      _path   [MED_FAMILY_GRP_SIZE+MED_NAME_SIZE+
		     MED_TAILLE_FAS_ENTITE+MED_NAME_SIZE+1] = MED_FAMILY_GRP;
  char      _family [MED_NAME_SIZE+1] = "";
  med_int   _n          = 0;
  med_size  _tmpn       = 0;
  int       _pathreflen = 0;
  int       _num        ;
  int       _nfamnoe=0,_nfammai=0;


  MED_VARGS_DECL(const, med_idt      , , fid      );
  MED_VARGS_DECL(const, char * , const , meshname );
  MED_VARGS_DECL(const, int          , , famit    );
  MED_VARGS_DECL(, med_int *          ,, fret     );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid      );
  MED_VARGS_DEF(const, char * , const , meshname );
  MED_VARGS_DEF(const, int          , , famit    );
  MED_VARGS_DEF(, med_int *          ,, fret     );

  _num        = famit-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();

  strcat(_path,meshname);
  _pathreflen=strlen(_path);

  /* Acces a la _family :
   * nfam = _nfamnoe + 1 + _nfammai
   * Repartition selon l'indice "num" dans le datagroup :
   *    - 0.._nfammai - 1 : _familys des mailles/faces/aretes
   *    - nfamai : _family 0
   *    - (nfamai + 1)..(_nfammai+_nfamnoe) : familles de noeuds
   */

  /* Comptage des familles de mailles/faces/aretes */
  strncpy(&_path[_pathreflen],MED_FAS_ELEME,MED_TAILLE_FAS_ENTITE+1);
  if ( (_err = _MEDnObjects(fid,_path,&_tmpn)) < 0 )
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }
  _nfammai = (med_int ) _tmpn;

  /* Pour la famille 0 */
  if (_num == _nfammai)  {_n=0;goto SORTIE;}

  /* C'est une _family de noeuds */
  if (_num > _nfammai) {
    strncpy(&_path[_pathreflen],MED_FAS_NOEUD,MED_TAILLE_FAS_ENTITE+1);
    _num = _num - _nfammai - 1;
  }

  /*
   * Si le Data Group de la _family n'existe pas => erreur
   */
  if ( _MEDobjectCrOrderGetName(fid, _path ,_num, _family) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(famit);
    goto ERROR;
  }
  strcat(_path,_family);

  if ((_famid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_FAMILY_MSG);
    SSCRUTE(_family);SSCRUTE(_path);
    goto ERROR;
  }

  if ((_datagroup = _MEDdatagroupOuvrir(_famid,MED_NOM_GRO)) < 0)
    _n = 0;
  else {
    if ( _MEDattrEntierLire(_datagroup,MED_NOM_NBR,&_n) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FAMILY_MSG);
      SSCRUTE(_family);SSCRUTE(_path);SSCRUTE(MED_NOM_GRO);
      SSCRUTE(MED_NOM_NBR);ISCRUTE(_n);goto ERROR;
    }
  }

 SORTIE:
  _ret=_n;

 ERROR:

 if (_datagroup>0)     if (_MEDdatagroupFermer(_datagroup) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_NOM_GRO);
   SSCRUTE(_path);ISCRUTE_id(_datagroup);
 }

 if (_famid>0)     if (_MEDdatagroupFermer(_famid) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_FAMILY_GRP);
   ISCRUTE_id(_famid);
 }

  va_end(params);
  *fret = _ret;

  return;
}
