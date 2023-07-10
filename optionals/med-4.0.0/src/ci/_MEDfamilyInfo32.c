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

#include <stdlib.h>
#include <string.h>

void
_MEDfamilyInfo32(int dummy,...)
{
  med_err    _ret=-1,_err=-1;
  med_idt    _datagroup=0,_famid=0;
  char       _path   [MED_FAMILY_GRP_SIZE+MED_NAME_SIZE+
		     MED_TAILLE_FAS_ENTITE+MED_NAME_SIZE+1] = MED_FAMILY_GRP;
  med_int    _n          = 0;
  med_size   _tmpn       = 0;
  int        _pathreflen = 0;
  int        _num        = 0;
  int        _nfammai=0;
  med_int    _ngroup=0;
  med_filter _filter=MED_FILTER_INIT;


  MED_VARGS_DECL(const, med_idt           , , fid           );
  MED_VARGS_DECL(const, char * , const      , meshname      );
  MED_VARGS_DECL(const, med_int           , , famit         );
  MED_VARGS_DECL(, char *      , const      , familyname    );
  MED_VARGS_DECL(, med_int *   , const      , familynumber  );
  MED_VARGS_DECL(, char *      , const      , groupname     );
  MED_VARGS_DECL(, med_err *              , , fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid           );
  MED_VARGS_DEF(const, char * , const      , meshname      );
  MED_VARGS_DEF(const, med_int           , , famit         );
  MED_VARGS_DEF(, char *      , const      , familyname    );
  MED_VARGS_DEF(, med_int *   , const      , familynumber  );
  MED_VARGS_DEF(, char *      , const      , groupname     );
  MED_VARGS_DEF(, med_err *              , , fret          );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (_MEDcheckVersion30(fid) < 0) goto ERROR;

 _num        = famit-1;

  /*
   * On recupere le nom de la famille
   */

  /* Acces a la famille :
   * nfam = nfamnoe + 1 + nfammai
   * Repartition selon l'indice "num" dans le datagroup :
   *    - 0..nfammai - 1 : familles des mailles/faces/aretes
   *    - nfamai : famille 0
   *    - (nfamai + 1)..(nfammai+nfamnoe) : familles de noeuds
   */

  /* Comptage des familles de mailles/faces/aretes */
  strcat(_path,meshname);
  _pathreflen=strlen(_path);

  strncpy(&_path[_pathreflen],MED_FAS_ELEME,MED_TAILLE_FAS_ENTITE+1);
  if ( (_err = _MEDnObjects(fid,_path,&_tmpn)) < 0 )
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }

  _nfammai = (med_int ) _tmpn;

/*   SSCRUTE(_path); */
/*   ISCRUTE(_nfammai); */

  /* Pour la famille 0 */
  /* (0<=_num<_nfammai) : famille éléments */
  /* (_num == _nfammai) : famille 0 */
  /* (_num > _nfammai ) : famille de noeuds */
  if (_num == _nfammai) {
    strcpy(familyname,FAMILLE_ZERO);
    groupname[0]='\0';
    *familynumber=0;
    goto SORTIE;
  }


  /* C'est une _family de noeuds */
  if (_num > _nfammai) {
    strncpy(&_path[_pathreflen],MED_FAS_NOEUD,MED_TAILLE_FAS_ENTITE+1);
    _num = _num - _nfammai - 1;
  }

/*   SSCRUTE(_path); */

  /*
   * Si le Data Group de la famille n'existe pas => erreur
   */
  if ( _MEDobjectCrOrderGetName(fid, _path ,_num, familyname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(famit);
    H5Eprint1(stderr);
    goto ERROR;
  }
/*   SSCRUTE(familyname); */
  strcat(_path,familyname);
/*   SSCRUTE(_path); */

  if ((_famid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_FAMILY_MSG);
    SSCRUTE(_path);
    goto ERROR;
  }

  /*
   * L'attribut NUM
   */
  if ( _MEDattrEntierLire(_famid,MED_NOM_NUM,familynumber) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FAMILY_MSG);
      SSCRUTE(familyname);SSCRUTE(_path);
      SSCRUTE(MED_NOM_NUM);ISCRUTE(*familynumber);goto ERROR;
  }
/*   ISCRUTE(*familynumber); */

  /*
   * Le Data Group "GRO"
   */
  if ((_datagroup = _MEDdatagroupOuvrir(_famid,MED_NOM_GRO)) >= 0) {

    /*
     * L'attribut "NBR"
     */
    if ( _MEDattrEntierLire(_datagroup,MED_NOM_NBR,&_ngroup) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FAMILY_MSG);
      SSCRUTE(familyname);SSCRUTE(_path);SSCRUTE(MED_NOM_GRO);
      SSCRUTE(MED_NOM_NBR);
      goto ERROR;
    }
/*     ISCRUTE(_ngroup); */

    /*
     * Data Set des noms des groupes "NOM"
     */
    if ( MEDfilterEntityCr(fid, _ngroup, 1, 1, MED_ALL_CONSTITUENT,
			   MED_NO_INTERLACE,MED_UNDEF_PFLMODE,
			   MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
      goto ERROR;
    }

    if ( _MEDdatasetRd(_datagroup,MED_NOM_NOM,MED_INTERNAL_LNAME,&_filter,
		       (unsigned char * const) groupname) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,MED_NOM_NOM);
      SSCRUTE(_path);SSCRUTE(MED_NOM_GRO);SSCRUTE(groupname);
      goto ERROR;
    }
/*     SSCRUTE(groupname); */

    if ( MEDfilterClose(&_filter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_FAMILY_MSG);
      SSCRUTE(familyname);SSCRUTE(_path);SSCRUTE(MED_NOM_GRO);
      goto ERROR;
    }

  }


 SORTIE:
 _ret = 0;

 ERROR:

 if (_datagroup>0)     if (_MEDdatagroupFermer(_datagroup) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_NOM_GRO);
   ISCRUTE_id(_datagroup);
 }

 if (_famid>0)         if (_MEDdatagroupFermer(_famid) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,&_path[_pathreflen]);
   ISCRUTE_id(_famid);
 }

 va_end(params);
 *fret = _ret;
 return;
}


