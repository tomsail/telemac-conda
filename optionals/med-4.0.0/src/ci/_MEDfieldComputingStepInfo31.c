/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2013  EDF R&D, CEA/DEN
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

void
_MEDfieldComputingStepInfo31(int dummy,...)
{


  med_err _ret=-1;
  med_idt _gid=0,_datagroup1=0;
  int     _num;
  char    _datagroupname1[2*MED_MAX_PARA+1]="";
  char    _path          [(MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1)+2*MED_MAX_PARA+1]=MED_FIELD_GRP;
  char    _cstpname[2*MED_MAX_PARA+1]="";
  med_int _meshnumdt=MED_NO_DT;
  med_int _meshnumit=MED_NO_IT;


  MED_VARGS_DECL(const, med_idt       , , fid       );
  MED_VARGS_DECL(const, char * , const  , fieldname );
  MED_VARGS_DECL(const, int           , , csit      );
  MED_VARGS_DECL(, med_int *, const     , numdt     );
  MED_VARGS_DECL(, med_int *, const     , numit     );
  MED_VARGS_DECL(, med_float *, const   , dt        );
  MED_VARGS_DECL(, med_err *           ,, fret      );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt       , , fid       );
  MED_VARGS_DEF(const, char * , const  , fieldname );
  MED_VARGS_DEF(const, int           , , csit      );
  MED_VARGS_DEF(, med_int *, const     , numdt     );
  MED_VARGS_DEF(, med_int *, const     , numit     );
  MED_VARGS_DEF(, med_float *, const   , dt        );
  MED_VARGS_DEF(, med_err *           ,, fret      );

  _num=csit-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On construit le nom du datagroup
   */
  strcat(_path,fieldname);

  /*
   * On recupere le nom de la séquence de calcul
   * dans l'ordre de création et non par ordre alphabétique
   * pour éviter à HDF de relancer un tri à chaque appel de
   * cet cette MEDfieldComputingStepInfo.
   */
  if ( _MEDobjectCrOrderGetName(fid, _path ,_num, _cstpname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(_num);
    goto ERROR;
  }

  strcat(_path,"/");
  strcat(_path,_cstpname);

  if ((_gid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }


  /*
   * Lecture des attributs
   */

  if (_MEDattrEntierLire(_gid,MED_NOM_NDT,(med_int*) numdt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NDT);
    SSCRUTE(_path);ISCRUTE(*numdt);goto ERROR;
  }

  if (_MEDattrFloatLire(_gid,MED_NOM_PDT,(med_float*) dt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_PDT);
    SSCRUTE(_path);RSCRUTE(*dt);goto ERROR;
  }

  if (_MEDattrEntierLire(_gid,MED_NOM_NOR,(med_int*) numit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NOR);
    SSCRUTE(_path);ISCRUTE(*numit);goto ERROR;
  }

  if (_MEDattrEntierLire(_gid,MED_NOM_RDT,(med_int*) & _meshnumdt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_RDT);
    SSCRUTE(_path);ISCRUTE(_meshnumdt);goto ERROR;
  }

  if ( _meshnumdt!= MED_NO_DT) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_ATTRIBUTE,MED_NOM_RDT);
    SSCRUTE(_path);ISCRUTE(_meshnumdt);goto ERROR;
  }

  if (_MEDattrEntierLire(_gid,MED_NOM_ROR,(med_int*) & _meshnumit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_ROR);
    SSCRUTE(_path);ISCRUTE(_meshnumit);goto ERROR;
  }

  if ( _meshnumit!= MED_NO_IT) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_ATTRIBUTE,MED_NOM_ROR);
    SSCRUTE(_path);ISCRUTE(_meshnumit);goto ERROR;
  }

  _ret = 0;

 ERROR:


  if (_gid>0)            if (_MEDdatagroupFermer(_gid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_gid);
  }

  va_end(params);
  *fret = _ret;
  return;
}
