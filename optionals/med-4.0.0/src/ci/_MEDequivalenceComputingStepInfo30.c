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

void _MEDequivalenceComputingStepInfo30(int dummy, ...) {


  med_err  _ret=-1,_err=-1;
  med_idt  _eqid=0,_datagroup1=0;
  int      _num;
  char     _cstppath[MED_EQUIVALENCE_GRP_SIZE+2*MED_NAME_SIZE+2+2*MED_MAX_PARA+1]=MED_EQUIVALENCE_GRP;
  char     _cstpname[2*MED_MAX_PARA+1]="";
  med_size _ncorrespondence=0;

  MED_VARGS_DECL(const, med_idt      , , fid                    );
  MED_VARGS_DECL(const, char * , const , meshname               );
  MED_VARGS_DECL(const, char * , const , equivname              );
  MED_VARGS_DECL(const, int          , , csit                   );
  MED_VARGS_DECL(, med_int *, const    , numdt                  );
  MED_VARGS_DECL(, med_int *, const    , numit                  );
  MED_VARGS_DECL(, med_int *, const    , nocstpncorrespondence  );
  MED_VARGS_DECL(, med_err *                  ,, fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid                    );
  MED_VARGS_DEF(const, char * , const , meshname               );
  MED_VARGS_DEF(const, char * , const , equivname              );
  MED_VARGS_DEF(const, int          , , csit                   );
  MED_VARGS_DEF(, med_int *, const    , numdt                  );
  MED_VARGS_DEF(, med_int *, const    , numit                  );
  MED_VARGS_DEF(, med_int *, const    , nocstpncorrespondence  );
  MED_VARGS_DEF(, med_err *                  ,, fret          );

  _num = csit -1;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

  strcat( _cstppath, meshname);
  strcat( _cstppath, "/");
  strcat( _cstppath, equivname);

  if ((_eqid = _MEDdatagroupOuvrir(fid,_cstppath)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_cstppath);
    ISCRUTE_id(_eqid);goto ERROR;
  }

  strcat( _cstppath, "/");
  /*
   * On recupere le nom de l'~tape de calcul
   */
  if ( _MEDobjectGetName(fid, _cstppath ,_num, &_cstppath[strlen(_cstppath)]) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_cstppath);ISCRUTE_int(csit);
    goto ERROR;
  }

  if ( (_datagroup1 = _MEDdatagroupOuvrir(fid,_cstppath)) < 0 ) {
    MED_ERR_(_ret,MED_ERR_DOESNTEXIST,MED_ERR_COMPUTINGSTEP,_cstppath);
    SSCRUTE(_cstppath);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_NDT pour lecture */
  if ( _MEDattrEntierLire(_datagroup1,MED_NOM_NDT,numdt) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_EQUIVALENCE_MSG);
    SSCRUTE(equivname);SSCRUTE(_cstppath);SSCRUTE(MED_NOM_NDT);
    ISCRUTE(*numdt);goto ERROR;
  }


  /*Cree ou ouvre l'attribut MED_NOM_PDT pour lecture */
/*   if ( _MEDattrFloatLire(_datagroup1,MED_NOM_PDT,dt) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG); */
/*     SSCRUTE(equivname);SSCRUTE(_cstppath);SSCRUTE(MED_NOM_PDT); */
/*     RSCRUTE(*dt);goto ERROR; */
/*   } */

  /*Cree ou ouvre l'attribut MED_NOM_NOR pour lecture */
  if ( _MEDattrEntierLire(_datagroup1,MED_NOM_NOR,numit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_EQUIVALENCE_MSG);
    SSCRUTE(equivname);SSCRUTE(_cstppath);SSCRUTE(MED_NOM_NOR);
    ISCRUTE(*numit); goto ERROR;
  }

  /*
   * Nombre de tableau d'~quivalence
   */
  if ((_err=_MEDnObjects(_datagroup1,".",&_ncorrespondence)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,equivname);
      goto ERROR;
    }

  *nocstpncorrespondence = (med_int) _ncorrespondence;

  _ret = 0;
 ERROR:

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_cstppath);
    ISCRUTE_id(_datagroup1);
  }

  if (_eqid>0)     if (_MEDdatagroupFermer(_eqid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,meshname);
    ISCRUTE_id(_eqid);
  }

  va_end(params);
  *fret = _ret;
  return;
}
