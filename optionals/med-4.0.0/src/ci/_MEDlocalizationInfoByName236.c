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

void
_MEDlocalizationInfoByName236(int dummy, ...) {


  med_idt _lzid=0;
  med_err _ret=-1;
  char    _path[MED_LOCALIZATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_LOCALIZATION_GRP;
  med_int _intgeotype = -1;


  MED_VARGS_DECL(const, med_idt                   , , fid                 );
  MED_VARGS_DECL(const, char        * , const       , localizationname    );
  MED_VARGS_DECL(, med_geometry_type *, const , geotype                   );
  MED_VARGS_DECL(, med_int           *, const , spacedimension            );
  MED_VARGS_DECL(, med_int           *, const , nipoint                );
  MED_VARGS_DECL(, char        *, const       , geointerpname             );
  MED_VARGS_DECL(, char        *, const       , sectionmeshname           );
  MED_VARGS_DECL(, med_int     *, const       , nsectionmeshcell          );
  MED_VARGS_DECL(, med_geometry_type *, const , sectiongeotype            );
  MED_VARGS_DECL(, med_err *                 ,, fret                      );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt                   , , fid                 );
  MED_VARGS_DEF(const, char        * , const       , localizationname    );
  MED_VARGS_DEF(, med_geometry_type *, const , geotype                   );
  MED_VARGS_DEF(, med_int           *, const , spacedimension            );
  MED_VARGS_DEF(, med_int           *, const , nipoint                );
  MED_VARGS_DEF(, char        *, const       , geointerpname             );
  MED_VARGS_DEF(, char        *, const       , sectionmeshname           );
  MED_VARGS_DEF(, med_int     *, const       , nsectionmeshcell          );
  MED_VARGS_DEF(, med_geometry_type *, const , sectiongeotype            );
  MED_VARGS_DEF(, med_err *                 ,, fret                      );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();


  /*
   * ouverture du groupe /GAUSS/"nom"
   */
  strcat(_path,localizationname);
  if ((_lzid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(_path);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_NBR
   */
  if (_MEDattrEntierLire(_lzid,MED_NOM_NBR,nipoint) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_NBR);ISCRUTE(*nipoint);
    goto ERROR;
  }


  /*
   * On lit <typgeo> sous forme d'attribut
   */
  /* sizeof(enum) tjrs = sizeof(int) en C, or
     sur machines 64 bits par défaut med_int==long,
     du coup sur  machines 64 bits _MEDattributeIntWr utilise
     le type hdf NATIVE_LONG, ce qui pose un problème qd on passe
     un enum.
  */
  if (_MEDattrEntierLire(_lzid,MED_NOM_GEO,&_intgeotype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE(_intgeotype);
    goto ERROR;
  };
  *geotype = (med_geometry_type) _intgeotype;

  *spacedimension = (_intgeotype/100);

  sectionmeshname[0]='\0';
  geointerpname[0]='\0';
  *nsectionmeshcell=0;
  *sectiongeotype=MED_UNDEF_GEOTYPE;

  _ret = 0;
 ERROR:

  if ( _lzid > 0 ) if ( _MEDdatagroupFermer(_lzid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_LOCALIZATION_GRP);
    ISCRUTE_id(_lzid);
  }

  va_end(params);
  *fret = _ret;
  return;
  }
