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


#include "med_config.h"
#include "med_outils.h"
#include "med_versioned.h"
#include <string.h>


#include "MAJ_236_300.h"

int MAJ_236_300_entites(med_idt fid,
			char * const _pathi,
			char * const _pathf,
			const char * const meshname,
			const med_entity_type enttype)
{
  med_err           _fret =-1,_ret=-1,ret=-1;
  int dummy=0;
  med_idt           _datagroupi=0,_datagroupf=0;
  char              _enttypename[MED_TAILLE_NOM_ENTITE+1]="";
  char              _geotypename[MED_TAILLE_NOM_ENTITE+1]="";
  med_geometry_type _geotype    = MED_UNDEF_GEOMETRY_TYPE;
  med_int           _intgeotype = MED_UNDEF_GEOMETRY_TYPE;
  med_int           _ngeotype  = 0;
  int               _itgeotype = 0;
  int               _itmplen=0,_isavlen=0;
  int               _ftmplen=0,_fsavlen=0;
  int               _tmplen = 0;
  med_int           _1 = 1;
  med_bool          _chgt=MED_FALSE, _trsf=MED_FALSE;
  char                   _profilename[MED_NAME_SIZE+1]="";
  med_int                _profilesize=0;

    /*
     * Remarque : Cette procédure modifie le contenue des paramètres _pathi et pathf
     */

    /* read how many geotype element there is in the mesh for each entitytype */
  _MEDmeshnEntity236(dummy,fid, meshname, (med_int) MED_NO_DT, (med_int) MED_NO_IT, enttype, MED_GEO_ALL,
		       MED_UNDEF_DATATYPE, MED_NO_CMODE,MED_UNDEF_INTERLACE,
		       _profilename, &_profilesize,&_chgt, &_trsf,&_ngeotype);
    if ( _ngeotype < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      SSCRUTE(meshname);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
      goto ERROR;
    }

/*     ISCRUTE(_ngeotype); */
    if (_ngeotype == 0) goto SORTIE;

    if (_MEDgetEntityTypeName(_enttypename,enttype) < 0) goto ERROR;

/*     SSCRUTE(_enttypename); */

    _itmplen=strlen(_pathi);
    _ftmplen=strlen(_pathf);
    _tmplen = strlen(_enttypename);

    strcpy(&_pathi[_itmplen],_enttypename);
    strcpy(&_pathf[_ftmplen],_enttypename);
    _itmplen += _tmplen;
    _ftmplen += _tmplen;;

/*     SSCRUTE(_pathi); */
/*     SSCRUTE(_pathf); */
/*     ISCRUTE(_itmplen); */
/*     ISCRUTE(_ftmplen); */
/*     ISCRUTE(_tmplen); */


    /*
     * Creation/Ecriture de l'attribut MED_NOM_CGT sur le datagroup <entite>
     */
    if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_CGT,
				  MED_INTERNAL_INT,(const unsigned char * const) &_1 ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(_pathi);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
      goto ERROR;
    }

    if (enttype != MED_NODE ) {
      _pathi[_itmplen]='/';++_itmplen;_pathi[_itmplen]='\0';
      _pathf[_ftmplen]='/';++_ftmplen;_pathf[_ftmplen]='\0';
    }
    _isavlen=_itmplen;
    _fsavlen=_ftmplen;

/*     SSCRUTE(_pathi); */
/*     SSCRUTE(_pathf); */

    for ( _itgeotype=1; _itgeotype <= _ngeotype; ++_itgeotype) {

/*       ISCRUTE_int(_itgeotype); */
      /* read the geotype */
      _MEDmeshEntityInfo236(dummy,fid, meshname, MED_NO_DT, MED_NO_IT, enttype,_itgeotype,
			    _geotypename,&_geotype,&_fret);
      if (_fret  < 0) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityInfo");
	SSCRUTE(meshname);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);ISCRUTE_int(_itgeotype);
	goto ERROR;
      }

      _tmplen = strlen(_geotypename);
      strcpy(&_pathi[_isavlen],_geotypename);
      strcpy(&_pathf[_fsavlen],_geotypename);

/*       _itmplen += _tmplen; */
/*       _ftmplen += _tmplen; */
/*       SSCRUTE(_pathi); */
/*       SSCRUTE(_pathf); */

      /*
       * Creation/Ecriture de l'attribut MED_NOM_GEO sur le datagroup <typegeo>
       */
      _intgeotype = (med_int) _geotype;
      if (enttype != MED_NODE )
	if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_GEO,MED_INTERNAL_INT,
				      (const unsigned char * const) &_intgeotype) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);ISCRUTE_int(_itgeotype);
	  ISCRUTE(_intgeotype);
	  goto ERROR;
	}

      /*
       * Creation/Ecriture de l'attribut MED_NOM_PFL sur le datagroup <typegeo>
       */
      if ( _MEDattributeStringWrByName(fid,_pathi,MED_NOM_PFL,MED_NAME_SIZE,MED_NO_PROFILE_INTERNAL) < 0) {
	MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	SSCRUTE(meshname);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);ISCRUTE_int(_itgeotype);
	ISCRUTE(_intgeotype);SSCRUTE(MED_NOM_PFL);SSCRUTE(MED_NO_PROFILE_INTERNAL);
	goto ERROR;
      }

      /*
       * Creation/Ecriture de l'attribut MED_NOM_CGT sur le datagroup <typegeo>
       */
      /* Le modify ne fonctionne pas dans _MEDattributeNumWrByName à cause hdf5 ? */
      if (enttype != MED_NODE )
	if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_CGT,MED_INTERNAL_INT,
				      (const unsigned char * const) &_1 ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}

      /*
       * Creation/Ecriture de l'attribut MED_NOM_CGT sur le dataset
       * Creation/Ecriture de l'attribut MED_NOM_CGS sur le datagroup
       */

      if ( MAJ_236_300_mesh_datasets(fid, _pathi, _pathf, meshname, enttype, _geotype)< 0) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MAJ_236_300_mesh_datasets");
	SSCRUTE(_pathi);
	goto ERROR;
      }

    }


    /* Déplacement de toute la branche du type de l'entité.*/
    _pathi[_isavlen]='\0';
    _pathf[_fsavlen]='\0';
/*     SSCRUTE(_pathf); */
/*     SSCRUTE(_pathi); */
    ret = H5Gmove(fid, _pathi, _pathf  );
    EXIT_IF(ret < 0,"Déplacement des entités de type",MED_GET_ENTITY_TYPENAME[enttype+1]);

 SORTIE:
  _ret=0;
 ERROR:
  return (int) _ret;
}
