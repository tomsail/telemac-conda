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

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include <2.3.6/med_proto.h>
#include "2.3.6/med23v30_misc.h"

#include "MAJ_236_300.h"
#include "MAJ_version.h"

#define MAX_SPACE_DIM 3

void MAJ_236_300_maillages(med_idt fid)
{
  med_err _ret  = -1;
  med_err  ret  = -1;
  med_err _fret = -1;
  med_err _err  = -1;
  int     dummy=0;
  char _pathi[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1+2*MED_TAILLE_NOM_ENTITE+1+3+1]=MED_MESH_GRP;
  char _pathf[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1+2*MED_MAX_PARA+1+2*MED_TAILLE_NOM_ENTITE+1+1]=MED_MESH_GRP;
  char _pathgridf[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1+2*MED_MAX_PARA+1+2*MED_TAILLE_NOM_ENTITE+1+1]=MED_MESH_GRP;
  char _pathgridi[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1+2*MED_TAILLE_NOM_ENTITE+1+3+1]=MED_MESH_GRP;
  char _pathfam[MED_FAMILY_GRP_SIZE+MED_NAME_SIZE+1+MED_NAME_SIZE+1]=MED_FAMILY_GRP;
  med_size _n=0;
  med_idt            _datagroup=0;
  int                _meshit=0;
  char               _imeshname[MED_NAME_SIZE+1];
  char               _fmeshname[MED_NAME_SIZE+1];
  char *             _meshname=_imeshname;
  med_int            _spacedim=0;
  med_int            _meshdim=0;
  med_mesh_type      _meshtype=MED_UNDEF_MESH_TYPE;
  char               _description[MED_COMMENT_SIZE+1]="";
  char               _dtunit[MED_SNAME_SIZE+1]="";
  med_sorting_type   _sortingtype=MED_SORT_UNDEF;
  med_int            _nstep=0;
  med_axis_type      _axistype=MED_UNDEF_AXIS_TYPE;
  char               _axisname[3*MED_SNAME_SIZE+1];
  char               _axisunit[3*MED_SNAME_SIZE+1];
  int                _itmplen=0;
  int                _ftmplen=0;
  int                _gitmplen=0;
  int                _gftmplen=0;
  med_int            _1=1;
  med_bool           _datagroupexist=MED_FALSE,_isasoftlink=MED_FALSE;
  med_type_grille    _gridtype;
  med_int            _gridstruct[MAX_SPACE_DIM] = {0,0,0};
  med_bool	     _mustwrite_gridstruct= MED_FALSE;
  hid_t              _ocp_plist_id;
  hid_t              _lcp_plist_id;
  htri_t  _groupexist;

  /*
   *  nombre de maillages
   */

  ret=_MEDnObjects(fid,MED_MESH_GRP,&_n);
  MED_ERR_EXIT_IF( ( ret == (MED_ERR_COUNT + MED_ERR_DATAGROUP)),
		     MED_ERR_COUNT,MED_ERR_MESH,MED_MESH_GRP);

  /*
   * Mise a jour des maillages :
   *
   */
/*   SSCRUTE(_pathi); */
  _groupexist= H5Aexists_by_name( fid, _pathi, MED_COMMENT_NAME, H5P_DEFAULT  );
  if (_groupexist > 0) {
    MED_ERR_EXIT_IF( _MEDattributeStringRdByName(fid,_pathi,MED_COMMENT_NAME,MED_COMMENT_SIZE,
						 _description ) < 0 ,
		     MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_COMMENT_NAME);
    
    H5Adelete_by_name( fid, _pathi,MED_COMMENT_NAME, H5P_DEFAULT );
    
    MED_ERR_EXIT_IF( _MEDattributeStringWrByName(fid,"/",MED_COMMENT_NAME,MED_COMMENT_SIZE,
						 _description ) < 0 ,
		     MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_COMMENT_NAME);
  }

  for (_meshit=0;_meshit<_n;++_meshit) {

    /*
     * On recupere le nom du maillage
     */
    MED_ERR_EXIT_IF( _MEDobjectGetName(fid, MED_MESH_GRP ,_meshit, _imeshname) < 0 ,
		     MED_ERR_ACCESS,MED_ERR_DATAGROUP,_imeshname);

    MAJ_version_num(fid,2,3,6);
    _MEDmeshInfoByName236(dummy,fid, _imeshname, &_spacedim, &_meshdim, &_meshtype,
			  _description, _dtunit, &_sortingtype, &_nstep,
			  &_axistype, _axisname, _axisunit, &_fret);
    MED_ERR_EXIT_IF(_fret < 0, MED_ERR_CALL, MED_ERR_API, "MEDmeshInfoByName236");
    MAJ_version_num(fid,3,0,8);

    MED_ERR_EXIT_IF( _spacedim > MAX_SPACE_DIM ,
		     MED_ERR_INVALID,MED_ERR_PARAMETER,"_spacedim");

    if ( MAJ_236_300_chaine(_imeshname,_fmeshname) ) {

      fprintf(stdout,"  >>> Normalisation du nom de maillage [%s] \n",_imeshname);
      /* on accede au maillage */
      strcpy(&_pathi[MED_MESH_GRP_SIZE],_imeshname);
      strcpy(&_pathf[MED_MESH_GRP_SIZE],_fmeshname);

      ret = H5Gmove(fid, _pathi, _pathf  );
      EXIT_IF(ret < 0,"Renommage du maillage en",_fmeshname);
      _meshname=_fmeshname;
      fprintf(stdout,"  >>> Normalisation du nom du maillage [%s] ... OK ... \n",_fmeshname);
    } else {
      strcpy(&_pathf[MED_MESH_GRP_SIZE],_meshname);
      strcpy(&_pathi[MED_MESH_GRP_SIZE],_meshname);
    }
    /*     SSCRUTE(_pathf); */
    /*     SSCRUTE(_pathi); */


    /*
     * MAJ des attributs sur le datagroup <_meshname>
     */
    MED_ERR_EXIT_IF( MEDmeshCr(fid, _meshname, _spacedim, _meshdim, _meshtype,
			       _description, _dtunit, MED_SORT_DTIT,
			       _axistype, _axisname, _axisunit)  < 0,
		     MED_ERR_CALL,MED_ERR_API,"MEDmeshCr");

    /*Normalisation des grilles */
    if (  _meshtype != MED_UNSTRUCTURED_MESH ) {
      MAJ_version_num(fid,2,3,6);
      MED_ERR_EXIT_IF( MEDnatureGrilleLire(fid, _meshname, &_gridtype) < 0
		       ,MED_ERR_CALL,MED_ERR_API," MEDnatureGrilleLire");

/*       strcpy(&_pathgridf[MED_MESH_GRP_SIZE],_meshname); */
      strcpy(&_pathgridi[MED_MESH_GRP_SIZE],_meshname);
/*       strcat(_pathgridf,"/-0000000000000000001-0000000000000000001/NOE/"); */
      strcat(_pathgridi,"/NOE/");
      _gitmplen=strlen(_pathgridi);
/*       _gftmplen=strlen(_pathgridf); */

      if ( (_gridtype == MED_GRILLE_CARTESIENNE) || (_gridtype == MED_GRILLE_POLAIRE) ) {

	strcpy(&_pathgridi[_gitmplen],"IN1");
/* 	SSCRUTE(_pathgridi); */
	H5Adelete_by_name( fid, _pathgridi,MED_NOM_UNI, H5P_DEFAULT  );
	H5Adelete_by_name( fid, _pathgridi,MED_NOM_NOM, H5P_DEFAULT  );
	strcpy(&_pathgridi[_gitmplen],"IN2");
/* 	SSCRUTE(_pathgridi); */
	H5Adelete_by_name( fid, _pathgridi,MED_NOM_UNI, H5P_DEFAULT  );
	H5Adelete_by_name( fid, _pathgridi,MED_NOM_NOM, H5P_DEFAULT  );
	strcpy(&_pathgridi[_gitmplen],"IN3");
/* 	SSCRUTE(_pathgridi); */
	H5Adelete_by_name( fid, _pathgridi,MED_NOM_UNI, H5P_DEFAULT  );
	H5Adelete_by_name( fid, _pathgridi,MED_NOM_NOM, H5P_DEFAULT  );

	MAJ_version_num(fid,3,0,8);

      } else {
	MED_ERR_EXIT_IF( MEDstructureCoordLire(fid,_meshname,_meshdim,_gridstruct) < 0
			 ,MED_ERR_CALL,MED_ERR_API,"MEDstructureCoordLire");
	strcpy(&_pathgridi[_gitmplen],"COO");
	H5Adelete_by_name( fid, _pathgridi,MED_NOM_IN1, H5P_DEFAULT  );
	H5Adelete_by_name( fid, _pathgridi,MED_NOM_IN2, H5P_DEFAULT  );
	H5Adelete_by_name( fid, _pathgridi,MED_NOM_IN3, H5P_DEFAULT  );

	MAJ_version_num(fid,3,0,8);

	_mustwrite_gridstruct= MED_TRUE;

      }
    }

    /*     SSCRUTE(_meshname); */
    MED_ERR_EXIT_IF( MEDmeshComputationStepCr(fid,_meshname,MED_NO_DT,MED_NO_IT,
					      MED_NO_DT,MED_NO_IT,0) < 0
		     ,MED_ERR_CALL,MED_ERR_API,"MEDmeshComputationStepCr");
    _ftmplen=strlen(_pathf);
    _pathf[_ftmplen]='/'; ++_ftmplen;
    _MEDgetComputationStepName(MED_SORT_DTIT,MED_NO_DT,MED_NO_IT,&_pathf[_ftmplen]);
    _ftmplen+=2*MED_MAX_PARA;
    

    /*     SSCRUTE(_pathf); */
    /*     H5Fflush(fid, H5F_SCOPE_GLOBAL  ); */
    /*     _MEDobjetsOuverts(fid); */

    /*
     * MAJ de l'attribut MED_NOM_CGT sur le datagroup d'étape de calcul
     */
    /* Le modify ne fonctionne pas dans _MEDattributeNumWrByName à cause hdf5 ? */
    H5Adelete_by_name( fid, _pathf,MED_NOM_CGT, H5P_DEFAULT  );
    MED_ERR_EXIT_IF( _MEDattributeNumWrByName(fid,_pathf,MED_NOM_CGT,MED_INTERNAL_INT,
					      (const unsigned char * const) &_1 ) < 0,
		     MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_CGT);

    _pathf[_ftmplen]='/';
    ++_ftmplen;
    _pathf[_ftmplen]='\0';

    strcat(_pathi,"/");
    _itmplen=strlen(_pathi);

    /*     SSCRUTE(_pathi); */
    /*     SSCRUTE(_pathf); */

    /* Normalisation des familles */
    strcpy(&_pathi[_itmplen-1],MED_FAMILY_GRP);
/*     SSCRUTE(_pathi); */
    MED_ERR_EXIT_IF( _MEDdatagroupExist(fid,_pathi,&_datagroupexist,&_isasoftlink) < 0 ,
		     MED_ERR_CALL,MED_ERR_API,"_MEDdatagroupExist");

    fprintf(stdout,"  >>> Normalisation des familles du maillage [%s] \n",_imeshname);

    _datagroup = _MEDdatagroupCreer(fid,_pathfam);
    MED_ERR_EXIT_IF(_datagroup < 0 ,MED_ERR_CREATE,MED_ERR_DATAGROUP,_pathfam);
    if (_datagroup>0) {
      _ret= _MEDdatagroupFermer(_datagroup);
      MED_ERR_EXIT_IF(_ret < 0,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_pathfam);
    }

    strcpy(&_pathfam[MED_FAMILY_GRP_SIZE],_meshname);strcat(_pathfam,"/");

    if( _datagroupexist) {

      _ocp_plist_id = H5Pcreate( H5P_OBJECT_COPY );
      _lcp_plist_id = H5Pcreate( H5P_LINK_CREATE );

      ret =  H5Ocopy(fid,_pathi,fid,_pathfam,_ocp_plist_id,_lcp_plist_id);
      EXIT_IF(ret < 0,"Copie des familles dans ","/");

      MED_ERR_EXIT_IF ( _MEDvisit(fid,_pathi,_pathfam,(medvisitorfunc) _MEDconvertStringDatasets ) < 0,
			MED_ERR_VISIT,MED_ERR_DATAGROUP,_pathfam);
      MED_ERR_EXIT_IF ( H5Ldelete(fid,_pathi,H5P_DEFAULT) < 0, MED_ERR_DELETE,MED_ERR_LINK,_pathi);

    }

    strcat(_pathfam,"FAMILLE_ZERO");
    MED_ERR_EXIT_IF( _MEDdatagroupExist(fid,_pathfam,&_datagroupexist,&_isasoftlink) < 0 ,
		     MED_ERR_CALL,MED_ERR_API,"_MEDdatagroupExist");

    if( !_datagroupexist) {
      MED_ERR_EXIT_IF ( (MEDfamilyCr(fid,_meshname,FAMILLE_ZERO, 0, 0, "") < 0),
			MED_ERR_CREATE,MED_ERR_FAMILY,"MEDfamilyCr");
    }

    /* Normalisation des equivalences */
    strcpy(&_pathi[_itmplen-1],MED_EQUIVALENCE_GRP);

    MED_ERR_EXIT_IF( MAJ_236_300_equivalence(fid,_meshname) < 0,
		     MED_ERR_CALL,MED_ERR_API,"MAJ_236_300_equivalence");
    _groupexist=H5Lexists(fid, _pathi, H5P_DEFAULT  );
    if (_groupexist) {
      MED_ERR_EXIT_IF( H5Ldelete(fid,_pathi,H5P_DEFAULT) < 0,
		       MED_ERR_DELETE,MED_ERR_LINK,_pathi);
    }

    /* Normalisation des joint */
    strcpy(&_pathi[_itmplen-1],MED_JOINT_GRP);

    MED_ERR_EXIT_IF( MAJ_236_300_joint(fid,_meshname) < 0,
		     MED_ERR_CALL,MED_ERR_API,"MAJ_236_300_joint");
    _groupexist=H5Lexists(fid, _pathi, H5P_DEFAULT  );
    if (_groupexist) {
      MED_ERR_EXIT_IF( H5Ldelete(fid,_pathi,H5P_DEFAULT) < 0,
		       MED_ERR_DELETE,MED_ERR_JOINT,_pathi);
    }

    _pathi[_itmplen]='\0';
/*     SSCRUTE(_pathi); */

    /*Normalisation des entités*/
    fprintf(stdout,"  >>> Normalisation des entites [%s] du maillage [%s] \n",
	    MED_GET_ENTITY_TYPENAME[MED_NODE+1],_meshname);
    MED_ERR_EXIT_IF( MAJ_236_300_entites( fid,_pathi, _pathf,_meshname, MED_NODE) < 0 ,
		     MED_ERR_CALL,MED_ERR_API,"MAJ_236_300_entites");

    /*On ne peut écrire la structure de la grille qu'une fois le datagroup NOE déplacé*/
    if (_mustwrite_gridstruct) {
      MED_ERR_EXIT_IF( MEDmeshGridStructWr(fid,_meshname,MED_NO_DT,MED_NO_IT, MED_UNDEF_DT, _gridstruct ) < 0
		       ,MED_ERR_CALL,MED_ERR_API,"MEDmeshGridStructWr");
      _mustwrite_gridstruct = MED_FALSE;
    }

    /*On annule les modifications de _pathi et _pathf effectuées dans MAJ_236_300_entites */
    _pathf[_ftmplen]='\0';
    _pathi[_itmplen]='\0';

    fprintf(stdout,"  >>> Normalisation des entites [%s] du maillage [%s] \n",
	    MED_GET_ENTITY_TYPENAME[MED_CELL+1],_meshname);
    MED_ERR_EXIT_IF( MAJ_236_300_entites( fid,_pathi, _pathf,_meshname, MED_CELL) < 0,
		     MED_ERR_CALL,MED_ERR_API,"MAJ_236_300_entites");

    _pathf[_ftmplen]='\0';
    _pathi[_itmplen]='\0';

    fprintf(stdout,"  >>> Normalisation des entites [%s] du maillage [%s] \n",
	    MED_GET_ENTITY_TYPENAME[MED_DESCENDING_FACE+1],_meshname);
    MED_ERR_EXIT_IF( MAJ_236_300_entites( fid,_pathi, _pathf,_meshname, MED_DESCENDING_FACE) < 0,
		     MED_ERR_CALL,MED_ERR_API,"MAJ_236_300_entites");
    _pathf[_ftmplen]='\0';
    _pathi[_itmplen]='\0';

    fprintf(stdout,"  >>> Normalisation des entites [%s] du maillage [%s] \n",
	    MED_GET_ENTITY_TYPENAME[MED_DESCENDING_EDGE+1],_meshname);
    MED_ERR_EXIT_IF( MAJ_236_300_entites( fid,_pathi, _pathf,_meshname, MED_DESCENDING_EDGE) < 0,
		     MED_ERR_CALL,MED_ERR_API,"MAJ_236_300_entites");

  }

/*   _MEDobjetsOuverts(fid); */

/*   _ret=0; */
/*  ERROR: */
/*   return (int) _ret; */
}
