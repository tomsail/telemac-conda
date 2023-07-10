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
#include <string.h>

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

#include "MAJ_236_300.h"

int MAJ_236_300_mesh_datasets(med_idt fid,
			      char * const _pathi,
			      char * const _pathf,
			      const char * const meshname,
			      const med_entity_type   enttype,
			      const med_geometry_type geotype)
{
  med_err   _err=-1,_ret=-1;
  med_size _ndatasets=0;
  int      _itdataset=0;
  char     _idatasetname[MED_NAME_SIZE+1]="";
  med_int  _cgs=MED_FALSE;
  char     _savpathi[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1+2*MED_TAILLE_NOM_ENTITE+1+3+1]="";
  int      _itmplen=0;
  int      _ftmplen=0;
  med_int  _1 = 1, _nbratt=0,_nbtype=0;

  /*
   * Remarque : Cette procédure modifie le contenu des paramètres _pathi et pathf
   */

   _itmplen=strlen(_pathi);
   _ftmplen=strlen(_pathf);

  /*
   *  nombre de datasets (en 2.3.6, il ne peut pas y avoir de datagroup)
   */
  if ((_err=_MEDnObjects(fid,_pathi,&_ndatasets)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_MESH,"");
      goto ERROR;
    }


    for ( _itdataset=0; _itdataset < _ndatasets; ++_itdataset) {

      if ( _MEDobjectGetName(fid, _pathi,_itdataset, _idatasetname) < 0 ) {
	MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_pathi);ISCRUTE_int(_itdataset);
	goto ERROR;
      }

/*       SSCRUTE(_idatasetname); */
/*       SSCRUTE(_pathf); */
/*       SSCRUTE(_pathi); */

      if ( strcmp(_idatasetname,MED_NOM_COR) &&
	   strcmp(_idatasetname,MED_NOM_COO) &&
	   strcmp(_idatasetname,MED_NOM_NOD) &&
	   strcmp(_idatasetname,MED_NOM_DES) &&
	   strcmp(_idatasetname,MED_NOM_IN1) &&
	   strcmp(_idatasetname,MED_NOM_IN2) &&
	   strcmp(_idatasetname,MED_NOM_IN3)  ) _cgs=MED_TRUE;


      /* Normalisation des noeuds */
      if ( (enttype == MED_NODE) && !strcmp(_idatasetname,MED_NOM_COO) ) {
	fprintf(stdout,"  >>> Normalisation des datasets sur les noeuds du maillage [%s] \n"
		,meshname);
	_pathi[_itmplen]='/';
	strcpy(&_pathi[_itmplen+1],_idatasetname);
	H5Adelete_by_name( fid, _pathi, MED_NOM_NOM, H5P_DEFAULT  );
	H5Adelete_by_name( fid, _pathi, MED_NOM_REP, H5P_DEFAULT  );
	H5Adelete_by_name( fid, _pathi, MED_NOM_UNI, H5P_DEFAULT  );
      }

      /* Normalisation des polygones */
      if ( (geotype == MED_POLYGON) && !strcmp(_idatasetname,MED_NOM_NOD) ) {

	fprintf(stdout,"  >>> Normalisation des datasets sur les entites [%s] du maillage [%s] \n",
		MED_GET_ENTITY_TYPENAME[enttype+1],meshname);

	_pathi[_itmplen]='/';
	strcpy(&_pathi[_itmplen+1],_idatasetname);

	/*
	 * Lecture puis supression de l'attribut MED_NOM_NBR sur le dataset MED_NOM_NOD
	 */
	if ( _MEDattributeNumRdByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				      ( unsigned char * const) &_nbratt ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_NBR);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}
	H5Adelete_by_name( fid, _pathi, MED_NOM_NBR, H5P_DEFAULT  );
	H5Arename_by_name( fid, _pathi, MED_NOM_TAI, MED_NOM_NBR, H5P_DEFAULT);

	_pathi[_itmplen]='/';
	strcpy(&_pathi[_itmplen+1],MED_NOM_INN);
/* 	SSCRUTE(_pathf); */
/* 	SSCRUTE(_pathi); */

	/*
	 * Creation/Ecriture de l'attribut MED_NOM_NBR sur le dataset MED_NOM_INN
	 */
	++_nbratt;
	if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				      (const unsigned char * const) &_nbratt ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_NBR);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}

      }

      /* Normalisation des polyèdres en connectivité descendante */
      if ( (geotype == MED_POLYHEDRON) && !strcmp(_idatasetname,MED_NOM_IFD) ) {

	fprintf(stdout,"  >>> Normalisation des datasets sur les entites [%s] du maillage [%s] \n",
		MED_GET_ENTITY_TYPENAME[enttype+1],meshname);

	_pathi[_itmplen]='/';
	strcpy(_savpathi,_pathi);

	/*Renomme l'actuel IND en IND_*/
	strcpy(&_pathi[_itmplen+1],MED_NOM_IND);
	strcpy(&_savpathi[_itmplen+1],MED_NOM_IND);strcat(_savpathi,"_");
	H5Lmove(fid, _pathi, fid, _savpathi, H5P_DEFAULT, H5P_DEFAULT );

	/*Renomme l'actuel IFD en IND*/
	strcpy(&_pathi[_itmplen+1],MED_NOM_IFD);
	strcpy(&_savpathi[_itmplen+1],MED_NOM_IND);
	H5Lmove(fid, _pathi, fid, _savpathi, H5P_DEFAULT, H5P_DEFAULT );
	/*Renomme l' IND_ en IFD*/
	strcpy(&_pathi[_itmplen+1],MED_NOM_IND);strcat(_pathi,"_");
	strcpy(&_savpathi[_itmplen+1],MED_NOM_IFD);
	H5Lmove(fid, _pathi, fid, _savpathi, H5P_DEFAULT, H5P_DEFAULT );

	/* Lecture puis déplacement de l'attribut MED_NOM_NBR du dataset DES vers le dataset IND*/
	_pathi[_itmplen]='/';
	strcpy(&_pathi[_itmplen+1],MED_NOM_DES);
	if ( _MEDattributeNumRdByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				      ( unsigned char * const) &_nbratt ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_NBR);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}
	/* Lecture puis déplacement de l'attribut MED_NOM_TTI du dataset DES vers le dataset IFD*/
	if ( _MEDattributeNumRdByName(fid,_pathi,MED_NOM_TTI,MED_INTERNAL_INT,
				      ( unsigned char * const) &_nbtype ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_TTI);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}
	H5Adelete_by_name( fid, _pathi, MED_NOM_NBR, H5P_DEFAULT  );
	H5Adelete_by_name( fid, _pathi, MED_NOM_TTI, H5P_DEFAULT  );
	H5Arename_by_name( fid, _pathi, MED_NOM_TAI, MED_NOM_NBR, H5P_DEFAULT);

	/* Creation/Ecriture de l'attribut MED_NOM_NBR sur le dataset MED_NOM_IND */
	++_nbratt;
	strcpy(&_pathi[_itmplen+1],MED_NOM_IND);
	if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				      (const unsigned char * const) &_nbtype ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_NBR);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}

	/* Creation/Ecriture de l'attribut MED_NOM_NBR sur le dataset MED_NOM_IFD */
	strcpy(&_pathi[_itmplen+1],MED_NOM_IFD);
	if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				      (const unsigned char * const) &_nbratt ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_NBR);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}

      }

      /* Normalisation des polyèdres en connectivité nodale */
      if ( (geotype == MED_POLYHEDRON) && !strcmp(_idatasetname,MED_NOM_IFN) ) {

	fprintf(stdout,"  >>> Normalisation des datasets sur les entites [%s] du maillage [%s] \n",
		MED_GET_ENTITY_TYPENAME[enttype+1],meshname);

	_pathi[_itmplen]='/';
	strcpy(_savpathi,_pathi);

	/*Renomme l'actuel INN en INN_*/
	strcpy(&_pathi[_itmplen+1],MED_NOM_INN);
	strcpy(&_savpathi[_itmplen+1],MED_NOM_INN);strcat(_savpathi,"_");
	H5Lmove(fid, _pathi, fid, _savpathi, H5P_DEFAULT, H5P_DEFAULT );

	/*Renomme l'actuel IFN en INN*/
	strcpy(&_pathi[_itmplen+1],MED_NOM_IFN);
	strcpy(&_savpathi[_itmplen+1],MED_NOM_INN);
	H5Lmove(fid, _pathi, fid, _savpathi, H5P_DEFAULT, H5P_DEFAULT );
	/*Renomme l' INN_ en IFN*/
	strcpy(&_pathi[_itmplen+1],MED_NOM_INN);strcat(_pathi,"_");
	strcpy(&_savpathi[_itmplen+1],MED_NOM_IFN);
	H5Lmove(fid, _pathi, fid, _savpathi, H5P_DEFAULT, H5P_DEFAULT );

	/* Lecture puis déplacement de l'attribut MED_NOM_NBR du dataset NOD vers le dataset INN*/
	_pathi[_itmplen]='/';
	strcpy(&_pathi[_itmplen+1],MED_NOM_NOD);
	if ( _MEDattributeNumRdByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				      ( unsigned char * const) &_nbratt ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_NBR);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}
	/* Lecture puis déplacement de l'attribut MED_NOM_TTI du dataset NOD vers le dataset IFN*/
	if ( _MEDattributeNumRdByName(fid,_pathi,MED_NOM_TTI,MED_INTERNAL_INT,
				      ( unsigned char * const) &_nbtype ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_TTI);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}
	H5Adelete_by_name( fid, _pathi, MED_NOM_NBR, H5P_DEFAULT  );
	H5Adelete_by_name( fid, _pathi, MED_NOM_TTI, H5P_DEFAULT  );
	H5Arename_by_name( fid, _pathi, MED_NOM_TAI, MED_NOM_NBR, H5P_DEFAULT);

	/* Creation/Ecriture de l'attribut MED_NOM_NBR sur le dataset MED_NOM_INN */
	++_nbratt;
	strcpy(&_pathi[_itmplen+1],MED_NOM_INN);
	if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				      (const unsigned char * const) &_nbtype ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_NBR);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}

	/* Creation/Ecriture de l'attribut MED_NOM_NBR sur le dataset MED_NOM_IFN */
	strcpy(&_pathi[_itmplen+1],MED_NOM_IFN);
	if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				      (const unsigned char * const) &_nbratt ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_NBR);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}

      }

      /* Normalisation des noms optionnels */
      if ( !strcmp(_idatasetname,MED_NOM_NOM) ) {

	fprintf(stdout,"  >>> Normalisation des noms optionnels sur les entites [%s] du maillage [%s] \n",
		MED_GET_ENTITY_TYPENAME[enttype+1],meshname);

	_pathi[_itmplen]='/';
	strcpy(&_pathi[_itmplen+1],_idatasetname);

	/*
	 * Lecture de l'attribut MED_NOM_NBR sur le dataset
	 */
	if ( _MEDattributeNumRdByName(fid,_pathi,MED_NOM_NBR,MED_INTERNAL_INT,
				      ( unsigned char * const) &_nbratt ) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	  SSCRUTE(_pathi);SSCRUTE(MED_NOM_NBR);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	  goto ERROR;
	}

	_pathi[_itmplen]='\0';
	
	MED_ERR_EXIT_IF( MAJ_236_300_string_datasets( fid, _pathi, _idatasetname,MED_TAILLE_PNOM,
						      MED_SNAME_SIZE,_nbratt)
			 < 0, MED_ERR_CALL, MED_ERR_API, "MAJ_236_300_string_datasets");

      }

      _pathi[_itmplen]='/';
      strcpy(&_pathi[_itmplen+1],_idatasetname);
/*       SSCRUTE(_pathf); */
/*       SSCRUTE(_pathi); */

      /*
       * Creation/Ecriture de l'attribut MED_NOM_CGT sur le dataset
       */

      if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_CGT,MED_INTERNAL_INT,
				    (const unsigned char * const) &_1 ) < 0 ) {
	MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	SSCRUTE(_pathi);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
	goto ERROR;
      }

      _pathi[_itmplen]='\0';

    }

    /*
     * Creation/Ecriture de l'attribut MED_NOM_CGS sur le datagroup
     */
    if ( _MEDattributeNumWrByName(fid,_pathi,MED_NOM_CGS,MED_INTERNAL_INT,
				  (const unsigned char * const) &_cgs ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(_pathi);SSCRUTE(MED_GET_ENTITY_TYPENAME[enttype+1]);
      SSCRUTE(MED_NOM_CGS);
      goto ERROR;
    }


  _ret=0;
 ERROR:

/*   if (_datagroup>0) */
/*     if (_MEDdatagroupFermer(_datagroup) < 0) { */
/*       _pathi[_itmplen]='\0'; */
/*       MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_pathi); */
/*     } */

/*   if (_tmpnombuff ) free(_tmpnombuff); */

  return (int) _ret;
}
