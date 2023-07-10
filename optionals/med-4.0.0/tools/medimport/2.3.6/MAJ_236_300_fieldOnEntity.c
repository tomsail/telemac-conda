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

#include "MAJ_236_300.h"
#include "MAJ_version.h"

med_err MAJ_236_300_fieldOnEntity(med_idt fid, const char * const nomcha, const char * const meshname,
				  med_field_type typcha, med_int ncomp, med_entity_type entite, med_int ncstp,
				  char * const _pathi, char * const _pathf) {

  med_err ret=-1;
  int i,j,k,l,m,n,nb_geo=0;
  med_int nbpdtnor=0,pflsize,*pflval,ngauss=0,ngroup,*vale=NULL,nval;
  med_int numdt=0,numo=0,_nprofile;
  med_int meshnumdt=0,meshnumit=0;
  med_float *valr=NULL,dt=0.0;
  unsigned char * _val=NULL;
  char pflname [MED_NAME_SIZE+1]="";
  char locname [MED_NAME_SIZE+1]="";
  char _meshname [MED_NAME_SIZE+1]="";
  char _fieldname [MED_NAME_SIZE+1]="";
  char _pathtmp[MED_FIELD_GRP_SIZE+3]="/CHA__/";
  char _pathfb[MED_FIELD_GRP_SIZE+2+MED_NAME_SIZE+1]="/CHA_/";
  char * lien = NULL;
  char dt_unit [MED_SNAME_SIZE+1]="unknown";
  med_bool localmesh;
  med_int nmesh=0;
  hid_t   _ocp_plist_id ;
  hid_t   _lcp_plist_id ;
  int     _isavlen=0;
  int     _fsavlen=0;
  int     _fieldnamelen=0;
  htri_t  _groupexist;

  char            _tmpmeshname[MED_NAME_SIZE+1]="";
  med_bool        _tmplocal=MED_FALSE;
  med_field_type  _tmptypcha;
  char           *_tmpcomp=NULL,*_tmpunit=NULL,_tmpdtunit[MED_SNAME_SIZE+1]="";
  med_int         _tmpncstp=0;

  med_geometry_type * type_geo;

  const char * const * AFF;
  const char * const * AFF_ENT=MED_GET_ENTITY_TYPENAME+1;
  switch (entite) {
  case MED_NODE :
    type_geo = MED_GET_NODE_GEOMETRY_TYPE;
    nb_geo   = MED_N_NODE_FIXED_GEO;
    AFF      = MED_GET_NODE_GEOMETRY_TYPENAME;
    break;
  case  MED_CELL :
  case  MED_NODE_ELEMENT :
    type_geo = MED_GET_CELL_GEOMETRY_TYPE;
    nb_geo   = MED_N_CELL_FIXED_GEO;
    AFF      = MED_GET_CELL_GEOMETRY_TYPENAME;
    break;
  case  MED_DESCENDING_FACE :
    type_geo = MED_GET_FACE_GEOMETRY_TYPE;
    nb_geo   = MED_N_FACE_FIXED_GEO;
    AFF      = MED_GET_FACE_GEOMETRY_TYPENAME;
    break;
  case  MED_DESCENDING_EDGE :
    type_geo = MED_GET_EDGE_GEOMETRY_TYPE;
    nb_geo   = MED_N_EDGE_FIXED_GEO;
    AFF      = MED_GET_EDGE_GEOMETRY_TYPENAME;
    break;
  }

  strcpy(_fieldname,nomcha);
  _isavlen=strlen(_pathi);
  _fsavlen=strlen(_pathf);
  _fieldnamelen=strlen(_fieldname);

  _ocp_plist_id = H5Pcreate( H5P_OBJECT_COPY );
  _lcp_plist_id = H5Pcreate( H5P_LINK_CREATE );

  /* Reconstruit les objets pointés par un lien symbolique */
  H5Pset_copy_object( _ocp_plist_id, H5O_COPY_EXPAND_SOFT_LINK_FLAG );
  /*Ne crée pas les liens intermédiaires */
  H5Pset_create_intermediate_group(_lcp_plist_id, -1);
  /* Ne recopie pas en profondeur*/
  H5Pset_copy_object( _ocp_plist_id, H5O_COPY_SHALLOW_HIERARCHY_FLAG);

/*   ISCRUTE(nbpdtnor); */
/*   SSCRUTE(_fieldname); */

  for (k=1;k<=nb_geo;k++) {

    /* Combien de (PDT,NOR) a lire */
    nbpdtnor = ncstp;
    if (nbpdtnor < 1 ) continue;

    for (j=0;j<nbpdtnor;j++) {

      if ( MEDfield23ComputingStepMeshInfo(fid,nomcha,j+1, &numdt, &numo, &dt,
					   &nmesh, _meshname,&localmesh, &meshnumdt, &meshnumit ) <0) {
	MESSAGE("Erreur a la demande d'information sur (pdt,nor) : ");
	ISCRUTE(numdt); ISCRUTE(numo);ISCRUTE(nmesh);SSCRUTE(_meshname);ISCRUTE_int(localmesh);
	ISCRUTE(meshnumdt);ISCRUTE(meshnumit);
	ret = -1; continue;
      }
/* SSCRUTE(_meshname);SSCRUTE(AFF_ENT[(int)entite]);SSCRUTE(AFF[k]);ISCRUTE(nmesh); */
      for (i=0;i< nmesh;++i) {

	if ( (_nprofile = MEDfield23nProfile(fid,nomcha,numdt,numo,entite,type_geo[k],i+1,_meshname,
						pflname,locname   ) ) < 0 ) {
	  MESSAGE("Erreur a la demande du nombre de profils referencés par le champ : ");
	  SSCRUTE(nomcha); ISCRUTE(numdt); ISCRUTE(numo);SSCRUTE(_meshname);
	  ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);SSCRUTE(pflname);SSCRUTE(locname);
	  SSCRUTE(AFF_ENT[(int)entite]);SSCRUTE(AFF[k]);
	  ret = -1; continue;
	};

/* ISCRUTE(_nprofile); */
/* SSCRUTE(_meshname); */
/* SSCRUTE(meshname); */
/* SSCRUTE(pflname); */

	for (l=0;l<_nprofile;l++) {

	  /* Si le maillage traité dans ce profil n'est pas celui par défaut :
	     - Vérifie que le nom du champ pour ce maillage secondaire existe.
	     - Initialise _fieldname au nom du champ qu'il faut traiter par la suite
	  */
	  if (strcmp(_meshname,meshname)) {

	    /* Lecture du type du champ, des noms des composantes et du nom de l'unité*/
	    _tmpcomp = (char*) malloc(ncomp*MED_SNAME_SIZE+1);
	    EXIT_IF(_tmpcomp == NULL,NULL,NULL);
	    _tmpunit = (char*) malloc(ncomp*MED_SNAME_SIZE+1);
	    EXIT_IF(_tmpunit == NULL,NULL,NULL);

	    /* Evite de passer les paramètres _tmpcomp,_tmpunit,_tmpdtunit,&_tmpncstp dans la fct MAJ_236...*/
	    ret = MEDfieldInfoByName(fid,nomcha,
				     _tmpmeshname,&_tmplocal,&_tmptypcha,_tmpcomp,_tmpunit,_tmpdtunit,&_tmpncstp);
	    MED_ERR_EXIT_IF(ret,MED_ERR_ACCESS,MED_ERR_FIELD,nomcha);
	    /*Ne pose pas de problème de taille de chaîne car MED_NAME_SIZE a doublé en 3.0 */
	    _fieldname[_fieldnamelen]='_';strcpy(&_fieldname[_fieldnamelen+1],_meshname);

	    MAJ_version_num(fid,3,0,8);
	    /*Sauvegarde du champ initial dans _pathtmp*/
	    EXIT_IF( H5Gmove(fid, _pathi, _pathtmp  ) < 0,"Switch to ",_pathtmp);
	    EXIT_IF( H5Gmove(fid, _pathf, _pathi  ) < 0  ,"Switch to ",_pathi);
	    MED_ERR_EXIT_IF( MEDfieldCr(fid,_fieldname,
					_tmptypcha,ncomp,_tmpcomp,_tmpunit,_tmpdtunit,_meshname ) < 0,
			     MED_ERR_CREATE,MED_ERR_FIELD,_fieldname);

	    /*Rétablissement des chemins d'accès au champ initial et au champ cible temporaire*/
	    EXIT_IF( H5Gmove(fid, _pathi  , _pathf  ) < 0,"Switch to ",_pathf);
	    EXIT_IF( H5Gmove(fid, _pathtmp, _pathi  ) < 0,"Switch to ",_pathi);

	    MAJ_version_num(fid,2,3,6);

	    free(_tmpcomp);
	    free(_tmpunit);
	  } else {
	    strcpy(_fieldname,nomcha);
	  }
/* SSCRUTE(_fieldname); */

	  if ( (nval = MEDfield23nValueWithProfile(fid, nomcha, numdt, numo,  entite, type_geo[k],_meshname,
						   l+1,  MED_COMPACT_PFLMODE, pflname,&pflsize,
						   locname, &ngauss) ) < 0 ) {
	    MESSAGE("Erreur a la lecture du nombre de valeurs du champ : ");
	    SSCRUTE(nomcha);ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(_meshname);
	    ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	    ret = -1; continue;
	  };
/* ISCRUTE(nval); */

/* 	  printf("\n  +Pas de Temps n."IFORMAT" (%f) [%s], n. d'ordre "IFORMAT", avec "IFORMAT" valeur(s) par entité.\n",numdt,dt,dt_unit,numo,ngauss); */
/* 	  printf("\t- Il y a "IFORMAT" entités qui portent des valeurs en mode %i. Chaque entite %s\ */
/*  de type geometrique %s associes au profile |%s| a "IFORMAT" valeurs associées \n", */
/* 		 nval,MED_COMPACT_PFLMODE,AFF_ENT[(int)entite],AFF[k],pflname,ngauss); */
/* 	  printf("\t- Le maillage associé est |%s|\n",_meshname); */

	  /*Lecture des valeurs du champ */
	  if (typcha == MED_FLOAT64) {

	    valr = (med_float*) calloc(ncomp*nval*ngauss,sizeof(med_float));
	    EXIT_IF(valr == NULL,NULL,NULL);

	    if (MEDfield23ValueWithProfileRd(fid, nomcha, numdt,numo, entite,type_geo[k],_meshname,
					     MED_COMPACT_PFLMODE, pflname, MED_NO_INTERLACE,MED_ALL_CONSTITUENT,
					     (unsigned char*) valr) < 0 ) {
	      MESSAGE("Erreur a la lecture des valeurs du champ : ");
	      SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	      ISCRUTE(numdt);ISCRUTE(numo);
	      ret = -1;
	    }
	    _val = (unsigned char*) valr;
	  } else {

	    vale = (med_int*) calloc(ncomp*nval*ngauss,sizeof(med_int));
	    EXIT_IF(vale == NULL,NULL,NULL);

	    if (MEDfield23ValueWithProfileRd(fid, nomcha, numdt,numo, entite,type_geo[k],_meshname,
					     MED_COMPACT_PFLMODE, pflname, MED_NO_INTERLACE,MED_ALL_CONSTITUENT,
					     (unsigned char*) vale) < 0 ) {
	      MESSAGE("Erreur a la lecture des valeurs du champ : ");
	      SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	      ISCRUTE(numdt);ISCRUTE(numo);
	      ret = -1;
	    }
	    _val = (unsigned char*) vale;
	  }

	  /* Ecriture du champ destination */
	  MAJ_version_num(fid,3,0,8);
	  /*Sauvegarde du champ initial dans _pathtmp*/
	  EXIT_IF( H5Gmove(fid, _pathi, _pathtmp  ) < 0,"Switch to ",_pathtmp);
	
	  _groupexist=H5Lexists( fid, _pathf, H5P_DEFAULT );
	  EXIT_IF(!_groupexist,"Le champ devrait déjà existé",_pathf);
	  /*Déplacement du champ cible temporaire au chemin des champs écrits par MED */
	  if (_groupexist ) { EXIT_IF( (H5Gmove(fid, _pathf, _pathi  ) < 0) ,"Switch to ",_pathi); }

	  /*Ecriture du champ cible au nouveau format*/
/* 	  ISCRUTE(nval); */
	  MED_ERR_EXIT_IF( MEDfieldValueWithProfileWr(fid, _fieldname, numdt, numo, dt, entite,type_geo[k],
						      MED_COMPACT_PFLMODE, pflname, locname, MED_NO_INTERLACE, MED_ALL_CONSTITUENT,
						      nval, _val) < 0,
			   MED_ERR_WRITE,MED_ERR_FIELD,_fieldname);
	  free(_val);
	  /*Rétablissement des chemins d'accès au champ initial et au champ cible temporaire*/
	  EXIT_IF( H5Gmove(fid, _pathi  , _pathf  ) < 0,"Switch to ",_pathf);
	  EXIT_IF( H5Gmove(fid, _pathtmp, _pathi  ) < 0,"Switch to ",_pathi);

	  MAJ_version_num(fid,2,3,6);

/* 	  if ( strlen(locname) ) */
/* 	    printf("\t- Modèle de localisation des points de Gauss de nom |%s|\n",locname); */

/* 	  if (entite == MED_NODE_ELEMENT) */
/* 	    ngroup = (type_geo[k] % 100); */
/* 	  else */
/* 	    ngroup = ngauss; */

/* 	  switch (MED_NO_INTERLACE) { */

/* 	  case MED_FULL_INTERLACE : */
/* 	    printf("\t- Valeurs :\n\t"); */
/* 	    for (m=0;m<(nval*ngauss)/ngroup;m++) { */
/* 	      printf("|"); */
/* 	      for (n=0;n<ngroup*ncomp;n++) */
/* 		if (typcha == MED_FLOAT64) */
/* 		  printf(" %f ",*(valr+(m*ngroup*ncomp)+n)); */
/* 		else */
/* 		  printf(" "IFORMAT" ",*(vale+(m*ngroup*ncomp)+n)); */

/* 	    } */
/* 	    break; */

/* 	  case MED_NO_INTERLACE : */
/* 	    printf("\t- Valeurs :\n\t"); */
/* 	    for (m=0;m<ncomp;m++) { */
/* 	      printf("|"); */
/* 	      for (n=0;n<(nval*ngauss);n++) */
/* 		if (typcha == MED_FLOAT64) */
/* 		  printf(" %f ",*(valr+(m*nval)+n)); */
/* 		else */
/* 		  printf(" "IFORMAT" ",*(vale+(m*nval)+n)); */
/* 	    } */
/* 	    break; */
/* 	  } */

/* 	  printf("|\n"); */
/* 	  if (typcha == MED_FLOAT64) { */
/* 	    if ( valr ) {free(valr);valr = NULL;}} */
/* 	  else */
/* 	    if (vale) { free(vale);vale = NULL; } */

/* 	  if (strcmp(pflname,MED_NO_PROFILE) == 0 ) */
/* 	    printf("\t- Profil : MED_NO_PROFILE\n"); */
/* 	  else { */
/* 	    if ( (pflsize = MEDprofileSizeByName(fid,pflname)) <0 )  { */
/* 	      MESSAGE("Erreur a la lecture du nombre de valeurs du profil : "); */
/* 	      SSCRUTE(pflname); */
/* 	      ret = -1; continue; */
/* 	    } */

/* 	    printf("\t- Profil : |%s| de taille "IFORMAT"\n",pflname,pflsize); */

/* 	    pflval = (med_int*) malloc(sizeof(med_int)*pflsize); */
/* 	    EXIT_IF(pflval == NULL,NULL,NULL); */
/* 	    if ( MEDprofileRd(fid,pflname,pflval) <0) { */
/* 	      MESSAGE("Erreur a la lecture des valeurs du profil : "); */
/* 	      SSCRUTE(pflname); */
/* 	      ret = -1; */
/* 	    } */
/* 	    printf("\t"); */
/* 	    for (m=0;m<pflsize;m++) printf(" "IFORMAT" ",*(pflval+m)); */
/* 	    printf("\n"); */
/* 	    free(pflval); */
/* 	  } */
	}
      }
    }
  } /* fin for sur les mailles*/

  ret = 0;

 ERROR:
  return ret;
}
