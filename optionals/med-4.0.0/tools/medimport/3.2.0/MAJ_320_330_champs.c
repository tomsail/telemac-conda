/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2016  EDF R&D, CEA/DEN
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
#include "med.h"

#include <string.h>

#include "MAJ_320_330.h"
#include "MAJ_version.h"

#define USER_MODE MED_COMPACT_STMODE

med_err getFieldsOn(med_idt                  fid,
		    const char * const       nommaa,
		    med_int                  nmodels,
		    const med_geometry_type* geotype_elst,
		    const char *             geotypename_elst,
		    const char * const       nomcha,
		    const char * const       dtunit,
		    const med_field_type     typcha,
		    const med_int            ncomp,
		    const char * const       comp,
		    const char * const       unit, 
		    const med_entity_type    entite,
		    const med_switch_mode    stockage,
		    const med_int            ncstp) {

  int       i,j,k,l,m,n,nb_geo=0;
  med_int   nbpdtnor=0,pflsize,*pflval,ngauss=0,ngroup,nval;
  med_int   numdt=0,numo=0,nprofile=0;
  med_int   meshnumdt=0, meshnumit=0 ;
  med_size  medtype_size=0;
  med_size  _sizei=0,_sizef=0,_sizeNbr=0;
  med_float dt=0.0;
  unsigned char *val = NULL; 
  med_err  ret=0;
  char     pflname      [MED_NAME_SIZE+1]="";
  char     _profilename [MED_NAME_SIZE+1]="";
  char     locname [MED_NAME_SIZE+1]="";
  char     _pathi[(MED_FIELD_GRP_SIZE  +MED_NAME_SIZE+1)+2*MED_MAX_PARA+1] = MED_FIELD_GRP;
  char     _pathf[(MED_FIELD_GRP_SIZE+1+MED_NAME_SIZE+1)+2*MED_MAX_PARA+1] = "/CHA_/";
  char     _pathtmp[MED_FIELD_GRP_SIZE+3]="/CHA__/";
  char     _getNBR[(MED_FIELD_GRP_SIZE+1+MED_NAME_SIZE+1)+(2*MED_MAX_PARA+1)+(2*MED_TAILLE_NOM_ENTITE+2)+(MED_NAME_SIZE+1)+MED_TAILLE_NOM_ENTITE+1] = MED_FIELD_GRP;
  char     _entitygeotypename[2*MED_TAILLE_NOM_ENTITE+2]="";
  char     * lien = NULL;
  med_bool            _fieldexist = MED_FALSE;
  med_bool            _meshexist  = MED_FALSE;
  med_int             _nentFromMesh  = 0;
  med_int             _nentFromField = 0;
  med_int             _nent           = 0;
  med_geometry_type   *type_geo;
  med_data_type       meddatatype    = MED_CONNECTIVITY;
  med_bool            changement     = MED_FALSE;
  med_bool            transformation = MED_FALSE;
  htri_t              _datasetexist;

  const char * const * AFF;
  const char * const * AFF_ENT=MED_GET_ENTITY_TYPENAME+1;
  const char * * AFF_STRUCT = NULL;

  switch (entite) {
  case MED_NODE :
    type_geo = MED_GET_NODE_GEOMETRY_TYPE;
    nb_geo   = MED_N_NODE_FIXED_GEO;
    AFF      = MED_GET_NODE_GEOMETRY_TYPENAME;
    meddatatype = MED_COORDINATE;
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
  case  MED_STRUCT_ELEMENT :
    AFF_STRUCT = (const char * *) calloc(sizeof(const char * ),nmodels+1);
    for(i=0;i<nmodels;++i) AFF_STRUCT[i+1]= &geotypename_elst[(MED_NAME_SIZE+1)*i];
    type_geo = (med_geometry_type*)(geotype_elst)-1;
    nb_geo   = nmodels;
    AFF      = AFF_STRUCT;
    break;

  }

   
  for (k=1;k<=nb_geo;k++) {

    /* Combien de séquences (PDT,NOR) a lire */
    nbpdtnor = ncstp;
    if (nbpdtnor < 1 ) continue;

    for (j=0;j<nbpdtnor;j++) {
	
      if ( MEDfieldComputingStepMeshInfo(fid, nomcha, j+1, &numdt, &numo,
					 &dt, &meshnumdt, &meshnumit) < 0 ) {
	MESSAGE("Erreur a l'appel de MEDfieldComputingStepMeshInfo : ");
	EXIT_IF(NULL == NULL,NULL,NULL);
      }

      if ( (nprofile = MEDfieldnProfile(fid,nomcha,numdt,numo,entite,type_geo[k],
					 pflname,locname   ) ) < 0 ) {
	MESSAGE("Erreur a la demande du nombre de profils referencés par le champ : ");
	SSCRUTE(nomcha);
	ISCRUTE(numdt); ISCRUTE(numo);
	ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	SSCRUTE(AFF_ENT[(int)entite]);SSCRUTE(AFF[k]);
	EXIT_IF(NULL == NULL,NULL,NULL);
      };


      for (l=0;l<nprofile;l++) {

	if ( (nval = MEDfieldnValueWithProfile(fid, nomcha, numdt, numo, entite, type_geo[k],
					       l+1,  USER_MODE, pflname, &pflsize,
					       locname, &ngauss) ) < 0 ) {
	  MESSAGE("Erreur a la lecture du nombre de valeurs du champ : ");
	  SSCRUTE(nomcha);ISCRUTE(numdt);ISCRUTE(numo);
	  ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	  ISCRUTE_int(USER_MODE);
          EXIT_IF(NULL == NULL,NULL,NULL);
	};

 /* 	printf("\n  +Pas de Temps n."IFORMAT" (%f) [%s], n. d'ordre "IFORMAT", avec "IFORMAT" valeur(s) par entité.\n",numdt,dt,dtunit,numo,ngauss); */
 /* 	printf("\t- Il y a "IFORMAT" entités qui portent des valeurs en mode %i. Chaque entite %s\ */
 /* de type geometrique %s associes au profile |%s| a "IFORMAT" valeurs associées \n", */
 /* 	       nval,USER_MODE,AFF_ENT[(int)entite],AFF[k],pflname,ngauss); */

	/*Lecture des valeurs du champ */
	switch(typcha)  {
	case MED_FLOAT64: medtype_size=sizeof(med_float64); break;
	case MED_FLOAT32: medtype_size=sizeof(med_float32); break;
	case MED_INT32  : medtype_size=sizeof(med_int32  ); break;
	case MED_INT64  : medtype_size=sizeof(med_int64  );
	  break;
	case MED_INT    : medtype_size=sizeof(med_int)    ; break;
	default:
	  MESSAGE("Erreur a la lecture du type de champ : ");
	  ISCRUTE_int(typcha);
          EXIT_IF(NULL == NULL,NULL,NULL);
	}

	val = (unsigned char*) calloc(ncomp*nval*ngauss,medtype_size);
	EXIT_IF(val == NULL,"L'appel de calloc a échoué ",NULL);
	
	if (MEDfieldValueWithProfileRd(fid, nomcha, numdt, numo, entite,type_geo[k],
				       USER_MODE, pflname, stockage, MED_ALL_CONSTITUENT, val) < 0 ) {
	  MESSAGE("Erreur a la lecture des valeurs du champ : ");
	  SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	  ISCRUTE(numdt);ISCRUTE(numo);
          EXIT_IF(NULL == NULL,NULL,NULL);
	}
	
	strncpy(&_getNBR[MED_FIELD_GRP_SIZE],nomcha,MED_NAME_SIZE+1);
	_sizeNbr = MED_FIELD_GRP_SIZE+strlen(nomcha);
	_getNBR[_sizeNbr]='/';++_sizeNbr;
	MED_ERR_EXIT_IF(_MEDgetComputationStepName(MED_SORT_DTIT,numdt,numo,&_getNBR[_sizeNbr])<0,
			MED_ERR_CALL,MED_ERR_API,"_MEDgetEntityGeoTypeName");
	_sizeNbr += 2*MED_MAX_PARA;
	_getNBR[_sizeNbr]='/';++_sizeNbr;
	MED_ERR_EXIT_IF(_MEDgetFieldEntityGeoTypeName(fid,_entitygeotypename,entite,type_geo[k])<0,
			MED_ERR_CALL,MED_ERR_API,"_MEDgetFieldEntityGeoTypeName");
	strncpy(&_getNBR[_sizeNbr],_entitygeotypename,2*MED_TAILLE_NOM_ENTITE+2);
	_sizeNbr += strlen(_entitygeotypename);
	_getNBR[_sizeNbr]='/';++_sizeNbr;
	if ( strlen(pflname) )
	  strncpy(_profilename,pflname,MED_NAME_SIZE+1);
	else
	  strncpy(_profilename,MED_NO_PROFILE_INTERNAL,MED_NAME_SIZE+1);
	strncpy(&_getNBR[_sizeNbr],_profilename,MED_NAME_SIZE+1);
	_sizeNbr += strlen(_profilename);

	/* SSCRUTE(_getNBR); */
	MED_ERR_EXIT_IF(_MEDattributeNumRdByName( fid, _getNBR, MED_NOM_NBR, MED_INTERNAL_INT,  (unsigned char *) &_nentFromField) <0,
			MED_ERR_CALL,MED_ERR_API,"_MEDattributeStringRdByName");
	/* ISCRUTE_int(_nentFromField); */
	  
	EXIT_IF( H5Gmove(fid, _pathi, _pathtmp ) < 0,"Switch to ",_pathtmp);
	/* Nous sommes dans une boucle sur les champs : 
	   On récupère donc les champs en cours de transformation dans _pathtmp
	   et on les replace ds _pathi pour y travailler.
	*/
	_datasetexist=H5Lexists( fid, _pathf, H5P_DEFAULT );
	if (_datasetexist ) { EXIT_IF( (H5Gmove(fid, _pathf, _pathi  ) < 0) ,"Switch to ",_pathi); }

 	/* Création du champ destination */
	/* ATTENTION appel de MAJ_version avec fid,num 
	   et non juste fid --> futures procédures de MAJ_330_... */
	/* MAJ_version(fid); */
	MAJ_version_num(fid,3,3,0);

	/* Ajout du champ au nouveau format dans le _pathi */
	/* Le champ peut déjà avoir été crée s'il y a plusieurs étapes de calcul */
	MEDfileObjectExist(fid,MED_FIELD,nomcha,&_fieldexist);
	if (!_fieldexist) {
	  MED_ERR_EXIT_IF( MEDfieldCr(fid,nomcha,typcha,ncomp,comp,unit,dtunit,nommaa ) < 0,
			   MED_ERR_CREATE,MED_ERR_FIELD,nomcha);
	}

	MEDfileObjectExist(fid,MED_MESH,nommaa,&_meshexist);
	if (_meshexist) {
	  if ( (_nentFromMesh = MEDmeshnEntity( fid, nommaa, meshnumdt, meshnumit, entite, type_geo[k],
						meddatatype, MED_NODAL, &changement, &transformation )
		) < 0 )
	    if ( (_nentFromMesh = MEDmeshnEntity( fid, nommaa, meshnumdt, meshnumit, entite, type_geo[k],
						  meddatatype, MED_DESCENDING, &changement, &transformation )
		  ) < 0 ) {
	      MESSAGE("Erreur a la lecture du nombre total d'entités supports au champ : ");
	      SSCRUTE(nomcha); ISCRUTE(numdt); ISCRUTE(numo);
	      SSCRUTE(nommaa); ISCRUTE(meshnumdt); ISCRUTE(meshnumit);
	      ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	      SSCRUTE(AFF_ENT[(int)entite]);SSCRUTE(AFF[k]);
	      EXIT_IF(_nentFromMesh < 0, NULL,NULL);  
	    }
	  MED_ERR_EXIT_IF ( _nentFromMesh != _nentFromField, MED_ERR_INVALID, MED_ERR_ATTRIBUTE,
			    "Le nombre global d'entités support au champ ne correspond pas au nombre d'entitées"
			    " du maillage associé !" );
	  _nent = _nentFromMesh;
	  /* ISCRUTE_int(_nentFromMesh); */

	} else {
	  _nent = _nentFromField;
	  fprintf(stdout,"  ... Le maillage [%s] associé au champ n'est pas local,"
		  " utilisation du champ pour déterminer le nbre global d'entités [%s] : "IFORMAT" ...\n",
		  nommaa,_entitygeotypename,_nent);
	}

	
	MED_ERR_EXIT_IF( MEDfieldValueWithProfileWr(fid, nomcha ,numdt, numo , dt,
						    entite, type_geo[k], USER_MODE, pflname,
						    locname, stockage, MED_ALL_CONSTITUENT,
						    _nent, (unsigned char*) val) < 0,
	                 MED_ERR_WRITE,MED_ERR_FIELD,_pathf);

	MED_ERR_EXIT_IF( MEDfieldComputingStepMeshWr(fid, nomcha, numdt, numo, meshnumdt, meshnumit ) < 0,
	                 MED_ERR_WRITE,MED_ERR_FIELD,nomcha);

	
	EXIT_IF( H5Gmove(fid, _pathi  , _pathf  ) < 0,"Switch to ",_pathf);
	EXIT_IF( H5Gmove(fid, _pathtmp, _pathi  ) < 0,"Switch to ",_pathi);
	
	if ( val ) {free(val);val = NULL;}
	
	MAJ_version_num(fid,3,2,1);
	fprintf(stdout,"  ... Normalisation des données de type entier effectuée...\n");

      } /* fin for sur les différents profils */
    } /* fin for sur les étapes de calcul */
  } /* fin for sur les types géométriques de maille */

  free(AFF_STRUCT);
  return ret;
}


  
void MAJ_320_330_champs(med_idt fid)
{
  med_err         ret=0,lret=0;
  med_field_type  typcha;
  char            nomcha   [MED_NAME_SIZE+1]="";
  char            meshname [MED_NAME_SIZE+1]="";
  char            dtunit   [MED_SNAME_SIZE+1]="";
  char            *comp= NULL, *unit= NULL;
  med_int         ncomp,ncha;
  med_int         ncstp=0;
  med_bool        local=MED_FALSE;
  htri_t          _datasetexist;
  char _pathi[(MED_FIELD_GRP_SIZE  +MED_NAME_SIZE+1)+2*MED_MAX_PARA+1] = MED_FIELD_GRP;
  char _pathf[(MED_FIELD_GRP_SIZE+1+MED_NAME_SIZE+1)+2*MED_MAX_PARA+1] = "/CHA_/";
  char _pathtmp[MED_FIELD_GRP_SIZE+3]="/CHA__/";
  int              i=0,_num=0;
  /* char            _cstpname[2*MED_MAX_PARA+1]=""; */
  med_int             _nmodels=0;
  med_switch_mode     mode_coo = MED_NO_INTERLACE;
  
  med_geometry_type * geotype_elst;
  char              * geotypename_elst;

  med_geometry_type _geotype=MED_NONE;
  char              _elementname[MED_NAME_SIZE+1]="";
  med_int           _elementdim=0;
  char              _supportmeshname[MED_NAME_SIZE+1]="";
  med_entity_type   _entitytype=MED_UNDEF_ENTITY_TYPE;
  med_int           _nnode=0;
  med_int           _ncell=0;
  med_geometry_type _geocelltype=MED_NONE;
  char              _geocelltypename[MED_SNAME_SIZE+1]="";
  med_int           _nconstantattribute=0;
  med_bool          _anyprofile=MED_FALSE;
  med_int           _nvariableattribute=0;

  MAJ_version_num(fid,3,2,1);

  _nmodels = MEDnStructElement(fid);
  EXIT_IF(_nmodels < 0,"lors de la lecture du nombre d'éléments de structure",NULL);

  /* nmailles_elst     = (med_int *)           malloc(_nmodels*sizeof(med_int)); */
  geotype_elst      = (med_geometry_type *) malloc(_nmodels*sizeof(med_geometry_type));
  geotypename_elst  = (char *)              malloc(_nmodels*sizeof(char)*(MED_NAME_SIZE+1));

  for (i=0; i < _nmodels; i++) {
    ret= MEDstructElementInfo(fid, i+1, &geotypename_elst[i*(MED_NAME_SIZE+1)], &geotype_elst[i],
			       &_elementdim, _supportmeshname,
			       &_entitytype, &_nnode, &_ncell, &_geocelltype,
			       &_nconstantattribute, &_anyprofile, &_nvariableattribute );
    EXIT_IF(ret < 0,"lors de la demande d'information sur les éléments de structure",NULL);

  }

  /* combien de champs dans le fichier */
  ncha = MEDnField(fid);
  EXIT_IF(ncha < 0,"lors de la lecture du nombre de champs",NULL);

  /* MAJ des champs */
  for (i=0;i<ncha;i++) {

    /* Lecture du nombre de composantes */
    ncomp = MEDfieldnComponent(fid,i+1);
    if (ncomp < 0) {
      MESSAGE("Erreur à la lecture du nombre de composantes : "); ISCRUTE(ncomp);
      exit(1);
    }

    /* Lecture du type du champ, des noms des composantes et du nom de l'unité*/
    comp = (char*) malloc(ncomp*MED_SNAME_SIZE+1);
    EXIT_IF(comp == NULL,NULL,NULL);
    unit = (char*) malloc(ncomp*MED_SNAME_SIZE+1);
    EXIT_IF(unit == NULL,NULL,NULL);

    ret = MEDfieldInfo(fid,i+1,nomcha,meshname,&local,&typcha,comp,unit,dtunit,&ncstp);
    MED_ERR_EXIT_IF(ret,MED_ERR_ACCESS,MED_ERR_FIELD,nomcha);

    if ( (typcha != MED_INT32) && (typcha != MED_INT64) ) goto CONT;

    fprintf(stdout,"  >>> Normalisation du champ [%s] \n",nomcha);
    
    /* champs aux noeuds */
    lret = getFieldsOn(fid, meshname, _nmodels, geotype_elst,geotypename_elst,
		       nomcha, dtunit, typcha, ncomp, comp, unit, MED_NODE, mode_coo, ncstp);

    /* champs sur les elements et aux points de Gauss */
    if (lret == 0) lret = getFieldsOn(fid,  meshname, _nmodels, geotype_elst, geotypename_elst,
				      nomcha, dtunit, typcha, ncomp, comp, unit, MED_CELL, mode_coo, ncstp);
    else { MESSAGE("Erreur à la lecture des champs aux noeuds "); ret = -1; continue;}
   
    if (lret == 0) lret = getFieldsOn(fid,  meshname, _nmodels, geotype_elst, geotypename_elst,
				      nomcha, dtunit, typcha, ncomp, comp, unit, MED_DESCENDING_FACE, mode_coo, ncstp);
    else { MESSAGE("Erreur à la lecture des champs aux mailles "); ret = -1; continue;}
   
    if (lret == 0) lret = getFieldsOn(fid,  meshname, _nmodels, geotype_elst, geotypename_elst,
				      nomcha, dtunit, typcha, ncomp, comp, unit, MED_DESCENDING_EDGE, mode_coo, ncstp);
    else {MESSAGE("Erreur à la lecture des champs aux faces "); ret = -1; continue;}
    
    if (lret == 0) lret = getFieldsOn(fid,  meshname, _nmodels, geotype_elst, geotypename_elst,
				      nomcha, dtunit, typcha, ncomp, comp, unit, MED_NODE_ELEMENT,mode_coo, ncstp);
    else {MESSAGE("Erreur a la lecture des champs aux aretes "); ret = -1; continue;}

    if  (lret != 0) {MESSAGE("Erreur a la lecture des champs aux noeuds des mailles "); ret = -1;};

    if (_nmodels)
      lret = getFieldsOn(fid,  meshname, _nmodels, geotype_elst,geotypename_elst,
			 nomcha, dtunit, typcha, ncomp, comp, unit, MED_STRUCT_ELEMENT,mode_coo, ncstp);
    if  (lret != 0) {MESSAGE("Erreur a la lecture des champs aux éléments de sructure "); ret = -1;};

    /*A ce stade _pathf contient les champs au nouveau format */
    /*A ce stade _pathi contient les champs initiaux */
    strncpy(&_pathi[MED_FIELD_GRP_SIZE]  ,nomcha,MED_NAME_SIZE+1);
    strncpy(&_pathf[MED_FIELD_GRP_SIZE+1],nomcha,MED_NAME_SIZE+1);
    
    EXIT_IF( (H5Ldelete(fid, _pathi , H5P_DEFAULT) < 0) ,"Delete "   ,_pathi);
    /* if ( H5Ldelete(fid, _pathi , H5P_DEFAULT) < 0) H5Eprint(stderr); */
    EXIT_IF( (H5Gmove  (fid, _pathf , _pathi     ) < 0) ,"Moving  ",_pathf);
    /* if (H5Gmove  (fid, _pathf , _pathi     ) < 0) H5Eprint(stdout); */

    fprintf(stdout,"  >>> Normalisation du champ [%s] : ... OK ... \n",nomcha);

    strncpy(_pathi,MED_FIELD_GRP,MED_FIELD_GRP_SIZE+1   );
    strncpy(_pathf,"/CHA_/"     ,MED_FIELD_GRP_SIZE+1 +1);

  CONT:

    free(comp);
    free(unit);
    /* MAJ_version_num(fid,3,2,1); */
  }
  
  _datasetexist=H5Lexists( fid, _pathf, H5P_DEFAULT );
  if (_datasetexist ) {
    /* EXIT_IF( (H5Ldelete(fid,_pathi, H5P_DEFAULT) < 0) ,"Delete ",_pathi); */
    /* EXIT_IF( (H5Gmove(fid, _pathf, _pathi  ) < 0) ,"Switch to ",_pathf); */
    EXIT_IF( (H5Ldelete(fid,_pathf, H5P_DEFAULT) < 0) ,"Delete ",_pathf);
  }
  free(geotype_elst);
  free(geotypename_elst);

  /* _MEDobjetsOuverts(fid);  */

}
