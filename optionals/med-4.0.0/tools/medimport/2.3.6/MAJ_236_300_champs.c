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
#include "med.h"

#include <string.h>

#include "MAJ_236_300.h"
#include "MAJ_version.h"

void MAJ_236_300_champs(med_idt fid)
{
  med_err lret,ret;
  /*   med_idt         _datagroup=0; */
  med_field_type   typcha;
  char nomcha    [MED_NAME_SIZE+1]="";
  char _meshname [MED_NAME_SIZE+1]="";
  char _dtunit   [MED_SNAME_SIZE+1]="";
  char *comp= NULL, *unit= NULL;
  med_int   ncomp,ncha;
  med_int  _ncstp=0;
  med_bool _local=MED_FALSE;
  htri_t   _datasetexist;
  char _pathi[MED_FIELD_GRP_SIZE+1+MED_NAME_SIZE+1]=MED_FIELD_GRP;
  char _pathf[MED_FIELD_GRP_SIZE+2+MED_NAME_SIZE+1]="/CHA_/";
  char _pathtmp[MED_FIELD_GRP_SIZE+3]="/CHA__/";
  int i,j;

  char nomlien[MED_NAME_SIZE+1]="";
  char * lien = NULL;
  med_int nln,nval;

  med_int  _nloc,_intgeotype,_sdim;
  char     _pathloc[MED_LOCALIZATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_LOCALIZATION_GRP;

  med_int  _npar,_numdt,_numit;
  char     _pathpari[MED_NUMERICAL_DATA_GRP_SIZE+MED_NAME_SIZE+1+2*MED_MAX_PARA+1+1]=MED_NUMERICAL_DATA_GRP;
  char     _pathparf[MED_NUMERICAL_DATA_GRP_SIZE+MED_NAME_SIZE+1+2*MED_MAX_PARA+1+1]=MED_NUMERICAL_DATA_GRP;
  int      _pathparlen;
  med_size _n=0;
  char     _cpstnamei[2*MED_MAX_PARA+1]="";
  char     _cpstnamef[2*MED_MAX_PARA+1]="";
  char     _uniname[MED_SNAME_SIZE+1]="";
/*   hid_t    _lac_plist_id; */
  hid_t    _lcp_plist_id;
  hid_t    _ocp_plist_id;
  med_bool _createunt = MED_TRUE;

  MAJ_version_num(fid,2,3,6);

  /* MAJ des varaibles scalaires */
  _npar = MEDnParameter(fid);
  if (_npar > 0) {
    fprintf(stdout,"  >>> Normalisation des paramètres scalaires\n");
    _pathparf[MED_NUMERICAL_DATA_GRP_SIZE-2]='_';
/*     _lac_plist_id = H5Pcreate( H5P_LINK_ACCESS ); */
    _ocp_plist_id = H5Pcreate( H5P_OBJECT_COPY );
    _lcp_plist_id = H5Pcreate( H5P_LINK_CREATE );
    H5Pset_create_intermediate_group( _lcp_plist_id, 1 );
    H5Pset_copy_object( _ocp_plist_id, H5O_COPY_SHALLOW_HIERARCHY_FLAG);
  }

  for (i=0 ; i < _npar ; i++ ) {

    MED_ERR_EXIT_IF (_MEDobjectGetName(fid, _pathpari ,i, &_pathpari[MED_NUMERICAL_DATA_GRP_SIZE]) < 0,
		     MED_ERR_ACCESS,MED_ERR_DATAGROUP,_pathpari);


    strcpy(&_pathparf[MED_NUMERICAL_DATA_GRP_SIZE],&_pathpari[MED_NUMERICAL_DATA_GRP_SIZE]);
/*     SSCRUTE(_pathparf); */
/*     SSCRUTE(_pathpari); */

    /*Copie le group avec ses attributs et les objets de premier niveau.*/
    ret =  H5Ocopy(fid,_pathpari,fid,_pathparf,_ocp_plist_id,_lcp_plist_id);
/*     ret =  H5Lcopy(fid,_pathpari,fid,_pathparf,_lcp_plist_id,_lac_plist_id); */
    EXIT_IF(ret < 0,"Copie du datagroup",_pathpari);

    _pathparlen=strlen(_pathpari);
    _pathpari[_pathparlen]='/';_pathparf[_pathparlen]='/';
    ++_pathparlen;
    _pathpari[_pathparlen]='\0';_pathparf[_pathparlen]='\0';

/*     SSCRUTE(_pathparf); */
/*     SSCRUTE(_pathpari); */

    ret =_MEDnObjects(fid,_pathpari, &_n);
    MED_ERR_EXIT_IF( (ret == (MED_ERR_COUNT + MED_ERR_DATAGROUP)), MED_ERR_COUNT,MED_ERR_PARAMETER,_pathpari);

    for (j=0 ; j < _n ; ++j ) {
      MED_ERR_EXIT_IF (_MEDobjectGetName(fid, _pathpari ,j, &_pathpari[_pathparlen]) < 0,
		       MED_ERR_ACCESS,MED_ERR_DATAGROUP,_pathpari);

      MED_ERR_EXIT_IF (_MEDattributeNumRdByName(fid,_pathpari,MED_NOM_NOR,
						MED_INTERNAL_INT,(unsigned char * const ) &_numit) < 0,
		       MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NOR);

      MED_ERR_EXIT_IF (_MEDattributeNumRdByName(fid,_pathpari,MED_NOM_NDT,
						MED_INTERNAL_INT,(unsigned char * const ) &_numdt) < 0,
		       MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NDT);
      
      MED_ERR_EXIT_IF (_MEDattributeStringRdByName(fid,_pathpari,MED_NOM_UNI,
						MED_SNAME_SIZE,_uniname) < 0,
		       MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NDT);
      
      _MEDgetComputationStepName(MED_SORT_DTIT,_numdt,_numit,&_pathparf[_pathparlen]);
/*       strcat(_pathpari,"/"); */
/*       strcat(_pathparf,"/"); */
/*       SSCRUTE(_pathparf); */
/*       SSCRUTE(_pathpari); */

/*       ret =  H5Ocopy(fid,_pathpari,fid,_pathparf,_ocp_plist_id,_lcp_plist_id); */
/*       ret =  H5Lcopy(fid,_pathpari,fid,_pathparf,_lcp_plist_id,_lac_plist_id); */
/*       H5Eprint1(stderr); */
/*       EXIT_IF(ret < 0,"Copie d'une étape de calcul du paramètre scalaire ",_pathpari); */

      /*On modifie temporairement _pathpari pour pointer dans _pathparf*/
      _pathpari[MED_NUMERICAL_DATA_GRP_SIZE-2]='_';
/*       SSCRUTE(_pathparf); */
/*       SSCRUTE(_pathpari); */
      ret = H5Gmove(fid, _pathpari, _pathparf  );
      EXIT_IF(ret < 0,"Renommage de l'étape de calcul",_pathpari);
      _pathpari[MED_NUMERICAL_DATA_GRP_SIZE-2]='A';


      MED_ERR_EXIT_IF(H5Adelete_by_name( fid, _pathparf, MED_NOM_UNI, H5P_DEFAULT ) < 0,
		      MED_ERR_DELETE,MED_ERR_ATTRIBUTE,_pathparf);

      _pathparf[_pathparlen]='\0';
      _pathpari[_pathparlen]='\0';
      if ( _createunt ) {
	MED_ERR_EXIT_IF (_MEDattributeStringWrByName(fid,_pathparf,MED_NOM_UNT, MED_SNAME_SIZE,_uniname) < 0,
			 MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_UNT);
	_createunt = MED_FALSE;
      }
    }
    _createunt = MED_TRUE;
    _pathpari[MED_NUMERICAL_DATA_GRP_SIZE]='\0';
  }

  if ( _npar > 0 ) {

    _pathpari[MED_NUMERICAL_DATA_GRP_SIZE]='\0';
    _pathparf[MED_NUMERICAL_DATA_GRP_SIZE]='\0';
    MED_ERR_EXIT_IF ( H5Ldelete(fid,_pathpari,H5P_DEFAULT) < 0 ,
		      MED_ERR_DELETE,MED_ERR_LINK,_pathpari);

    ret = H5Gmove(fid, _pathparf, _pathpari  );
    EXIT_IF(ret < 0,"Renommage du group de paramètres scalaires",_pathparf);

  }

  /* MAJ des localisations */
  _nloc = MEDnLocalization(fid);
/*   ISCRUTE(_nloc); */
  if (_nloc > 0)
    fprintf(stdout,"  >>> Normalisation des localisations des points d'intégration\n");
  for (i=0 ; i < _nloc ; i++ ) {

/*     SSCRUTE(_pathloc); */
    MED_ERR_EXIT_IF (_MEDobjectGetName(fid, _pathloc ,i, &_pathloc[MED_LOCALIZATION_GRP_SIZE]) < 0,
		     MED_ERR_ACCESS,MED_ERR_DATAGROUP,_pathloc);
/*     SSCRUTE(_pathloc); */
    MED_ERR_EXIT_IF (_MEDattributeNumRdByName(fid,_pathloc,MED_NOM_GEO,
					      MED_INTERNAL_INT,(unsigned char * const ) &_intgeotype) < 0,
		     MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_GEO);
    _sdim = (_intgeotype/100);

    MED_ERR_EXIT_IF (_MEDattributeNumWrByName(fid,_pathloc,MED_NOM_DIM,
					      MED_INTERNAL_INT,(const unsigned char * const) &_sdim) < 0,
		     MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_DIM);

    MED_ERR_EXIT_IF ( _MEDattributeStringWrByName(fid,_pathloc,MED_NOM_INM,MED_NAME_SIZE,"") < 0,
		      MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_INM);
    _pathloc[MED_LOCALIZATION_GRP_SIZE]='\0';

  }

  /* MAJ des liens */
  nln = MEDnLink(fid);
  if (nln > 0)
    fprintf(stdout,"  >>> Normalisation des liens\n");
  for (i=1 ; i <= nln ; i++ ) {


    ret =  MEDlinkInfo(fid, i, nomlien, &nval);
    EXIT_IF(ret,"Erreur a la demande d'information sur le lien",NULL);

/*     printf("\t- Lien n°%i de nom |%s| et de taille "IFORMAT"\n",i,nomlien,nval); */

    lien = (char *) malloc((nval+1)*sizeof(char));
    EXIT_IF(lien == NULL,NULL,NULL);

    ret = MEDlinkRd(fid, nomlien, lien );
    EXIT_IF(ret,"Erreur a la lecture du lien : ",nomlien);

    MAJ_version_num(fid,3,0,8);
    ret = MED30linkWr(fid,nomlien,lien);
    EXIT_IF(ret,"Erreur a l'écrtiure du lien : ",nomlien);
    MAJ_version_num(fid,2,3,6);
 
    lien[nval] = '\0';
/*     printf("\t\t|%s|\n\n",lien); */

    free(lien);
  }

  /* combien de champs dans le fichier */
  ncha = MEDnField(fid);
  EXIT_IF(ncha < 0,"lors de la lecture du nombre de champs",NULL);

  /* MAJ des champs */
  for (i =0;i<ncha;i++) {
    lret = 0;

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

    /*     ret = MED231champInfoEtRen(fid,i+1,nomcha,&typcha,comp,unit,ncomp); */
    ret = MEDfieldInfo(fid,i+1,nomcha,_meshname,&_local,&typcha,comp,unit,_dtunit,&_ncstp);
    MED_ERR_EXIT_IF(ret,MED_ERR_ACCESS,MED_ERR_FIELD,nomcha);

    /* creation du champ destination */
    MAJ_version_num(fid,3,0,8);

    EXIT_IF( H5Gmove(fid, _pathi, _pathtmp  ) < 0,"Switch to ",_pathtmp);
    _datasetexist=H5Lexists( fid, _pathf, H5P_DEFAULT );
    if (_datasetexist ) { EXIT_IF( (H5Gmove(fid, _pathf, _pathi  ) < 0) ,"Switch to ",_pathi); }

    MED_ERR_EXIT_IF( MEDfieldCr(fid,nomcha,typcha,ncomp,comp,unit,_dtunit,_meshname ) < 0,
		     MED_ERR_CREATE,MED_ERR_FIELD,_pathf);
    EXIT_IF( H5Gmove(fid, _pathi  , _pathf  ) < 0,"Switch to ",_pathf);
    EXIT_IF( H5Gmove(fid, _pathtmp, _pathi  ) < 0,"Switch to ",_pathi);

    MAJ_version_num(fid,2,3,6);

    free(comp);
    free(unit);

    lret = MAJ_236_300_fieldOnEntity( fid, nomcha, _meshname, typcha, ncomp, MED_NODE,_ncstp, _pathi, _pathf);
    if (lret != 0) {
      MESSAGE("Erreur à la lecture des champs aux noeuds "); exit(1);
    }

    lret = MAJ_236_300_fieldOnEntity( fid, nomcha, _meshname, typcha, ncomp, MED_CELL,_ncstp, _pathi, _pathf);
    if (lret != 0) {
      MESSAGE("Erreur à la lecture des champs aux mailles "); exit(1);
    }

    lret = MAJ_236_300_fieldOnEntity( fid, nomcha, _meshname, typcha, ncomp, MED_DESCENDING_FACE,_ncstp, _pathi, _pathf);
    if (lret != 0) {
      MESSAGE("Erreur à la lecture des champs aux faces "); exit(1);
    }

    lret = MAJ_236_300_fieldOnEntity( fid, nomcha, _meshname, typcha, ncomp, MED_DESCENDING_EDGE,_ncstp, _pathi, _pathf);
    if (lret != 0) {
      MESSAGE("Erreur à la lecture des champs aux aretes "); exit(1);
    }

    lret = MAJ_236_300_fieldOnEntity( fid, nomcha, _meshname, typcha, ncomp, MED_NODE_ELEMENT,_ncstp, _pathi, _pathf);
    if (lret != 0) {
      MESSAGE("Erreur à la lecture des champs aux aretes "); exit(1);
    }

  }
  _datasetexist=H5Lexists( fid, _pathf, H5P_DEFAULT );

  if (_datasetexist ) {
    EXIT_IF( (H5Ldelete(fid,_pathi, H5P_DEFAULT) < 0) ,"Delete ",_pathi);
    EXIT_IF( (H5Gmove(fid, _pathf, _pathi  ) < 0) ,"Switch to ",_pathf);
  }


}
