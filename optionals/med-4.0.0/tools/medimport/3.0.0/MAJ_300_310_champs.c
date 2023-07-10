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

#include "MAJ_300_310.h"
#include "MAJ_version.h"

void MAJ_300_310_champs(med_idt fid)
{
  med_err ret;
  med_field_type   typcha;
  char nomcha    [MED_NAME_SIZE+1]="";
  char _meshname [MED_NAME_SIZE+1]="";
  char _dtunit   [MED_SNAME_SIZE+1]="";
  char *comp= NULL, *unit= NULL;
  med_int   ncomp,ncha;
  med_int  _ncstp=0;
  med_bool _local=MED_FALSE;
  htri_t   _datasetexist;
  char _pathi[(MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1)+2*MED_MAX_PARA+1  ] = MED_FIELD_GRP;
  char _pathf[(MED_FIELD_GRP_SIZE+1+MED_NAME_SIZE+1)+2*MED_MAX_PARA+1] = "/CHA_/";
  char _pathtmp[MED_FIELD_GRP_SIZE+3]="/CHA__/";
  int i=0,_num=0;
  med_size _sizei=0,_sizef=0;
  char    _cstpname[2*MED_MAX_PARA+1]="";

  MAJ_version_num(fid,3,0,8);

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

    ret = MEDfieldInfo(fid,i+1,nomcha,_meshname,&_local,&typcha,comp,unit,_dtunit,&_ncstp);
    MED_ERR_EXIT_IF(ret,MED_ERR_ACCESS,MED_ERR_FIELD,nomcha);

    /* Création du champ destination */
    /* ATTENTION appel de MAJ_version avec fid,num 
       et non juste fid --> futures procédures de MAJ_310_... */
    /* MAJ_version(fid); */
    MAJ_version_num(fid,3,1,0);

    EXIT_IF( H5Gmove(fid, _pathi, _pathtmp  ) < 0,"Switch to ",_pathtmp);
    /* Nous sommes dans une boucle sur les champs : 
       On récupère donc les champs en cours de transformation
    */
    _datasetexist=H5Lexists( fid, _pathf, H5P_DEFAULT );
    if (_datasetexist ) { EXIT_IF( (H5Gmove(fid, _pathf, _pathi  ) < 0) ,"Switch to ",_pathi); }
    /* Ajout du champ au nouveau format dans le _pathi */
    MED_ERR_EXIT_IF( MEDfieldCr(fid,nomcha,typcha,ncomp,comp,unit,_dtunit,_meshname ) < 0,
		     MED_ERR_CREATE,MED_ERR_FIELD,_pathf);

    EXIT_IF( H5Gmove(fid, _pathi  , _pathf  ) < 0,"Switch to ",_pathf);
    EXIT_IF( H5Gmove(fid, _pathtmp, _pathi  ) < 0,"Switch to ",_pathi);

    strncpy(&_pathi[MED_FIELD_GRP_SIZE]  ,nomcha,MED_NAME_SIZE+1);
    strncpy(&_pathf[MED_FIELD_GRP_SIZE+1],nomcha,MED_NAME_SIZE+1);
    _sizei = MED_FIELD_GRP_SIZE+strlen(nomcha);
    _sizef = _sizei+1;
    _pathi[_sizei]='/';++_sizei; 
    _pathf[_sizef]='/';++_sizef; 
  
 
    for (_num=0; _num<_ncstp; ++_num) {
 
      _pathi[_sizei]='\0';
      _pathf[_sizef]='\0';
      /* SSCRUTE(_pathi); */
      /* SSCRUTE(_pathf); */
   
      /*
       * On recupere le nom de la séquence de calcul 
       */

      /* Attention : On enlève les groupes DTIT au fur et à mesure,
	 il ne faut pas utiliser _num mais 0.
	ret =  _MEDobjectGetName(fid, _pathi ,_num, &_pathi[_sizei]) < 0;
      */
      ret =  _MEDobjectGetName(fid, _pathi ,0, &_pathi[_sizei]) < 0;
      MED_ERR_EXIT_IF(ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_pathi);

      strncpy(&_pathf[_sizef],&_pathi[_sizei],2*MED_MAX_PARA+1);

      /* SSCRUTE(_pathi); */
      /* SSCRUTE(_pathf); */
      EXIT_IF(  H5Lmove( fid, _pathi, fid, _pathf, H5P_DEFAULT, H5P_DEFAULT ) < 0, "Failed to move field ",_pathi); 
    }

    _pathi[MED_FIELD_GRP_SIZE]='\0';
    _pathf[MED_FIELD_GRP_SIZE+1]='\0';

    
    MAJ_version_num(fid,3,0,8);
    
    free(comp);
    free(unit);

  }
  
  _datasetexist=H5Lexists( fid, _pathf, H5P_DEFAULT );

  if (_datasetexist ) {
    EXIT_IF( (H5Ldelete(fid,_pathi, H5P_DEFAULT) < 0) ,"Delete ",_pathi);
    EXIT_IF( (H5Gmove(fid, _pathf, _pathi  ) < 0) ,"Switch to ",_pathf);
  }

   /* _MEDobjetsOuverts(fid); */

}
