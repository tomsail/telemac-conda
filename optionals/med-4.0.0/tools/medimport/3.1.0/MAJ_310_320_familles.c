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

#include "MAJ_310_320.h"
#include "MAJ_version.h"

static  void _move_families(med_idt fid, char * const _pathi,char * const _pathf) {
  med_err _ret=0;
  med_int _nfam=0;
  med_int _sizei,_sizef;
  med_size _size=0;
  int i=0;

  _ret=_MEDnObjects(fid,_pathi,&_size);
  MED_ERR_EXIT_IF(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_pathi);
  _nfam = (med_int ) _size;
  
  _sizef=strlen(_pathf); _sizei =_sizef-1;
    
  /* MAJ des familles */
  for (i=0;i<_nfam;i++) {

    _pathi[_sizei]='/';
    _pathf[_sizef]='/';
    _pathi[++_sizei]='\0';
    _pathf[++_sizef]='\0';
    /* SSCRUTE(_pathi); */
    /* SSCRUTE(_pathf); */
      
    /*
     * On recupere le nom de la famille 
     */

    /* Attention : On enlève les groupes fam au fur et à mesure,
       il ne faut pas utiliser _num mais 0.
       ret =  _MEDobjectGetName(fid, _pathi ,_num, &_pathi[_sizei]) < 0;
    */
    _ret =  _MEDobjectGetName(fid, _pathi ,0, &_pathi[_sizei]) < 0;
    MED_ERR_EXIT_IF(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_pathi);

    strncpy(&_pathf[_sizef],&_pathi[_sizei],MED_NAME_SIZE+1);

    /* SSCRUTE(_pathi); */
    /* SSCRUTE(_pathf); */

    EXIT_IF(  H5Lmove( fid, _pathi, fid, _pathf, H5P_DEFAULT, H5P_DEFAULT ) < 0, "Failed to move family ",_pathi);
    --_sizei;--_sizef;
  }
}


void MAJ_310_320_familles(med_idt fid)
{
  med_err  _ret=0;
  htri_t   _datasetexist;
  char     _pathi[(MED_FAMILY_GRP_SIZE+MED_NAME_SIZE+1)  + MED_TAILLE_FAS_ENTITE+MED_NAME_SIZE+1] = MED_FAMILY_GRP;
  char     _pathf[(MED_FAMILY_GRP_SIZE+1+MED_NAME_SIZE+1)+ MED_TAILLE_FAS_ENTITE+MED_NAME_SIZE+1] = "/FAS_/";
  /* char _pathtmp[MED_FAMILY_GRP_SIZE+3]="/FAS__/"; */
  int      _meshi=0;
  med_size _sizei=0,_sizef=0,_size=0,_nmesh=0;
  med_idt  _famId=0,_famMeshId=0,_famZeId=0,_famElId=0,_famNoeId=0;
  MAJ_version_num(fid,3,1,0);

  /*Itération sur les différents maillages présents dans le groupe famille*/
  _ret=_MEDnObjects(fid,_pathi,&_nmesh);
  MED_ERR_EXIT_IF(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_pathi);

  for (_meshi=0;_meshi<_nmesh;_meshi++) {

    /*
     * On recupere le nom du maillage
     */
    _pathi[MED_FAMILY_GRP_SIZE  ]='\0';
    _pathf[MED_FAMILY_GRP_SIZE+1]='\0';

   _ret =  _MEDobjectGetName(fid, _pathi ,_meshi, &_pathi[MED_FAMILY_GRP_SIZE]) < 0;
    MED_ERR_EXIT_IF(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_pathi);
    /* SSCRUTE(_pathi); */

    /*Création du chemin cible _pathf s'il n'existe pas dans le fichier*/
    _datasetexist=H5Lexists( fid, _pathf, H5P_DEFAULT );
    if(!_datasetexist) {
      _famId = _MEDdatagroupCreer(fid,_pathf);
      MED_ERR_EXIT_IF(_famId < 0, MED_ERR_CREATE,MED_ERR_FAMILY,_pathf);
      if (_famId>0) _ret=_MEDdatagroupFermer(_famId);
      MED_ERR_EXIT_IF(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_pathf);
    }
    /*Création de la famille sur le maillage courant*/
    strncpy(&_pathf[MED_FAMILY_GRP_SIZE+1],&_pathi[MED_FAMILY_GRP_SIZE],MED_NAME_SIZE+1);
    _datasetexist=H5Lexists( fid, _pathf, H5P_DEFAULT );
    if(!_datasetexist) {
      _famMeshId = _MEDdatagroupCreer(fid,_pathf);
      MED_ERR_EXIT_IF(_famMeshId < 0, MED_ERR_CREATE,MED_ERR_FAMILY,_pathf);
      if (_famMeshId>0) _ret=_MEDdatagroupFermer(_famMeshId);
      MED_ERR_EXIT_IF(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_pathf);
    }

    _sizef=strlen(_pathf);_sizei =_sizef-1;

    /* Si une famille d'éléments existe ds pathi crée la dans _path */
    strncpy(&_pathi[_sizei],MED_FAS_NOEUD,MED_TAILLE_FAS_ENTITE-1);
    _pathi[_sizei+(MED_TAILLE_FAS_ENTITE-1)]='\0';
    _datasetexist=H5Lexists( fid, _pathi, H5P_DEFAULT );
    if (_datasetexist) {
      strncpy(&_pathf[_sizef],MED_FAS_NOEUD,MED_TAILLE_FAS_ENTITE-1);
      _pathf[_sizef+(MED_TAILLE_FAS_ENTITE-1)]='\0';
     _famNoeId = _MEDdatagroupCrOrderCr(fid,_pathf);
      MED_ERR_EXIT_IF(_famNoeId < 0, MED_ERR_CREATE,MED_ERR_FAMILY,_pathf);
      if (_famNoeId>0) _ret=_MEDdatagroupFermer(_famNoeId);
      MED_ERR_EXIT_IF(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_pathf);
      _move_families( fid, _pathi,_pathf);
    }
  

    /* Si une famille de noeud existe crée la dans _path */
    strncpy(&_pathi[_sizei],MED_FAS_ELEME,MED_TAILLE_FAS_ENTITE-1);
    _pathi[_sizei+(MED_TAILLE_FAS_ENTITE-1)]='\0';
    _datasetexist=H5Lexists( fid, _pathi, H5P_DEFAULT );
    if (_datasetexist) {
      strncpy(&_pathf[_sizef],MED_FAS_ELEME,MED_TAILLE_FAS_ENTITE-1);
      _pathf[_sizef+(MED_TAILLE_FAS_ENTITE-1)]='\0';
     _famElId = _MEDdatagroupCrOrderCr(fid,_pathf);
      MED_ERR_EXIT_IF(_famElId < 0, MED_ERR_CREATE,MED_ERR_FAMILY,_pathf);
      if (_famElId>0) _ret=_MEDdatagroupFermer(_famElId);
      MED_ERR_EXIT_IF(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_pathf);
      _move_families( fid, _pathi,_pathf);
    }

    /* Création de la famille zéro dans _pathf */
    strncpy(&_pathf[_sizef],MED_FAS_ZERO,MED_TAILLE_FAS_ZERO-1);
    _pathf[_sizef+(MED_TAILLE_FAS_ZERO-1)]='\0';
    _famZeId = _MEDdatagroupCrOrderCr(fid,_pathf);
    MED_ERR_EXIT_IF(_famZeId < 0, MED_ERR_CREATE,MED_ERR_FAMILY,_pathf);
    if (_famZeId>0) _ret=_MEDdatagroupFermer(_famZeId);
    MED_ERR_EXIT_IF(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_pathf);
  }

  _pathi[MED_FAMILY_GRP_SIZE  ]='\0';
  _pathf[MED_FAMILY_GRP_SIZE+1]='\0';
  _datasetexist=H5Lexists( fid, _pathf, H5P_DEFAULT );

  if (_datasetexist ) {
    EXIT_IF( (H5Ldelete(fid,_pathi, H5P_DEFAULT) < 0) ,"Delete ",_pathi);
    EXIT_IF( (H5Gmove(fid, _pathf, _pathi  ) < 0) ,"Switch to ",_pathf);
  }

   /* _MEDobjetsOuverts(fid); */

   /* MAJ_version_num(fid,3,1,0); */

}
