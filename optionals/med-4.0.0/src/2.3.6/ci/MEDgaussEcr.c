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

med_err
MEDgaussEcr(med_idt fid, med_geometrie_element type_geo, med_float *refcoo, med_mode_switch mode_coo,
	    med_int ngauss, med_float *gscoo, med_float * wg, char * locname )
{
  med_idt gid=0, chid=0;
  med_size dimd[1];
  med_err ret = -1;
  med_int typegeo = -1;
  char chemin[MED_TAILLE_GAUSS+1]="";
  med_mode_acces MED_MODE_ACCES;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier ");
    goto ERROR;
  }

  /*
   * MED_GAUSS_ELNO est un mot cle reserve
   */
  if (! strcmp(locname,MED_GAUSS_ELNO)) {
    MESSAGE("MED_GAUSS_ELNO est un mot cle reserve : points Gauss sur les noeuds de l'element");
    goto ERROR;   
   }

  /* 
   * Si le groupe "GAUSS" n'existe pas, on le cree
   */
  strncpy(chemin,MED_GAUSS,MED_TAILLE_GAUSS-1);
  chemin[MED_TAILLE_GAUSS-1] = '\0';
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((gid = _MEDdatagroupCreer(fid,chemin)) < 0) {
      MESSAGE("Impossible de creer le groupe MED_GAUSS : ");
      SSCRUTE(chemin); goto ERROR;
    }
  
  /* 
   * Si le groupe <locname> n'existe pas, on le cree
   * Sinon => erreur
   */
  NOFINALBLANK(locname,ERROR);

  if ((chid = _MEDdatagroupOuvrir(gid,locname)) >= 0) {
    if ( MED_MODE_ACCES != MED_LECTURE_ECRITURE ) {
      MESSAGE("Le nom de localisation existe déjà : ");
      SSCRUTE(locname); goto ERROR;
    }
  } else
    if ((chid = _MEDdatagroupCreer(gid,locname)) < 0)
      goto ERROR;

  /*
   * On stocke <ngauss> sous forme d'attribut
   */
  if (_MEDattrEntierEcrire(chid,MED_NOM_NBR,&ngauss) < 0) {
    MESSAGE("Erreur à l'écriture de l'attribut MED_NOM_NBR : ");
    ISCRUTE(ngauss);goto ERROR;
  };

  /*
   * On stocke <type_geo> sous forme d'attribut
   */
  typegeo = (med_int) type_geo; 
  /* sizeof(enum) tjrs = sizeof(int) en C, or
     sur machines 64 bits par défaut med_int==long,
     du coup sur  machines 64 bits _MEDattrEntierEcrire utilise 
     le type hdf NATIVE_LONG, ce qui pose un problème qd on passe
     un enum.
  */
  if (_MEDattrEntierEcrire(chid,MED_NOM_GEO,&typegeo) < 0) {
    MESSAGE("Erreur à l'écriture de l'attribut MED_NOM_GEO : ");
    ISCRUTE(type_geo);goto ERROR;
  };


  /*
   * On stocke les coordonnées de référence dans un dataset
   */

  dimd[0] = (type_geo%100)*(type_geo/100);
  if ( _MEDdatasetNumEcrire(chid,MED_NOM_COO,MED_FLOAT64,mode_coo,(type_geo/100),MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
			    (unsigned char*) refcoo)  < 0 ) {
    MESSAGE("Impossible d'ecrire le dataset : ");SSCRUTE(MED_NOM_COO);
    ISCRUTE_size(dimd[0]); goto ERROR;
  }

  /*
   * On stocke les points d'intégration dans un dataset
   */
   
  dimd[0] = ngauss*(type_geo/100);
  if ( _MEDdatasetNumEcrire(chid,MED_NOM_GAU,MED_FLOAT64,mode_coo,(type_geo/100),MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
			    (unsigned char*) gscoo)  < 0 ) {
    MESSAGE("Impossible d'ecrire le dataset : ");SSCRUTE(MED_NOM_GAU);
    ISCRUTE_size(dimd[0]); goto ERROR;
  }
  
  /*
   * On stocke les poids dans un dataset
   */
   
  dimd[0] = ngauss; 
  if ( _MEDdatasetNumEcrire(chid,MED_NOM_VAL,MED_FLOAT64,mode_coo,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
			    (unsigned char*) wg)  < 0 ) {
    MESSAGE("Impossible d'ecrire le dataset : ");SSCRUTE(MED_NOM_VAL);
    ISCRUTE_size(dimd[0]); goto ERROR;
  }


  ret = 0;

 ERROR:
  
  /*
   * On ferme tout
   */
  
  if (chid>0)     if (_MEDdatagroupFermer(chid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(chid); ret = -1; 
  }

  if (gid>0)     if (_MEDdatagroupFermer(gid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(gid); ret = -1; 
  }
  
  return ret;
}

