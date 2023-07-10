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
MEDgaussLire(med_idt fid, med_float *refcoo, med_float *gscoo, med_float * wg, med_mode_switch mode_coo, char *locname)
{
  med_err ret = -1;
  med_idt locid=0;
  char chemin[MED_TAILLE_GAUSS+MED_TAILLE_NOM+1]=""; 
  med_int type_geo;
  
  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /* 
   * ouverture du groupe /PROFILS/"nom"
   */  
  strcpy(chemin,MED_GAUSS);
  strcat(chemin,locname); 
  if ((locid = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Impossible d'ouvrir le datagroup : ");
    SSCRUTE(chemin); goto ERROR;
  }

  
  /* Lecture <type_geo> sous forme d'attribut */
  if (_MEDattrEntierLire(locid,MED_NOM_GEO,&type_geo) < 0) {
    MESSAGE("Erreur à la lecture de l'attribut MED_NOM_GEO : ");
    ISCRUTE(type_geo);goto ERROR;
  };

  /*
   * Lecture de la localisation
   */

  /*
   * On stocke les coordonnées de référence dans un dataset
   */
 
  if ( _MEDdatasetNumLire(locid,MED_NOM_COO,MED_FLOAT64,mode_coo,(type_geo/100),MED_ALL,MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
			  (unsigned char*) refcoo)  < 0 ) {
    MESSAGE("Impossible de lire le dataset : ");SSCRUTE(MED_NOM_COO);
    goto ERROR;
  }

  /*
   * On stocke les points d'intégration dans un dataset
   */
   
  if ( _MEDdatasetNumLire(locid,MED_NOM_GAU,MED_FLOAT64,mode_coo,(type_geo/100),MED_ALL,MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
			  (unsigned char*) gscoo)  < 0 ) {
    MESSAGE("Impossible de lire le dataset : ");SSCRUTE(MED_NOM_GAU);
    goto ERROR;
  }
  
  /*
   * On stocke les poids dans un dataset
   */
   
  if ( _MEDdatasetNumLire(locid,MED_NOM_VAL,MED_FLOAT64,mode_coo,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
			  (unsigned char*) wg)  < 0 ) {
    MESSAGE("Impossible de lire le dataset : ");SSCRUTE(MED_NOM_VAL);
    goto ERROR;
  }
  

  /*
   * On ferme tout
   */

  ret = 0;
 ERROR:

  if ( locid > 0 ) if ( _MEDdatagroupFermer(locid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(locid); ret = -1;
  }
  
  return ret;
}

