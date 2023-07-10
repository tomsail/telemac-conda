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
MEDgaussInfo(med_idt fid, int indice, char * locname, med_geometrie_element * type_geo,
	     med_int * ngauss )
{
  int numero=0;
  med_idt locid=0;
  med_err ret=-1;
  char chemin[MED_TAILLE_GAUSS+MED_TAILLE_NOM+1]="";
  med_int typegeo;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On recupere le nom du groupe de rang "indice"
   */ 
  numero = indice-1;
  if ( _MEDobjetIdentifier(fid,MED_GAUSS,numero,locname) < 0)
    goto ERROR;

  /*
   * On va chercher l'attribut ngauss 
   */
  strcpy(chemin,MED_GAUSS);
  strcat(chemin,locname);
  if ((locid = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Impossible d'ouvrir le datagroup : ");
    SSCRUTE(chemin); goto ERROR;
  }

  /* Lecture de  <ngauss> */
  if (_MEDattrEntierLire(locid,MED_NOM_NBR,ngauss) < 0) {
    MESSAGE("Erreur à la lecture de l'attribut MED_NOM_NBR : ");
    ISCRUTE(*ngauss);goto ERROR;
  };
  
  /* Lecture <type_geo> sous forme d'attribut */
  if (_MEDattrEntierLire(locid,MED_NOM_GEO,&typegeo) < 0) {
    MESSAGE("Erreur à la lecture de l'attribut MED_NOM_GEO : ");
    ISCRUTE(*type_geo);goto ERROR;
  };
  *type_geo = ( med_geometrie_element ) typegeo;

  ret = 0;

 ERROR:
  if ( locid > 0 ) if (_MEDdatagroupFermer(locid) < 0)
    goto ERROR;
  
  return 0;
}
