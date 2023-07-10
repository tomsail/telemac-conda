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
#include "med_config.h"
#include "med_outils.h"
#include <string.h>
/*
 * - Nom de la fonction : MEDnChampRef
 * - Description : Renvoi le nbre de maillages associés au champ ( <cha>/<type_ent>[.<type_geo>]/<numdtt>.<numoo> )
 * - Parametres :
 *     - fid        (IN) : ID du fichier HDF courant
 *     - cha        (IN) : le nom du champ 
 *     - type_ent   (IN) : entité du champ concernée  {MED_NOEUD,MED_ARETE,MED_FACE,MED_MAILLE}
 *      - type_geo  (IN) : type géométrique de l'entité concerné {MED_POINT,MED_SEG2 ......}
 *       - numdt    (IN) : n° du pas de temps (MED_NOPDT si aucun)
 *       - numo     (IN) : n° d'ordre utilisé (MED_NONOR si aucun)
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 


med_int MEDnChampRef(med_idt fid, char * cha, med_entite_maillage type_ent, 
		     med_geometrie_element type_geo,
		     med_int numdt, med_int numo)

{
  char chemin[(MED_TAILLE_CHA+MED_TAILLE_NOM+1)+(2*MED_TAILLE_NOM_ENTITE+2)+2*MED_MAX_PARA+1];
  int n1;
  char nomdatagroup1[2*MED_TAILLE_NOM_ENTITE+2];
  char nomdatagroup2[2*MED_MAX_PARA+1];
  char tmp1         [MED_TAILLE_NOM_ENTITE+1];

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On construit le chemin d'accès de niveau 0
   */
  strcpy(chemin,MED_CHA);
  strcat(chemin,cha);
  strcat(chemin,"/");

  /* 
   * On construit le nom du datagroup de niveau 1 <_type_ent>[.<type_geo>] n'existe pas => erreur
   */

  if ( _MEDnomEntite(nomdatagroup1,type_ent ) < 0) {
    MESSAGE("L'entité demandée n'est pas une entité <med_entite_maillage> : ");
    SSCRUTE(chemin); ISCRUTE(type_ent); return -1;
  };
  if ((type_ent != MED_NOEUD)) {
    if ( _MEDnomGeometrie30(tmp1,type_geo) < 0) {
      MESSAGE("Le type géométrique demandé n'est pas un <med_geometrie_element> : ");
      SSCRUTE(chemin); ISCRUTE(type_geo); return -1;
    };
    strcat(nomdatagroup1,".");
    strcat(nomdatagroup1,tmp1);
  }
  strcat(chemin,nomdatagroup1);
  strcat(chemin,"/");

  
  /*
   *  On construit le nom du datagroup de niveau 2 <numdtt>.<numoo> n'existe pas => erreur
   */

  sprintf(nomdatagroup2,"%*li%*li",MED_MAX_PARA,(long ) numdt,MED_MAX_PARA,(long ) numo);
  strcat(chemin,nomdatagroup2);
  
  n1 = 0;
  if ( _MEDnObjets(fid,chemin,&n1) < 0 ) {
    MESSAGE("Impossible d'itérer dans le groupe : ");
    SSCRUTE(chemin); return -1;
  };

  return (med_int) n1;

}
