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

/*
 * - Nom de la fonction : MEDchampRefInfo
 * - Description : Itérateur renvoyant les maillages associés au champ <cha>
 *                 avec le lien eventuel à un fichier externe. 
 * - Parametres :
 *     - fid        (IN) : ID du fichier HDF courant
 *     - cha        (IN) : le nom du champ 
 *     - type_ent   (IN) : entité du champ concernée  {MED_NOEUD,MED_ARETE,MED_FACE,MED_MAILLE}
 *      - type_geo  (IN) : type géométrique de l'entité concerné {MED_POINT,MED_SEG2 ......}
 *       - indice   (IN) : itérateur commançant à 1.
 *       - numdt    (IN) : n° du pas de temps (MED_NOPDT si aucun)
 *       - numo     (IN) : n° d'ordre utilisé (MED_NONOR si aucun)
 *        - maa    (OUT) : le nom du  maillage  sur lequel le champ résultat s'applique au couple (numdt,numo) donné.
 *        - local  (OUT) : MED_VRAI si le lien est local, MED_FAUX sinon.
 *        - ngauss (OUT) : nombre de points de GAUSS, MED_NOGAUSS si pas de points de gauss
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

med_err 
MEDchampRefInfo(med_idt fid,char *champ,
		med_entite_maillage type_ent, med_geometrie_element type_geo,
		int indice, med_int numdt, med_int numo,
		char * maa, med_booleen * local, med_int *ngauss)
{

  med_err ret=-1;
  int num;
  med_idt datagroup3=0,gid_maa=0,gid_lien=0;
  char chemin[(MED_TAILLE_CHA+MED_TAILLE_NOM+1)+(2*MED_TAILLE_NOM_ENTITE+2)+(2*MED_MAX_PARA+1)+(MED_TAILLE_NOM)+1]="";
  char chemin_maa[MED_TAILLE_MAA+MED_TAILLE_NOM+1]="";
  char chemin_lien[MED_TAILLE_LIENS+MED_TAILLE_NOM+1]=""; 
  char nomdatagroup1[2*MED_TAILLE_NOM_ENTITE+2]="";
  char nomdatagroup2[2*MED_MAX_PARA+1]="";
  char tmp1         [MED_TAILLE_NOM_ENTITE+1]="";

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On construit le nom du datagroup
   */
  strcpy(chemin,MED_CHA);
  strcat(chemin,champ);
  strcat(chemin,"/");

  /* 
   * Si le Data Group  de niveau 1 <type_ent>[.<type_geo>] n'existe pas => erreur
   */
  /* modif pour la version 2.3.3 */
  
  if (_MEDnomEntite(nomdatagroup1,type_ent) < 0)
    goto ERROR;
  if ((type_ent != MED_NOEUD)) {
    if (_MEDnomGeometrie30(tmp1,type_geo) < 0)
      goto ERROR;
    strcat(nomdatagroup1,".");
    strcat(nomdatagroup1,tmp1);
  }
  strcat(chemin,nomdatagroup1);
  strcat(chemin,"/");

  /*
   * Si le Data Group de niveau 2 <numdtt>.<numoo> n'existe pas => erreur
   */
  sprintf(nomdatagroup2,"%*li%*li",MED_MAX_PARA,(long ) numdt,MED_MAX_PARA,(long ) numo);
  
  strcat(chemin,nomdatagroup2);
  strcat(chemin,"/");


  /*
   * Cherche le datagroup de niveau 3 <maa> correspondant à l'indice <num>
   */
  num = indice - 1;
  if (_MEDobjetIdentifier(fid,chemin,num,maa) < 0) {
    MESSAGE("Impossible de trouver un groupe à l'indice spécifié : ");
    SSCRUTE(chemin); ISCRUTE_int(num); goto ERROR;
  };
  strcat(chemin,maa);
 

  /*
   * Si le Data Group de niveau 3 <maa> n'existe pas => erreur
   */
 
  if ((datagroup3 = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Erreur d'ouverture du datagroup lien au maillage : ");
    SSCRUTE(chemin); goto ERROR;
  };


  /* Lire le nbre des points de GAUSS*/
  if (_MEDattrEntierLire(datagroup3,MED_NOM_NGA,ngauss) < 0) {
    MESSAGE("Erreur à la lecture de l'attribut MED_NOM_NGA : ");
    ISCRUTE(*ngauss);goto ERROR;
  };


  /* Maillage local ou distant */
  strcpy(chemin_maa,MED_MAA);
  strcat(chemin_maa,maa);
  /* Le maillage est il distant */
  if ( (gid_maa = _MEDdatagroupOuvrir(fid,chemin_maa)) < 0)  {
    
    /* Verifie que le maillage est bien référencé comme distant */  
    strcpy(chemin_lien,MED_LIENS);
    strcat(chemin_lien,maa); 
    if ((gid_lien = _MEDdatagroupOuvrir(fid,chemin_lien)) < 0) {
/*       MESSAGE("Le maillage n'est ni local, ni distant : "); */
/*        SSCRUTE(chemin_maa);SSCRUTE(chemin_lien); goto ERROR; */
      *local = MED_FAUX;
    }
  
    *local = MED_FAUX;
    
  } else  
    *local = MED_VRAI;
    
  
  /*
   * On ferme tout 
   */

  ret = 0;

 ERROR:
  
  if (datagroup3>0)     if (_MEDdatagroupFermer(datagroup3) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      ISCRUTE_int(datagroup3); ret = -1; 
  }
  
  if (gid_maa>0)  if (_MEDdatagroupFermer(gid_maa) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      ISCRUTE_id(gid_maa); ret = -1; 
  }
  
  if (gid_lien>0) if (_MEDdatagroupFermer(gid_lien) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      SSCRUTE(chemin_lien); ret = -1; 
  }

  return ret; 
}
