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
 * - Nom de la fonction : MEDpasdetempsInfo
 * - Description : Itérateur renvoyant (n°pdt,n°or), le nbre de point de GAUSS pour le type d'élément,
 *                 et le  maillage par défaut avec son eventuel lien à un autre fichier. 
 * - Parametres :
 *     - fid        (IN) : ID du fichier HDF courant
 *     - cha        (IN) : le nom du champ 
 *     - type_ent   (IN) : entité du champ concernée  {MED_NOEUD,MED_ARETE,MED_FACE,MED_MAILLE}
 *      - type_geo  (IN) : type géométrique de l'entité concerné {MED_POINT,MED_SEG2 ......}
 *       - indice   (IN) : itérateur commançant à 1.
 *       - ngauss  (OUT) : nbre de point de gauss utilisé (MED_NOPG si aucun)
 *       - numdt   (OUT) : n° du pas de temps (MED_NOPDT si aucun)
 *       - numo    (OUT) : n° d'ordre utilisé (MED_NONOR si aucun)
 *       - dt_unit (OUT) : chaine de taille MED_NOMP indiquant l'unité du champ
 *       - dt      (OUT) : valeur du pas de temps 
 *        - maa   (OUT)  : le nom du  maillage par défaut sur lequel le champ résultat s'applique au couple (numdt,numo) donné.
 *        - local (OUT)  : MED_VRAI si le lien est local, MED_FAUX sinon.
 *        - nmaa (OUT)  : le nombre de maillages référencés
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

med_err
MEDpasdetempsInfo(med_idt fid,char *champ,
		  med_entite_maillage type_ent, med_geometrie_element type_geo,
		  int indice, med_int * ngauss, med_int * numdt, med_int * numo,
                  char * dt_unit, med_float * dt,  char * maa, med_booleen * local, med_int *nmaa)

{
  med_err ret=-1;
  med_idt gid=0,datagroup3=0,gid_maa=0,gid_lien=0;
  char chemin      [(MED_TAILLE_CHA+MED_TAILLE_NOM+1)+(2*MED_TAILLE_NOM_ENTITE+2)+2*MED_MAX_PARA+1]="";
  char chemin_maa  [MED_TAILLE_MAA+MED_TAILLE_NOM+1]="";
  char chemin_lien [MED_TAILLE_LIENS+MED_TAILLE_NOM+1]=""; 
  int nmaa_i=0;
  int num=0;
  char tmp1         [MED_TAILLE_NOM_ENTITE+1]="";
  char nomdatagroup1[2*MED_TAILLE_NOM_ENTITE+2]="";
  char nomdatagroup2[2*MED_MAX_PARA+1]="";

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
  
  if (_MEDnomEntite(nomdatagroup1,type_ent) < 0) {
    MESSAGE("L'entité demandée n'est pas une entité <med_entite_maillage> : ");
    SSCRUTE(chemin); ISCRUTE(type_ent); goto ERROR;
  };

  if ((type_ent != MED_NOEUD)) {
    if (_MEDnomGeometrie30(tmp1,type_geo) < 0) {
      MESSAGE("Le type géométrique demandé n'est pas un <med_geometrie_element> : ");
      SSCRUTE(chemin); ISCRUTE(type_geo); goto ERROR;
    };
    strcat(nomdatagroup1,".");
    strcat(nomdatagroup1,tmp1);
  }
  strcat(chemin,nomdatagroup1);
  strcat(chemin,"/");

  /*
   * Ouvre le datagroup  <numdtt>.<numoo> correspondant à l'indice num
   */
  num = indice - 1;
  if (_MEDobjetIdentifier(fid,chemin,num,nomdatagroup2) < 0) {
    MESSAGE("Impossible de trouver un groupe à l'indice spécifié : ");
    SSCRUTE(chemin); ISCRUTE(num); goto ERROR;
  };
  
  strcat(chemin,nomdatagroup2);
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Erreur d'ouverture du datagroup  : ");
    SSCRUTE(chemin); goto ERROR;
  };
  
  /*
   * Calcul du nombre de maillages
   */
  nmaa_i = 0;
  if ( _MEDnObjets(fid,chemin,&nmaa_i) < 0) {
    MESSAGE("Impossible d'itérer dans le groupe : ");
    SSCRUTE(chemin); goto ERROR;
  };
  *nmaa = nmaa_i;

  /*
   * Lecture des attributs
   */
  

  if (_MEDattrEntierLire(gid,MED_NOM_NDT,(med_int*) numdt) < 0) {
    MESSAGE("Erreur d'ouverture de l'attribut numdt : ");
    SSCRUTE(chemin); goto ERROR;
  };
  
  if (_MEDattrFloatLire(gid,MED_NOM_PDT,(med_float*) dt) < 0) {
    MESSAGE("Erreur d'ouverture de l'attribut dt : ");
    SSCRUTE(chemin); goto ERROR;
  };

  if (_MEDattrStringLire(gid,MED_NOM_UNI,MED_TAILLE_PNOM,dt_unit) < 0) {
    MESSAGE("Erreur d'ouverture de l'attribut dt_unit : ");
    SSCRUTE(chemin); goto ERROR;
  };
  
  if (_MEDattrEntierLire(gid,MED_NOM_NOR,(med_int*) numo) < 0) {
    MESSAGE("Erreur d'ouverture de l'attribut numo : ");
    SSCRUTE(chemin); goto ERROR;
  };


  /* Lecture du nom du maillage par défaut  */
  
  if (_MEDattrStringLire(gid,MED_NOM_MAI,MED_TAILLE_NOM,maa) < 0) {
    MESSAGE("Erreur d'ouverture de l'attribut maa : ");
    SSCRUTE(chemin);  goto ERROR;
  };

  /*
   * Si le Data Group de niveau 3 <nom de maillage> n'existe pas => erreur
   */
 
  if ((datagroup3 = _MEDdatagroupOuvrir(gid,maa)) < 0) {
    MESSAGE("Erreur d'ouverture du datagroup lien au maillage : ");
    SSCRUTE(chemin); SSCRUTE(maa); goto ERROR;
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
/*       SSCRUTE(chemin_maa);SSCRUTE(chemin_lien); goto ERROR; */
      *local = MED_FAUX;
    }
  
    *local = MED_FAUX;
    
  } else
    
    *local = MED_VRAI;
    
  /* Lire le nbre des points de GAUSS*/
  if (_MEDattrEntierLire(datagroup3,MED_NOM_NGA,ngauss) < 0) {
    MESSAGE("Erreur à la lecture de l'attribut MED_NOM_NGA : ");
    ISCRUTE(*ngauss);goto ERROR;
  };


  /*
   * On ferme tout 
   */

  ret = 0;

 ERROR:
  

  if (gid_lien>0) if (_MEDdatagroupFermer(gid_lien) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      SSCRUTE(chemin_lien); ret = -1; 
  }

  if (gid_maa>0)  if (_MEDdatagroupFermer(gid_maa) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      ISCRUTE_id(gid_maa); ret = -1; 
  }
    
 
  if (datagroup3>0)     if (_MEDdatagroupFermer(datagroup3) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      ISCRUTE_int(datagroup3); ret = -1; 
  }

  if (gid>0)     if (_MEDdatagroupFermer(gid) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      ISCRUTE_id(gid); ret = -1; 
  }
  
  return ret; 


}
