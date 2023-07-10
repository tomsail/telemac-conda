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
#include <string.h>
#include <stdlib.h>
#include <med_outils.h>

/*
 * - Nom de la fonction : MEDnVal
 * - Description :Renvoie le nbre d'�l�ment d'un champ
 * - Parametres :
 *     - fid      (IN)  : ID du fichier HDF courant
 *     - cha      (IN)  : le nom du champ 
 *     - type_ent (IN)  : entit� concern� par le champ {MED_NOEUD,MED_ARETE,MED_FACE,MED_MAILLE}
 *     - type_geo (IN)  : type g�om�trique de l'entit� concern� {MED_POINT,MED_SEG2 ......}
 *     - numdt    (IN)  : n� du pas de temps (MED_NOPDT si aucun)
 *     - numo     (IN)  : n� d'ordre utilis� MED_NONOR si inutile
 *     - maa      (IN)  : le nom du maillage sur lequel s'applique le champ, eventuellemnt MED_NOREF 
 *     - pflmod   (IN)  : Indique s'il faut calculer la taille m�moire pour un mode MED_COMPACT ou MED_GLOBAL . 
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

med_int 
MEDnVal(med_idt fid, char *cha, med_entite_maillage type_ent, 
	med_geometrie_element type_geo,med_int numdt, med_int numo, char * maa, med_mode_profil pflmod)
{
  med_int n=-1, ngauss=0, pfluse=0;
  med_idt datagroup3=0;
  char nomdatagroup1[2*MED_TAILLE_NOM_ENTITE+2]="";
  char nomdatagroup2[2*MED_MAX_PARA+1]="";
  char tmp1   [MED_TAILLE_NOM_ENTITE+1]="";
  char chemin [MED_TAILLE_CHA+(MED_TAILLE_NOM+1)+(2*MED_TAILLE_NOM_ENTITE+2)+(2*MED_MAX_PARA+1)+(MED_TAILLE_NOM)+1]="";
  char chemin_i [MED_TAILLE_CHA+(MED_TAILLE_NOM+1)+(2*MED_TAILLE_NOM_ENTITE+2)+(2*MED_MAX_PARA+1)+(MED_TAILLE_NOM)+1]="";  
  char pfltmp[MED_TAILLE_NOM+1]="";
  char maa_i [MED_TAILLE_NOM+1]="";
  med_int   psize=0;
  
  /*
   * On inhibe le gestionnaire d'erreur HDF 
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On cree le chemin d'acc�s
   */
  strcpy(chemin,MED_CHA);
  strcat(chemin,cha);
  strcat(chemin,"/");

  /* On cree le nom du datagroup de niveau 1  <type_ent>[.<type_geo>] */
  /* modif pour la version 2.3.3 */
  
  if ( _MEDnomEntite(nomdatagroup1,type_ent) < 0 )
    goto ERROR;
  if ((type_ent != MED_NOEUD)) {
    if ( _MEDnomGeometrie30(tmp1,type_geo) < 0)
      goto ERROR;
    strcat(nomdatagroup1,".");
    strcat(nomdatagroup1,tmp1);
  }
  strcat(chemin,nomdatagroup1);
  strcat(chemin,"/");

  /* Creation du nom du datagroup de niveau 2 <numdt>.<numoo> */
  sprintf(nomdatagroup2,"%*li%*li",MED_MAX_PARA,(long ) numdt,MED_MAX_PARA,(long ) numo);
  strcat(chemin,nomdatagroup2);
  strcat(chemin,"/");

  /* Creation du nom du datagroup de niveau 3 <maa> */
  if ( strcmp(maa,MED_NOREF) ) 
    strcat(chemin,maa);
  else {
    strcpy(chemin_i,chemin);

    if ((datagroup3 = _MEDdatagroupOuvrir(fid,chemin_i)) < 0) return 0;

    if (_MEDattrStringLire(datagroup3,MED_NOM_MAI,MED_TAILLE_NOM,maa_i) < 0)
      goto ERROR;

    strcat(chemin,maa_i);

    if ( _MEDdatagroupFermer(datagroup3) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      ISCRUTE_int(datagroup3); goto ERROR;
    }
  }

  /*
   *  Acces au champ
   */
  if ((datagroup3 = _MEDdatagroupOuvrir(fid,chemin)) < 0) 
    return 0;

  switch(pflmod) {
    
  case MED_GLOBAL :
    
    if ( _MEDattrEntierLire(datagroup3,MED_NOM_NBR,&n) < 0) {
      MESSAGE("Erreur à la lecture de l'attribut MED_NOM_NBR : ");
      ISCRUTE(n);goto ERROR;
    };    
    break;
    
  case MED_COMPACT :
 
    /* Vérifier l'existence d'un profil*/
    
    if (_MEDattrStringLire(datagroup3,MED_NOM_PFL,MED_TAILLE_NOM,pfltmp) < 0){
      MESSAGE("Erreur à la lecture de l'attribut MED_NOM_PFL : ");
      SSCRUTE(pfltmp);goto ERROR;
    };   

    /* Si un profil est trouv� la taille est calcul�e en fonction du nbre d'�l�ments du profil*/
    /* Sinon l'appel est �quivalent au mode MED_GLOBAL ); */
    if ( (pfluse = (strcmp(pfltmp,MED_NOPFLi) && strcmp(pfltmp,"")) ) ) {
      if ( (psize = MEDnValProfil(fid,pfltmp)) < 0 ) {
	MESSAGE("Erreur à l'appel de MEDnValProfil : ");
	SSCRUTE(pfltmp);goto ERROR;
      };
    } else {
      if ( _MEDattrEntierLire(datagroup3,MED_NOM_NBR,&n) < 0) {
	MESSAGE("Erreur à la lecture de l'attribut MED_NOM_NBR : ");
	ISCRUTE(n);goto ERROR;
      };    
       break;     
    };
    
    /* Lire le nbre des points de GAUSS*/
    if (_MEDattrEntierLire(datagroup3,MED_NOM_NGA,&ngauss) < 0) {
      MESSAGE("Erreur à la lecture de l'attribut MED_NOM_NGA : ");
      ISCRUTE(ngauss);goto ERROR;
    };   

    /* NE PAS REMPLACER ICI type_ent par type_ent */
    if (type_ent == MED_NOEUD_MAILLE )
      ngauss = type_geo % 100;

    n= ngauss * psize;
    
    break;
    
  default :
      MESSAGE("Erreur : <pflmod> doit etre positionné soit à MED_GLOBAL soit à MED_COMPACT ");
    break;
    
  }
  
  ERROR :
    
    if (datagroup3>0) 
      if ( _MEDdatagroupFermer(datagroup3) < 0) {
	MESSAGE("Impossible de fermer le datagroup : ");
	ISCRUTE_int(datagroup3); n = -1; 
      } 
  
  return n;
}



