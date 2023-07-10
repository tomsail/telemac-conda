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


#ifdef TRACEPFL
#define tracepfl(x) x
#else
#define tracepfl(x)
#endif

/*
 * - Nom de la fonction : MEDchampEcr
 * - Description : ecriture d'un Champ Résultat
 * - Parametres :
 *     - fid      (IN)  : ID du fichier HDF courant
 *     - maa      (IN)  : le nom du maillage sur lequel s'applique le champ (eventuellement distant)
 *     - cha      (IN)  : le nom du champ 
 *     - val      (IN)  : valeurs du champ à stocker
 *     - interlace(IN)  : entrelacement utilisé en mémoire {MED_FULL_INTERLACE,MED_NO_INTERLACE} 
 *     - nbelem   (IN)  : nombre d'éléments (prend en compte le nbre 
 *                         de points de Gauss (c'est demandé à l'utilisateur ds la doc) 
                           mais pas le nbre de composantes)
 *     - locname  (IN)  : clé utilisée pour la définition de la localisation 
                          des points de GAUSS (MED_NOGAUSS si aucun, MED_GAUSS_ELNO si les points de Gauss
 *                        portent sur les noeuds de l'element). La localisation doit exister 
 *                        avant l'appel à MEDchampEcr.
 *     - numco    (IN)  : n° de la composante à stocker (MED_ALL si toutes)
 *     - profil   (IN)  : nom du profil utilisé (MED_NOPFL si inutilisé)
 *     - pflmod   (IN)  : Indique comment lire les informations en mémoire { MED_COMPACT, MED_GLOBAL }. 
 *     - type_ent (IN)  : entité concerné par le champ {MED_NOEUD,MED_ARETE,MED_FACE,MED_MAILLE}
 *     - type_geo (IN)  : type géométrique de l'entité concerné {MED_POINT,MED_SEG2 ......}
 *     - numdt    (IN)  : n° du pas de temps (MED_NOPDT si aucun)
 *     - dt_unit  (IN)  : chaine de taille MED_NOMP indiquant l'unité du champ
 *     - dt       (IN)  : valeur du pas de temps 
 *     - numo     (IN)  : n° d'ordre utilisé MED_NONOR si inutile
 * - Resultat : 0 en cas de succes, -1 sinon
 */ 

void  MEDchampEcr232(int dummy,...) {


  med_idt                    fid ;
  char *                     maa ;
  char *                     cha ;
  unsigned char *            val ;
  med_mode_switch      interlace ;
  med_int                 nbelem ;
  char *                 locname ;
  med_int                  numco ;
  char *                  profil ;
  med_mode_profil         pflmod ;
  med_entite_maillage   type_ent ;
  med_geometrie_element type_geo ;
  med_int                  numdt ;
  char *                 dt_unit ;
  med_float                   dt ;
  med_int                   numo ;
  med_err *                 fret ;

  med_err ret=-1;
  med_idt gid=0,datagroup1=0,datagroup2=0,datagroup3=0,attr=0,gid_loc=0;
  med_int ncomp=0, chtype=0, i=0, ngauss=0, pfluse=0;
  char nomdatagroup1[2*MED_TAILLE_NOM_ENTITE+2]="";
  char nomdatagroup2[2*MED_MAX_PARA+1]="";
  char pflname   [MED_TAILLE_NOM+1]="";
  char locname_i [MED_TAILLE_NOM+1]="";
  char maillage[MED_TAILLE_NOM+1]="";
  char tmp1[MED_TAILLE_NOM_ENTITE+1]="";
  med_size dimd[1],psize=0,lsize=0;
  med_int   *pfltabtmp=0;
  med_size *pfltab=0;
  char chemin[MED_TAILLE_CHA+MED_TAILLE_NOM+1]="";
  char chemin_loc[MED_TAILLE_GAUSS+MED_TAILLE_NOM+1]="";
  med_geometrie_element type_geo_g;
  med_int type_geo_g_int=0;
  char oldpflname   [MED_TAILLE_NOM+1]=MED_NOPFLi;
  med_int maj, pfluseold, modifpfl;
  med_mode_acces MED_MODE_ACCES;

  va_list params;
  va_start(params,dummy);

  fid = va_arg(params,med_idt);
  maa = va_arg(params,char *);
  cha = va_arg(params,char *);
  val = va_arg(params,  unsigned char *);
  interlace = va_arg(params,med_mode_switch);
  nbelem = va_arg(params,med_int);
  locname = va_arg(params,char *);
  numco = va_arg(params,med_int);
  profil = va_arg(params,char *);
  pflmod = va_arg(params,med_mode_profil);
  type_ent = va_arg(params,med_entite_maillage);
  type_geo = va_arg(params,med_geometrie_element);
  numdt = va_arg(params,med_int);
  dt_unit = va_arg(params,char *);
  dt = va_arg(params,med_float);
  numo = va_arg(params,med_int);
  fret = va_arg(params,med_err *);
  

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) {*fret=-1;return;}


  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier ");
    goto ERROR;
  }

  if ( MED_MODE_ACCES == MED_LECTURE) {
    MESSAGE("Impossible d'écrire un champ en mode MED_LECTURE : ");
    goto ERROR;
  };


  /* 
   * Si le Data Group CHA n'existe pas => erreur
   */
  strcpy(chemin,MED_CHA);
  strcat(chemin,cha);
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    goto ERROR;

  /* Lecture de l'attribut MED_NOM_NCO */
  if (_MEDattrEntierLire(gid,MED_NOM_NCO,&ncomp) < 0)
    goto ERROR;

  /* Lecture de l'attribut MED_NOM_TYP */
  if ( _MEDattrEntierLire(gid,MED_NOM_TYP,&chtype) < 0)
    goto ERROR;
 

  
  /* 
   *  Creation/Ouverture du datagroup de niveau 1 <type_ent>[.<type_geo>] 
   */
  if ( type_ent == MED_NOEUD_MAILLE ) {
    SSCRUTE("L'écriture aux noeuds des éléments n'est pas disponible.");
    goto ERROR;
  }
  
  if (_MEDnomEntite(nomdatagroup1,type_ent) < 0)
    goto ERROR;
  if ((type_ent != MED_NOEUD)) {
    if ( _MEDnomGeometrie(tmp1,type_geo) < 0)
      goto ERROR;
    strcat(nomdatagroup1,".");
    strcat(nomdatagroup1,tmp1);
  }
  datagroup1 = 0;
  if ( (datagroup1 = _MEDdatagroupOuvrir(gid,nomdatagroup1)) < 0) 
    if ((datagroup1 = _MEDdatagroupCreer(gid,nomdatagroup1)) < 0)
      goto ERROR;
  

  /* 
   * Creation/Ouverture du datagroup de niveau 2 <numdt>.<numoo> 
   */
 
  sprintf(nomdatagroup2,"%*li%*li",MED_MAX_PARA,(long ) numdt,MED_MAX_PARA,(long ) numo);

  datagroup2 = 0;   
  if ( (datagroup2 = _MEDdatagroupOuvrir(datagroup1,nomdatagroup2)) < 0 )    
    if ((datagroup2 = _MEDdatagroupCreer(datagroup1,nomdatagroup2)) < 0 )   
      goto ERROR;   
  
  /*Cree ou ouvre l'attribut MED_NOM_NDT pour écriture */
  if ( _MEDattrEntierEcrire(datagroup2,MED_NOM_NDT,&numdt) < 0)
    goto ERROR;
  
  /*Cree ou ouvre l'attribut MED_NOM_PDT pour écriture */
  if ( _MEDattrFloatEcrire(datagroup2,MED_NOM_PDT,&dt) < 0)
    goto ERROR;
    
  /*Cree ou ouvre l'attribut MED_NOM_NOR pour écriture */
  if (_MEDattrEntierEcrire(datagroup2,MED_NOM_NOR,&numo) < 0)
    goto ERROR;
  
  /*Cree ou ouvre l'attribut  MED_NOM_UNI pour écriture */ 
  if ( numdt == MED_NOPDT ) {
    if ( _MEDattrStringEcrire(datagroup2,MED_NOM_UNI,MED_TAILLE_PNOM,MED_PNOM_BLANC) < 0)
      goto ERROR;
  } else
    if ( _MEDattrStringEcrire(datagroup2,MED_NOM_UNI,MED_TAILLE_PNOM,dt_unit) < 0)
      goto ERROR;  


  /*
   * Si c'est la première référence à un maillage, initialise l'attribut MED_MAA à ce maillage
   */
  if ((attr = _MEDattrOuvrir(datagroup2,MED_NOM_MAI)) < 0 ) {
    if (_MEDattrStringEcrire(datagroup2,MED_NOM_MAI,MED_TAILLE_NOM,maa) < 0)
      goto ERROR;
  } else {   
    if ( _MEDattrFermer(attr) < 0) goto ERROR;
  }
      
  

  /* 
   * Cree ou ouvre le datagroup de niveau 3 <maa> 
   */

  NOFINALBLANK(maa,ERROR)

  datagroup3 = 0;   
  if (((datagroup3 = _MEDdatagroupOuvrir(datagroup2,maa)) > 0)    
      && ( MED_MODE_ACCES == MED_LECTURE_AJOUT )) {
    MESSAGE("Impossible de créer une reference à un maillage existant en mode MED_LECTURE_AJOUT :");
    goto ERROR;   
  } else   
    if (datagroup3 < 0)
      if ((datagroup3 = _MEDdatagroupCreer(datagroup2,maa)) < 0)   
	goto ERROR;   
  
  /*Cree ou ouvre l'attribut MED_NOM_NBR */
  if ( _MEDattrEntierEcrire(datagroup3,MED_NOM_NBR,&nbelem) < 0)
    goto ERROR;
  
  /* Lit le nom du profil associé s'il en existe déjà un */
  if ( _MEDattrStringLire(datagroup3,MED_NOM_PFL,MED_TAILLE_NOM,oldpflname) < 0 ) {
    strncpy(oldpflname,MED_NOPFLi,MED_TAILLE_NOM);
  }
  
  /*Cree ou ouvre  l'attribut MED_NOM_PFL   */
  pfluse = 0;
  if ( strlen(profil) == 0)   /* idem MED_NOPFL*/
    /* Ecriture de MED_NOPFLi car MED_NOM_PFL est forcément de taille MED_TAILLE_NOM*/
    strncpy(pflname,MED_NOPFLi,MED_TAILLE_NOM+1);
/*     strncpy(pflname,MED_NOPFLi,MED_TAILLE_NOM); */
  else {
    strncpy(pflname,profil,MED_TAILLE_NOM);
    pflname[MED_TAILLE_NOM]='\0';
    pfluse = 1;
  }

  /* on reporte à la fin l'ecriture du nouveau profil pour ne pas avoir une taille de dataset */
  /* incompatible avec le profil en cas d'ereur d'ecriture du champ */


  /* Lit le nombre de points de gauss et vérifie   */
  /* que la localisation porte sur le meme type géométrique  */
  
  if ( strlen(locname) == 0) {
    /* Ecriture de MED_NOGAUSSi car MED_NOM_GAUSS est forcément de taille MED_TAILLE_NOM*/
    strcpy(locname_i,MED_NOGAUSSi);
    ngauss = MED_NOPG ;
  } else if (! strcmp(locname,MED_GAUSS_ELNO)) { 
    /* Les points de Gauss sont définis sur les noeuds de l'element (mot cle) */
    /* le nombre de points de Gauss est egal au nombre de noeuds de l'element */
    ngauss = type_geo % 100;
    strcpy(locname_i,locname);

  } else { 
    strcpy(locname_i,locname);

    strcpy(chemin_loc,MED_GAUSS);
    strcat(chemin_loc,locname_i);
    
    if ((gid_loc = _MEDdatagroupOuvrir(fid,chemin_loc)) < 0) {
      MESSAGE("Ouverture du datagroup : ");
      SSCRUTE(chemin_loc); goto ERROR;
    }
    
    if (_MEDattrEntierLire(gid_loc,MED_NOM_NBR,&ngauss) < 0) {
      MESSAGE("Erreur à la lecture de l'attribut MED_NOM_NBR : ");
      ISCRUTE(ngauss);goto ERROR;
    };
    
    
    if (_MEDattrEntierLire(gid_loc,MED_NOM_GEO,&type_geo_g_int) < 0) {
      MESSAGE("Erreur à la lecture de l'attribut MED_NOM_GEO : ");
      ISCRUTE_int(type_geo_g_int);goto ERROR;
    };
    type_geo_g = type_geo_g_int;

    if ( type_geo_g != type_geo ) {
      MESSAGE("Erreur, la localisation ne porte pas sur le meme type géométrique : ");
      SSCRUTE(locname);ISCRUTE_int(type_geo);ISCRUTE_int(type_geo_g);goto ERROR;
    };
    
  }

  /* Cree ou ouvre l'attribut MED_NOM_GAU         */ 
  /* Ecriture de la localisation des pts de gauss  */
  if ( _MEDattrStringEcrire(datagroup3,MED_NOM_GAU,MED_TAILLE_NOM,locname_i) < 0) {
    MESSAGE("Erreur d'écriture de l'attribut MED_NOM_GAU : ");
    SSCRUTE(locname); goto ERROR;
  }

  /* Cree ou ouvre l'attribut MED_NOM_NGA         */ 
  /* Ecriture de l'attribut portant le nombre de points de gauss  */
  if ( _MEDattrEntierEcrire(datagroup3,MED_NOM_NGA,&ngauss) < 0) {
    MESSAGE("Erreur d'écriture de l'attribut MED_NOM_NGA : ");
    ISCRUTE(ngauss); goto ERROR;
  }

  /*Determination de la taille dimd[0] du dataset à stocker*/
  dimd[0] = nbelem*ncomp;

  /* Gestion des profils*/
  if ( pfluse ) {

    if ( ( i = MEDnValProfil(fid,pflname) ) < 0 ) {
      MESSAGE("Erreur à la lecture du nombre de valeurs sur le profil : ");
      SSCRUTE(pflname);
      goto ERROR;
    } else
      psize = i;
    
    pfltabtmp = (med_int *)   malloc (sizeof(med_int)*psize);
    pfltab    = (med_size *)  malloc (sizeof(med_size)*psize);
    if (MEDprofilLire(fid,pfltabtmp,pflname) < 0) {
      MESSAGE("Erreur à la lecture du profil : ");
      SSCRUTE(pflname);goto ERROR;
    };
    for (i=0;i<psize;i++)
      pfltab[i] = (med_size) pfltabtmp[i];
    
  }
  else {
    psize = MED_NOPF;
  }
 

  /* Vérifie si le profil a changé (l'API MED ne permet pas de modifier un profil existant) */
  if (strcmp(pflname, oldpflname))
    modifpfl = 1;              /* le profil a changé */
  else
    modifpfl = 0;              /* le profil n'a pas changé */


  /*
   * Ecriture du champ
   */
 
  switch(chtype)
    {
    case MED_FLOAT64 :
      if ( _MEDdatasetNumEcrire(datagroup3,MED_NOM_CO,MED_FLOAT64,interlace,
				ncomp,numco,psize,pflmod,modifpfl,pfltab,ngauss,dimd,val) < 0) {
	MESSAGE("Impossible d'ecrire le dataset : ");
	SSCRUTE(MED_NOM_CO);ISCRUTE((int)(dimd[0])); goto ERROR;
      }
      break;
      
    case MED_INT32 :
#if defined(HAVE_F77INT64) 
      if ( _MEDdatasetNumEcrire(datagroup3,MED_NOM_CO,MED_INT64,interlace,
				ncomp,numco,psize,pflmod,modifpfl,pfltab,ngauss,dimd,val) < 0) {
	MESSAGE("Impossible d'ecrire le dataset : ");
	SSCRUTE(MED_NOM_CO);ISCRUTE((int)(dimd[0])); goto ERROR;
      }
#else
      if ( _MEDdatasetNumEcrire(datagroup3,MED_NOM_CO,MED_INT32,interlace,
				ncomp,numco,psize,pflmod,modifpfl,pfltab,ngauss,dimd,val) < 0){
	MESSAGE("Impossible d'ecrire le dataset : ");
	SSCRUTE(MED_NOM_CO);ISCRUTE((int)(dimd[0])); goto ERROR;
      }
#endif
      break;

    case MED_INT64 :
#if defined(HAVE_F77INT64) 
      if ( _MEDdatasetNumEcrire(datagroup3,MED_NOM_CO,MED_INT64,interlace,
				ncomp,numco,psize,pflmod,modifpfl,pfltab,ngauss,dimd,val) < 0){
	MESSAGE("Impossible d'ecrire le dataset : ");
	SSCRUTE(MED_NOM_CO);ISCRUTE(dimd); goto ERROR;
      }
#else
      MESSAGE("Impossible d'ecrire le dataset de type MED_INT64 sur une plateforme autre que IRIX64 et OSF1 !");
      goto ERROR;
#endif
      break;   

    default :
      goto ERROR;
    }

  if ( _MEDattrStringEcrire(datagroup3,MED_NOM_PFL,MED_TAILLE_NOM,pflname) < 0) {
    MESSAGE("Erreur d'écriture de l'attribut pflname : ");
    SSCRUTE(chemin); SSCRUTE(pflname);goto ERROR;
  }

  /*
   * On ferme tout
   */

  ret = 0;

 ERROR:

  if ( pfluse ) { free(pfltab); free(pfltabtmp);}

  if (datagroup3>0)     if (_MEDdatagroupFermer(datagroup3) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_int(datagroup3); ret = -1; 
  }
  
  if (datagroup2>0)     if (_MEDdatagroupFermer(datagroup2) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_int(datagroup2); ret = -1; 
  }

  if (datagroup1>0)     if (_MEDdatagroupFermer(datagroup1) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_int(datagroup1); ret = -1; 
  }
  
  if (gid>0)     if (_MEDdatagroupFermer(gid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(gid); ret = -1; 
  }
  
  if (gid_loc>0)     if (_MEDdatagroupFermer(gid_loc) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(gid_loc); ret = -1; 
  }

  va_end(params);
  *fret = ret;
  return;
}

