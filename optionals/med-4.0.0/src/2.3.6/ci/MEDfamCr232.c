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

void
MEDfamCr232(int dummy,...)
{

  med_idt fid;
  char    *maa;
  char    *famille;
  med_int numero; 
  med_int *attr_ident;
  med_int *attr_val;
  char    *attr_desc; 
  med_int n_attr;
  char    *groupe; 
  med_int n_groupe;
  med_err *fret ;

  med_idt root=0, datagroup=0, famid=0;
  med_err ret=-1;
  med_size dimd[1];
  char chemin[MED_TAILLE_MAA+MED_TAILLE_FAS+MED_TAILLE_FAS_ENTITE+MED_TAILLE_NOM+1];
  char tmp[MED_TAILLE_FAS+1];
  char stockage[MED_TAILLE_FAS_ENTITE+1] = "NONE";
  med_mode_acces MED_MODE_ACCES;

  va_list params;
  va_start(params,dummy);

  fid        = va_arg(params,  med_idt );
  maa        = va_arg(params,  char*   );
  famille    = va_arg(params,  char*   );
  numero     = va_arg(params,  med_int ); 
  attr_ident = va_arg(params,  med_int*);
  attr_val   = va_arg(params,  med_int*);
  attr_desc  = va_arg(params,  char*   ); 
  n_attr     = va_arg(params,  med_int );
  groupe     = va_arg(params,  char*   ); 
  n_groupe   = va_arg(params,  med_int );
  fret       = va_arg(params,  med_err*);

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) {*fret=-1;return;}


  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier.");
    goto ERROR;
  }

  if ( MED_MODE_ACCES == MED_LECTURE) {
    MESSAGE("Impossible de créer une famille en mode MED_LECTURE.");
    goto ERROR;
  };

  /* 
   * Si le Data Group FAS n'existe pas, on le cree
   */
  strcpy(chemin,MED_MAA);
  NOFINALBLANK(maa,ERROR);
  strcat(chemin,maa);
  strncpy(tmp,MED_FAS,MED_TAILLE_FAS-1);
  tmp[MED_TAILLE_FAS-1] = '\0';
  strcat(chemin,tmp);

  if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    if ((root = _MEDdatagroupCreer(fid,chemin)) < 0) {
      MESSAGE("Erreur à la création du datagroup : ");
      SSCRUTE(chemin);
      goto ERROR;
    }

  
  /*
   * Le lieu de stockage varie selon le signe du "numero"
   * de famille
   * = 0 => FAMILLE NEUTRE
   * > 0 => FAMILLE DE NOEUD 
   * < 0 => FAMILLE DE MAILLE/FACE/ARETE
   */

  if (numero != 0) {
   if ((ret = _MEDdatagroupFermer(root)) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(root);ret = -1; 
    goto ERROR;
   }

    if (numero > 0) 
      strncpy(stockage,MED_FAS_NOEUD,MED_TAILLE_FAS_ENTITE-1);
    
    if (numero < 0) 
      strncpy(stockage,MED_FAS_ELEME,MED_TAILLE_FAS_ENTITE-1);

    stockage[MED_TAILLE_FAS_ENTITE-1] = '\0'; 
    strcat(chemin,stockage);

    if ((root = _MEDdatagroupOuvrir(fid,chemin)) < 0)
      if ((root = _MEDdatagroupCreer(fid,chemin)) < 0) {
	MESSAGE("Erreur à la création du datagroup : ");
	SSCRUTE(chemin);
	goto ERROR;
      }
  } 


  /*
   * Si le Data Group de meme nom que famille existe => erreur
   * Sinon on le cree
   */

  /*
   * POUR LA FAMILLE DE NUMERO 0, ON FORCE LE NOM 
   */
  if (numero == 0) {
    if ((famid = _MEDdatagroupCreer(root,FAMILLE_ZERO)) < 0) {
      MESSAGE("Erreur à la création du famille : ");
      SSCRUTE(famille);
      goto ERROR;
    }
  } else {
    /*
     * Création de la famille
     */
    NOFINALBLANK(famille,ERROR);
    if ((famid = _MEDdatagroupCreer(root,famille)) < 0) {
      MESSAGE("Erreur à la création du famille : ");
      SSCRUTE(famille);
      goto ERROR;
    }
  }

  /*
   * L'attribut NUM
   */
  if ((ret = _MEDattrEntierEcrire(famid,MED_NOM_NUM,&numero)) < 0) {
    MESSAGE("Erreur à l'écriture du numéro de famille : ");
    ISCRUTE(numero);
    goto ERROR;
  }


  /*
   * Le Data Group "GRO"
   */
  if (n_groupe > 0)  {

    /*
     * On cree le Data Group 
     */
    if ((datagroup = _MEDdatagroupCreer(famid,MED_NOM_GRO)) < 0) {
      MESSAGE("Erreur à la création du datagroup : ");
      SSCRUTE(MED_NOM_GRO);
      goto ERROR;
    }
    /*
     * L'attribut "NBR"
     */
    if ((ret = _MEDattrEntierEcrire(datagroup,MED_NOM_NBR,&n_groupe)) < 0) {
      MESSAGE("Erreur à l'écriture du nombre de groupe : ");
      ISCRUTE(n_groupe);
      goto ERROR;
    }      
    /* 
     * Data Set des noms des groupes "NOM"
     */
    dimd[0] = n_groupe*MED_TAILLE_LNOM+1;
    if ((ret = _MEDdatasetStringEcrire(datagroup,MED_NOM_NOM,dimd,groupe))<0) {
      MESSAGE("Erreur à l'écriture des noms des groupes : ");
      SSCRUTE(groupe);
      goto ERROR;
    }      
    /* 
     * On ferme le Data Group
     */
    if ((ret = _MEDdatagroupFermer(datagroup)) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      ISCRUTE_int(datagroup);
      goto ERROR; 
    }

  }

  
  /*
   * Le Data Group "ATT"
   */
  
  if (n_attr > 0) {

    if ((datagroup = _MEDdatagroupCreer(famid,MED_NOM_ATT)) < 0) {
      MESSAGE("Erreur à la création du datagroup : ");
      SSCRUTE(MED_NOM_ATT);
      goto ERROR;
    } 
    /*
     * L'attribut "NBR"
     */
    if ((ret = _MEDattrEntierEcrire(datagroup,MED_NOM_NBR,&n_attr)) < 0) {
      MESSAGE("Erreur à l'écriture du nombre d'attributs : ");
      ISCRUTE(n_attr);
      goto ERROR;
    } 
    /*
     * Le Data Set "IDE"
     */
    dimd[0] = n_attr;
#if defined(HAVE_F77INT64)
    if ((ret = _MEDdatasetNumEcrire(datagroup,MED_NOM_IDE,MED_INT64,MED_NO_INTERLACE,MED_DIM1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				    (unsigned char *)attr_ident)) < 0) {
      MESSAGE("Erreur à l'écriture du dataset : ");
      SSCRUTE(MED_NOM_IDE);
      goto ERROR;
    }    
#else
    if ((ret = _MEDdatasetNumEcrire(datagroup,MED_NOM_IDE,MED_INT32,MED_NO_INTERLACE,MED_DIM1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				    (unsigned char *)attr_ident)) < 0) {
      MESSAGE("Erreur à l'écriture du dataset : ");
      SSCRUTE(MED_NOM_IDE);
      goto ERROR;
    }    
#endif
    /*
     * Le Data Set "VAL"
     */
    dimd[0] = n_attr;
#if defined(HAVE_F77INT64)
    if ((ret = _MEDdatasetNumEcrire(datagroup,MED_NOM_VAL,MED_INT64,MED_NO_INTERLACE,MED_DIM1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				    (unsigned char*)attr_val)) < 0) {
      MESSAGE("Erreur à l'écriture du dataset : ");
      SSCRUTE(MED_NOM_VAL);
      goto ERROR;
    }    
#else
    if ((ret = _MEDdatasetNumEcrire(datagroup,MED_NOM_VAL,MED_INT32,MED_NO_INTERLACE,MED_DIM1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				    (unsigned char*)attr_val)) < 0) {
      MESSAGE("Erreur à l'écriture du dataset : ");
      SSCRUTE(MED_NOM_VAL);
      goto ERROR;
    }    
#endif
    /*
     * Le Data Set "DES"
     */
    dimd[0] = n_attr*MED_TAILLE_DESC+1;
    if ((ret = _MEDdatasetStringEcrire(datagroup,MED_NOM_DES,dimd,attr_desc)) < 0) {
      MESSAGE("Erreur à l'écriture du dataset : ");
      SSCRUTE(MED_NOM_DES);
      goto ERROR;
    }    
    /* 
     * On ferme le Data Group
     */
    if ((ret = _MEDdatagroupFermer(datagroup)) < 0) {
      MESSAGE("Impossible de fermer le datagroup : ");
      ISCRUTE_int(datagroup);
      goto ERROR; 
    }
  }


  /* 
   * On ferme tout
   */ 
  ret=0;
 ERROR:
  if (famid>0)     if (_MEDdatagroupFermer(famid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(famid);ret = -1; 
  }
  
  if (root>0)     if (_MEDdatagroupFermer(root) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(root); ret = -1; 
  }

  va_end(params);
  *fret = ret;
  return;
}
