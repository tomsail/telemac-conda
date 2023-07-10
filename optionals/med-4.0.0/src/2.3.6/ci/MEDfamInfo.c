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

#include <stdlib.h>
#include <string.h>

med_err
MEDfamInfo(med_idt fid,char *maa,int indice, char *famille,
	   med_int *numero,
	   med_int *attr_ident, med_int *attr_val, char *attr_desc,
	   med_int *n_attr, char *groupe ,med_int *n_groupe)
{
  med_idt famid,datagroup;
  med_err ret;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_FAS+MED_TAILLE_FAS_ENTITE+2*MED_TAILLE_NOM+1];
  char stockage[MED_TAILLE_MAA+MED_TAILLE_FAS+MED_TAILLE_FAS_ENTITE+2*MED_TAILLE_NOM+1];
  int num;
  int nfamnoe,nfammai;
  int n_tmp;
  med_booleen _isfam0=MED_FAUX;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
  if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On recupere le nom de la famille
   */
  num = indice - 1;
  strcpy(famille,"");

  /* Acces a la famille :
   * nfam = nfamnoe + 1 + nfammai
   * Repartition selon l'indice "num" dans le datagroup :
   *    - 0..nfammai - 1 : familles des mailles/faces/aretes
   *    - nfamai : famille 0
   *    - (nfamai + 1)..(nfammai+nfamnoe) : familles de noeuds
   */

  /* On va compter les familles de mailles/faces/aretes */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  strcat(chemin,MED_FAS);

  strcpy(stockage,chemin);
  strcat(stockage,MED_FAS_ELEME_NOM);
  nfammai = 0;
  n_tmp = 0;
  if ((ret =_MEDnObjets(fid,stockage,&n_tmp)) == 0)
    nfammai = (med_int ) n_tmp;
  strcat(stockage,"/");

  /* Pour la famille 0 */
  if (num == nfammai) {
    strcpy(stockage,chemin);
    strcpy(famille,FAMILLE_ZERO);
  }

  if (num > nfammai) {
    /* C'est une famille de noeuds */
    strcpy(stockage,chemin);
    strcat(stockage,MED_FAS_NOEUD_NOM);
    strcat(stockage,"/");
    num = num - nfammai - 1;
  }


  /*
   * Si le Data Group de la famille n'existe pas => erreur
   */
  /* pour la famille 0, on connait déjà le nom */
  if (strcmp(famille,FAMILLE_ZERO)) {
    _isfam0=MED_FAUX;
    if ((ret = _MEDobjetIdentifier(fid,stockage,num, famille)) < 0)
      return -1;
  } else
    _isfam0=MED_VRAI;

  strcat(stockage,famille);
  if ((famid = _MEDdatagroupOuvrir(fid,stockage)) < 0)
    return -1;

  /*
   * L'attribut NUM
   */
  if ((ret = _MEDattrEntierLire(famid,MED_NOM_NUM,numero)) < 0)
    return -1;

  /*
   * Le Data Group "GRO"
   */
  if (!_isfam0) { /*famille différente de la famille zéro*/
    if ((datagroup = _MEDdatagroupOuvrir(famid,MED_NOM_GRO)) >= 0) {
      /*
       * L'attribut "NBR"
       */
      if ((ret = _MEDattrEntierLire(datagroup,MED_NOM_NBR,n_groupe)) < 0)
	return -1;

      /*
       * Data Set des noms des groupes "NOM"
       */
      if ((ret = _MEDdatasetStringLire(datagroup,MED_NOM_NOM,groupe)) < 0)
	return -1;

      /*
       * On ferme le Data Group
       */
      if ((ret = _MEDdatagroupFermer(datagroup)) < 0) return -1;
    } else
      *n_groupe = 0;
  } else
    *n_groupe = 0;


  /*
   * Le Data Group "ATT"
   */

  if (!_isfam0) { /*famille différente de la famille zéro*/
    if ( (datagroup = _MEDdatagroupOuvrir(famid,MED_NOM_ATT)) >= 0) {
      /*
       * L'attribut "NBR"
       */
      if ((ret = _MEDattrEntierLire(datagroup,MED_NOM_NBR,n_attr)) < 0)
	return -1;

      /*
       * Le Data Set "IDE"
       */
#if defined(HAVE_F77INT64)
      if ((ret = _MEDdatasetNumLire(datagroup,MED_NOM_IDE,MED_INT64,
				    MED_NO_INTERLACE,1,MED_ALL,
				    MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				    (unsigned char*) attr_ident)) < 0)
	return -1;
#else
      if ((ret = _MEDdatasetNumLire(datagroup,MED_NOM_IDE,MED_INT32,
				    MED_NO_INTERLACE,1,MED_ALL,
				    MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				    (unsigned char*) attr_ident)) < 0)
	return -1;
#endif

      /*
       * Le Data Set "VAL"
       */
#if defined(HAVE_F77INT64)
      if ((ret = _MEDdatasetNumLire(datagroup,MED_NOM_VAL,MED_INT64,
				    MED_NO_INTERLACE,1,MED_ALL,
				    MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				    (unsigned char *) attr_val)) < 0)
	return -1;
#else
      if ((ret = _MEDdatasetNumLire(datagroup,MED_NOM_VAL,MED_INT32,
				    MED_NO_INTERLACE,1,MED_ALL,
				    MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				    (unsigned char *) attr_val)) < 0)
	return -1;
#endif

      /*
       * Le Data Set "DES"
       */
      ret = _MEDdatasetStringLire(datagroup,MED_NOM_DES,attr_desc);

      /*
       * On ferme le Data Group
       */
      if ((ret = _MEDdatagroupFermer(datagroup)) < 0)
	return -1;
    } else
      *n_attr = 0;
  } else
    *n_attr = 0;

  /*
   * On ferme tout
   */

  if ((ret = _MEDdatagroupFermer(famid)) < 0)
    return -1;

  return 0;
}



