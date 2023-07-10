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

med_int
MEDnGroupe(med_idt fid,char *maa, int indice)
{
  med_idt datagroup,famid;
  med_err ret;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_FAS+MED_TAILLE_FAS_ENTITE+2*MED_TAILLE_NOM+1];
  char stockage[MED_TAILLE_MAA+MED_TAILLE_FAS+MED_TAILLE_FAS_ENTITE+2*MED_TAILLE_NOM+1];
  med_int n;
  int n_tmp;
  int num;
  char famille[MED_TAILLE_NOM+1];
  int nfamnoe,nfammai;

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  strcat(chemin,MED_FAS);

  num = indice - 1;
  
  /* Acces a la famille :
   * nfam = nfamnoe + 1 + nfammai
   * Repartition selon l'indice "num" dans le datagroup :
   *    - 0..nfammai - 1 : familles des mailles/faces/aretes
   *    - nfamai : famille 0
   *    - (nfamai + 1)..(nfammai+nfamnoe) : familles de noeuds 
   */

  /* On va compter les familles de mailles/faces/aretes */
  strcpy(stockage,chemin);
  strcat(stockage,MED_FAS_ELEME_NOM);
  nfammai = 0;
  n_tmp = 0;
  if ((ret =_MEDnObjets(fid,stockage,&n_tmp)) == 0) 
    nfammai = (med_int ) n_tmp;
  strcat(stockage,"/");
  
  /* Pour la famille 0 */
  if (num == nfammai) 
    return 0;

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
  if ((ret = _MEDobjetIdentifier(fid,stockage,num,
				 famille)) < 0)
    return -1;
  strcat(stockage,famille);
  
  if ((famid = _MEDdatagroupOuvrir(fid,stockage)) < 0)
    return -1;
  
  if ((datagroup = _MEDdatagroupOuvrir(famid,MED_NOM_GRO)) < 0)
    n = 0;
  else
    {
      if ((ret = _MEDattrEntierLire(datagroup,MED_NOM_NBR,&n)) < 0)
	return -1;
      if ((ret = _MEDdatagroupFermer(datagroup)) < 0)
	return -1;
    }

  if ((ret = _MEDdatagroupFermer(famid)) < 0)
    return -1;

  return (med_int) n;
}
