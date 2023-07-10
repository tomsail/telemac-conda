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

extern int mode_interlace;

med_err 
MEDcoordLire(med_idt fid, char *maa, med_int mdim, med_float *coo,
	     med_mode_switch mode_coo,med_int numco,
	     med_int * pfltabtmp, med_size psize, med_repere *type_rep, char *nom, char *unit)
{
  med_idt   maaid, noeid, dataset;
  med_err   ret;
  char      chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  int       i,j;
  med_float *new_coo;
  med_int   type_rep_int;
  med_size * pfltab;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * Si le maillage n'existe pas => erreur
   * Sinon on recupere sa dimension au passage
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
      return -1;

  /*
   * Si le Data Group "NOE" n'existe pas => erreur
   */
  if ((noeid = _MEDdatagroupOuvrir(maaid,MED_NOM_NOE)) < 0)
      return -1;

  /*
   * Convertion de med_int en med_size
   */
  if ( psize != MED_NOPF ) {  
    pfltab = (med_size *) malloc (sizeof(med_size)*psize);
    for (i=0;i<psize;i++)
      pfltab[i] = (med_size) pfltabtmp[i];
  }

  /*
   * Lecture du Data Set "COO"
   */
  if ((ret = _MEDdatasetNumLire(noeid,MED_NOM_COO,MED_FLOAT64,
				mode_coo,mdim,numco,
				psize,MED_COMPACT,MED_PFL_NON_COMPACT,pfltab,MED_NOPG,0,
				(unsigned char*) coo)) < 0)
    return -1;


  /*
   * On re-ouvre le Data Set "COO" pour y lire des attributs
   */
  if ((dataset = _MEDdatasetOuvrir(noeid,MED_NOM_COO)) < 0)
    return -1;

  /*
   * L'attribut "REP"
   */
  if ((ret = _MEDattrEntierLire(dataset,MED_NOM_REP,&type_rep_int)) < 0)
    return -1;
  else
    *type_rep = (med_repere) type_rep_int;

  /*
   * Attribut "NOM"
   */
  if ((ret = _MEDattrStringLire(dataset,MED_NOM_NOM,mdim*MED_TAILLE_PNOM,
				nom)) < 0)
    return -1;

  /*
   * Attribut "UNI"
   */
  if ((ret = _MEDattrStringLire(dataset,MED_NOM_UNI,mdim*MED_TAILLE_PNOM,
				unit)) < 0)
    return -1;

  /*
   * On ferme tout
   */
  if ( psize != MED_NOPF ) free(pfltab); 
  
  if ((ret = _MEDdatasetFermer(dataset)) < 0)
    return -1;
  if ((ret = _MEDdatagroupFermer(noeid)) < 0)
    return -1;
  if ((ret = _MEDdatagroupFermer(maaid)) < 0)
    return -1;

  return 0; 
}
