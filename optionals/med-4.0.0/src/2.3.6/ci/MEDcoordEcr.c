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
MEDcoordEcr(med_idt fid, char *maa, med_int mdim, med_float *coo, 
	    med_mode_switch mode_coo,med_int n,
	    med_repere type_rep, char *nom, char *unit)
{
  med_idt maaid, noeid, dataset;
  med_err ret;
  med_size dimd[1];
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  med_int type_rep_int; 
  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * Si le maillage n'existe pas => erreur
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  if ((maaid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
      return -1;

  /*
   * Si le Data Group "NOE" n'existe pas
   * on le cree
   */
  if ((noeid = _MEDdatagroupOuvrir(maaid,MED_NOM_NOE)) < 0)
    if ((noeid = _MEDdatagroupCreer(maaid,MED_NOM_NOE)) < 0)
      return -1;

  /*
   * Creation du Data Set "COO"
   */
  dimd[0] = n*mdim;
  if ((ret = _MEDdatasetNumEcrire(noeid,MED_NOM_COO,MED_FLOAT64,mode_coo,mdim,MED_ALL,
                                  MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				  (unsigned char*) coo)) < 0)
    return -1;
  
  /*
   * On re-ouvre le Data Set "COO" pour y placer des attributs
   */
  if ((dataset = _MEDdatasetOuvrir(noeid,MED_NOM_COO)) < 0)
    return -1;

  /*
   * Attribut NBR (nombre de noeuds)
   */
  if ((ret = _MEDattrEntierEcrire(dataset,MED_NOM_NBR,&n)) < 0)
    return -1;

  /*
   * L'attribut "REP"
   */
  type_rep_int = (med_int) type_rep; 
  if ((ret = _MEDattrEntierEcrire(dataset,MED_NOM_REP,&type_rep_int)) < 0)
    return -1;

  /*
   * Attribut "NOM"
   */
  if ((ret = _MEDattrStringEcrire(dataset,MED_NOM_NOM,mdim*MED_TAILLE_PNOM,nom)) < 0)
    return -1;

  /*
   * Attribut "UNI"
   */
  if ((ret = _MEDattrStringEcrire(dataset,MED_NOM_UNI,mdim*MED_TAILLE_PNOM,unit)) < 0)
    return -1;

  /*
   * On ferme tout
   */
  if ((ret = _MEDdatasetFermer(dataset)) < 0)
    return -1;
  if ((ret = _MEDdatagroupFermer(noeid)) < 0)
    return -1;
  if ((ret = _MEDdatagroupFermer(maaid)) < 0)
    return -1;

  return 0; 
}
