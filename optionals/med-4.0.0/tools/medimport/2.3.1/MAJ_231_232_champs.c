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



#include "med_config.h"
#include "med_outils.h"

/* #ifdef __cplusplus */
/* } */
/* #endif */

#include "med_hdfi231.h"
#include "MAJ_231_232.h"

void MAJ_231_232_champs(med_idt fid)
{
  med_int ncomp,ncha;
  med_err lret,rett;
  med_type_champ typcha;
  char nomcha  [MED_TAILLE_NOM+1]="";
  char *comp, *unit;
  int i;

  /* combien de champs dans le fichier */
  ncha = MEDnChamp(fid,0);
  EXIT_IF(ncha < 0,"lors de la lecture du nombre de champs",NULL);

  /****************************************************************************
  *                       LECTURE DES CHAMPS                                  *
  ****************************************************************************/

  /* lecture de tous les champs  */
  for (i =0;i<ncha;i++) {
    lret = 0;

    /* Lecture du nombre de composantes */
    ncomp = MEDnChamp(fid,i+1);
    if (ncomp < 0) {
      MESSAGE("Erreur à la lecture du nombre de composantes : "); ISCRUTE(ncomp); 
      exit(1);
    }
    
    /* Lecture du type du champ, des noms des composantes et du nom de l'unité*/
    comp = (char*) malloc(ncomp*MED_TAILLE_PNOM+1);
    EXIT_IF(comp == NULL,NULL,NULL);
    unit = (char*) malloc(ncomp*MED_TAILLE_PNOM+1);
    EXIT_IF(unit == NULL,NULL,NULL);
      
    rett = MED231champInfoEtRen(fid,i+1,nomcha,&typcha,comp,unit,ncomp);
    if ( rett < 0 ) {
      MESSAGE("Erreur à la demande d'information sur les champs ");
      exit(1);
    }
      
    free(comp);
    free(unit);

      
    lret = MED231champNormaliser(fid, nomcha, typcha, ncomp, MED_NOEUD);
    if (lret != 0) {
      MESSAGE("Erreur à la lecture des champs aux noeuds "); exit(1);
    }

    lret = MED231champNormaliser(fid, nomcha, typcha, ncomp, MED_MAILLE);
    if (lret != 0) {
      MESSAGE("Erreur à la lecture des champs aux mailles "); exit(1);
    }

    lret = MED231champNormaliser(fid, nomcha, typcha, ncomp, MED_FACE);
    if (lret != 0) {
      MESSAGE("Erreur à la lecture des champs aux faces "); exit(1);
    }

    lret = MED231champNormaliser(fid, nomcha, typcha, ncomp, MED_ARETE);
    if (lret != 0) {
      MESSAGE("Erreur à la lecture des champs aux aretes "); exit(1);
    }
  }
}
