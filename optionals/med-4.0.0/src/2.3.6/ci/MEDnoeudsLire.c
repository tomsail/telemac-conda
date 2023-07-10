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

med_err
MEDnoeudsLire(med_idt fid,char *maa,med_int mdim, med_float *coord,
	      med_mode_switch mode_coo,
	      med_repere *repere,char *nomcoo, char *unicoo,char *nom,
	      med_booleen *inom,med_int *num,med_booleen *inum,med_int *fam,
	      med_int nnoeuds)
{
  med_int i;

  /* 
   * Lecture des coordonnees des noeuds (presence obligatoire => on renvoie -1 en cas d'echec)
   */
  if (MEDcoordLire(fid,maa,mdim,coord,mode_coo,MED_ALL,0,MED_NOPF,repere,nomcoo,unicoo) < 0)
    return -1;

  /* 
   * Lecture des noms des noeuds (presence facultative) 
   */
  if (MEDnomLire(fid,maa,nom,nnoeuds,MED_NOEUD,MED_POINT1) < 0)
    *inom = MED_FAUX;
  else
    *inom = MED_VRAI;

  /* 
   * Lecture des numeros (presence facultative) 
   */
  if (MEDnumLire(fid,maa,num,nnoeuds,MED_NOEUD,MED_POINT1) < 0)
    *inum = MED_FAUX;
  else
    *inum = MED_VRAI;  

  /* 
   * Lecture des numeros de familles : 
   * - si pas de numeros de familles, cela siginifie qu'ils 
   *   tous egaux a 0 (convention définie à partir de MED V2.2) 
   */
  if (MEDfamLire(fid,maa,fam,nnoeuds,MED_NOEUD,MED_POINT1) < 0)
    for (i=0;i<nnoeuds;i++)
      *(fam+i) = 0;

  /*
   * Tout s'est bien passe
   */
  return 0;
}
