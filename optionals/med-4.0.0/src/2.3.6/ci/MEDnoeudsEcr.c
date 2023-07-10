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
MEDnoeudsEcr(med_idt fid,char *maa,med_int mdim,med_float *coord,
	     med_mode_switch mode_coo,
	     med_repere repere,char *nomcoo, char *unicoo,char *nom,
	     med_booleen inom,med_int *num,med_booleen inum,med_int *fam,
	     med_int nnoeuds)
{
  med_err ret;

  /* ecriture des coordonnees */
  if ((ret = MEDcoordEcr(fid,maa,mdim,coord,mode_coo,
			 nnoeuds,repere,nomcoo,
			 unicoo)) < 0)
    return -1;

  /* ecriture des noms (facultatifs) */
  if (inom == MED_VRAI)
    if ((ret = MEDnomEcr(fid,maa,nom,nnoeuds,MED_NOEUD,MED_POINT1)) < 0)
      return -1;

  /* ecriture des numeros (facultatifs) */
  if (inum == MED_VRAI)
    if ((ret = MEDnumEcr(fid,maa,num,nnoeuds,MED_NOEUD,MED_POINT1)) < 0)
      return -1;

  /* ecriture des numeros de familles */
  if ((ret = MEDfamEcr(fid,maa,fam,nnoeuds,MED_NOEUD,MED_POINT1)) < 0)
    return -1;

  return 0;
}
