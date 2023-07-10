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

med_err 
MEDscalairePasdetempsInfo(med_idt fid,char *scalaire,int indice, 
			  med_int * numdt, char * dt_unit, med_float * dt,med_int * numo)
{
  med_err ret=0;
  med_idt gid;
  char chemin[MED_TAILLE_NUM_DATA+MED_TAILLE_NOM+1+2*MED_MAX_PARA+1];
  int num;
  char nomdatagroup[2*MED_MAX_PARA+1];

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * On construit le chemine d'acces 
   */
  strcpy(chemin,MED_NUM_DATA);
  strcat(chemin,scalaire);
  strcat(chemin,"/");
  num = indice - 1;
  if ((ret = _MEDobjetIdentifier(fid,chemin,num,nomdatagroup)) < 0)
    return -1;
  strcat(chemin,nomdatagroup);
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    return -1;

  /*
   * La liste des attributs
   */
 if ((ret = _MEDattrEntierLire(gid,MED_NOM_NDT,(med_int*) numdt)) < 0)
   return -1;

 if ((ret = _MEDattrFloatLire(gid,MED_NOM_PDT,(med_float*) dt)) < 0)
   return -1;

 if ((ret = _MEDattrStringLire(gid,MED_NOM_UNI,MED_TAILLE_PNOM,dt_unit)) < 0)
   return -1;
 
 if ((ret = _MEDattrEntierLire(gid,MED_NOM_NOR,(med_int*) numo)) < 0)
   return -1;

 /*
  * On ferme tout
  */
 if ((ret = _MEDdatagroupFermer(gid)) < 0)
   return -1;
 
 return ret; 
}
