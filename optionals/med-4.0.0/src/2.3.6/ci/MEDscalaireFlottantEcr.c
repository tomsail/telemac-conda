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

med_err
MEDscalaireFlottantEcr(med_idt fid, char *scalaire,med_float val, med_int numdt, char *dt_unit, med_float dt, med_int numo)
{
  med_err ret = 0;
  med_idt gid,datagroup;
  char nomdatagroup[2*MED_MAX_PARA+1];
  char chemin[MED_TAILLE_NUM_DATA+MED_TAILLE_NOM+1];
  med_int type;
  med_mode_acces MED_MODE_ACCES;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  if ( (MED_MODE_ACCES = _MEDmodeAcces(fid) ) == MED_UNDEF_MODE_ACCES ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier ");
    return -1;
  }

  /* 
   * Si le groupe HDF "scalaire"  n'existe pas => erreur
   */
  strcpy(chemin,MED_NUM_DATA);
  strcat(chemin,scalaire);
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    return -1;

  /* Creation du groupe HDF correspondant au couple <numdt>.<numoo> */
  sprintf(nomdatagroup,"%*li%*li",MED_MAX_PARA,(long ) numdt,MED_MAX_PARA,(long ) numo);

  /* Cree ou ouvre le datagroup nomdatagroup */
  datagroup = 0;   
  if (((datagroup = _MEDdatagroupOuvrir(gid,nomdatagroup)) > 0)    
      && ( MED_MODE_ACCES == MED_LECTURE_AJOUT ))   
    return -1;   
  else   
    if (datagroup < 0)
      if ((datagroup = _MEDdatagroupCreer(gid,nomdatagroup)) < 0)   
	return -1;   
  
  /* Cree ou ouvre l'attribut MED_NOM_NDT */
  if ((ret = _MEDattrEntierEcrire(datagroup,MED_NOM_NDT,&numdt)) < 0)
    return -1;
  
  /* Cree ou ouvre l'attribut MED_NOM_PDT */
  if ((ret = _MEDattrFloatEcrire(datagroup,MED_NOM_PDT,&dt)) < 0)
    return -1;
    
  /* Cree ou ouvre l'attribut MED_NOM_NOR */
  if ((ret = _MEDattrEntierEcrire(datagroup,MED_NOM_NOR,&numo)) < 0)
    return -1;

  /* Cree ou ouvre l'attribut  MED_NOM_UNI */ 
  if ( numdt == MED_NOPDT ) {
    if ((ret = _MEDattrStringEcrire(datagroup,MED_NOM_UNI,MED_TAILLE_PNOM,MED_PNOM_BLANC)) < 0)
      return -1;
  } else
    if ((ret = _MEDattrStringEcrire(datagroup,MED_NOM_UNI,MED_TAILLE_PNOM,dt_unit)) < 0)
      return -1;

  /*
   * Ecriture de la valeur scalaire
   */
  if ((ret = _MEDattrEntierLire(gid,MED_NOM_TYP,&type)) < 0)
    return -1;
  switch(type) 
    {
      case MED_FLOAT64 :
      if ((ret = _MEDattrFloatEcrire(datagroup,MED_NOM_VAL,&val)) < 0)
	return -1;	
      break;

    default :
      return -1;
    }

  /*
   * On ferme tout 
   */
  if ((ret = _MEDdatagroupFermer(datagroup)) < 0)
    return -1;
  if ((ret = _MEDdatagroupFermer(gid)) < 0)
    return -1;

  return ret;
}
