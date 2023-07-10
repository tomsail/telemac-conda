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
MEDequivLire(med_idt fid, char *maa, char *eq, med_int *corr, med_int n,
            med_entite_maillage type_ent,med_geometrie_element typ_geo)
{
  med_idt eqid, datagroup;
  med_err ret;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_EQS+2*MED_TAILLE_NOM+1]; 
  char nomdatagroup[MED_TAILLE_NOM+1];
  char tmp[MED_TAILLE_NOM_ENTITE+1];
  med_entite_maillage _type_ent=type_ent;

  if ( type_ent == MED_NOEUD_MAILLE ) _type_ent=MED_NOEUD ;

  if (typ_geo == MED_TETRA4 || typ_geo == MED_TETRA10 ||
      typ_geo == MED_HEXA8  || typ_geo == MED_HEXA20  ||
      typ_geo == MED_PENTA6 || typ_geo == MED_PENTA15 ||
      typ_geo == MED_PYRA5  || typ_geo == MED_PYRA13)
    return -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /*
   * Si le Data Group de "eq" n'existe pas => erreur
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  strcat(chemin,MED_EQS);
  strcat(chemin,eq);
  if ((eqid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    return -1;

  /*
   * Lecture de l'equivalence
   */
  if ((ret = _MEDnomEntite(nomdatagroup,_type_ent)) < 0)
    return -1;
  if ((_type_ent != MED_NOEUD))
    {
      if ((ret = _MEDnomGeometrie30(tmp,typ_geo)) < 0)
	return -1;
      strcat(nomdatagroup,".");
      strcat(nomdatagroup,tmp);
    }
  if ((datagroup = _MEDdatagroupOuvrir(eqid,nomdatagroup)) < 0)
    return -1;
#if defined(HAVE_F77INT64)
  if ((ret =  _MEDdatasetNumLire(datagroup,MED_NOM_COR,MED_INT64,
				 MED_NO_INTERLACE,1,MED_ALL,
				 MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				 (unsigned char *) corr)) < 0)
    return -1;
#else
  if ((ret =  _MEDdatasetNumLire(datagroup,MED_NOM_COR,MED_INT32,
				 MED_NO_INTERLACE,1,MED_ALL,
				 MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				 (unsigned char *) corr)) < 0)
    return -1;
#endif

  /*
   * On ferme tout 
   */
  if ((ret = _MEDdatagroupFermer(datagroup)) < 0)
    return -1;
  if ((ret = _MEDdatagroupFermer(eqid)) < 0)
    return -1;

  return 0;  
}




