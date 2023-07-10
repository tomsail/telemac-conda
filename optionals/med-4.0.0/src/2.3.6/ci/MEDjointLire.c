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
MEDjointLire  (med_idt fid, char *maa, char *jn, med_int *corrtab,med_int n,
	       med_entite_maillage type_ent_local,   med_geometrie_element typ_geo_local,
	       med_entite_maillage type_ent_distant, med_geometrie_element typ_geo_distant)
	      
{
  med_entite_maillage _type_ent_local   = (med_entite_maillage) ( (int)(type_ent_local) % 10 );
  med_entite_maillage _type_ent_distant = (med_entite_maillage) ( (int)(type_ent_distant) % 10 );
  med_idt corrid, datagroup;
  med_err ret;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_JNT+2*MED_TAILLE_NOM+1]; 
  char nomdatagroup[MED_TAILLE_NOM+1+MED_TAILLE_NOM+1];
  char tmp[MED_TAILLE_NOM_ENTITE+1];
  med_size dimd[1];

/*   if (typ_geo_local   == MED_TETRA4 || typ_geo_local    == MED_TETRA10 || */
/*       typ_geo_local   == MED_HEXA8  || typ_geo_local    == MED_HEXA20  || */
/*       typ_geo_local   == MED_PENTA6 || typ_geo_local    == MED_PENTA15 || */
/*       typ_geo_local   == MED_PYRA5  || typ_geo_local    == MED_PYRA13  || */
/*       typ_geo_distant == MED_TETRA4 || typ_geo_distant == MED_TETRA10 || */
/*       typ_geo_distant == MED_HEXA8  || typ_geo_distant == MED_HEXA20  || */
/*       typ_geo_distant == MED_PENTA6 || typ_geo_distant == MED_PENTA15 || */
/*       typ_geo_distant == MED_PYRA5  || typ_geo_distant == MED_PYRA13) */
/*     return -1; */

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /* 
   * Si le Data Group de "JNT/corr" n'existe pas => erreur
   */
  strcpy(chemin,MED_MAA);
  strcat(chemin,maa);
  strcat(chemin,MED_JNT);
  strcat(chemin,jn);
  if ((corrid = _MEDdatagroupOuvrir(fid,chemin)) < 0)
    return -1;  
  
  /*
   * lecture de la correspondance
   *   construction du tag HDF "reperant" la correspondance 
   *   
   */
  if ((ret = _MEDnomEntite(nomdatagroup,_type_ent_local)) < 0)
    return -1;
  if ((_type_ent_local != MED_NOEUD))
    {
      if ((ret = _MEDnomGeometrie30(tmp,typ_geo_local)) < 0)
	return -1;
      strcat(nomdatagroup,".");
      strcat(nomdatagroup,tmp);
    }



  if ((ret = _MEDnomEntite(tmp,_type_ent_distant)) < 0)
    return -1;
  strcat(nomdatagroup,".");
  strcat(nomdatagroup,tmp);
  if ((_type_ent_distant != MED_NOEUD))
    {
      if ((ret = _MEDnomGeometrie30(tmp,typ_geo_distant)) < 0)
	return -1;
      strcat(nomdatagroup,".");
      strcat(nomdatagroup,tmp);
    }


  /* 
   * Lecture de la correspondance
   */

  if (((datagroup = _MEDdatagroupOuvrir(corrid,nomdatagroup)) < 0) )
    return -1;


#if defined(HAVE_F77INT64)
  if ((ret =  _MEDdatasetNumLire(datagroup,MED_NOM_COR,MED_INT64,
				 MED_NO_INTERLACE,1,MED_ALL,
				 MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				 (unsigned char *) corrtab)) < 0)
    return -1;
#else
  if ((ret =  _MEDdatasetNumLire(datagroup,MED_NOM_COR,MED_INT32,
				 MED_NO_INTERLACE,1,MED_ALL,
				 MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				 (unsigned char *) corrtab)) < 0)
    return -1;
#endif


  /*
   * On ferme tout 
   */
  if ((ret = _MEDdatagroupFermer(datagroup)) < 0)
    return -1;
  if ((ret = _MEDdatagroupFermer(corrid)) < 0)
    return -1;

  return 0; 
}



