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
MEDpolygoneConnLire(med_idt fid, char *maa, med_int *index, med_int ni, 
		   med_int *con, med_entite_maillage type_ent, 
		   med_connectivite type_conn)
{
  med_err ret;
  med_idt maaid, entid, geoid;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];  
  char nom_ent[MED_TAILLE_NOM_ENTITE+1];
  char nom_geo[MED_TAILLE_NOM_ENTITE+1];
  char nom_dataset1[MED_TAILLE_NOM_ENTITE+1], nom_dataset2[MED_TAILLE_NOM_ENTITE+1];
  med_geometrie_element type_geo = MED_POLYGONE;
  med_entite_maillage _type_ent=type_ent;

  if ( type_ent == MED_NOEUD_MAILLE ) _type_ent=MED_NOEUD ;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
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
   * On met a jour le nom du groupe HDF representant
   * le type des entites
   */
   if ((ret = _MEDnomEntite(nom_ent,_type_ent)) < 0)
     return -1;

   /*
    * Si le groupe HDF  des entites n'existe pas => erreur
    */
   if ((entid = _MEDdatagroupOuvrir(maaid,nom_ent)) < 0)
     return -1;

   /*
    * Si le groupe HDF du type geometrique n'existe pas => erreur
    */
  if ((ret = _MEDnomGeometrie30(nom_geo,type_geo)) < 0)
     return -1;
   if ((geoid = _MEDdatagroupOuvrir(entid,nom_geo)) < 0)
     return -1;

   /*
    * lecture des tableaux "index" et "con"
    * stockes dans des dataset HDF
    */
   switch(type_conn)
     {
     case MED_NOD :
       strcpy(nom_dataset1,MED_NOM_INN);
       strcpy(nom_dataset2,MED_NOM_NOD);
       break;

     case MED_DESC :
       strcpy(nom_dataset1,MED_NOM_IND);
       strcpy(nom_dataset2,MED_NOM_DES);
       break;
       
     default :
       return -1;
     }

#if defined(HAVE_F77INT64)
   if ((ret = _MEDdatasetNumLire(geoid,nom_dataset1,MED_INT64,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				 (unsigned char*) index)) < 0)
     return -1;
   if ((ret = _MEDdatasetNumLire(geoid,nom_dataset2,MED_INT64,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				   (unsigned char*) con)) < 0)
     return -1;
#else
   if ((ret = _MEDdatasetNumLire(geoid,nom_dataset1,MED_INT32,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				   (unsigned char*) index)) < 0)
     return -1;
   if ((ret = _MEDdatasetNumLire(geoid,nom_dataset2,MED_INT32,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				   (unsigned char*) con)) < 0)
     return -1;
#endif

   /*
    * On ferme tout 
    */
   if ((ret = _MEDdatagroupFermer(geoid)) < 0)
     return -1;
   if ((ret = _MEDdatagroupFermer(entid)) < 0)
     return -1;
   if ((ret = _MEDdatagroupFermer(maaid)) < 0)
     return -1;

  return ret;
}
