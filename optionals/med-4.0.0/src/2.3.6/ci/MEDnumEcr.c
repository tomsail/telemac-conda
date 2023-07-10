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
MEDnumEcr(med_idt fid,char *maa, med_int *num, med_int n,
	  med_entite_maillage type_ent,med_geometrie_element type_geo)
{
  med_idt root, maaid, entid, geoid, dataset;
  med_err ret;
  med_size dimd[1];
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  char nom_ent[MED_TAILLE_NOM_ENTITE+1];
  char nom_geo[MED_TAILLE_NOM_ENTITE+1];
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
   * On met a jour le nom du Data Group representant
   * le type des entites
   */
   if ((ret = _MEDnomEntite(nom_ent,_type_ent)) < 0)
     return -1;

   /*
    * Si le Data Group des entites n'existe pas on le cree
    */
   if ((entid = _MEDdatagroupOuvrir(maaid,nom_ent)) < 0)
     if ((root = _MEDdatagroupCreer(maaid,nom_ent)) < 0)
       return -1;

   /*
    * Pour les mailles, les faces et le aretes, on cree
    * s'il n'existe pas le Data Group du type geometrique
    */
   if ((_type_ent==MED_MAILLE)||(_type_ent==MED_FACE)||(_type_ent==MED_ARETE))
     {
       if ((ret = _MEDnomGeometrie(nom_geo,type_geo)) < 0)
	 return -1;

       if ((geoid = _MEDdatagroupOuvrir(entid,nom_geo)) < 0)
	 if ((geoid = _MEDdatagroupCreer(entid,nom_geo)) < 0)
	   return -1;
     }
   else 
     geoid = -1;

   /*
    * Creation du Data Set "NUM" 
    */
   if (geoid == -1)
     root = entid;
   else
     root = geoid;
   dimd[0] = n;
#if defined(HAVE_F77INT64)
   if ((ret = _MEDdatasetNumEcrire(root,MED_NOM_NUM,MED_INT64,MED_NO_INTERLACE,MED_DIM1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				(unsigned char*) num)) < 0)
     return -1;
#else
   if ((ret = _MEDdatasetNumEcrire(root,MED_NOM_NUM,MED_INT32,MED_NO_INTERLACE,MED_DIM1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				(unsigned char*) num)) < 0)
     return -1;
#endif

  /*
   * Attribut NBR (nombre de noeuds)
   */
   if ((dataset = _MEDdatasetOuvrir(root,MED_NOM_NUM)) < 0)
     return -1;
   if ((ret = _MEDattrEntierEcrire(dataset,MED_NOM_NBR,&n)) < 0)
     return -1;

   /*
    * On ferme tout
    */
   if ((ret = _MEDdatasetFermer(dataset)) < 0)
     return -1;
   if (geoid != -1)
     if ((ret = _MEDdatagroupFermer(geoid)) < 0)
       return -1;
   if ((ret = _MEDdatagroupFermer(entid)) < 0)
     return -1;
   if ((ret = _MEDdatagroupFermer(maaid)) < 0)
     return -1; 

  return 0; 
}



