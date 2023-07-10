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
MEDpolyedreConnEcr(med_idt fid,char *maa,med_int *indexp,med_int np,med_int *indexf,med_int nf,
		   med_int *con,med_connectivite type_conn)
{
  med_err ret;
  med_idt maaid, entid, geoid, dataset;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];  
  char nom_ent[MED_TAILLE_NOM_ENTITE+1];
  char nom_geo[MED_TAILLE_NOM_ENTITE+1];
  char nom_dataset1[MED_TAILLE_NOM_ENTITE+1];
  char nom_dataset2[MED_TAILLE_NOM_ENTITE+1];
  char nom_dataset3[MED_TAILLE_NOM_ENTITE+1];
  med_geometrie_element type_geo;
  med_entite_maillage type_ent;
  med_int n;
  med_size dimd[1];
  med_int consize;

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
  type_ent = MED_MAILLE;
   if ((ret = _MEDnomEntite(nom_ent,type_ent)) < 0)
     return -1;

   /*
    * Si le groupe HDF  des entites n'existe pas on le cree
    */
   if ((entid = _MEDdatagroupOuvrir(maaid,nom_ent)) < 0)
     if ((entid = _MEDdatagroupCreer(maaid,nom_ent)) < 0)
       return -1;

   /*
    * On cree s'il n'existe pas le groupe HDF du type geometrique
    */
   type_geo = MED_POLYEDRE;
   if ((ret = _MEDnomGeometrie(nom_geo,type_geo)) < 0)
     return -1;
   if ((geoid = _MEDdatagroupOuvrir(entid,nom_geo)) < 0)
     if ((geoid = _MEDdatagroupCreer(entid,nom_geo)) < 0)
       return -1;

   /*
    * On regarde si le dataset HDF existe et on le cree sinon
    */
   switch(type_conn)
     {
     case MED_NOD :
       strcpy(nom_dataset1,MED_NOM_INN);
       strcpy(nom_dataset2,MED_NOM_IFN);
       strcpy(nom_dataset3,MED_NOM_NOD);
       consize = indexf[nf-1] - indexf[0];
       break;

     case MED_DESC :
       strcpy(nom_dataset1,MED_NOM_IND);
       strcpy(nom_dataset2,MED_NOM_IFD);
       strcpy(nom_dataset3,MED_NOM_DES);
       consize = nf;
       break;
       
     default :
       return -1;
     }

#if defined(HAVE_F77INT64)
   dimd[0] = np;
   if ((ret = _MEDdatasetNumEcrire(geoid,nom_dataset1,MED_INT64,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				   (unsigned char*) indexp)) < 0)
     return -1;
   dimd[0] = nf;
   if ((ret = _MEDdatasetNumEcrire(geoid,nom_dataset2,MED_INT64,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				   (unsigned char*) indexf)) < 0)
     return -1;
   dimd[0] = consize;
   if ((ret = _MEDdatasetNumEcrire(geoid,nom_dataset3,MED_INT64,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				   (unsigned char*) con)) < 0)
     return -1;
#else
   dimd[0] = np;
   if ((ret = _MEDdatasetNumEcrire(geoid,nom_dataset1,MED_INT32,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				   (unsigned char*) indexp)) < 0)
     return -1;
   dimd[0] = nf;
   if ((ret = _MEDdatasetNumEcrire(geoid,nom_dataset2,MED_INT32,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				   (unsigned char*) indexf)) < 0)
     return -1;
   dimd[0] = consize;
   if ((ret = _MEDdatasetNumEcrire(geoid,nom_dataset3,MED_INT32,MED_NO_INTERLACE,1,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,0,MED_NOPG,dimd,
				   (unsigned char*) con)) < 0)
     return -1;
#endif

  /*
   * On stocke le nombre de polyedre : (np-1)
   */
   n = np - 1;
   if ((dataset = _MEDdatasetOuvrir(geoid,nom_dataset3)) < 0)
     return -1;
   if ((ret = _MEDattrEntierEcrire(dataset,MED_NOM_NBR,&n)) < 0)
     return -1;
   if ((ret = _MEDdatasetFermer(dataset)) < 0)
     return -1;

   /*
    * On stocke la taille du tableau indexf (nf)
    */
   n = nf;
   if ((dataset = _MEDdatasetOuvrir(geoid,nom_dataset3)) < 0)
     return -1;
   if ((ret = _MEDattrEntierEcrire(dataset,MED_NOM_TTI,&n)) < 0)
     return -1;
   if ((ret = _MEDdatasetFermer(dataset)) < 0)
     return -1;

   /*
    * On stocke la taille du tableau conn (consize)
    */
   n = consize;
   if ((dataset = _MEDdatasetOuvrir(geoid,nom_dataset3)) < 0)
     return -1;
   if ((ret = _MEDattrEntierEcrire(dataset,MED_NOM_TAI,&n)) < 0)
     return -1;
   if ((ret = _MEDdatasetFermer(dataset)) < 0)
     return -1;

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
