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
MEDconnLire(med_idt fid,char *maa,med_int mdim,med_int *connectivite,med_mode_switch mode_switch,
	    med_int * pfltabtmp, med_size psizetmp,
	    med_entite_maillage type_ent, med_geometrie_element type_geo,med_connectivite type_conn)
{
  med_idt maaid,entid,geoid;
  med_err ret;
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  char nom_ent[MED_TAILLE_NOM_ENTITE+1];
  char nom_geo[MED_TAILLE_NOM_ENTITE+1];
  char nom_dataset[MED_TAILLE_NOM_ENTITE+1];
  med_size * pfltab=NULL;
  med_size    psize;
  int dim,nnoe,ndes;
  int taille;  
  int i,j;
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
    * Si le Data Group des entites n'existe pas => erreur
    */
   if ((entid = _MEDdatagroupOuvrir(maaid,nom_ent)) < 0)
     return -1;

   /*
    * si le Data Group du type geometrique n'existe pas => erreur
    */
   if ((ret = _MEDnomGeometrie30(nom_geo,type_geo)) < 0)
     return -1;
   if ((geoid = _MEDdatagroupOuvrir(entid,nom_geo)) < 0)
     return -1;

   /*
    * Si le Data Set de la connectivite n'existe pas => erreur
    * Si oui => on le lit
    */
   if ((ret=_MEDparametresGeometrie(_type_ent,type_geo,&dim,&nnoe,&ndes))<0)
     return -1;

   psize = psizetmp;
   switch(type_conn)
     {
     case MED_NOD :
       strcpy(nom_dataset,MED_NOM_NOD);
       taille = nnoe;
       break;

     case MED_DESC :
       strcpy(nom_dataset,MED_NOM_DES);
       taille = ndes;
       if ( psizetmp != MED_NOPF ) {
	 psize = psizetmp;
	 pfltab = (med_size *) malloc (sizeof(med_size)*psize);
	 for (i=0;i<psizetmp;i++)
	   pfltab[i] = (med_size) (pfltabtmp[i]);
       };
       
       break;
       
     default :
       return -1;
     }


#if defined(HAVE_F77INT64)
   if ((ret = _MEDdatasetNumLire(geoid,nom_dataset,MED_INT64,
				 mode_switch,(med_size)taille,MED_ALL,
				 psize,MED_COMPACT,MED_PFL_NON_COMPACT,pfltab,MED_NOPG,0,
				 (unsigned char*) connectivite)) < 0)
     return -1;
#else
   if ((ret = _MEDdatasetNumLire(geoid,nom_dataset,MED_INT32,
				 mode_switch,(med_size) taille,MED_ALL,
				 psize,MED_COMPACT,MED_PFL_NON_COMPACT,pfltab,MED_NOPG,0,
				 (unsigned char*) connectivite)) < 0)
     return -1;
#endif 

   /*
    * On ferme tout 
    */

   if ( (psize != MED_NOPF) && (type_conn == MED_DESC) ) free(pfltab);
  
   if ((ret = _MEDdatagroupFermer(geoid)) < 0)
     return -1;
   if ((ret = _MEDdatagroupFermer(entid)) < 0)
     return -1;
   if ((ret = _MEDdatagroupFermer(maaid)) < 0)
     return -1;

   return 0; 
}

