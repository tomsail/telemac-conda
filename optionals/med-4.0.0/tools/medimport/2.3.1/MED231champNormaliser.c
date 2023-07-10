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



#include "med_config.h"
#include "med_outils.h"
#include <string.h>

/* #ifdef __cplusplus */
/* } */
/* #endif */

#include "MAJ_231_232.h"

med_err MED231champNormaliser(med_idt fid, char * nomcha, med_type_champ typcha, med_int ncomp,
		    med_entite_maillage entite) {
	      
  int j,k,l,m,n,nb_geo;
  med_int nbpdtnor=0,ngauss=0,*vale=NULL,nval;
  med_int numdt=0,numo=0,nbrefmaa;
  med_float *valr=NULL,dt=0.0;
  med_err ret=0, rett;
  med_booleen local;
  char pflname   [MED_TAILLE_NOM+1]="";
  char locname   [MED_TAILLE_NOM+1]="";
  char maa_ass_i [MED_TAILLE_NOM+1]="";
  char maa_ass   [MED_TAILLE_NOM+1]="";
  char dt_unit   [MED_TAILLE_PNOM+1]="";


  med_geometrie_element * type_geo;
  med_geometrie_element typ_noeud[1] = { MED_NONE };
  med_geometrie_element typmai[MED_NBR_GEOMETRIE_MAILLE+2] = {MED_POINT1, MED_SEG2, MED_SEG3, MED_TRIA3,
							      MED_QUAD4, MED_TRIA6,MED_QUAD8, MED_TETRA4,
							      MED_PYRA5, MED_PENTA6, MED_HEXA8, MED_TETRA10, 
							      MED_PYRA13, MED_PENTA15, MED_HEXA20, 
							      MED_POLYGONE, MED_POLYEDRE};
  med_geometrie_element typfac[MED_NBR_GEOMETRIE_FACE+1] = {MED_TRIA3,MED_TRIA6,
							    MED_QUAD4,MED_QUAD8,
							    MED_POLYGONE};
  med_geometrie_element typare[MED_NBR_GEOMETRIE_ARETE] = {MED_SEG2,MED_SEG3};
  
  char ** AFF;

  switch (entite) {
    case MED_NOEUD :
      type_geo = typ_noeud;
      nb_geo   = 1;
      AFF      = MED_GEOMETRIE_NOEUD_AFF;
      break;
    case  MED_MAILLE :
      type_geo = typmai;
      nb_geo   = MED_NBR_GEOMETRIE_MAILLE+2;
      AFF      =  MED_GEOMETRIE_MAILLE_AFF;
      break;
    case  MED_FACE :
      type_geo = typfac;
      nb_geo   = MED_NBR_GEOMETRIE_FACE+1;
      AFF      =  MED_GEOMETRIE_FACE_AFF;
      break;
    case  MED_ARETE :
      type_geo = typare;
      nb_geo   = MED_NBR_GEOMETRIE_ARETE;
      AFF      =  MED_GEOMETRIE_ARETE_AFF;
     break;
  }

  
  for (k=0;k<nb_geo;k++) {
    
    /* Combien de (PDT,NOR) a lire */
    nbpdtnor = MEDnPasdetemps(fid,nomcha,entite,type_geo[k]);
    if (nbpdtnor < 1 ) continue;

    for (j=0;j<nbpdtnor;j++) {

      /*le nom du maillage associé va être modifié dans MED231champRefInfoEtRenMaa */
      rett = MEDpasdetempsInfo(fid,nomcha,entite,type_geo[k],
			     j+1, &ngauss, &numdt, &numo, dt_unit,
			     &dt, maa_ass_i, &local, &nbrefmaa);
      if ( rett <0) {
	    MESSAGE("Erreur a la demande d'information sur (pdt,nor) : ");
	    ISCRUTE(numdt); ISCRUTE(numo);
	    ret = -1; continue;
      };


      /* Combien de maillages lies aux (nomcha,ent,geo,numdt,numo)  */
      /* Notons que cette information est egalement disponible a partir de MEDpasdetempsInfo */
      nbrefmaa = MEDnChampRef(fid,nomcha,entite,type_geo[k],numdt,numo);
      if ( nbrefmaa < 0 ) {
	    MESSAGE("Erreur a la demande du nombre de maillages references par le champ : ");
	    SSCRUTE(nomcha); ISCRUTE(numdt); ISCRUTE(numo);
	    ret = -1; continue;
      };

      for (l=0;l<nbrefmaa;l++) {

        rett = MED231champRefInfoEtRenMaa(fid,nomcha,entite,type_geo[k],
					  l+1,numdt, numo, maa_ass, &local, &ngauss);
	    if ( rett < 0 ) {
	      MESSAGE("Erreur a la demande d'information sur le maillage utilise par le champ n° : ");
	      ISCRUTE_int(l+1);
	      ret = -1; continue;
	    };

	    
	    /* Prend en compte le nbre de pt de gauss automatiquement */
        nval = MEDnVal(fid,nomcha,entite,type_geo[k],numdt,numo,maa_ass,MED_GLOBAL);
	    if (nval <= 0)   {
	      MESSAGE("Erreur a la lecture du nombre de valeurs du champ : ");
	      SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	      ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(maa_ass);
	      ret = -1; continue;
	    };

	    /*Lecture des valeurs du champ */
	    if (typcha == MED_FLOAT64) {

	      valr = (med_float*) calloc(ncomp*nval,sizeof(med_float));
	      EXIT_IF(valr == NULL,NULL,NULL);
	      rett = MED231champLireEtUnlink(fid,maa_ass,nomcha,(unsigned char*)valr,MED_NO_INTERLACE,MED_ALL,locname,
		    	    pflname,MED_GLOBAL,entite,type_geo[k],numdt,numo);
	      if ( rett < 0 ) {
	        MESSAGE("Erreur a la lecture des valeurs du champ : ");
	        SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	        ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(maa_ass);
	        ret = -1;
	      };

	    }
        else {
	  
	      vale = (med_int*) calloc(ncomp*nval,sizeof(med_int));
	      EXIT_IF(vale == NULL,NULL,NULL);
	      rett = MED231champLireEtUnlink(fid,maa_ass,nomcha,(unsigned char*)vale,MED_NO_INTERLACE,MED_ALL,locname,
		    	    pflname,MED_GLOBAL,entite,type_geo[k],numdt,numo);
	      if ( rett < 0 ) {
	        MESSAGE("Erreur a la lecture des valeurs du champ : ");
	        SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
	        ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(maa_ass);
	        ret = -1;
	      };
	  
	    }

        if (strcmp(pflname,MED_NOPFL)) {
    	    if (typcha == MED_FLOAT64) {

              rett = MEDchampEcr(fid,maa_ass,nomcha,(unsigned char*)valr,MED_NO_INTERLACE,nval,locname,
    	                MED_ALL,pflname,MED_GLOBAL,entite,type_geo[k],numdt,dt_unit,dt,numo);
    	      if ( rett < 0 ) {
    	        MESSAGE("Erreur a l'ecriture du nombre de valeurs du champ : ");
    	        SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
    	        ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(maa_ass);
    	        ret = -1;
    	      };

    	    }
            else {

              rett = MEDchampEcr(fid,maa_ass,nomcha,(unsigned char*)vale,MED_NO_INTERLACE,nval,locname,
    	                MED_ALL,pflname,MED_GLOBAL,entite,type_geo[k],numdt,dt_unit,dt,numo);
    	      if ( rett < 0 ) {
    	        MESSAGE("Erreur a l'ecriture des valeurs du champ : ");
    	        SSCRUTE(nomcha);ISCRUTE_int(entite);ISCRUTE_int(type_geo[k]);
    	        ISCRUTE(numdt);ISCRUTE(numo);SSCRUTE(maa_ass);
    	        ret = -1;
    	      };

    	    }
    	}


        if (typcha == MED_FLOAT64) {
	      if ( valr ) {free(valr);valr = NULL;}}
        else
	      if (vale) { free(vale);vale = NULL; }

      }
    }
  } /* fin for sur les mailles*/

  return ret;
}
