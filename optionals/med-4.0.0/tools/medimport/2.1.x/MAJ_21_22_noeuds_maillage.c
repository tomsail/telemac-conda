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

/* #ifdef __cplusplus */
/* } */
/* #endif */

#include "med_hdfi21.h"
#include "med21.h"
#include "med_hdfi231.h"
#include "MAJ_21_22.h"

void MAJ_21_22_noeuds_maillage(med_idt mid, med_int dimension)
{
  med_idt nid, gid, tid;
  med_float *coo;
  char *nom, *unit;
  char tmp[MED_TAILLE_PNOM+1];
  char *nouvelle_chaine;
  med_int n;
  med_size dimd[1];
  med_err ret;
  int i;
  hid_t hdf_type;
  med_repere repere;

  /* Accès aux noeuds du maillage */
  nid = _MEDdatagroupOuvrir(mid,(char *)(MED_NOM_NOE));
  EXIT_IF(nid < 0,"Ouverture du groupe HDF MED_NOM_NOE",NULL);

  /* Lecture du nombre de noeuds */
  /* Attention sur les grilles en version 2.1.6-OCC, cet attribut n'existait pas ! */
  /* Ces fichiers n'ont jamais été officiellement supportés par MED */
  gid = _MEDdatasetOuvrir(nid,(char *)(MED_NOM_COO));
  if (gid > 0) 
     ret = _MEDattrEntierLire(gid,(char *)(MED_NOM_NBR),&n);
  EXIT_IF(gid < 0,"Lecture du nombre de noeuds",NULL);
  
  /* Mise a jour des coordonnees */
  /* On cree un groupe HDF tampon */
  /* Lecture des coordonnées */
  hdf_type = H5Dget_type(gid);
  EXIT_IF(hdf_type < 0,"Lecture du type HDF de stockage des coordonnées",NULL);
  coo = (med_float *) malloc(sizeof(med_float)*n*dimension);
  EXIT_IF(coo == NULL,NULL,NULL);
  ret = _MED21datasetNumLire(nid,(char *)(MED_NOM_COO),MED_FLOAT64, 
 			      MED_FULL_INTERLACE,dimension,MED_ALL, 
 			      0,NULL,MED_NOPG, 
 			      (unsigned char*) coo,hdf_type); 
  EXIT_IF(ret < 0,"Lecture des coordonnées des noeuds",NULL);
  dimd[0] = n*dimension;
  /*   printf("EGALITE des types : %d \n",H5Tequal(hdf_type,H5T_NATIVE_DOUBLE)); */
  /*   printf("Avant conversion : [%f] [%f] [%f] \n",*(coo),*(coo+1),*(coo+2)); */
  if (! H5Tequal(hdf_type,H5T_NATIVE_DOUBLE)) {
    ret = H5Tconvert(hdf_type,H5T_NATIVE_DOUBLE,(hsize_t)*dimd,(void *)coo,NULL,0);
    /*   printf("Après conversion éventuelle : [%f] [%f] [%f] \n",*(coo),*(coo+1),*(coo+2)); */
    EXIT_IF(ret < 0,"Conversion des coordonnées des noeuds",NULL);
  }
  ret = H5Tclose(hdf_type);
  EXIT_IF(ret < 0,"Fermeture du data type HDF",NULL);
  /* On reporte les coordonnées dans le groupe HDF TMP */
  ret = _MED231datasetNumEcrire(nid,"TMP",MED_FLOAT64,MED_FULL_INTERLACE,
				dimension,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,MED_NOPG,dimd,
				(unsigned char*) coo);
  EXIT_IF(ret < 0,"Ecriture des coordonnées des noeuds après conversion",NULL);
  free(coo);

  /* Ecriture du nombre de noeuds et du type du repertoire dans le dataset TMP */
  tid = _MEDdatasetOuvrir(nid,"TMP");
  EXIT_IF(tid < 0,"Ouverture du dataset HDF TMP",NULL);
  ret = _MEDattrEntierEcrire(tid,(char *)(MED_NOM_NBR),&n);
  EXIT_IF(ret < 0,"Ecriture du nombre de noeuds dans le dataset HDF TMP",NULL);
  ret = _MEDattrEntierLire(gid,(char *)(MED_NOM_REP),&repere);
  EXIT_IF(ret < 0,"Lecture du type de repere des coordonnées", NULL);
  ret = _MEDattrEntierEcrire(tid,(char *)(MED_NOM_REP),&repere);
  EXIT_IF(ret < 0,"Ecriture du type de répertoire dans le dataset HDF TMP",NULL);

  /* Mise a jour des noms et types des unités des coordonnees */
  nom = (char *) malloc(dimension*ANCIEN_MED_TAILLE_PNOM+1);
  EXIT_IF(nom == NULL,NULL,NULL);
  unit = (char *) malloc(dimension*ANCIEN_MED_TAILLE_PNOM+1);
  EXIT_IF(unit == NULL,NULL,NULL);
  nouvelle_chaine = (char *) malloc(dimension*MED_TAILLE_PNOM+1);
  EXIT_IF(nouvelle_chaine == NULL,NULL,NULL);
  ret = _MEDattrStringLire(gid,(char *)(MED_NOM_NOM),dimension*ANCIEN_MED_TAILLE_PNOM,nom);
  EXIT_IF(ret < 0,"Lecture des noms des coordonnées des noeuds",NULL);
  ret = _MEDattrStringLire(gid,(char *)(MED_NOM_UNI),dimension*ANCIEN_MED_TAILLE_PNOM,unit);
  EXIT_IF(ret < 0,"Lecture des unités des coordonnées des noeuds",NULL);

/*   MAJ_21_22_chaine(nom,nouvelle_chaine,dimension); */
  _MED23v30stringConvert(nouvelle_chaine, MED_TAILLE_PNOM,
			 nom, ANCIEN_MED_TAILLE_PNOM, dimension );
  ret = _MEDattrStringEcrire(tid,(char *)(MED_NOM_NOM),dimension*MED_TAILLE_PNOM,nouvelle_chaine);
  EXIT_IF(ret < 0,"Ecriture des nouveaux noms des coordonnées des noeuds",NULL);
  _MED23v30stringConvert(nouvelle_chaine, MED_TAILLE_PNOM,
			 unit, ANCIEN_MED_TAILLE_PNOM, dimension );
/*   MAJ_21_22_chaine(unit,nouvelle_chaine,dimension); */
  ret = _MEDattrStringEcrire(tid,(char *)(MED_NOM_UNI),dimension*MED_TAILLE_PNOM,nouvelle_chaine);
  EXIT_IF(ret < 0,"Ecriture des nouvelles unités des coordonnées des noeuds",NULL);
  free(nom);
  free(unit);
  free(nouvelle_chaine);

  /* Fermeture de l'accès aux coordonnées */
  ret = _MEDdatasetFermer(gid);
  EXIT_IF(ret < 0,"Fermeture du dataset HDF MED_NOM_COO",NULL);
  ret = H5Gunlink(nid,(char *)(MED_NOM_COO));
  EXIT_IF(ret < 0,"Suppression des anciennes coordonnées",NULL);
  ret = _MEDdatasetFermer(tid);
  EXIT_IF(ret < 0,"Fermeture du dataset HDF TMP",NULL);
  ret = H5Gmove(nid,"TMP",(char *)(MED_NOM_COO));
  EXIT_IF(ret < 0,"Mise en place des nouvelles coordonnées",NULL);

  /* Mise a jour des noms des noeuds */
  nom = (char *) malloc(n*ANCIEN_MED_TAILLE_PNOM+1);
  EXIT_IF(nom == NULL,NULL,NULL);
  nouvelle_chaine = (char *) malloc(n*MED_TAILLE_PNOM+1);
  EXIT_IF(nouvelle_chaine == NULL,NULL,NULL);
  ret = _MEDdatasetStringLire(nid,(char *)(MED_NOM_NOM),nom);
  if (ret == 0) {
    MAJ_21_22_chaine(nom,nouvelle_chaine,n);
    H5Gunlink(nid,(char *)(MED_NOM_NOM));
    dimd[0] = n*MED_TAILLE_PNOM+1;
    ret = _MEDdatasetStringEcrire(nid,(char *)(MED_NOM_NOM),dimd,nouvelle_chaine);  
    EXIT_IF(ret < 0,"Ecriture des nouveaux noms des noeuds",NULL);  
    gid = _MEDdatasetOuvrir(nid,(char *)(MED_NOM_NOM));
    ret = _MEDattrEntierEcrire(gid,(char *)(MED_NOM_NBR),&n);
    ret = _MEDdatasetFermer(gid);
  }
  free(nom);
  free(nouvelle_chaine);

  /* on ferme tout */
  ret = _MEDdatagroupFermer(nid);
  EXIT_IF(ret < 0,"Fermeture de l'accès aux noeuds",NULL);
}
