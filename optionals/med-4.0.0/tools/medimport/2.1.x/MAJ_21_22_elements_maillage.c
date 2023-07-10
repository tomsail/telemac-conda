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
#include "med_hdfi231.h"
#include "med21.h"
#include "MAJ_21_22.h"

void MAJ_21_22_elements_maillage(med_idt mid, med_int dimension)
{
  med_idt eid,gid,did,tid;
  med_err ret;
  int i,j;
  med_geometrie_element typmai[MED_NBR_GEOMETRIE_MAILLE] = {MED_POINT1,MED_SEG2, 
							    MED_SEG3,MED_TRIA3,
							    MED_TRIA6,MED_QUAD4,
							    MED_QUAD8,MED_TETRA4,
							    MED_TETRA10,MED_HEXA8,
							    MED_HEXA20,MED_PENTA6,
							    MED_PENTA15,MED_PYRA5,
							    MED_PYRA13};
  int taille, edim;
  char *nom, *nouvelle_chaine;
  char nomgroup[MED_TAILLE_NOM_ENTITE+1];
  med_int n;
  med_size dimd[1];
  med_int *old_conn,*conn;
  
  /* On ne regarde que les mailles et la connectivité nodale */
  eid = _MEDdatagroupOuvrir(mid,(char *)(MED_NOM_MAI));
  EXIT_IF(eid < 0,"Ouverture du groupe HDF MED_NOM_MAI",NULL);

  /* On normalise selon tous les types geometriques */
  for (i=0;i<MED_NBR_GEOMETRIE_MAILLE;i++) {
    
    /* On recupere le nom du groupe HDF */
    _MEDnomGeometrie(nomgroup,typmai[i]);

    /* On accède au type s'il existe dans le fichier */
    gid = _MEDdatagroupOuvrir(eid,nomgroup);
    if (gid < 0)
      continue;

    /* Nombre d'element ? */
    did = _MEDdatasetOuvrir(gid,(char *)(MED_NOM_NOD));
    EXIT_IF(did < 0,"Ouverture du dataset HDF MED_NOM_NOD",NULL);
    ret = _MEDattrEntierLire(did,(char *)(MED_NOM_NBR),&n);
    EXIT_IF(ret < 0,"Lecture du nombre d'elements",NULL);
    ret = _MEDdatasetFermer(did);
    EXIT_IF(ret < 0,"Fermeture du dataset HDF MED_NOM_NOD",NULL);

    /* on normalise la connectivité si edim < dimension */
    edim = typmai[i] / 100;
    if (edim < dimension) {
      taille = typmai[i]%100 + 1;
      old_conn = (med_int *) malloc(sizeof(med_int)*taille*n);
      EXIT_IF(old_conn == NULL,NULL,NULL);
#if defined(HAVE_F77INT64)
      ret = _MED21datasetNumLire(gid,(char *)(MED_NOM_NOD),MED_INT64,
				  MED_NO_INTERLACE,(med_size)taille,MED_ALL,
				  0,NULL,MED_NOPG,
				  (unsigned char*) old_conn,H5T_NATIVE_INT);
#else
      ret = _MED21datasetNumLire(gid,(char *)(MED_NOM_NOD),MED_INT32,
				  MED_NO_INTERLACE,(med_size) taille,MED_ALL,
				  0,NULL,MED_NOPG,
				  (unsigned char*) old_conn,H5T_NATIVE_INT);
#endif 
      /* On recopie dans le bon tableau */
      taille --;
      conn = (med_int *) malloc(sizeof(med_int)*taille*n);
      EXIT_IF(conn == NULL,NULL,NULL);
      for (j=0;j<n*taille;j++)
	*(conn+j) = *(old_conn+j);
      dimd[0] = n*taille;
#if defined(HAVE_F77INT64)
      ret = _MED231datasetNumEcrire(gid,(char *) "TMP",MED_INT64,MED_NO_INTERLACE,
				    taille,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,MED_NOPG,dimd,
				 (unsigned char*) conn);
#else
      ret = _MED231datasetNumEcrire(gid,(char *) "TMP",MED_INT32,MED_NO_INTERLACE,
				    taille,MED_ALL,MED_NOPF,MED_NO_PFLMOD,0,MED_NOPG,dimd,
				 (unsigned char*) conn);
#endif
      EXIT_IF(ret < 0,"Ecriture de la nouvelle connectivité des mailles",NULL);
      
      /* Ecriture du nombre de mailles dans le dataset HDF TMP */
      tid = _MEDdatasetOuvrir(gid,"TMP");
      EXIT_IF(tid < 0,"Ouverture du dataset HDF TMP",NULL);
      ret = _MEDattrEntierEcrire(tid,(char *)(MED_NOM_NBR),&n);
      EXIT_IF(ret < 0,"Ecriture du nombre de noeuds dans le dataset HDF TMP",NULL);
      ret = _MEDdatasetFermer(tid);
      EXIT_IF(ret < 0,"Fermeture du dataset HDF TMP",NULL);

      /* Fermeture de l'accès aux connectivites */
      ret = H5Gunlink(gid,(char *)(MED_NOM_NOD));
      EXIT_IF(ret < 0,"Suppression des anciennes connectivités",NULL);
      ret = H5Gmove(gid,"TMP",(char *)(MED_NOM_NOD));
      EXIT_IF(ret < 0,"Mise en place des nouvelles connectivités",NULL);

      /* on libere la memoire */
      free(old_conn);
      free(conn);
    }
     
    /* Mise a niveau des noms */
    nom = (char *) malloc(n*ANCIEN_MED_TAILLE_PNOM+1);
    EXIT_IF(nom == NULL,NULL,NULL);
    nouvelle_chaine = (char *) malloc(n*MED_TAILLE_PNOM+1);
    EXIT_IF(nouvelle_chaine == NULL,NULL,NULL);
    ret = _MEDdatasetStringLire(gid,(char *)(MED_NOM_NOM),nom);
    if (ret == 0) {
      _MED23v30stringConvert(nouvelle_chaine, MED_TAILLE_PNOM,
			     nom, ANCIEN_MED_TAILLE_PNOM, n );
/*       MAJ_21_22_chaine(nom,nouvelle_chaine,n); */
      H5Gunlink(gid,(char *)(MED_NOM_NOM));
      dimd[0] = n*MED_TAILLE_PNOM+1;
      ret = _MEDdatasetStringEcrire(gid,(char *)(MED_NOM_NOM),dimd,nouvelle_chaine);  
      EXIT_IF(ret < 0,"Ecriture des nouveaux noms des éléments",NULL);  
      did = _MEDdatasetOuvrir(gid,(char *)(MED_NOM_NOM));
      ret = _MEDattrEntierEcrire(did,(char *)(MED_NOM_NBR),&n);
      ret = _MEDdatasetFermer(did);
    }
    free(nom);
    free(nouvelle_chaine);

    /* on ferme avant de passer au type geometrique suivant */
    ret = _MEDdatagroupFermer(gid);
    EXIT_IF(ret < 0,"Fermeture de l'accès aux mailles",NULL);
  }

  /* On ferme tout */
  ret = _MEDdatagroupFermer(eid);
  EXIT_IF(ret < 0,"Fermeture de l'accès aux mailles",NULL);
}
