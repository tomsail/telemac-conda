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

#include "med21.h"
#include "med_hdfi21.h"
#include "MAJ_21_22.h"

void MAJ_21_22_localisation_Gauss(med_idt fid,char *nom_modele,med_int ngauss)
{
  med_err ret;
  med_geometrie_element type;
  med_float *refcoo, *gaucoo, *poids;
  char nom_type[4];
  med_int edim, nsom,n,nlu;
  int i;
  char nom[MED_TAILLE_NOM+1];

  /* On regarde si la localisation bidon existe deja 
     Si oui on s'en va */
  n = MEDnGauss(fid);
  if (n > 0)
    for (i=1;i<=n;i++) {
      ret = MEDgaussInfo(fid,i,nom,&type,&nlu);
      if (! strcmp(nom,nom_modele)) return;
    }
  
  /* On deduit le type en fonction de nom_modele */
  strncpy(nom_type,nom_modele,3);
  nom_type[3] = '\0';
  if (! strcmp(nom_type,(char *)(MED_NOM_SE2)))
    type = MED_SEG2;
  if (! strcmp(nom_type,(char *)(MED_NOM_SE3)))
    type = MED_SEG3; 
  if (! strcmp(nom_type,(char *)(MED_NOM_TR3)))
    type = MED_TRIA3; 
  if (! strcmp(nom_type,(char *)(MED_NOM_TR6)))
    type = MED_TRIA6;
  if (! strcmp(nom_type,(char *)(MED_NOM_QU4)))
    type = MED_QUAD4; 
  if (! strcmp(nom_type,(char *)(MED_NOM_QU8)))
    type = MED_QUAD8;
  if (! strcmp(nom_type,(char *)(MED_NOM_TE4)))
    type = MED_TETRA4; 
  if (! strcmp(nom_type,(char *)(MED_NOM_T10)))
    type = MED_TETRA10; 
    if (! strcmp(nom_type,(char *)(MED_NOM_HE8)))
    type = MED_HEXA8; 
  if (! strcmp(nom_type,(char *)(MED_NOM_H20)))
    type = MED_HEXA20;
  if (! strcmp(nom_type,(char *)(MED_NOM_PE6)))
    type = MED_PENTA6; 
  if (! strcmp(nom_type,(char *)(MED_NOM_P15)))
    type = MED_PENTA15;
  if (! strcmp(nom_type,(char *)(MED_NOM_PY5)))
    type = MED_PYRA5; 
  if (! strcmp(nom_type,(char *)(MED_NOM_P13)))
    type = MED_PYRA13;

  /* Dimension de la maille */
  edim = type / 100;
  nsom = type % 100;

  /* On definit des coordonnées et des poids bidon */
  refcoo = (med_float*) malloc(sizeof(med_float)*edim*nsom);
  EXIT_IF(refcoo == NULL,NULL,NULL);
  for (i=0;i<edim*nsom;i++) *(refcoo+i) = 0.;
  gaucoo = (med_float*) malloc(sizeof(med_float)*edim*ngauss);
  EXIT_IF(gaucoo == NULL,NULL,NULL);
  for (i=0;i<edim*ngauss;i++) *(gaucoo+i) = 0.;
  poids  = (med_float*) malloc(sizeof(med_float)*ngauss);
  EXIT_IF(poids == NULL,NULL,NULL);
  for (i=0;i<ngauss;i++) *(poids+i) = 0.;

  /* On ecrit la localisation bidon */
  ret = MEDgaussEcr(fid,type,refcoo,MED_FULL_INTERLACE,ngauss,gaucoo,poids,nom_modele);
  EXIT_IF(ret < 0,"Ecriture de la localisation des points de Gauss",nom_modele);

  /* On libere la memoire */
  free(refcoo);
  free(gaucoo);
  free(poids);
}
