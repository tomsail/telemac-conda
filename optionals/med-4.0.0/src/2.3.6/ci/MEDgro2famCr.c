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
MEDgro2famCr(med_idt fid,char *maillage,char *groupes,med_int *index,med_int ngroup,med_int *entites,
	     med_int nent,med_entite_maillage type_ent,med_geometrie_element *type_geo,med_int *indexgeo,
	     med_int ngeo)
{
  med_err ret = 0;
  med_int nfam = 0;
  int i,j,k,l,m;
  int col,bit,som;
  int est_egale, trouvee;
  unsigned char ajout,tmp;
  unsigned char *groupes_par_entite;
  med_int *numfam;
  med_int numgro;
  char nom[MED_TAILLE_NOM+1];
  char nomgro[MED_TAILLE_LNOM+1];
  char *gro;
  med_int num,ngro,natt;
  med_int *nummai;
  med_entite_maillage _type_ent=type_ent;

  if ( type_ent == MED_NOEUD_MAILLE ) _type_ent=MED_NOEUD ;

  /* Premiere phase : on etablit la liste des groupes
     pour chaque entite */
  groupes_par_entite = (unsigned char *) malloc(sizeof(unsigned char)*nent*(ngroup/8+1));
  for (i=0;i<nent;i++)
    for (j=0;j<ngroup/8+1;j++)
      *(groupes_par_entite+i*(ngroup/8+1)+j) = 0;
  for (i=0;i<ngroup;i++) {
    col = i/8;
    bit = 7-i%8;
    for (j=*(index+i)-1;j<*(index+i+1)-1;j++) {
      ajout = 1;
      for (k=0;k<bit;k++) 
	ajout = ajout*2;
      *(groupes_par_entite + (*(entites+j)-1)*(ngroup/8+1) + col) += ajout;
    }
  }

  /* 
   * Deuxieme phase : on construit la liste des numeros de familles 
   */
  numfam = (med_int *) malloc(sizeof(med_int)*nent);
  for (i=0;i<nent;i++) {

    som = 0;
    for (j=0;j<ngroup/8+1;j++) 
      som += (int) *(groupes_par_entite+i*(ngroup/8+1)+j);

    if (som == 0) 
      /* si som == 0 => famille 0 */
      *(numfam+i) = 0;
    else {
      /* on regarde si l'entite appartient a une famille existante :
	 - Si c'est le cas, on enrgistre le numero correspondant
	 - Si ce n'est pas le cas, on cree un nouveau numero 
      */
      trouvee = 0;
      for (j=0;j<i;j++)
	if (*(numfam+j) != 0) {
	  est_egale = 1;
	  for (k=0;k<ngroup/8+1;k++)
	    if (*(groupes_par_entite+i*(ngroup/8+1)+k) != *(groupes_par_entite+j*(ngroup/8+1)+k))
	      est_egale = 0;
	  if (est_egale) {
	    *(numfam+i) = *(numfam+j);
	    trouvee = 1;
	    break;
	  }
	}

      if (! trouvee) {
	nfam ++;
	if (_type_ent == MED_NOEUD)
	  *(numfam+i) = nfam;
	else
	  *(numfam+i) = -nfam;
      }
    }
  }

  /* 
   * Troisieme phase : on construit chaque famille 
   *  et on la stocke dans le fichier MED 
   */
  natt = 0;
  gro = (char *) malloc(sizeof(char)*(MED_TAILLE_LNOM*ngroup+1));
  for (i=1;i<nfam+1;i++) {
    trouvee = 0;
    for (j=0;j<nent;j++)
      if ((*(numfam+j) == -i) || (*(numfam+j) == i)) {
	trouvee = 1;
	/* on definit, le nom, le numero, la liste des noms de groupes 
	   de la famille */
	num = *(numfam+j);
	if (_type_ent == MED_NOEUD)
	  sprintf(nom,"FAMILLE_NOEUD_%d",i);
	else 
	  sprintf(nom,"FAMILLE_ELEMENT_%d",i);
	ngro = 0;
	for (k=0;k<ngroup/8+1;k++)
	  if (*(groupes_par_entite+j*(ngroup/8+1)+k) != 0) {
	    tmp = *(groupes_par_entite+j*(ngroup/8+1)+k);
	    for (l=0;l<ngroup;l++) {
	      col = l/8;
	      bit = 7-l%8;
	      ajout = 1;
	      for (m=0;m<bit;m++)
		ajout = ajout*2;
	      if (ajout & tmp) {
		numgro = col + (7 - bit);
		ngro++;
		strncpy(nomgro,groupes+numgro*MED_TAILLE_LNOM,MED_TAILLE_LNOM);
		nomgro[MED_TAILLE_LNOM] = '\0';
		if (ngro == 1) 
		  strcpy(gro,nomgro);
		else
		  strcat(gro+(ngro-1)*MED_TAILLE_LNOM,nomgro);
	      }
	    }
	  }

	ret = MEDfamCr(fid,maillage,nom,num,NULL,NULL,NULL,natt,gro,ngro);

	break;
      }
  }

  /* 
   * Quatrieme phase : on ecrit les numeros de familles 
   */
  if (ret == 0) {
    if (_type_ent == MED_NOEUD) 
      ret = MEDfamEcr(fid,maillage,numfam,nent,MED_NOEUD,(med_geometrie_element) 0);
    if (_type_ent == MED_MAILLE || _type_ent == MED_FACE || _type_ent == MED_ARETE) {
      som = 0;
      for (i=0;i<ngeo;i++) {
	if (ret == 0) {
	  /* m = le nombre de mailles d'un type geometrique donne */
	  m = *(indexgeo+i+1)-*(indexgeo+i);
	  nummai = (med_int *) malloc(sizeof(med_int)*m);
	  for(j=0;j<m;j++)
	    *(nummai+j) = *(numfam+som+j);
	  ret = MEDfamEcr(fid,maillage,nummai,m,_type_ent,*(type_geo+i));
	  free(nummai);
	  som += m;
	}
      }
    }
  }


  /* 
   * Derniere phase : on nettoie la memoire 
   */
  free(groupes_par_entite);
  free(numfam);
  free(gro);

  return ret; 
}
