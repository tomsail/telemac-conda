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

/******************************************************************************
 * - Nom du fichier : test8.c
 *
 * - Description : exemple d'ecriture de familles dans un maillage MED 
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include "med_utils.h"
#include <string.h>

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_LECTURE_ECRITURE
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_LECTURE_AJOUT
#else
#define MODE_ACCES MED_CREATION
#endif

int main (int argc, char **argv)


{
  med_idt fid;
  char maa[MED_TAILLE_NOM+1] ="maa1";
  med_int mdim = 2;
  char nomfam[MED_TAILLE_NOM+1]="";
  med_int numfam;
  char attdes[MED_TAILLE_DESC+1]="";
  med_int natt;
  med_int attide;
  med_int attval;
  med_int ngro;
  char gro[MED_TAILLE_LNOM+1]="";
  int i;
  int nfame = 3; 
  int nfamn = 2;

  /* Creation du fichier "test8.med" */
  if ((fid = MEDouvrir("test8.med",MODE_ACCES)) < 0) {
    MESSAGE("Erreur a la creation du fichier test8.med");
    return -1;
  }
  
  /* Creation d'un maillage */
  if (MEDmaaCr(fid,maa,2,MED_NON_STRUCTURE,
	       "un maillage pour test8") < 0) {
    MESSAGE("Erreur a la creation du maillage");
    return -1;
  }
  
  /* Ecriture des familles                                                */
  /* Conventions appliquees dans MED :
     - Toujours creer une famille de numero 0 ne comportant aucun attribut
     ni groupe (famille de reference pour les noeuds ou les elements
     qui ne sont rattaches a aucun groupe ni attribut)
     - Les numeros de familles de noeuds sont > 0
     - Les numeros de familles des elements sont < 0
     - Rien d'imposer sur les noms de familles. 
     */ 
  
  /* Creation de la  famille 0 */
  strcpy(nomfam,"FAMILLE_0");
  numfam = 0;
  if (MEDfamCr(fid,maa,nomfam,numfam,&attide,&attval,attdes,0,
		 gro,0) < 0) {
    MESSAGE("Erreur a la creation de la famille 0");
    return -1;
  }
  
  /* Creation pour correspondre aux cas test precedent de :
     - 3 familles d'elements (-1,-2,-3) 
     - 2 familles de noeuds (1,2) */
  nfame = 3;
  for (i=0;i<nfame;i++) {
    numfam = -(i+1);
    sprintf(nomfam,"%s"IFORMAT,"FAMILLE_ELEMENT_",-numfam);
    attide = 1;
    attval = numfam*100;
    natt = 1;
    strcpy(attdes,"description attribut");
    strcpy(gro,"groupe1");
    ngro = 1;
    printf("%s - "IFORMAT" - "IFORMAT" - "IFORMAT" - "IFORMAT" \n",nomfam,numfam,attide,attval,
	   ngro);
    if (MEDfamCr(fid,maa,nomfam,numfam,&attide,&attval,attdes,
		 natt,gro,ngro) < 0) {
      MESSAGE("Erreur a la creation de la famille :");
      SSCRUTE(nomfam); ISCRUTE(numfam);
      return -1;
    }
  }
  
  nfamn = 2;
  for (i=0;i<nfamn;i++) {
    numfam = i+1;
    sprintf(nomfam,"%s"IFORMAT,"FAMILLE_NOEUD_",numfam);
    attide = 1;
    attval = numfam*100;
    natt = 1;
    strcpy(attdes,"description attribut");
    strcpy(gro,"groupe1");
    ngro = 1;
    if (MEDfamCr(fid,maa,nomfam,numfam,&attide,&attval,attdes,
		 natt,gro,ngro) < 0) {
      MESSAGE("Erreur a la creation de la famille :");
      SSCRUTE(nomfam); ISCRUTE(numfam);
      return -1;
    }
  }
  
  
  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier :");
    return -1;
  }          
  
  return 0;
}




