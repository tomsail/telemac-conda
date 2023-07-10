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
#define MESGERR 1
#include <med_utils.h>
#include <math.h>

#ifndef M_PI_2
# define M_PI_2         1.57079632679489661923  /* pi/2 */
#endif

#ifndef M_PI_4
# define M_PI_4         0.78539816339744830962  /* pi/4 */
#endif

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid;
  /* la dimension du maillage */
  med_int mdim = 2;
  /* nom du maillage de longueur maxi MED_NAME_SIZE */
  char maa[MED_NAME_SIZE+1] = "maa1";
  /* le nombre de noeuds */
  med_int nnoe = 4;
  /* table des coordonnees
      (dimension * nombre de noeuds) */
  med_float coo[8] = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0};
  med_float coo_2[8] = {0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0};
  /* tables des noms et des unites des coordonnees
      (dimension*MED_SNAME_SIZE+1) */
  /*                                  12345678901234561234567890123456*/
  char nomcoo[2*MED_SNAME_SIZE+1] = "x               y               ";
  char unicoo[2*MED_SNAME_SIZE+1] = "cm              cm              ";
  /* tables des noms, numeros, numeros de familles des noeuds
     autant d'elements que de noeuds - les noms ont pout longueur
     MED_SNAME_SIZE */
  /*                                  1234567890123456123456789012345612345678901234561234567890123456*/
  char nomnoe[4*MED_SNAME_SIZE+1] = "nom1            nom2            nom3            nom4            ";
  med_int numnoe[4] = {1,2,3,4};
  med_int nufano[4] = {0,1,2,2};
  const med_float phi1=0;
  const med_float phi2=M_PI_4;
  const med_float phi3=M_PI_2;
  med_float       trsf1[7]= {0,0,0, cos(phi1/2), 0, sin(phi1/2), 0 };
  med_float       trsf2[7]= {0,0,0, cos(phi2/2), 0, sin(phi2/2), 0 };
  med_float       trsf3[7]= {0,0,0, cos(phi3/2), 0, sin(phi3/2), 0 };

  /* ouverture du fichier */
  if ((fid = MEDfileOpen("Test_MEDmeshNodeCoordinateTrsfWr.med",MODE_ACCES)) < 0){
    MESSAGE("Erreur à l'ouverture du fichier : ");
    return -1;
  }

  /* Creation du maillage "maa" de type MED_NON_STRUCURE
     et de dimension 2 */
  if (MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,
	     "un maillage pour Test_MEDmeshNodeCoordinateTrsfWr","s", MED_SORT_DTIT,
		 MED_CARTESIAN, nomcoo, unicoo) < 0) {
    MESSAGE("Erreur a la creation du maillage : "); SSCRUTE(maa);
    ret = -1;
  }

  /* Ecriture des coordonnees des noeuds en mode MED_FULL_INTERLACE :
     (X1,Y1, X2,Y2, X3,Y3, ...) dans un repere cartesien */
  if (MEDmeshNodeCoordinateWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
			      MED_FULL_INTERLACE,nnoe,  coo) < 0) {
    MESSAGE("Erreur a l'ecriture des coordonnees des noeuds");
    ret = -1;
  }

  /* Ecriture des noms des noeuds (optionnel dans un maillage MED) */
  if (MEDmeshEntityNameWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,nnoe,nomnoe) < 0) {
    MESSAGE("Erreur a l'ecriture des noms des noeuds");
    ret = -1;
  }

  /* Ecriture des numeros des noeuds (optionnel dans un maillage MED) */
  if (MEDmeshEntityNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,nnoe,numnoe) < 0) {
    MESSAGE("Erreur a l'ecriture des numeros des noeuds");
    ret = -1;
  }

  /* Ecriture des numeros de famille des noeuds */
  if (MEDmeshEntityFamilyNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,nnoe,nufano) < 0) {
    MESSAGE("Erreur a l'ecriture des numeros de familles des noeuds");
    ret = -1;
  }

  /* Transformation des coordonnees des noeuds coo_1 :
   */
  fprintf(stdout,"Un message d'erreur est attendu: \n");
  if ( MEDmeshNodeCoordinateTrsfWr(fid,maa,MED_NO_DT,MED_NO_IT,0.4,trsf1) >= 0) {
    MESSAGE("Erreur a l'ecriture de la transformation géométrique n°1");
    MESSAGE("Aucune transformation géométrique à la séquence de calcul MED_NO_DT,MED_NO_IT n'est possible.");
  }
  fprintf(stdout,"Fin du message d'erreur attendu\n :");

  /* Transformation des coordonnees des noeuds coo_1 :
   */
  if ( MEDmeshNodeCoordinateTrsfWr(fid,maa,1,1,0.4,trsf1) < 0) {
    MESSAGE("Erreur a l'ecriture de la transformation géométrique n°1");
    ret = -1;
  }

  /* Ecriture des coordonnees des noeuds en mode MED_FULL_INTERLACE :
     (X1,Y1, X2,Y2, X3,Y3, ...) dans un repere cartesien */
  fprintf(stdout,"Un message d'erreur est attendu : \n");
  if (MEDmeshNodeCoordinateWr(fid,maa,1,1, 0.5,
			      MED_FULL_INTERLACE,nnoe, coo_2) >= 0) {
    MESSAGE("Erreur a l'ecriture des coordonnees des noeuds");
    MESSAGE("Aucune ecriture des coordonnees des noeuds n'est possible sur une séquence de calcul comportant "\
	    "une transformation géométrique.");
  }
  fprintf(stdout,"Fin du message d'erreur attendu :\n");

  /* Ecriture des coordonnees des noeuds en mode MED_FULL_INTERLACE :
     (X1,Y1, X2,Y2, X3,Y3, ...) dans un repere cartesien */
  if (MEDmeshNodeCoordinateWr(fid,maa,2,1, 0.5,
			      MED_FULL_INTERLACE,nnoe, coo_2) < 0) {
    MESSAGE("Erreur a l'ecriture des coordonnees des noeuds");
    ret = -1;
  }

  /* Transformation des coordonnees des noeuds coo_2 :
   */
  fprintf(stdout,"Un message d'erreur est attendu\n :");
  if ( MEDmeshNodeCoordinateTrsfWr(fid,maa,2,1,0.5,trsf2) >= 0) {
    MESSAGE("Erreur a l'ecriture de la transformation géométrique n°2");
    MESSAGE("Aucune transformation géométrique à une séquence de calcul comportant de nouvelles coordonnées n'est possible.");
  }
  fprintf(stdout,"Fin du message d'erreur attendu\n :");

  /* Transformation des coordonnees des noeuds coo_2 :
   */
  if ( MEDmeshNodeCoordinateTrsfWr(fid,maa,2,2,0.6,trsf2) < 0) {
    MESSAGE("Erreur a l'ecriture de la transformation géométrique n°2");
    ret = -1;
  }

  /* Transformation des coordonnees des noeuds coo_3 :
   */
  if ( MEDmeshNodeCoordinateTrsfWr(fid,maa,3,2,0.7,trsf3) < 0) {
    MESSAGE("Erreur a l'ecriture de la transformation géométrique n°3");
    ret = -1;
  }

  if ( MEDmeshComputationStepCr(fid,maa, 3,2, 3,3, 3.3) < 0) {
    MESSAGE("Erreur a la creation d'un pas de temps du maillage maa");
  }

 /* Modification des numeros des noeuds (optionnel dans un maillage MED) */
  numnoe[2]=200;
  if (MEDmeshEntityNumberWr(fid,maa,3,3,MED_NODE,MED_NONE,nnoe,numnoe) < 0) {
    MESSAGE("Erreur a l'ecriture des numeros des noeuds");
    ret = -1;
  }

  if ( MEDmeshComputationStepCr(fid,maa, 3,3, 3,4, 3.4) < 0) {
    MESSAGE("Erreur a la creation d'un pas de temps du maillage maa");
  }

  /*TODO : Ecrire un test de comparaison interne next prev et ordre de découverte itératif */

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier test4.med");
    return -1;
  }
  return ret;
}




