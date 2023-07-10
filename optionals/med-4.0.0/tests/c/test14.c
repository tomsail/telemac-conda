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
 * - Nom du fichier : test14.c
 *
 * - Description : ecriture des noeuds d'un maillage MED
 *                 a l'aide des routines de niveau 2
 *                 equivalent a test4.c
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

int main (int argc, char **argv)


{
  med_idt fid;
  /* la dimension du maillage */
  med_int mdim = 2;
  /* nom du maillage de longueur maxi MED_NAME_SIZE */
  char maa[MED_NAME_SIZE+1] = "maa1";
  /* le nombre de noeuds */
  med_int nnoe = 4;
  /* table des coordonnees  
     profil : (dimension * nombre de noeuds) */
  med_float coo[8] = {0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0};
  /* tables des noms et des unites des coordonnees 
     profil : (dimension*MED_SNAME_SIZE+1) */
  /*                                  12345678901234561234567890123456 */
  char nomcoo[2*MED_SNAME_SIZE+1] = "x               y               ";
  char unicoo[2*MED_SNAME_SIZE+1] = "cm              cm              ";
  /* tables des noms, numeros, numeros de familles des noeuds
     autant d'elements que de noeuds - les noms ont pout longueur
     MED_SNAME_SIZE */
  /*                                  1234567890123456123456789012345612345678901234561234567890123456 */
  char nomnoe[4*MED_SNAME_SIZE+1] = "nom1            nom2            nom3            nom4             ";
  med_int numnoe[4] = {1,2,3,4};
  med_int nufano[4] = {0,1,2,2};

  /* Creation du fichier test14.med */
  if ((fid = MEDfileOpen("test14.med",MODE_ACCES)) < 0) {
    MESSAGE("Erreur a la creation du fichier test14.med");
    return -1;
  }


  /* Creation du maillage */
  if (MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,
		 "un maillage pour test14","s", MED_SORT_DTIT,
		 MED_CARTESIAN, nomcoo, unicoo) < 0) {
    MESSAGE("Erreur a la creation du maillage : "); SSCRUTE(maa);
    return -1;
  }

  /* Ecriture des noeuds d'un maillage MED :
     - Des cooordonnees en mode MED_FULL_INTERLACE : (X1,Y1,X2,Y2,X3,Y3,...)
     dans un repere cartesien
     - Des noms (optionnel dans un fichier MED)
     - Des numeros (optionnel dans un fichier MED)
     - Des numeros de familles des noeuds */
  if (MEDmeshNodeWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MED_FULL_INTERLACE,
		    nnoe,coo,MED_TRUE,nomnoe,MED_TRUE,numnoe,MED_TRUE,
		    nufano) < 0) {
    MESSAGE("Erreur a l'ecriture des noeuds du maillage");
    return -1;
  }

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }

  return 0;
}




