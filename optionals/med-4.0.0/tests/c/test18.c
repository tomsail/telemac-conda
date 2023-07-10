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
 * - Nom du fichier : test18.c
 *
 * - Description : routines de test de la conformite d'un fichier MED.
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
  med_idt   fid=0;
  med_int   majeur=0, mineur=0, release=0;
  med_bool  hdfok=MED_FALSE,medok=MED_FALSE;
  char      medversion[MED_SNAME_SIZE+1]="";

  /* Creation du fichier test18.med */
  if ((fid = MEDfileOpen("test18.med",MODE_ACCES)) < 0) {
    MESSAGE("Erreur a la creation du fichier test18.med");
    return -1;
  }
  fprintf(stdout,"- Creation du fichier test18.med \n");

  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  fprintf(stdout,"- Fermeture du fichier \n");

  /*
   * Quelle version de la bibliotheque MED est utilisee ?
   */
  MEDlibraryNumVersion(&majeur, &mineur, &release);
  fprintf(stdout,"- Version de MED utilisee pour lire le fichier : "IFORMAT"."IFORMAT"."IFORMAT" \n",majeur,mineur,release); 
  /*
   * Le fichier à lire est-il au bon format de fichier HDF ?
   */
  if (MEDfileCompatibility("test18.med",&hdfok,&medok)<0 ) {
    MESSAGE("Erreur à la vérification de la compatibilité du fichier avec les bibliothèques med et hdf.");
    return -1;
  }
  if ( hdfok )
    fprintf(stdout,"- Format HDF du fichier MED conforme au format HDF utilise par la bibliotheque \n");
  else
    fprintf(stdout,"- Format HDF du fichier MED non conforme au format HDF utilise par la bibliotheque \n");

  /*
   * Le fichier a lire a-t-il été créé avec une version de la bibliothèque MED conforme avec celle utilise ?
   * (Numéros majeur et mineur identiques).
   */
  if ( medok)
    fprintf(stdout,"- Version MED du fichier conforme a la bibliotheque MED utilisee \n");
  else
    fprintf(stdout,"- Version MED du fichier non conforme a la bibliotheque MED utilisee \n");

  if ((fid = MEDfileOpen("test18.med",MED_ACC_RDONLY)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test18.med");
    return -1;
  }
  fprintf(stdout,"- Ouverture du fichier en lecture \n");

  /*
   * Une fois le fichier ouvert on peut avoir acces au numero de version complet
   */
  if (MEDfileNumVersionRd(fid, &majeur, &mineur, &release) < 0) {
    MESSAGE("Erreur a la lecture du numero de version de la bibliothèque ");
    return -1;
  }
  fprintf(stdout,"- Ce fichier a ete cree avec MED "IFORMAT"."IFORMAT"."IFORMAT" \n",majeur,mineur,release); 

  if ( MEDfileStrVersionRd(fid, medversion) < 0 ) {
    MESSAGE("Erreur à la lecture de la version du fichier MED");
    return -1;
  }
  fprintf(stdout,"- Ce fichier a ete cree avec %s\n",medversion);

  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  fprintf(stdout,"- Fermeture du fichier \n");

  return 0;
}
