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
 * - Nom du fichier : test22.c
 *
 * - Description : lecture de valeurs scalaires numeriques crees dans test21.
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

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
  char nom_scalaire[MED_TAILLE_NOM+1];
  char description[MED_TAILLE_DESC+1];
  med_int vali;
  med_float valr;
  med_int i,n,npdt,j;
  med_type_champ type;
  med_int numdt,numo;
  med_float dt;
  char dt_unit[MED_TAILLE_PNOM+1];

  /* Ouverture du fichier test21.med en lecture seule */
  if ((fid = MEDouvrir("test21.med",MED_LECTURE)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test21.med");
    return -1;
  }

  /* Lecture du nombre de variable scalaire */
  n = MEDnScalaire(fid);
  if (n < 0) {
    MESSAGE("Erreur a la lecture du nombre de variable scalaire");
    return -1;
  }
  printf("Nombre de variables scalaires dans tes21.med = %d\n",n);

  /* Lecture des infos sur les variables (type,description) */
  for (i=1;i<=n;i++) {
    
    if (MEDscalaireInfo(fid,i,nom_scalaire,&type,description) < 0) {
      MESSAGE("Erreur a la lecture des infos sur la variable d'indice : ");
      ISCRUTE(i);
      return -1;
    }
    printf("- Scalaire n°"IFORMAT" de nom %s \n",i,nom_scalaire);
    if (type == MED_FLOAT64)
      printf("  Type flottant. \n");
    else
      printf("  Type entier. \n");
    printf("  Description associee : [%s] \n",description);

    /* Pour chaque scalaire on regarde les valeurs associees 
       eventuellement a des pas de temps et des numeros d'ordre */
    npdt = MEDnScalairePasdetemps(fid,nom_scalaire);
    if (npdt < 0) {
      MESSAGE("Erreur a la lecture du nombre de pas de temps");
      return -1;
    }
    printf("   Nombre de valeurs stockees : "IFORMAT" \n",npdt);

    for (j=1;j<=npdt;j++) {

      if (MEDscalairePasdetempsInfo(fid,nom_scalaire,j,&numdt,dt_unit,&dt,&numo) < 0) {
	MESSAGE("Erreur a la lecture des informations sur le pas de temps d'indice :");
	ISCRUTE(j);
	return -1;
      }

      printf("   Valeur n°"IFORMAT" : \n",j);
      if (numdt == MED_NOPDT) 
	printf("   - Aucun de pas de temps \n");
      else
	printf("   - Pas de de temps de numero "IFORMAT" de valeur %f [%s] \n",numdt,dt,dt_unit);
      if (numo == MED_NONOR)
	printf("   - Aucun numero d'ordre \n");
      else
	printf("   - Numero d'ordre : "IFORMAT" \n",numo);

      /* Lecture de la valeur flottante associee au pas de temps */
      if (type == MED_FLOAT64) {
	if (MEDscalaireFlottantLire(fid,nom_scalaire,&valr,numdt,numo) < 0) {
	  MESSAGE("Erreur a la lecture de la valeur flottante : ");
	  SSCRUTE(nom_scalaire); ISCRUTE(numdt);ISCRUTE(numo);
	  return -1;
	}
	printf("    - Valeur : %f \n",valr);	      
      } else {
	/* Lecture de la valeur scalaire associee au pas de temps */
	if (MEDscalaireEntierLire(fid,nom_scalaire,&vali,numdt,numo) < 0) {
	  MESSAGE("Erreur a la lecture de la valeur entiere : ");
	  SSCRUTE(nom_scalaire); ISCRUTE(numdt);ISCRUTE(numo);
	  return -1;
	}
	printf("    - Valeur : "IFORMAT" \n",vali);
      }			  
    }

  }

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier ");
    return -1;
  }

  return 0;
}
