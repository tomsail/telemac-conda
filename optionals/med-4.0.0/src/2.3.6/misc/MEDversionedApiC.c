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


#include <stdio.h>
#include <med_config.h>
#include <med_misc.h>
#include <med_utils.h>

extern MedFuncType getVersionedApi(const char * const keycharpart,
				   const char * const keynumpart);


/*  La gestion suivante suppose de ne pas utiliser des versions > 9.9.9 */
/*  ex 9.10.9 ne peut pas être géré. */
MedFuncType _MEDversionedApi( char * key, med_int majeur,
			      med_int mineur, med_int release) {
  char    version[4]="";
  int     n=0;
  int     itrelease, minrelease=0;
  int     versionMMR,versionMM;
  MedFuncType func=(MedFuncType) NULL;
  int imajeur=(int)majeur, imineur=(int)mineur, irelease=(int)release;

  versionMM  = 100*imajeur+10*imineur;
  versionMMR = versionMM+irelease;

  if ( versionMMR <  220 ) {
    MESSAGE("Cette bibliothèque MED n'est pas capable de lire un fichier MED de version < 2.2.0");
    MESSAGE("La version demandée est :");
    ISCRUTE_int(versionMMR);
    goto QUIT;
  }

  if ( versionMM > 100*MED_NUM_MAJEUR+10*MED_NUM_MINEUR ) {
    MESSAGE("Cette bibliothèque MED n'est pas capable de lire un fichier MED dont le mineur de la version"
	    "excède celui de la bibliothèque.");
    MESSAGE("La version demandée est :");ISCRUTE_int(versionMMR);
    goto QUIT;
  }

  if ( versionMMR <= 231 ) {
    imajeur=2;imineur=3;irelease=1;
  }

  /*  Recherche du numéro de release pour les versions de la bibliothèque  */
  /*  qui ne respectent pas la règle de versionement MED : 2.3.1, 2.3.2, 2.3.3, (2.3.4) */
  /*  Cette recherche de symboles existe uniquement à partir de la 2.3.4 */
  /*  Dans les versions précédentes la bibliothèque doit trouver exactement le symbole */
  /*  qui contient son numéro de version. */
  if (versionMMR < 240 ) {

    minrelease=1;

    /* Recherche décroissante à partir du numéro de release du fichier */
    itrelease = irelease;
    while ( ( func == (MedFuncType)NULL) && (minrelease <= itrelease ) ) {

#ifdef PPRO_NT_CALL
      n = _snprintf(version,4,"%d%d%d",imajeur,imineur,itrelease);
#else
      n = snprintf(version,4,"%d%d%d",imajeur,imineur,itrelease);
#endif
      if ( (n < 0) || (n > 3) ) {
	MESSAGE("Impossible d'obtenir un numéro de version valide : ");
	version[3]='\0';
	SSCRUTE(version);
	break;
      }

      func=getVersionedApi(key,version);
      --itrelease;
    }
    goto QUIT;
  }

  /* A partir de la 2.4.0, on oblige le developpeur à versionner
     uniquement des routines en x.y.0 ;  x.y+1.* et x.y.* étant incompatible
     et  x.y.0 et x.y.a étant compatible, x.y.a n'apparait pas dans la table
     de versionement */
#ifdef PPRO_NT_CALL
  n = _snprintf(version,4, IFORMAT IFORMAT IFORMAT,majeur,mineur,0);
#else
  n = snprintf(version,4, IFORMAT IFORMAT IFORMAT,majeur,mineur,0);
#endif
  if ( (n < 0) || (n > 3) ) {
    MESSAGE("Impossible d'obtenir un numéro de version valide : ");
    version[3]='\0';
    SSCRUTE(version);
    goto QUIT;
  }

  func=getVersionedApi(key,version);

 QUIT:
  if (func == NULL) {
    MESSAGE("Impossible d'obtenir une implémentation de : ");
    SSCRUTE(key);
    MESSAGE("en version :");
    ISCRUTE_int(versionMMR);
    MESSAGE("Vérifiez votre fichier .med");
  }

  return func;
}


