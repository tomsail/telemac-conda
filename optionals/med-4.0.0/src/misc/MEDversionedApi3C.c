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

extern MedFuncType getVersionedApi3(const char * const keycharpart,
				    const char * const keynumpart);

/*   - Seules les fonctions MEDnomdefonction versionnées (définies dans leur fichier MEDnomdefonction.c) font appel à _MEDversionApi3()
 *  pour obtenir l'implémentation qui correspond à la version du fichier .med traité par la bibliothèque.
 *  Une fonction MEDnomdefonction versionnée possède au moins un fichier source _MEDnomdefonctionX0.c qui contient l'implémentation
 *  par défaut pour la version majeure X et surement un autre fichier _MEDnomdefonctionXY.c qui contient l'implémentation 
 *  comportant le changement de modèle en version X.Y.
 *   - Les fonctions non versionnées sont uniquement définies dans leur fichier MEDnomdefonction.c et peuvent être appelées quelque
 *  soit la version du fichier car l'implémentation n'a jamais été modifiée.
 *
 *  A la création d'une bibliothèque en version majeur X+1 :
 *  - Soit on reprend l'ensemble des dernières implémentations que l'on place dans leur fichier respectif MEDnomdefonction.c 
 *  et l'on efface toute trace de versionement ds les version X.Y et inférieures (on crée pour l'occasion un objet MED_VERSION_APIX+1)
 *  - Soit on laisse tel quel et l'on augmente l'objet MED_VERSION_APIX des nouvelles implémentations X+1.
 *  Le premier cas permet de continuer à lire les fichiers en version X et inférieurs.
 *  Le second cas permet de simplifier les sources et la recherche de l'implémentation correcte.
 *  En ce qui concerne MED4 on veut pouvoir relire les fichier .med3 en version HDF1.8.z (et les fichiers debian .med3 en version HDF1.10!).
 */

/*  La gestion suivante suppose d'utiliser un seul digit par numéro intervenant dans la verion X.Y.Z  */
/*  ex: 9.10.9 ne peut pas être géré pour l'instant. */
MedFuncType _MEDversionedApi3( const char * const key,
			       const med_int majeur,const  med_int mineur,const  med_int release) {

  MedFuncType func=(MedFuncType) NULL;
  char        _version[4]="", _version236[4]="236";
  int         _n=0;
  int         _fversionMM  = 100*majeur+10*mineur;
  int         _fversionMMR = _fversionMM+release;
  int         _litminor    = mineur;
  int         _litmajeur   = majeur;
  const int   _lminminor3   = 0;
  const int   _lminminor4   = 0;

  if ( _fversionMMR <  220 ) {
    MESSAGE("Cette bibliothèque MED n'est pas capable de lire un fichier MED de version < 2.2.0");
    MESSAGE("La version demandée est :");
    ISCRUTE_int(_fversionMMR);
    goto QUIT;
  }

  if ( _fversionMM > 100*MED_NUM_MAJEUR+10*MED_NUM_MINEUR ) {
    MESSAGE("Cette bibliothèque MED n'est pas capable de lire un fichier MED dont le majeur.mineur de la version "
	    "excède celui de la bibliothèque.");
    MESSAGE("La version demandée est :");ISCRUTE_int(_fversionMMR);
    goto QUIT;
  }


  /* Dans la bibliothèque 3.0 toutes les APIs en lecture/info
   *  sont versionées en 2.3.6. Celles-ci sont capables de compatibilité
   * ascendante des fichiers 2.2.0 à 2.3.6 */
  if ( _fversionMMR <= 236 ) { func=getVersionedApi3(key,_version236); goto QUIT;}


  /*  - Recherche du dernier numéro mineur disponible pour la routine versionné
      dans la bibliothèque MED actuelle  versionMMR 100*MED_NUM_MAJEUR+10*MED_NUM_MINEUR
      -A partir de la 2.4.0, on oblige le developpeur à versionner
      uniquement des routines en x.y.0 ;  x.y+1.* et x.y.* étant  incompatibles (en terme de modèle)
      et  x.y.0 et x.y.a étant compatible, x.y.a n'apparait pas dans la table
      de versionement
  */

  /*
   * Lorsqu'une routine non versionnée doit être versionnée, la nouvelle
   * implémentation est numérotée en "MED_NUM_MAJEUR"MED_NUM_MINEUR" et l'ancienne
   * en "MED_NUM_MAJEUR".0 pour que l'implémentation existe pour tous les fichiers
   * précédement crées.
   *
   */

  /*
   * La recherche d'une routine compatible se fait de façon décroissante
   * à partir du mineur du fichier jusqu'à 0 ou une poentielle rupture de compatibilité
   * (le mineur a changé)
   *
   */


  /*Rem (_fversionMMR > 290) car cette version est une version de developpement et aucun fichier
    issu de bibliotèques 2.y.z  ne devrait être en circulation.*/
  if ( (_fversionMMR <= 290) || (_fversionMM > (100*MED_NUM_MAJEUR+10*MED_NUM_MINEUR) ) ) goto QUIT;
  // Gestion pour les fichiers 3.y.z (>290) les autres versions ont été traitées précédement
  // Gestion pour les fichiers dont le MAJEUR.MINEUR et <= à celui de la bibliothèque

  /* if ( (_fversionMMR > 290) && */
  /*      (_fversionMM <= (100*MED_NUM_MAJEUR+10*MED_NUM_MINEUR) ) */
  /*      ) { */


  if (_litmajeur == 4) {
 
    if (_litminor > MED_4_LATEST_MINOR) {
      MESSAGE("Cette bibliothèque MED n'est pas capable de lire un fichier med-4 dont le mineur de la version "
	      "excède celui dont la bibliothèque à connaissance.");
      MESSAGE("La version demandée est       : ");ISCRUTE_int(_fversionMMR);
      MESSAGE("La dernière version med-4 connue de cette bibliothèque : ");ISCRUTE_int(400+10*MED_4_LATEST_MINOR+9);
      }
    // Fait à l'initialisation.
    // _litmajeur=4;_litminor=mineur;
  
    /* Recherche décroissante à partir du numéro de release de la bibliothèque */
    while ( ( func == (MedFuncType)NULL) && (_litminor >= _lminminor4 ) ) {

#ifdef PPRO_NT_CALL
      _n = _snprintf(_version,4,"%d%d%d",(int) _litmajeur,_litminor,0);
#else
      _n = snprintf(_version,4,"%d%d%d",(int) _litmajeur,_litminor,0);
#endif
      if ( (_n < 0) || (_n > 3) ) {
	MESSAGE("Impossible d'obtenir un numéro de version valide : ");
	_version[3]='\0';SSCRUTE(_version);
	break;
      }

      func=getVersionedApi3(key,_version);
      --_litminor;
    }
    
    if (func != NULL) { goto QUIT; }
    else {_litmajeur=3;_litminor=MED_3_LATEST_MINOR;}
  }
    
  if (_litmajeur == 3) {

    if (_litminor > MED_3_LATEST_MINOR) {
      MESSAGE("Cette bibliothèque MED n'est pas capable de lire un fichier med-3 dont le mineur de la version "
	      "excède celui dont la bibliothèque à connaissance.");
      MESSAGE("La version demandée est       : ");ISCRUTE_int(_fversionMMR);
      MESSAGE("La dernière version med-3 connue de cette bibliothèque : ");ISCRUTE_int(300+10*MED_3_LATEST_MINOR+9);
    }
    
    /* Recherche décroissante à partir du numéro de release de la bibliothèque */
    while ( ( func == (MedFuncType)NULL) && (_litminor >= _lminminor3 ) ) {

#ifdef PPRO_NT_CALL
      _n = _snprintf(_version,4,"%d%d%d",(int) _litmajeur,_litminor,0);
#else
      _n = snprintf(_version,4,"%d%d%d",(int) _litmajeur,_litminor,0);
#endif
      if ( (_n < 0) || (_n > 3) ) {
	MESSAGE("Impossible d'obtenir un numéro de version valide : ");
	_version[3]='\0';SSCRUTE(_version);
	break;
      }

      func=getVersionedApi3(key,_version);
      --_litminor;
    }
    
    goto QUIT;
  }


 QUIT:

  if (func == NULL) {
    MESSAGE("Impossible d'obtenir une implémentation de : ");
    SSCRUTE(key);
    MESSAGE("en version :");
    ISCRUTE_int(_fversionMMR);
    MESSAGE("Vérifiez votre fichier .med et votre version de bibliothèque.");
  }

  return func;
}


