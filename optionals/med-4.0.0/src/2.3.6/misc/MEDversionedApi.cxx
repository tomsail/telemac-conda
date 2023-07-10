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

#include "MEDversionedApi.hxx"

extern "C" {
#include "med_config.h"
}

#include "med_versioned.h"
#include <med_utils.h>

using namespace std;

static MED_VERSIONED_API & MedVersionedApi=MED_VERSIONED_API::Instance();

MED_VERSIONED_API& MED_VERSIONED_API::Instance() {
  static MED_VERSIONED_API obj;
  return obj;
}

void MED_VERSIONED_API::f77ApiIsSet() {
  //SSCRUTE ("initMedVersionedApiF thing");
  _f77ApiIsSet=true;
}

// Le mécanisme de driver permet de faire évoluer la représentation des informations
// dans les fichiers MED nouvellement crées en restant
// capable de lire/écrire des fichiers MED avec des représentations antérieures.
// Cela permet donc une compatibilité ascendante en lecture/écriture des fichiers.
//
// 1) Les modifications d'implémentations d'une routine qui rendent impossible/incohérent
// la lecture d'anciens fichiers avec les nouvelles modifications du modèle interne necessitent
// le versionement de la routine dans cette table.
//
// 2) Les modifications d'implémentations d'une routine qui rendent impossible/incohérent
// la lecture de nouveaux fichiers avec les anciennes bibliothèques doivent également faire
// l'objet d'un  versionement dans cette table et non juste une modification dans les versions existantes.
//
// Ex : L'implémentation de MEDchampEcr entre 232 et 233 pour l'écriture des champs aux noeuds par maille.
//    La nouvelle bibliothèque est capable de lire les anciens fichiers qui ne contiennent pas cette fonctionnalité.
//    Cependant les anciennes bibliothèques ne sont pas capable de relire un fichier produit en 233 même
//    si l'utilisateur veut ignorer la lecture de tels champs. En effet, MEDnChamp et MEDchampInfo nous indiqueront 
//    la présence de champs qui seront illisible pour l'ancienne implémentation de l'API !
//
// Ceci permet à la nouvelle bibliothèque (233) qui lit un fichier 232 de se comporter comme la bibliothèque 232 
// et éviter toute corrompuption du modèle 232.
// Dans l'exemple précedent, si cette fonctionnalité n'avait pas eu d'impact sur le comportement
// de MEDnChamp et MEDchampInfo en 232, il n'aurait pas été necessaire de versionner cette routine.
// La seule protection possible pour les bibliothèques antérieures est de refuser la lecture de fichier MED dont le mineur
// est supérieur à celui de la bibliothèque courante. (l'exemple précédent est versionné sur le release au lieu du mineur, 
// la règle de versionement MED n'a pas été respectée).
//
// Les routines versionnées dont les numéros n'existent pas dans la version courante de la bibliothèque
// n'ont pas étés modifiées depuis la version de la bibliothèque qui correspond à leur numéro de versionement.
// Lorsque les routines wrappers (MEDchampEcr pour MEDchampEcr231,MEDchampEcr232,MEDchampEcr233) demandent leur implémentation
// à getVersionedApi en fonction de la version avec laquelle le fichier traité à été crée, la première implémentation 
// dont le numéro de versionement est inférieur ou égal à celui du fichier est renvoyé.
// getVersionedApi gère les ruptures de compatibilité globale de bibliothèques (ex : on ne relit
// pas de fichiers < 220 avec les biblioth�ques 2.3.x.
// Dans ce mécanisme, il est également necessaire de se protéger d'une lecture de fichier dont le mineur du numéro
// de version serait supérieur à celui de la bibliothèque.


//
// ******** POLITIQUE DE VERSIONEMENT DE LA BIBLIOTHEQUE MED ******
//
// Le versionement de la bibliothèque MED dans le fichier Makefile.am n'indique rien
// sur la compatibilité/incompatibilité descendante.
// Seule la capacité des anciens programmes à utiliser cette nouvelle bibliothèque dynamique est indiquée (ce qui
// sera toujours le cas avec le système de driver si l'API utilisateur ne change pas (sauf ajout)).
// Si l'API utilisateur change, le versionement libtool indique l'incompatibilité des anciens programmes à utiliser
// la nouvelle biblioth�que. Le numéro de version majeur de la bibliothèque devrait également être incrémenté.
// Si une nouvelle version majeure d'hdf est utilisée et qu'elle est incompatible avec l'ancienne le numéro majeur devrait
// être également augmenté.
//
// Si le numéro de version mineur de la bibliothèque avec laquelle le fichier a été crée est supèrieur à celui de la
// version de bibliothèque utilisée la bibliothèque doit renvoyer une erreur.
//
// Ceci implique qu'un ajout dans la table de versionement suppose l'incrément du numéro mineur de
// la bibliothèque pour plus de lisiblité par les utilisateurs quand à l'incompatibilité descendante :
// Une version de numéro mineur antérieur à celui de la bibliotèque MED courante ne pourra relire un fichier nouvellement
// crée par cette nouvelle version. Celà suppose également que les numéros de versions dont seuls la partie
// release change ne devrait pas apparaitre dans cette table. Ils correspondent à des corrections de BUG
// qui n'entrainent pas une incompatibilité descendante.
//
// Ceci n'a pas toujours été respecté, ex1: 231, 232, 233
// Par contre, ex2: 233 peut relire du 234
//
//
// En résumé le versionement dans MED doit fonctionner de la manière suivante à partir de la 2.3.4 :
//
// -  x.y.z+1 indique qu'il s'agit d'une correction de BUG qui n'engendre pas d'évolution du modèle
// ni d'incompatibilité de lecture de fichier avec x.y.z. Aucune nouvelle routine ne doit apparaître
// dans la table de versionement. Les programmes compilés avec medx.y.z peuvent utiliser medx.y.z+1 sans
// recompilation. Il y a donc compatibilité ascendante et descendante entre x.y.z  et x.y.z+1
//
// - x.y+1.z indique qu'il s'agit d'une correction de BUG et/ou évolution qui engendre soit une évolution
// du modèle interne ou une incohérence avec l'utilisation de l'implémentation medx.y.z pour un fichier x.y+1.z.
//  L'API de la bibliothèque ne change pas. Le système de driver permet la compatibilité ascendante.
//  Il n'est pas necessaire de recompiler les programmes pour bénéficier de cette nouvelle version
//  de la bibliothèque. Il n'y a pas de compatibilité descendante entre x.y+1.z x.y.z
//
// - x+1.y.z indique que l'API MED a changé ou que la version d'HDF utilisée n'est pas compatible
//   avec celle de x.y.z. La documentation indique si les drivers permettent d'assurer la compatibilité
//   ascendante. Il n'y a pas de compatibilité descendante avec x.y.z. Les programmes doivent être recompilés
//   et certainement modifiés pour fonctionner avec x+1.Y.z
//



// REM : Ce container singleton est complété par les symboles fortran
//       à l'appel de edfouv(...) dans la partie fortran de la bibliothèque.
//       La méthode  f77ApiIsSet est alors appelée.
//
MED_VERSIONED_API::MED_VERSIONED_API() : map<keyType,
					     MedFuncType>(),
					 _f77ApiIsSet(false)
{
  map<keyType,MedFuncType > &
    table  = ( map<keyType,
	       MedFuncType > & ) *this ;
   table[ "MEDchampEcr231" ]           = MEDchampEcr231 ;
   table[ "MEDchampEcr232" ]           = MEDchampEcr232 ;
   table[ "MEDchampEcr233" ]           = MEDchampEcr233 ;
   table[ "MEDjointCr231" ]            = MEDjointCr231 ;
   table[ "MEDjointCr232" ]            = MEDjointCr232 ;
   table[ "MEDfamCr231" ]              = MEDfamCr231 ;
   table[ "MEDfamCr232" ]              = MEDfamCr232 ;
   table[ "_MEDdatasetNumEcrire231" ]  = _MEDdatasetNumEcrire231 ;
   table[ "_MEDdatasetNumEcrire232" ]  = _MEDdatasetNumEcrire232 ;
   table[ "_MEDdatasetNumLire231" ]    = _MEDdatasetNumLire231 ;
   table[ "_MEDdatasetNumLire232" ]    = _MEDdatasetNumLire232 ;
}

MED_VERSIONED_API::~MED_VERSIONED_API() {};

MedFuncType MED_VERSIONED_API::operator[]( const keyType & c ) const
{
  map<keyType,MedFuncType > &table = (map<keyType,
				   MedFuncType >&)*this ;

  map<keyType,MedFuncType >::iterator it = table.find( c );
  if ( it == table.end() ) return (MedFuncType) NULL;
  return (*it).second;
}


extern "C" {
  MedFuncType getVersionedApi(const char * const keycharpart,
			      const char * const keynumpart) {
    return MedVersionedApi[std::string(keycharpart)+std::string(keynumpart)];
  }

  void f77ApiIsSet(void * obj) {
    static_cast<MED_VERSIONED_API*>(obj)->f77ApiIsSet();
  }
}
