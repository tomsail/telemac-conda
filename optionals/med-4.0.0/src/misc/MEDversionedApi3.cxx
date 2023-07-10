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
#include "MEDversionedApi3.hxx"


extern "C" {
#include "med_config.h"
}

#include "med_versioned.h"
#include <med_utils.h>

using namespace std;

static MED_VERSIONED_API3 & MedVersionedApi3=MED_VERSIONED_API3::Instance();

MED_VERSIONED_API3::~MED_VERSIONED_API3() {};

MED_VERSIONED_API3& MED_VERSIONED_API3::Instance() {
  static MED_VERSIONED_API3 obj;
  return obj;
}

void MED_VERSIONED_API3::f77ApiIsSet() {
//   SSCRUTE ("initMedVersionedApi3F thing");
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
// getVersionedApi gère également les ruptures de compatibilité globale de bibliothèques (ex : on ne relit
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
// la nouvelle bibliothèque. Le numéro de version majeur de la bibliothèque devrait également être incrémenté.
// Si une nouvelle version majeure d'hdf est utilisée et qu'elle est incompatible avec l'ancienne le numéro majeur devrait
// être également augmenté.
//
// Si le numéro de version mineur de la bibliothèque avec laquelle le fichier a été crée est supérieur à celui de la
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
// ni d'incompatibilité de lecture de fichier avec une bibiothèque x.y.z. Aucune nouvelle routine ne doit apparaître
// dans la table de versionement. Les programmes compilés avec medx.y.z peuvent utiliser medx.y.z+1 sans
// recompilation. Il y a donc compatibilité ascendante et descendante entre x.y.z et x.y.z+1
//
// - x.y+1.z indique qu'il s'agit d'une correction de BUG et/ou évolution qui engendre soit une évolution
// du modèle interne ou une incohérence avec l'utilisation de l'implémentation medx.y.z pour un fichier x.y+1.z.
//  L'API de la bibliothèque ne change pas. Le système de driver permet la compatibilité ascendante.
//  Il n'est pas necessaire de recompiler les programmes pour bénéficier de cette nouvelle version
//  de la bibliothèque.
//  Il n'y a pas de compatibilité descendante entre un fichier x.y+1.z  et une bibliothèque x.y.z.
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
MED_VERSIONED_API3::MED_VERSIONED_API3() : map<keyType,
					     MedFuncType>(),
					 _f77ApiIsSet(false)
{
  map<keyType,MedFuncType > &
    table  = ( map<keyType,
	       MedFuncType > & ) *this ;

   table[ "_MEDequivalenceInfo300"                   ] = _MEDequivalenceInfo30 ;
   table[ "_MEDequivalenceInfo236"                   ] = _MEDequivalenceInfo236 ;
   table[ "_MEDequivalenceCorrespondenceSize300"     ] = _MEDequivalenceCorrespondenceSize30 ;
   table[ "_MEDequivalenceCorrespondenceSize236"     ] = _MEDequivalenceCorrespondenceSize236 ;
   table[ "_MEDequivalenceCorrespondenceRd330"       ] = _MEDequivalenceCorrespondenceRd33 ;
   table[ "_MEDequivalenceCorrespondenceRd300"       ] = _MEDequivalenceCorrespondenceRd30 ;
   table[ "_MEDequivalenceCorrespondenceWr330"       ] = _MEDequivalenceCorrespondenceWr33 ;
   table[ "_MEDequivalenceCorrespondenceWr300"       ] = _MEDequivalenceCorrespondenceWr30 ;
   table[ "_MEDequivalenceCorrespondenceRd236"       ] = _MEDequivalenceCorrespondenceRd236 ;
   table[ "_MEDequivalenceComputingStepInfo300"      ] = _MEDequivalenceComputingStepInfo30 ;
   table[ "_MEDequivalenceComputingStepInfo236"      ] = _MEDequivalenceComputingStepInfo236 ;
   table[ "_MEDequivalenceCorrespondenceSizeInfo300" ] = _MEDequivalenceCorrespondenceSizeInfo30 ;
   table[ "_MEDequivalenceCorrespondenceSizeInfo236" ] = _MEDequivalenceCorrespondenceSizeInfo236 ;
   table[ "_MEDnEquivalence300"                      ] = _MEDnEquivalence30 ;
   table[ "_MEDnEquivalence236"                      ] = _MEDnEquivalence236 ;

   table[ "_MEDfieldCr300"                      ]   = _MEDfieldCr30 ;
   table[ "_MEDfieldCr236"                      ]   = _MEDfieldCr30 ;
   table[ "_MEDfieldCr310"                      ]   = _MEDfieldCr31 ;
   table[ "_MEDfieldValueAdvancedWr300"         ]   = _MEDfieldValueAdvancedWr30 ;
   table[ "_MEDfieldValueAdvancedRd300"         ]   = _MEDfieldValueAdvancedRd30 ;
   table[ "_MEDfieldValueAdvancedWr330"         ]   = _MEDfieldValueAdvancedWr33 ;
   //La lecture se fait selon la nouvelle API.
   // table[ "_MEDfieldValueAdvancedRd330"         ]   = _MEDfieldValueAdvancedRd33 ;
   table[ "_MEDfieldValueAdvancedRd236"         ]   = _MEDfieldValueAdvancedRd236 ;
   table[ "_MEDfield23ComputingStepMeshInfo310" ]   = _MEDfield23ComputingStepMeshInfo31 ;
   table[ "_MEDfield23ComputingStepMeshInfo300" ]   = _MEDfield23ComputingStepMeshInfo30 ;
   table[ "_MEDfield23ComputingStepMeshInfo236" ]   = _MEDfield23ComputingStepMeshInfo236 ;
   table[ "_MEDfield23nProfile300"              ]   = _MEDfield23nProfile30 ;
   table[ "_MEDfield23nProfile236"              ]   = _MEDfield23nProfile236 ;
   table[ "_MEDfield23nValue300"                ]   = _MEDfield23nValue30 ;
   table[ "_MEDfield23nValue236"                ]   = _MEDfield23nValue236 ;
   table[ "_MEDfieldInfoByName300"              ]   = _MEDfieldInfoByName30 ;
   table[ "_MEDfieldInfoByName236"              ]   = _MEDfieldInfoByName236 ;
   table[ "_MEDfieldComputingStepInfo310"       ]   = _MEDfieldComputingStepInfo31 ;
   table[ "_MEDfieldComputingStepInfo300"       ]   = _MEDfieldComputingStepInfo30 ;
   table[ "_MEDfieldComputingStepInfo236"       ]   = _MEDfieldComputingStepInfo236 ;
   table[ "_MEDfieldComputingStepMeshInfo310"   ]   = _MEDfieldComputingStepMeshInfo31 ;
   table[ "_MEDfieldComputingStepMeshInfo300"   ]   = _MEDfieldComputingStepMeshInfo30 ;
   table[ "_MEDfieldComputingStepMeshInfo236"   ]   = _MEDfieldComputingStepMeshInfo236 ;
   table[ "_MEDfieldnProfile300"                ]   = _MEDfieldnProfile30 ;
   table[ "_MEDfieldnProfile236"                ]   = _MEDfieldnProfile236 ;
   table[ "_MEDfieldnValue300"                  ]   = _MEDfieldnValue30 ;
   table[ "_MEDfieldnValue236"                  ]   = _MEDfieldnValue236 ;

   table[ "_MEDfileCommentRd300"      ] = _MEDfileCommentRd30 ;
   table[ "_MEDfileCommentRd236"      ] = _MEDfileCommentRd236 ;
   table[ "_MEDfileObjectsMount300"   ] = _MEDfileObjectsMount30 ;
   table[ "_MEDfileObjectsMount236"   ] = _MEDfileObjectsMount236 ;
   table[ "_MEDfileObjectsUnmount300" ] = _MEDfileObjectsUnmount30 ;
   table[ "_MEDfileObjectsUnmount236" ] = _MEDfileObjectsUnmount236 ;

   table[ "_MEDfilterEntityCr300"         ] = _MEDfilterEntityCr30 ;
   table[ "_MEDfilterEntityCr236"         ] = _MEDfilterEntityCr236 ;

   table[ "_MEDnFamily23Attribute236"   ]   = _MEDnFamily23Attribute236 ;
   table[ "_MEDnFamily23Attribute300"   ]   = _MEDnFamily23Attribute30 ;
   table[ "_MEDnFamily300"              ]   = _MEDnFamily30 ;
   table[ "_MEDnFamily236"              ]   = _MEDnFamily236 ;
   table[ "_MEDnFamilyGroup320"         ]   = _MEDnFamilyGroup32 ;
   table[ "_MEDnFamilyGroup300"         ]   = _MEDnFamilyGroup30 ;
   table[ "_MEDnFamilyGroup236"         ]   = _MEDnFamilyGroup236 ;
   table[ "_MEDfamily23Info236"         ]   = _MEDfamily23Info236 ;
   table[ "_MEDfamily23Info300"         ]   = _MEDfamily23Info30 ;
   table[ "_MEDfamilyCr320"         ]   = _MEDfamilyCr32 ;
   table[ "_MEDfamilyCr300"         ]   = _MEDfamilyCr30 ;
   table[ "_MEDfamilyInfo320"       ]   = _MEDfamilyInfo32 ;
   table[ "_MEDfamilyInfo300"       ]   = _MEDfamilyInfo30 ;


   table[ "_MEDlinkRd300"         ] = _MEDlinkRd30 ;
   table[ "_MEDlinkRd236"         ] = _MEDlinkRd236 ;

   table[ "_MEDlocalizationRd300" ] = _MEDlocalizationRd30 ;
   table[ "_MEDlocalizationRd236" ] = _MEDlocalizationRd236 ;
   table[ "_MEDlocalizationInfoByName300" ] = _MEDlocalizationInfoByName30 ;
   table[ "_MEDlocalizationInfoByName236" ] = _MEDlocalizationInfoByName236 ;


   table[ "_MEDmeshEntityInfo300" ] = _MEDmeshEntityInfo30 ;
   table[ "_MEDmeshEntityInfo236" ] = _MEDmeshEntityInfo236 ;
   table[ "_MEDmeshComputationStepInfo300" ] = _MEDmeshComputationStepInfo30 ;
   table[ "_MEDmeshComputationStepInfo236" ] = _MEDmeshComputationStepInfo236 ;
   table[ "_MEDmeshGridIndexCoordinateRd300" ] = _MEDmeshGridIndexCoordinateRd30 ;
   table[ "_MEDmeshGridIndexCoordinateRd236" ] = _MEDmeshGridIndexCoordinateRd236 ;
   table[ "_MEDmeshInfoByName300"         ] = _MEDmeshInfoByName30 ;
   table[ "_MEDmeshInfoByName236"         ] = _MEDmeshInfoByName236 ;
   table[ "_MEDmeshnAxisByName300"         ] = _MEDmeshnAxisByName30 ;
   table[ "_MEDmeshnAxisByName236"         ] = _MEDmeshnAxisByName236 ;
   table[ "_MEDmeshUniversalNameRd300"         ] = _MEDmeshUniversalNameRd30 ;
   table[ "_MEDmeshUniversalNameRd236"         ] = _MEDmeshUniversalNameRd236 ;
   table[ "_MEDmeshnEntity300"         ] = _MEDmeshnEntity30 ;
   table[ "_MEDmeshnEntity236"         ] = _MEDmeshnEntity236 ;
   table[ "_MEDmeshAdvancedRd300"         ] = _MEDmeshAdvancedRd30 ;
   table[ "_MEDmeshAdvancedRd236"         ] = _MEDmeshAdvancedRd236 ;

   table[ "_MEDparameterInfoByName300" ] = _MEDparameterInfoByName30 ;
   table[ "_MEDparameterInfoByName236" ] = _MEDparameterInfoByName236 ;
   table[ "_MEDparameterValueRd300"    ] = _MEDparameterValueRd30 ;
   table[ "_MEDparameterValueRd236"    ] = _MEDparameterValueRd236 ;

   table[ "_MEDprofileRd300" ] = _MEDprofileRd30 ;
   table[ "_MEDprofileRd236" ] = _MEDprofileRd236 ;

   table[ "_MEDnSubdomainJoint300" ] = _MEDnSubdomainJoint30 ;
   table[ "_MEDnSubdomainJoint236" ] = _MEDnSubdomainJoint236 ;
   table[ "_MEDsubdomainJointInfo300" ] = _MEDsubdomainJointInfo30 ;
   table[ "_MEDsubdomainJointInfo236" ] = _MEDsubdomainJointInfo236 ;
   table[ "_MEDsubdomainCorrespondenceSize300" ] = _MEDsubdomainCorrespondenceSize30 ;
   table[ "_MEDsubdomainCorrespondenceSize236" ] = _MEDsubdomainCorrespondenceSize236 ;
   table[ "_MEDsubdomainCorrespondenceRd300" ] = _MEDsubdomainCorrespondenceRd30 ;
   table[ "_MEDsubdomainCorrespondenceRd236" ] = _MEDsubdomainCorrespondenceRd236 ;
   table[ "_MEDsubdomainComputingStepInfo300" ] = _MEDsubdomainComputingStepInfo30 ;
   table[ "_MEDsubdomainComputingStepInfo236" ] = _MEDsubdomainComputingStepInfo236 ;
   table[ "_MEDsubdomainCorrespondenceSizeInfo300" ] = _MEDsubdomainCorrespondenceSizeInfo30 ;
   table[ "_MEDsubdomainCorrespondenceSizeInfo236" ] = _MEDsubdomainCorrespondenceSizeInfo236 ;

   table[ "_MEDgetInternalGeometryTypeName330" ] = _MEDgetInternalGeometryTypeName33 ;
   table[ "_MEDgetInternalGeometryTypeName300" ] = _MEDgetInternalGeometryTypeName30 ;
   /* On devrait appeler l'API2.3.6 pour résoudre les types internes 
      mais ce sont les mêmes en 236 et 30 */
   table[ "_MEDgetInternalGeometryTypeName236" ] = _MEDgetInternalGeometryTypeName30 ;


}



MedFuncType MED_VERSIONED_API3::operator[]( const keyType & c ) const
{
  map<keyType,MedFuncType > &table = (map<keyType, MedFuncType >&)*this ;

  map<keyType,MedFuncType >::iterator it = table.find( c );
  if ( it == table.end() ) return (MedFuncType) NULL;
  return (*it).second;
}


extern "C" {
  MedFuncType getVersionedApi3(const char * const keycharpart,
			      const char * const keynumpart) {
    return MedVersionedApi3[std::string(keycharpart)+std::string(keynumpart)];
  }

  void f77Api3IsSet(void * obj) {
    static_cast<MED_VERSIONED_API3*>(obj)->f77ApiIsSet();
  }
}
