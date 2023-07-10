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

#ifndef MED_VERSIONED_PROTO_EN_H
#define MED_VERSIONED_PROTO_EN_H

#include "medC_win_dll.h"
#include <stdarg.h>
MEDC_EXPORT extern void  _MEDmeshInfoByName30(int dummy,...);
MEDC_EXPORT extern void  _MEDmeshInfoByName236(int dummy,...);
MEDC_EXPORT extern void  _MEDmeshnAxisByName30(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshnAxisByName236(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshUniversalNameRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshUniversalNameRd236(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshnEntity30(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshnEntity236(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshAdvancedRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshAdvancedRd236(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshEntityInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshEntityInfo236(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshComputationStepInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshComputationStepInfo236(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshGridIndexCoordinateRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDmeshGridIndexCoordinateRd236(int dummy, ...);

MEDC_EXPORT extern void  _MEDfilterEntityCr30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfilterEntityCr236(int dummy, ...);

MEDC_EXPORT extern void  _MEDnFamily30(int dummy, ...);
MEDC_EXPORT extern void  _MEDnFamily236(int dummy, ...);
MEDC_EXPORT extern void  _MEDnFamilyGroup32(int dummy, ...);
MEDC_EXPORT extern void  _MEDnFamilyGroup30(int dummy, ...);
MEDC_EXPORT extern void  _MEDnFamilyGroup236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfamilyCr32(int dummy, ...);
MEDC_EXPORT extern void  _MEDfamilyCr30(int dummy, ...);
MEDC_EXPORT extern void  _MEDnFamily23Attribute236(int dummy,...);
MEDC_EXPORT extern void  _MEDnFamily23Attribute30(int dummy,...);
MEDC_EXPORT extern void  _MEDfamilyInfo32(int dummy,...);
MEDC_EXPORT extern void  _MEDfamilyInfo30(int dummy,...);
MEDC_EXPORT extern void  _MEDfamily23Info30(int dummy,...);
MEDC_EXPORT extern void  _MEDfamily23Info236(int dummy,...);

MEDC_EXPORT extern void  _MEDlinkRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDlinkRd236(int dummy, ...);

MEDC_EXPORT extern void  _MEDprofileRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDprofileRd236(int dummy, ...);

MEDC_EXPORT extern void  _MEDlocalizationRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDlocalizationRd236(int dummy, ...);
MEDC_EXPORT extern void  _MEDlocalizationInfoByName30(int dummy, ...);
MEDC_EXPORT extern void  _MEDlocalizationInfoByName236(int dummy, ...);

MEDC_EXPORT extern void  _MEDnEquivalence30(int dummy, ...);
MEDC_EXPORT extern void  _MEDnEquivalence236(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceInfo236(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceCorrespondenceSize30(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceCorrespondenceSize236(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceCorrespondenceRd33(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceCorrespondenceRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceCorrespondenceRd236(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceCorrespondenceWr33(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceCorrespondenceWr30(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceComputingStepInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceComputingStepInfo236(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceCorrespondenceSizeInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDequivalenceCorrespondenceSizeInfo236(int dummy, ...);

MEDC_EXPORT extern void  _MEDnSubdomainJoint30(int dummy, ...);
MEDC_EXPORT extern void  _MEDnSubdomainJoint236(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainJointInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainJointInfo236(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainCorrespondenceSize30(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainCorrespondenceSize236(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainCorrespondenceRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainCorrespondenceRd236(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainComputingStepInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainComputingStepInfo236(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainCorrespondenceSizeInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDsubdomainCorrespondenceSizeInfo236(int dummy, ...);

MEDC_EXPORT extern void  _MEDparameterInfoByName30(int dummy, ...);
MEDC_EXPORT extern void  _MEDparameterInfoByName236(int dummy, ...);
MEDC_EXPORT extern void  _MEDparameterValueRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDparameterValueRd236(int dummy, ...);

MEDC_EXPORT extern void  _MEDfileCommentRd30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfileCommentRd236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfileObjectsMount30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfileObjectsMount236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfileObjectsUnmount30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfileObjectsUnmount236(int dummy, ...);

MEDC_EXPORT extern void  _MEDfieldCr30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldCr31(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldInfoByName30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldInfoByName236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldComputingStepInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldComputingStepInfo31(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldComputingStepInfo236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldComputingStepMeshInfo31(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldComputingStepMeshInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldComputingStepMeshInfo236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldnProfile30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldnProfile236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldnValue30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldnValue236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfield23ComputingStepMeshInfo30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfield23ComputingStepMeshInfo31(int dummy, ...);
MEDC_EXPORT extern void  _MEDfield23ComputingStepMeshInfo236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfield23nProfile30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfield23nProfile236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfield23nValue30(int dummy, ...);
MEDC_EXPORT extern void  _MEDfield23nValue236(int dummy, ...);
MEDC_EXPORT extern void  _MEDfieldValueAdvancedWr30(int dummy,...);
MEDC_EXPORT extern void  _MEDfieldValueAdvancedRd30(int dummy,...);
MEDC_EXPORT extern void  _MEDfieldValueAdvancedWr33(int dummy,...);
MEDC_EXPORT extern void  _MEDfieldValueAdvancedRd33(int dummy,...);
MEDC_EXPORT extern void  _MEDfieldValueAdvancedRd236(int dummy,...);

#endif
