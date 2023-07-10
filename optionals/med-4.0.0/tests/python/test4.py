#! /usr/bin/env python
# -*- coding:utf-8 -*-
# /*  This file is part of MED.
#  *
#  *  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
#  *  MED is free software: you can redistribute it and/or modify
#  *  it under the terms of the GNU Lesser General Public License as published by
#  *  the Free Software Foundation, either version 3 of the License, or
#  *  (at your option) any later version.
#  *
#  *  MED is distributed in the hope that it will be useful,
#  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  *  GNU Lesser General Public License for more details.
#  *
#  *  You should have received a copy of the GNU Lesser General Public License
#  *  along with MED.  If not, see <http://www.gnu.org/licenses/>.
#  */


# /******************************************************************************
#  * - Nom du fichier : test4.py
#  *
#  * - Description : ecriture des noeuds d'un maillage MED.
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *

MODE_ACCES=MED_ACC_CREAT
# /* la dimension du maillage */
mdim = 2;
# /* nom du maillage de longueur maxi MED_NAME_SIZE */
maa  = "maa1";
# /* le nombre de noeuds */
nnoe = 4;
# /* table des coordonnees (dimension * nombre de noeuds) */
coo   = MEDFLOAT([0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0])
coo_2 = MEDFLOAT([0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0])
# /* tables des noms et des unites des coordonnees (dimension*MED_SNAME_SIZE+1) */
# /*                                  12345678901234561234567890123456*/
nomcoo = "x               y               "
unicoo = "cm              cm              "
# /* tables des noms, numeros, numeros de familles des noeuds
#    autant d'elements que de noeuds - les noms ont pout longueur  MED_SNAME_SIZE */
# /*                                  1234567890123456123456789012345612345678901234561234567890123456*/
nomnoe = MEDCHAR("nom1            nom2            nom3            nom4            ")
numnoe = MEDINT([ 1,2,3,4])
nufano = MEDINT([ 0,1,2,2])


# /* Creation du fichier "test4.med" */
# /* ouverture du fichier */
fid = MEDfileOpen("test4.med",MODE_ACCES)

# /* Creation du maillage "maa" de type MED_NON_STRUCURE  et de dimension 2 */
     
MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,
           "un maillage pour test4","s", MED_SORT_DTIT,
           MED_CARTESIAN, nomcoo, unicoo)

# /* Ecriture des coordonnees des noeuds en mode MED_FULL_INTERLACE :
#   (X1,Y1, X2,Y2, X3,Y3, ...) dans un repere cartesien */
MEDmeshNodeCoordinateWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT, MED_FULL_INTERLACE,nnoe,  coo)

param=(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,nnoe)
# /* Ecriture des noms des noeuds (optionnel dans un maillage MED) */
MEDmeshEntityNameWr(*param,name=nomnoe)

# /* Ecriture des numeros des noeuds (optionnel dans un maillage MED) */
MEDmeshEntityNumberWr(*param,number=numnoe)

# /* Ecriture des numeros de famille des noeuds */
MEDmeshEntityFamilyNumberWr(*param,number=nufano)

# /* Ecriture des coordonnees des noeuds en mode MED_FULL_INTERLACE :
#      (X1,Y1, X2,Y2, X3,Y3, ...) dans un repere cartesien */
MEDmeshNodeCoordinateWr(fid,maa,0,5,0.5, MED_FULL_INTERLACE,nnoe, coo_2)

# /* Ecriture des coordonnees des noeuds en mode MED_FULL_INTERLACE :
#     (X1,Y1, X2,Y2, X3,Y3, ...) dans un repere cartesien */
MEDmeshNodeCoordinateWr(fid,maa,2,0,2.0, MED_FULL_INTERLACE,nnoe,coo)

MEDmeshComputationStepCr(fid,maa,-1,-1,0,2,0.25)

MEDmeshComputationStepCr(fid,maa,0,2,0,3,0.35)
     
# /* Fermeture du fichier */
MEDfileClose(fid)
