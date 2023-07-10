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
#  * - Nom du fichier : test27.py
#  *
#  * - Description : creation de maillages structures (grille cartesienne |
#  *                 grille standard ) dans le fichier test27.med
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *

mdim=2;axe=0;nind=0;
indiceX=MEDFLOAT([1.0,1.1,1.2,1.3])
indiceY=MEDFLOAT([2.0,2.1,2.2,2.3])
coo=MEDFLOAT([0.0,0.0,1.0,0.0,0.0,1.0,1.0,1.0])
nnoeuds    = 4
structure_grille = MEDINT([2,2])
maa = "grille_cartesian"
maa2= "grille_curvilinear"

# /* composantes et unites */
# /           12345678901234561234567890123456 */
comp = "X               Y               "
unit = "cm              cm              "

# /* Creation du fichier test27.med */
fid = MEDfileOpen("test27.med",MED_ACC_CREAT)

# /* Creation du maillage "maa" de type MED_NON_STRUCURE  et de dimension 2 */
MEDmeshCr( fid, "maillage vide",mdim, mdim, MED_UNSTRUCTURED_MESH,
           "un maillage vide","s", MED_SORT_DTIT,
           MED_CARTESIAN, comp, unit)
print("Creation d'un maillage structure MED_STRUCTURED_MESH \n")

# /* creation d'une grille cartesienne de dimension 2 */
# /* on commence par definir un maillage MED_STRUCTURED_MESH
#    de dimension 2 */
MEDmeshCr( fid, maa, mdim, mdim, MED_STRUCTURED_MESH,
      	 "un exemple de grille cartesienne","s", MED_SORT_DTIT,
      	 MED_CARTESIAN, comp, unit)
print("Creation d'un maillage structure MED_STRUCTURED_MESH \n")

MEDmeshGridTypeWr(fid,maa, MED_CARTESIAN_GRID)
print("On definit la nature du maillage structure : MED_GRILLE_CARTESIENNE \n")

# /* on definit les indices des coordonnees de la grille selon chaque dimension  */
# /* axe des "X" */
nind = 4
axe  = 1

MEDmeshGridIndexCoordinateWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
                             axe,nind,indiceX)
print("Ecriture des indices des coordonnees selon l'axe des X \n")


# /* axe des "Y" */
nind = 4
axe  = 2

MEDmeshGridIndexCoordinateWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
                             axe,nind,indiceY)
print("Ecriture des indices des coordonnees selon l'axe des Y \n")

# /* Creation d'une grille MED_CURVILINEAR_GRID de dimension 2 */
# /* on commence par definir un maillage MED_STRUCTURED_MESH
#    de dimension 2 */

MEDmeshCr(fid, maa2, mdim, mdim, MED_STRUCTURED_MESH,
          "un exemple de grille standard","s", MED_SORT_DTIT,
          MED_CARTESIAN, comp, unit)
print("Creation d'un maillage structure MED_STRUCTURED_MESH \n")

# /* On specifie la nature du maillage structure : MED_CURVILINEAR_GRID */
MEDmeshGridTypeWr(fid,maa2, MED_CURVILINEAR_GRID)
print("On definit la nature du maillage structure : MED_CURVILINEAR_GRID \n")

MEDmeshNodeCoordinateWr(fid,maa2,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,
                        MED_FULL_INTERLACE,nnoeuds,coo)
print("Ecriture des coordonnees des noeuds \n")

# /* On definit la structure de la grille */
MEDmeshGridStructWr(fid,maa2,MED_NO_DT,MED_NO_IT, MED_UNDEF_DT, structure_grille )
print("Ecriture de la structure de la grille : / 2,2 / \n")

MEDfileClose(fid)
