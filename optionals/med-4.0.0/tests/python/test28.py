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
#  * - Nom du fichier : test28.py
#  *
#  * - Description : lecture d'un maillage structure (grille cartesienne |
#  *                 grille curvilinéaire) dans le fichier test27.med
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *
from med.medenum import *

# /* Ouverture du fichier test27.med */
fid = MEDfileOpen("test27.med",MED_ACC_RDONLY)

# /* Lecture du nombre de maillage */
nmaa = MEDnMesh(fid)

for i in range(nmaa):
    maa, sdim, mdim, meshtype, desc, dtunit, sort, nstep,  repere, axisname, axisunit = MEDmeshInfo(fid, i+1)

    print("\nMaillage de nom : |%s| , de dimension : %d , et de type %s\n"%(maa,mdim,meshtype))
    print("\t -Dimension de l'espace : %d\n"%(sdim))
    print("\t -Description du maillage : |%s|\n"%(desc))
    print("\t -Noms des axes : |%s|\n"%(axisname))
    print("\t -Unités des axes : |%s|\n"%(axisunit))
    print("\t -Type de repère : %s\n"%(repere))
    print("\t -Nombre d'étapes de calcul : %d\n"%(nstep))
    print("\t -Unité des dates : |%s|\n"%(dtunit))
    if (meshtype.val == MED_STRUCTURED_MESH):
      print("\t - Type : Maillage structuré \n")
    else:
      print("\t - Type : Maillage non structuré \n")

    gridtype=MED_GRID_TYPE()
    if (meshtype.val == MED_STRUCTURED_MESH):
        gridtype=MEDmeshGridTypeRd(fid,maa)
    if (gridtype.val == MED_CARTESIAN_GRID):    print("\t - Grille cartesienne \n")
    if (gridtype.val == MED_CURVILINEAR_GRID):  print("\t - Grille déstructuré \n")

# /* On regarde les coordonnees de la grille standard */
    if ( (meshtype.val == MED_STRUCTURED_MESH) and (gridtype.val == MED_CURVILINEAR_GRID)):

        nnoeuds,chgt,trsf = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
                                               MED_NODE, MED_NONE, MED_COORDINATE, MED_NO_CMODE)
        print("\t   Nombre de noeuds : %d"%(nnoeuds))

        structure_grille = MEDINT(mdim)
        MEDmeshGridStructRd(fid,maa,MED_NO_DT,MED_NO_IT, structure_grille)
        print("\t   Structure des noeuds de la grille : ",structure_grille)

        coo = MEDFLOAT(nnoeuds*mdim)
        MEDmeshNodeCoordinateRd(fid, maa, MED_NO_DT, MED_NO_IT,
                                MED_FULL_INTERLACE, coo)
        print("\t   Coordonnees des oeuds de la grille: ",coo)

        nfam,chgt,trsf=MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
                                      MED_NODE, MED_NONE, MED_FAMILY_NUMBER,MED_NODAL)

        if (nfam != 0):print("Le nombre de famille de vrait être nul")

 # /* On regarde les coordonnees des indices de la grille cartesienne */
    if (meshtype.val == MED_STRUCTURED_MESH) and (gridtype.val == MED_CARTESIAN_GRID):
      for cdim in range(mdim):
          quoi = [MED_COORDINATE_AXIS1,MED_COORDINATE_AXIS2,MED_COORDINATE_AXIS3][cdim]

          nind,chgt,trsf = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
                                          MED_NODE, MED_NONE, quoi, MED_NO_CMODE)
          print("\t   Lecture de la taille de l'indice : ",nind)

          # /* on lit le tableau des indices */
          indices = MEDFLOAT(nind)
          MEDmeshGridIndexCoordinateRd(fid,maa,MED_NO_DT,MED_NO_IT,cdim+1,indices)

          print("\t   Axe %s [%s] : "%(axisname[cdim*MED_SNAME_SIZE:cdim*MED_SNAME_SIZE+MED_SNAME_SIZE],
                                       axisunit[cdim*MED_SNAME_SIZE:cdim*MED_SNAME_SIZE+MED_SNAME_SIZE]))
          print("\t\t",indices)


          nfam,chgt,trsf=MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,  
                                        MED_NODE, MED_NONE, MED_FAMILY_NUMBER,MED_NODAL)

          if (nfam != 0):print("Le nombre de famille de vrait être nul") 
          
MEDfileClose(fid)
