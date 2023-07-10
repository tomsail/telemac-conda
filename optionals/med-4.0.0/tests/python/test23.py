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
#  * - Nom du fichier : test23.py
#  *
#  * - Description : ecriture de mailles/faces de type MED_POLYGONE
#  *                 dans un maillage MED
#  *
#  *****************************************************************************/


from med.medfile import *
from med.medmesh import *

maa   = "maa1"
mdim  = 3
index = MEDINT([1,6,12,17])
#Attention ces numéros de noeuds sont fantaisistes (pour test)
con   = MEDINT([1,2,3,4,5,      6,7,8,9,10,11,     12,13,14,15,16])
con2  = MEDINT([1,2,3,4,5,      101,102,103,104,105,
                6,7,8,9,10,11,  106,107,108,109,110,111,     
                12,13,14,15,16, 112,113,114,115,116])
index2 = MEDINT([1,11,23,33])

#Table des coordonnees (pour test ) */
nnoe = 1
coo   = MEDFLOAT([0.0, 1.0, 2.0])

n=3;ni = 4;
#/*  123456789012345612345678901234561234567890123456 */
nom=MEDCHAR("poly1           poly2           poly3           ")
nomb=MEDCHAR("".join( [("poly"+str(i)).zfill(MED_SNAME_SIZE) for i in range(n)] ) )

num = MEDINT([1,2,3])
fam = MEDINT([0,-1,-2])
nomcoo = "x               y               z               "
unicoo = "cm              cm              cm              "

# /* Creation du fichier test23.med */
fid = MEDfileOpen("test23.med",MED_ACC_CREAT);
print("Creation du fichier test23.med \n")

# /* Creation du maillage */
MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,
           "un maillage pour test23","s", MED_SORT_DTIT,
           MED_CARTESIAN, nomcoo, unicoo)
print("Creation du maillage \n")

# Ecriture des coordonnees des noeuds en mode MED_FULL_INTERLACE :
#   (X1,Y1, X2,Y2, X3,Y3, ...) dans un repere cartesien */
MEDmeshNodeCoordinateWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT, MED_FULL_INTERLACE,nnoe,  coo)

# /* Ecriture de la connectivite des mailles polygones en mode nodal */
# MEDmeshPolygonWr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MED_CELL,MED_NODAL,
#                  ni,index,con)
MEDmeshPolygon2Wr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MED_CELL,MED_POLYGON,MED_NODAL,
                  ni,index,con)
print("Ecriture des connectivites de mailles de type MED_POLYGONE en mode nodal \n")

# /* Ecriture de la connectivite des mailles polygones en mode nodal */
MEDmeshPolygon2Wr(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MED_CELL,MED_POLYGON2,MED_NODAL,
                  ni,index2,con2)
print("Ecriture des connectivites de mailles de type MED_POLYGONE Quadratique en mode nodal \n")

param=(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_POLYGON,n)
# /* Ecriture des noms des polygones */
# /* ecriture (optionnelle) des noms des polygones */
MEDmeshEntityNameWr(*param,name=nom)
# MEDmeshEntityNameWr(fid,maa,MED_NO_DT,MED_NO_IT,
#                     MED_CELL,MED_POLYGON,n,nom)
print("Ecriture des noms des polygones \n")

# /* ecriture (optionnelle) des numeros des polygones */
MEDmeshEntityNumberWr(*param,number=num)
print("Ecriture des numeros des polygones \n")

# /* ecriture des numeros des familles des polygones */
MEDmeshEntityFamilyNumberWr(*param,number=fam)
print("Ecriture des numeros des familles des polygones \n")

# /* Fermeture du fichier */
MEDfileClose(fid)
