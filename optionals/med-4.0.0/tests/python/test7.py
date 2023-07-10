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
#  * - Nom du fichier : test7.py
#  *
#  * - Description : lecture des elements du maillage MED crees par test6
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *
from med.medfilter import *

MODE_ACCES=MED_ACC_CREAT
NULL=MEDINT(0)

flt = MEDINT([ 2, 3 ])
fltsize=2

# /* Ouverture du fichier test4.med */
fid = MEDfileOpen("test6.med",MED_ACC_RDONLY)

maa, sdim, mdim, meshtype, desc, dtunit, sort, nstep,  repere, axisname, axisunit = MEDmeshInfo(fid, 1)

print("\nMaillage de nom : |%s| , de dimension : %d , et de type %s\n"%(maa,mdim,meshtype))
print("\t -Dimension de l'espace : %d\n"%(sdim))
print("\t -Description du maillage : |%s|\n"%(desc))
print("\t -Noms des axes : |%s|\n"%(axisname))
print("\t -Unités des axes : |%s|\n"%(axisunit))
print("\t -Type de repère : %s\n"%(repere))
print("\t -Nombre d'étapes de calcul : %d\n"%(nstep))
print("\t -Unité des dates : |%s|\n"%(dtunit))

# /* Combien de noeuds a lire ? */
nse2, chgt, trsf = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
                                  MED_DESCENDING_EDGE, MED_SEG2,MED_CONNECTIVITY, MED_DESCENDING)

ntr3, chgt, trsf = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
                                  MED_CELL,MED_TRIA3,MED_CONNECTIVITY, MED_DESCENDING)

print("Nombre de MED_SEG2 : %d - nombre de MED_TRIA3 : %d"%(nse2,ntr3))

# /* Allocations memoires */
tse2    = 2;
se2_1   = MEDINT(tse2*nse2)
se2_2   = MEDINT(tse2*nse2)
numse2  = MEDINT(nse2)
nufase2 = MEDINT(nse2)
nomse2  = MEDCHAR(MED_SNAME_SIZE*nse2+1)

ttr3    = 3;
tr3     = MEDINT(ntr3*ttr3)
numtr3  = MEDINT(ntr3)
nufatr3 = MEDINT(ntr3)
nomtr3  = MEDCHAR(MED_SNAME_SIZE*ntr3+1)

#   MEDfilterAllocate & MEDfilterDeAllocate ne sont pas utiles :
#   [med_filter() for x in xrange(5)]

filter=med_filter()
MEDfilterEntityCr( fid, nse2, 1, sdim, 2,
                   MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
                   MED_NO_PROFILE, fltsize,
                   flt, filter)
#/* Lecture des connectivites des segments avec flt */

MEDmeshElementConnectivityAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT,
                                     MED_DESCENDING_EDGE, MED_SEG2, MED_DESCENDING, filter, se2_1)

MEDfilterClose(filter)

#/* Lecture de la connectivite des segments */
MEDmeshElementConnectivityRd(fid, maa, MED_NO_DT, MED_NO_IT,
                             MED_DESCENDING_EDGE, MED_SEG2, MED_DESCENDING,
                             MED_FULL_INTERLACE, se2_2)

#/* Lecture (optionnelle) des noms des segments */
try: MEDmeshEntityNameRd(fid, maa, MED_NO_DT, MED_NO_IT, MED_DESCENDING_EDGE, MED_SEG2,nomse2)
except RuntimeError as ex: inoele = MED_FALSE
else:                      inoele = MED_TRUE

#/* Test complémentaire */
nname,chgt,trsf = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT, MED_DESCENDING_EDGE,MED_SEG2,MED_NAME, MED_NO_CMODE)
print("Nombre de nom de MED_SEG2 : %d \n"%(nname))

#/* Lecture (optionnelle) des numeros des segments */
try: MEDmeshEntityNumberRd(fid, maa, MED_NO_DT, MED_NO_IT, MED_DESCENDING_EDGE, MED_SEG2, numse2)
except RuntimeError as ex: inuele = MED_FALSE
else: inuele = MED_TRUE

#/* Lecture des numeros des familles des segments */
MEDmeshEntityFamilyNumberRd(fid,maa, MED_NO_DT, MED_NO_IT, MED_DESCENDING_EDGE, MED_SEG2,nufase2)
 
#/* Lecture de la connectivite des triangles */
MEDmeshElementConnectivityRd(fid, maa, MED_NO_DT, MED_NO_IT,
                             MED_CELL, MED_TRIA3, MED_DESCENDING, MED_FULL_INTERLACE, tr3)

#/* Lecture (optionnelle) des noms des triangles */
try: MEDmeshEntityNameRd(fid, maa, MED_NO_DT, MED_NO_IT, MED_CELL, MED_TRIA3,nomtr3)
except RuntimeError as ex: inoele3 = MED_FALSE
else:                      inoele3 = MED_TRUE

#/* Lecture (optionnelle) des numeros des triangles */
try: MEDmeshEntityNumberRd(fid, maa, MED_NO_DT, MED_NO_IT, MED_CELL, MED_TRIA3,numtr3)
except RuntimeError as ex: inuele = MED_FALSE
else: inuele = MED_TRUE

#/* Lecture des numeros des familles des triangles */
MEDmeshEntityFamilyNumberRd(fid,maa, MED_NO_DT, MED_NO_IT, MED_CELL, MED_TRIA3,nufatr3)

print("Connectivite des segments (1): ",se2_1)
print("Connectivite des segments (1): ",se2_2)
print("Nom des coordonnees : ")
print("Noms des segments : ",["".join(nomse2[i*MED_SNAME_SIZE:i*MED_SNAME_SIZE+MED_SNAME_SIZE])
                                for i in range(nse2)])
print("Numeros des segments : ",numse2)
print("Numeros des familles des segments : ",nufase2)
print("Connectivite des triangles: ",tr3)
print("Noms des triangles : ",["".join(nomtr3[i*MED_SNAME_SIZE:i*MED_SNAME_SIZE+MED_SNAME_SIZE])
                                for i in range(ntr3)])
print("Numeros des triangles : ",numtr3)
print("Numeros des familles des triangles : ",nufatr3)

# /* Fermeture du fichier */
MEDfileClose(fid)
