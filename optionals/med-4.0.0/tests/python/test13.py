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
#  * - Nom du fichier : test13.py
#  *
#  * - Description : Lecture des equivalences d'un maillage MED 
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *
from med.medequivalence import *
from med.medenum import *

fid = MEDfileOpen("test12.med",MED_ACC_RDONLY)

#En python, il n'est pas necessaire de demander la dimension de l'espace
#avant la demande d'information sur le maillage

# /* Lecture des infos concernant le premier maillage */
maa, sdim, mdim, type, desc, dtunit, sort, nstep, rep, nomcoo, unicoo = MEDmeshInfo(fid, 1)

# print "Maillage de nom : |%s| , de dimension : %ld , et de type %d\n"%(maa,mdim,type._val)
print("Maillage de nom : |%s| , de dimension : %ld , et de type %s\n"%(maa,mdim,type))
print("\t -Dimension de l'espace : %ld\n"%(sdim))
print("\t -Description du maillage : %s\n"%(desc))
print("\t -Noms des axes : |%s|\n"%(nomcoo))
print("\t -Unités des axes : |%s|\n"%(unicoo))
# print "\t -Type de repère : %d\n"%(rep._val)
print("\t -Type de repère : %s\n"%(rep))
print("\t -Nombre d'étape de calcul : %ld\n"%(nstep))
print("\t -Unité des dates : |%s|\n"%(dtunit))

nequ = MEDnEquivalence(fid,maa)
print("Nombre d'equivalences : %d \n"%(nequ))

# /* Lecture de toutes les equivalences du maillage */
if nequ > 0:
    for i in range(0,nequ):
        print("Equivalence numero : %d \n"%(i+1))

        # /* Lecture des infos sur l'equivalence */
        equ,des,nstep,nocstpncor=MEDequivalenceInfo(fid,maa,i+1)
        print("Nom de l'equivalence: |%s| \n"%(equ))
        print("Description de l'equivalence : |%s| \n"%(des))
        print("Nombre d'étapes de calcul : %d \n"%(nstep))
        print("Nombre de correspondances pour l'étape de calcul MED_NO_DT,MED_NO_IT : %d \n"%(nocstpncor))
        
        for (entitype,entitypename) in MED_GET_NODAL_ENTITY_TYPE:
            for (geotype,geotypename) in MED_GET_GEOTYPE[entitype]:
                
                ncor=MEDequivalenceCorrespondenceSize(fid,maa,equ,MED_NO_DT,MED_NO_IT,entitype,geotype)
                
                if ncor > 0:
                    print("\tIl y a %d correspondances sur les (%s,%s) \n"%(ncor,entitypename,geotypename))
                    cor=MEDINT(ncor*2)
                    cor=MEDequivalenceCorrespondenceRd(fid,maa,equ,MED_NO_DT,MED_NO_IT,
                                                           entitype,geotype,cor)
                    for j in range(0,ncor):
                        print("\tCorrespondance %d : %d et %d \n"%(j+1,cor[2*j],cor[2*j+1]))

MEDfileClose(fid)
