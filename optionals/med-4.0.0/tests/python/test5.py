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
# * - Nom du fichier : test5.py
# *
# * - Description : lecture des noeuds d'un maillage MED.
# *
# *****************************************************************************/

from med.medfile import *
from med.medmesh import *
from med.medfilter import *

MODE_ACCES=MED_ACC_CREAT
NULL=MEDINT(0)

flt = MEDINT([ 2, 3 ])

# /* Ouverture du fichier test4.med */
fid = MEDfileOpen("test4.med",MED_ACC_RDONLY)

maa, sdim, mdim, meshtype, desc, dtunit, sort, nstep,  repere, axisname, axisunit = MEDmeshInfo(fid, 1)

print("\nMaillage de nom : |%s| , de dimension : %d , et de type %s\n"%(maa,mdim,meshtype))
print("\t -Dimension de l'espace : %d\n"%(sdim))
print("\t -Description du maillage : |%s|\n"%(desc))
print("\t -Noms des axes : |%s|\n"%(axisname))
print("\t -Unités des axes : |%s|\n"%(axisunit))
print("\t -Type de repère : %s\n"%(repere))
print("\t -Nombre d'étapes de calcul : %d\n"%(nstep))
print("\t -Unité des dates : |%s|\n"%(dtunit))

# /* Lecture des attributs des noeuds du maillage  */
isolatednodes, verticesnodes, cellmaxnodes = MEDmeshAttributeRd( fid, maa)
print("\t -Nombre de noeuds isolés             : %d\n"%(isolatednodes))
print("\t -Nombre de noeuds sommets            : %d\n"%(verticesnodes))
print("\t -Nombre maximum de noeuds par maille : %d\n"%(cellmaxnodes))

# /* Combien de noeuds a lire ? */
nnoe, chgt, trsf = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
                                  MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE)

# /* Allocations memoires */

if nnoe > 0:
    # /* table des coordonnees
    #    flt : (dimension * nombre de noeuds ) */
    coo1 = MEDFLOAT(nnoe*sdim)
    coo2 = MEDFLOAT(nnoe*sdim)
    # /* table des des numeros, des numeros de familles des noeuds
    #    flt : (nombre de noeuds) */
    numnoe = MEDINT(nnoe)
    nufano = MEDINT(nnoe)
    # /* table des noms des noeuds
    #    flt : (nnoe*MED_SNAME_SIZE+1) */
    nomnoe = MEDCHAR(MED_SNAME_SIZE*nnoe+1)

#   MEDfilterAllocate & MEDfilterDeAllocate ne sont pas utiles :
#   [med_filter() for x in xrange(5)]

    filter=med_filter()
    MEDfilterEntityCr( fid, nnoe, 1, sdim, 2,
                       MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
                       MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, filter)

    # /* Lecture des composantes n°2 des coordonnees des noeuds */
    if (nnoe > 0):
        MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT, filter, coo1)
        print("Valeur de coo1 : ",coo1)
        
    MEDfilterClose(filter)

    MEDfilterEntityCr( fid, nnoe, 1, sdim, 1,
                       MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
                       MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, filter)
    
    # /* Lecture des composantes n°1 des coordonnees des noeuds */ 
    MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT, filter, coo1)
    print("Valeur de coo1 : ",coo1)
    MEDfilterClose(filter)

    MEDfilterEntityCr( fid, nnoe, 1, sdim, 1,
                       MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
                       MED_NO_PROFILE, 2, flt, filter)
    # /* Lecture des composantes n°1 des coordonnees des noeuds du filtre */
    if (nnoe > 0):
        MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT, filter, coo2)
        print("Valeur de coo2 : ",coo2)
    MEDfilterClose(filter)

    MEDfilterEntityCr( fid, nnoe, 1, sdim, 2,
                       MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
                       MED_NO_PROFILE, 2, flt, filter)
    # /* Lecture des composantes n°2 des coordonnees des noeuds du filtre */
    if (nnoe > 0):
        MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT, filter, coo2)
        print("Valeur de coo2 : ",coo2)
    MEDfilterClose(filter)

    MEDfilterEntityCr( fid, nnoe, 1, sdim, MED_ALL_CONSTITUENT,
			  MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
			  MED_NO_PROFILE, 2,flt, filter)
    # /* Lecture de toutes les composantes des coordonnees des noeuds du filtre */
    if (nnoe > 0):
        MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT, filter, coo2)
        print("Valeur de coo2 : ",coo2)
    MEDfilterClose(filter)

    # /* Lecture des composantes des coordonnees des noeuds */
    if (nnoe > 0):
        MEDmeshNodeCoordinateRd(fid, maa, MED_NO_DT, MED_NO_IT, MED_FULL_INTERLACE, coo2)
        print("Valeur de coo2 : ",coo2)

        #/* Lecture des noms des noeuds (optionnel dans un maillage MED) */
        try: MEDmeshEntityNameRd(fid,maa, MED_NO_DT, MED_NO_IT, MED_NODE,MED_NONE,nomnoe)
        except RuntimeError as ex: inonoe=MED_FALSE
        else: inonoe=MED_TRUE
        
        #/* Lecture des numeros des noeuds (optionnel dans un maillage MED) */
        try: MEDmeshEntityNumberRd(fid,maa, MED_NO_DT, MED_NO_IT,MED_NODE,MED_NONE,numnoe)
        except RuntimeError as ex: inunoe = MED_FALSE
        else:   inunoe = MED_TRUE

        #/* Lecture des numeros de familles des noeuds */
        MEDmeshEntityFamilyNumberRd(fid,maa, MED_NO_DT, MED_NO_IT, MED_NODE,MED_NONE,nufano)
        print("Type de repere : %s "%(repere))
        print("Nom des coordonnees : ")
        # for i in xrange(sdim): print "|%s| "%(axisname[i*MED_SNAME_SIZE:i*MED_SNAME_SIZE+MED_SNAME_SIZE])
        print([axisname[i*MED_SNAME_SIZE:i*MED_SNAME_SIZE+MED_SNAME_SIZE] for i in range(sdim)])
        print("\nUnites des coordonnees :")
        print([axisunit[i*MED_SNAME_SIZE:i*MED_SNAME_SIZE+MED_SNAME_SIZE] for i in range(sdim)])
        print("\nCoordonnees des noeuds : ",coo2)
        print("\nNoms des noeuds : ",["".join(nomnoe[i*MED_SNAME_SIZE:i*MED_SNAME_SIZE+MED_SNAME_SIZE]) for i in range(nnoe)])
        print("\nNumeros des noeuds : ",numnoe)
        print("\nNumeros des familles des noeuds : ",nufano)

# /* Fermeture du fichier */
MEDfileClose(fid)
