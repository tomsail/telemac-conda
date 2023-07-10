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
#  * - Nom du fichier : test24.py
#  *
#  * - Description : lecture de mailles/faces de type MED_POLYGONE
#  *                 dans le maillage MED du fichier test23.med 
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *

ret=0
fid = MEDfileOpen("test23.med",MED_ACC_RDONLY)

# /* Lecture du nombre de maillages */
nmaa = MEDnMesh(fid)
print("Nombre de maillages = \n",nmaa)

for i in range(nmaa):
    maa, spacedim, mdim, type, desc, dtunit, sort, nstep,  rep, nomcoo, unicoo = MEDmeshInfo(fid, i+1)

    print("\nMaillage de nom : |%s| , de dimension : %d , et de type %s\n"%(maa,mdim,type))
    print("\t -Dimension de l'espace : %d\n"%(spacedim))
    print("\t -Description du maillage : |%s|\n"%(desc))
    print("\t -Noms des axes : |%s|\n"%(nomcoo))
    print("\t -Unités des axes : |%s|\n"%(unicoo))
    print("\t -Type de repère : %s\n"%(rep))
    print("\t -Nombre d'étapes de calcul : %d\n"%(nstep))
    print("\t -Unité des dates : |%s|\n"%(dtunit))

    for (polytype,polyname) in [( MED_POLYGON, MEDmeshGeotypeName(fid,MED_POLYGON) ),
                                ( MED_POLYGON2, MEDmeshGeotypeName(fid,MED_POLYGON2) )]:
        # /* Combien de mailles polygones en mode nodal */
        nind,chgt,trsf = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
                                        MED_CELL,polytype,MED_INDEX_NODE,MED_NODAL)
        npoly = nind-1;
        print("Nombre de mailles ",polyname," en mode nodal : \n",npoly)

        # /* Quelle taille pour le tableau des connectivites, nombre de noeuds
        #     tous polygones confondus*/
        nnoe,chgt,trsf = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
                                        MED_CELL,polytype,MED_CONNECTIVITY,MED_NODAL)

        # /* Allocation memoire :
        #  *  - tableau d'index : npoly + 1
        #  *  - tableau des connectivites : nnoe
        #  *  - tableaux numeros et numeros de familles : npoly
        #  *  - tableau des noms : MED_SNAME_SIZE*npoly + 1
        #  */
        index = MEDINT(nind)
        con   = MEDINT(nnoe)
        num   = MEDINT(npoly)
        fam   = MEDINT(npoly)
        nom =   MEDCHAR(MED_SNAME_SIZE*npoly+1)

        # /* Lecture de la connectivite des mailles polygones */
        MEDmeshPolygon2Rd(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,polytype,MED_NODAL,index,con)
        print("Lecture de la connectivite des mailles ",polyname," en mode nodal \n")

        # /* Lecture (optionnelle) des noms des polygones */
        try:
            nom=MEDmeshEntityNameRd(fid, maa, MED_NO_DT, MED_NO_IT, MED_CELL, polytype, nom)
        except RuntimeError as ex:
            print("Une RuntimeError exception a été attrapée : ",ex)
            ret=ex.args[1]
            
        if (ret <0):
            inoele = MED_FALSE;
        else:
            inoele = MED_TRUE;

        # /* Lecture (optionnelle) des numeros des polygones  */
        try:
            num=MEDmeshEntityNumberRd(fid, maa, MED_NO_DT, MED_NO_IT,MED_CELL, polytype,num)
        except RuntimeError as ex:
            print("Une RuntimeError exception a été attrapée : ",ex)
            ret=ex.args[1]
        if (ret < 0):
            inuele = MED_FALSE;
        else:
            inuele = MED_TRUE;

        # /* Lecture des numeros des familles des segments */
        try:
            fam = MEDmeshEntityFamilyNumberRd(fid,maa, MED_NO_DT, MED_NO_IT,MED_CELL, polytype,fam)
        except RuntimeError as ex:
            print("Une RuntimeError exception a été attrapée : ",ex)
            
        print("Affichage des resultats \n")
        for j in range(npoly):
            print(">> Maille %s %d : \n"%(polyname,j+1))
            ind1 = index[j]-1
            ind2 = index[j+1]-1
            print("---- Connectivite       ----- :",con[ind1:ind2])
                                                  
            if inoele:
                print("---- Nom                ----- : |%s| "%("".join(nom[j*MED_SNAME_SIZE:j*MED_SNAME_SIZE+MED_SNAME_SIZE])))
            if inuele:
                print("---- Numero             ----- : %d "%(num[j]))
            print("---- Numero de famille  ----- : %d \n"%(fam[j]))

# /* Fermeture du fichier */
MEDfileClose(fid)
