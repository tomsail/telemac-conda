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
#  * - Nom du fichier : test30.c
#  *
#  * - Description : lecture des joints d'un maillage MED.
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *
from med.medenum import *
from med.medsubdomain import *
import sys

MODE_ACCES=MED_ACC_CREAT

if (len(sys.argv) > 1) and (len(argv[1])>0): file=argv[1]
else: file="test29.med"

# /* Ouverture du fichier passe en argument en lecture seule */
fid = MEDfileOpen(file,MED_ACC_RDONLY)


def afficheCorres(fid, maa, jnt, typ_ent_local, typ_geo_local, typ_ent_distant, typ_geo_distant, type):

  nc = MEDsubdomainCorrespondenceSize(fid,maa,jnt,MED_NO_DT,MED_NO_IT,
                                      typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant) 
  print("nb de couples d'entites en regard |%s|: %d"%(type,nc))

  if nc > 0:
    cortab = MEDINT(nc*2)
    MEDsubdomainCorrespondenceRd(fid,maa,jnt,MED_NO_DT,MED_NO_IT,
                                 typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant,
                                 cortab)
    for k in range(nc):
        print("Correspondance %d : %d et %d"%(k+1,cortab[2*k],cortab[2*k+1]))



maa, sdim, mdim, meshtype, desc, dtunit, sort, nstep,  repere, axisname, axisunit = MEDmeshInfo(fid, 1)

print("\nMaillage de nom : |%s| , de dimension : %d , et de type %s\n"%(maa,mdim,meshtype))
print("\t -Dimension de l'espace : %d\n"%(sdim))
print("\t -Description du maillage : |%s|\n"%(desc))
print("\t -Noms des axes : |%s|\n"%(axisname))
print("\t -Unités des axes : |%s|\n"%(axisunit))
print("\t -Type de repère : %s\n"%(repere))
print("\t -Nombre d'étapes de calcul : %d\n"%(nstep))
print("\t -Unité des dates : |%s|\n"%(dtunit))


#/* Lecture du nombre de joints */
njnt = MEDnSubdomainJoint(fid,maa)
print("Nombre de joints : %d"%(njnt))

#/* Lecture de tous les joints du maillage */
for i in range(njnt):
    print("Joint numero : %d "%(i+1))

    #/* Lecture des infos sur le joints */
    jnt,des,ndom,maa_dist,njstep,nodtitncor = MEDsubdomainJointInfo(fid,maa,i+1)
    print("Nom du joint: |%s| \n"%(jnt))
    print("Description du joint      : |%s|"%(des))
    print("Domaine en regard         : %d"%(ndom))
    print("Maillage distant          : |%s|"%(maa_dist))
    print("Nombre d'étapes de calcul : %d"%(njstep))
    print("Nombre de correspondance pour (NO_DT,NO_IT) : %d"%(nodtitncor))

    # /* lecture des correspondances une par une
    #    en connaissant leur type a priori */


    # /* Lecture de la correspondance Noeud Noeud */
    afficheCorres(fid,maa,jnt,MED_NODE,0,MED_NODE,0,"noeud/noeud")

    # /* Lecture de la correspondance Noeud Maille */
    afficheCorres(fid,maa,jnt,MED_NODE,0,MED_CELL,MED_TRIA3,"noeud/TRIA3")

    # /* lecture des correspondances une par une
    #   sans connaitre leur type a priori
    #   -> utilisation de la fonction MEDjointTypeCorres */

  
    for ncor in range(1,nodtitncor+1):
        typ_ent_local,typ_geo_local,typ_ent_distant,typ_geo_distant, nentity = MEDsubdomainCorrespondenceSizeInfo(fid,maa,jnt,MED_NO_DT,MED_NO_IT,ncor)

        # /* Lecture de la correspondance Noeud Noeud */
        afficheCorres(fid,maa,jnt,typ_ent_local.val,typ_geo_local,typ_ent_distant.val,typ_geo_distant,
                      str(typ_ent_local)   +"-"+MEDmeshGeotypeName(fid,typ_geo_local)+"/"+
                      str(typ_ent_distant)+"-"+MEDmeshGeotypeName(fid,typ_geo_distant))

    
MEDfileClose(fid)


