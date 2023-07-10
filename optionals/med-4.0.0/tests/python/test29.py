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
#  * - Nom du fichier : test29.py
#  *
#  * - Description : ecriture d'un joint dans un maillage MED 
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *
from med.medenum import *
from med.medsubdomain import *

MODE_ACCES=MED_ACC_CREAT

maa  = "maa1"
jnt  = "joint"
des_jnt = "joint avec le sous-domaine 2"
maa_distant = "maa1"
dom_dist    = 2
mdim        = 3
ncor        = 3
cor         = MEDINT([1,2,3,4,5,6])
cor2        = MEDINT([10,20,30,40,50,60])
nomcoo      = "x               y               z               "
unicoo      = "cm              cm              cm              "


#/* Creation du fichier "test29.med" */
fid = MEDfileOpen("test29.med",MODE_ACCES)

MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,
           "un maillage pour test29","s", MED_SORT_DTIT,
           MED_CARTESIAN, nomcoo, unicoo)

#/* Creation du joint */
MEDsubdomainJointCr(fid,maa,jnt,des_jnt,dom_dist,maa_distant)

#/* Ecriture de la correspondance Noeud, Noeud */
MEDsubdomainCorrespondenceWr(fid,maa,jnt,MED_NO_DT,MED_NO_IT,
                             MED_NODE,MED_NONE,MED_NODE,MED_NONE,ncor,cor)

#/* Ecriture de la correspondance Noeud Maille */
MEDsubdomainCorrespondenceWr(fid,maa,jnt,MED_NO_DT,MED_NO_IT,
                             MED_NODE,MED_NONE,MED_CELL,MED_TRIA3,ncor,cor2)
MEDfileClose(fid)
