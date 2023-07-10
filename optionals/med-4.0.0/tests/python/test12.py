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
#  * - Nom du fichier : test12.c
#  *
#  * - Description : ecriture d'une equivalence dans un maillage MED 
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *
from med.medequivalence import *

maa="maa1"
equ="equivalence"
des="equivalence sur les mailles MED_TRIA3"
mdim=3
nomcoo = "x".rjust(MED_SNAME_SIZE)+"y".rjust(MED_SNAME_SIZE)+"z".rjust(MED_SNAME_SIZE)
unicoo = "cm".rjust(MED_SNAME_SIZE)*3
ncor=3
cor = MEDINT([1,2,3,4,5,6])

fid = MEDfileOpen("test12.med",MED_ACC_CREAT)

MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,
           "un maillage pour test12","s",
           MED_SORT_DTIT, MED_CARTESIAN, nomcoo, unicoo)

MEDequivalenceCr(fid,maa,equ,des)

MEDequivalenceCorrespondenceWr(fid,maa,equ,MED_NO_DT,MED_NO_IT,
                               MED_CELL,MED_TRIA3,ncor,cor)
  
MEDfileClose(fid)
