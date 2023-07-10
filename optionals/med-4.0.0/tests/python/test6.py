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
#  * - Nom du fichier : test6.py
#  *
#  * - Description : ecriture d'elements dans un maillage MED
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *
from med.medfilter import *

MODE_ACCES=MED_ACC_CREAT
NULL=MEDINT(0)

flt = MEDINT([ 2, 3 ])
fltsize=2

nse2 = 5;
se2  = MEDINT( [1,2,1,3,2,4,3,4,2,3])
# /*              12345678901234561234567890123456123456789012345612345678901234561234567890123456*/
nomse2  = MEDCHAR("se1             se2             se3             se4             se5             ")
numse2  = MEDINT([1,2,3,4,5])
nufase2 = MEDINT([-1,-1,0,-2,-3])
ntr3    = 2
tr3     = MEDINT([1,2,-5,-5,3,-4])
#/*               12345678901234561234567890123456*/
nomtr3  = MEDCHAR("tr1             tr2             ")
numtr3  = MEDINT([4,5])
nufatr3 = MEDINT([0,-1])
maa  = "maa1"
mdim = 2
nomcoo = "x               y               ";
unicoo = "cm              cm              ";


# /* Ouverture du fichier test4.med */
fid = MEDfileOpen("test6.med",MODE_ACCES)

MEDmeshCr( fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,"un maillage pour test6","s",
           MED_SORT_DTIT,MED_CARTESIAN, nomcoo, unicoo)

#/* ecriture des connectivites des segments */
MEDmeshElementConnectivityWr(fid,maa, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
                             MED_DESCENDING_EDGE, MED_SEG2, MED_DESCENDING,
                             MED_FULL_INTERLACE, nse2,se2)

#/* ecriture (optionnelle) des noms des segments */
MEDmeshEntityNameWr(fid,maa,MED_NO_DT,MED_NO_IT,
                    MED_DESCENDING_EDGE,MED_SEG2,nse2,nomse2)

#/* ecriture (optionnelle) des numeros des segments */
MEDmeshEntityNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,
                      MED_DESCENDING_EDGE,MED_SEG2,nse2,numse2)

#/* ecriture des numeros des familles des segments */
MEDmeshEntityFamilyNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,
                            MED_DESCENDING_EDGE,MED_SEG2,nse2,nufase2)

#/* ecriture des connectivites des triangles */
MEDmeshElementConnectivityWr(fid,maa, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
                             MED_CELL, MED_TRIA3, MED_DESCENDING,
                             MED_FULL_INTERLACE, ntr3,tr3)

#/* ecriture (optionnelle) des noms des triangles */
MEDmeshEntityNameWr(fid,maa,MED_NO_DT,MED_NO_IT,
                    MED_CELL, MED_TRIA3,ntr3,nomtr3)

#/* ecriture (optionnelle) des numeros des triangles */
MEDmeshEntityNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,
                      MED_CELL, MED_TRIA3,ntr3,numtr3)

#/* ecriture des numeros des familles des triangles */
MEDmeshEntityFamilyNumberWr(fid,maa,MED_NO_DT,MED_NO_IT,
                            MED_CELL, MED_TRIA3,ntr3,nufatr3)
      
# /* Fermeture du fichier */
MEDfileClose(fid)
