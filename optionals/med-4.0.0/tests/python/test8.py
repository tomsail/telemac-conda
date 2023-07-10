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
#  * - Nom du fichier : test8.py
#  *
#  * - Description : écriture des familles d'un maillage MED 
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medmesh import *
from med.medfamily import *

fid = MEDfileOpen("test8.med",MED_ACC_CREAT)

maa="maa1"
mdim=2
nomcoo="x               y               "
unicoo="cm              cm              "

try:
  MEDmeshCr(fid, maa, mdim, mdim, MED_UNSTRUCTURED_MESH,"un maillage pour test8","s", MED_SORT_DTIT, MED_CARTESIAN, nomcoo, unicoo)
except RuntimeError as ex:
    print("Erreur a la creation du maillage : ",maa,ex)

# /* Ecriture des familles                                                */
# /* Conventions appliquees dans MED :
#    - Toujours creer une famille de numero 0 ne comportant aucun attribut
#    ni groupe (famille de reference pour les noeuds ou les elements
#    qui ne sont rattaches a aucun groupe ni attribut)
#    - Les numeros de familles de noeuds sont > 0
#    - Les numeros de familles des elements sont < 0
#    - Rien d'imposer sur les noms de familles.
#    */

  # /* Creation de la  famille 0 */
nomfam="FAMILLE_0"
numfam = 0;
gro=MEDCHAR(0)

try:
    MEDfamilyCr(fid,maa,nomfam,numfam,0,gro)
except RuntimeError as ex:
    print("Erreur a la creation  de la famille 0 : ",maa,ex)

  # /* Creation pour correspondre aux cas test precedent de :
  #    - 3 familles d'elements (-1,-2,-3)
  #    - 2 familles de noeuds (1,2) */
nfame = 3
gro=MEDCHAR(MED_COMMENT_SIZE)
gro[:]="groupe1"
ngro = 1

for i in range(0,nfame):
    numfam = -(i+1)
    nomfam="FAMILLE_ELEMENT_"+str(-numfam)
    print("%s - %d - %d \n"%(nomfam,numfam, ngro))
    try:
        MEDfamilyCr(fid,maa,nomfam,numfam,ngro,gro)
    except RuntimeError as ex:
      print("Erreur a la creation de la famille :",nomfam,"(",numfam,"(")

nfamn = 2
for i in range(0,nfamn):
    numfam = i+1
    nomfam="FAMILLE_NOEUD_"+str(numfam)
    try:
        MEDfamilyCr(fid,maa,nomfam,numfam,ngro,gro)
    except RuntimeError as ex:
      print("Erreur a la creation de la famille :",nomfam,"(",numfam,"(")


err = MEDfileClose(fid)
