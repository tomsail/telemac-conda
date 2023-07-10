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
#  * - Nom du fichier : test9.py
#  *
#  * - Description : lecture des familles d'un maillage MED 
#  *
#  *****************************************************************************/

# from med.medenum import *

from med.medfile import *
from med.medmesh import *
from med.medfamily import *

fid = MEDfileOpen("test8.med",MED_ACC_RDONLY)

#En python, il n'est pas necessaire de demander la dimension de l'espace
#avant la demande d'information sur le maillage


# /* Lecture des infos concernant le premier maillage */
maa, sdim, mdim, type, desc, dtunit, sort, nstep, rep, nomcoo,unicoo = MEDmeshInfo(fid, 1)

#print "Maillage de nom : |%s| , de dimension : %ld , et de type %d\n"%(maa,mdim,type)
print("Maillage de nom : |%s| , de dimension : %ld , et de type %s\n"%(maa,mdim,type))
print("\t -Dimension de l'espace : %ld\n"%(sdim))
print("\t -Description du maillage : %s\n"%(desc))
print("\t -Noms des axes : |%s|\n"%(nomcoo))
print("\t -Unités des axes : |%s|\n"%(unicoo))
#print "\t -Type de repère : %d\n"%(rep)
print("\t -Type de repère : %s\n"%(rep))
print("\t -Nombre d'étape de calcul : %ld\n"%(nstep))
print("\t -Unité des dates : |%s|\n"%(dtunit))

# /* Lecture du nombre de familles */
nfam = MEDnFamily(fid,maa)
print("Nombre de familles : %d \n"%(nfam))

# /* Lecture de chaque famille */
for i in range(0,nfam):

  # /* Lecture du nombre de groupe */
  ngro = MEDnFamilyGroup(fid,maa,i+1)

  # /* Lecture du nombre d'attribut */
  natt = MEDnFamily23Attribute(fid,maa,i+1) 


  print("Famille %d a %d attributs et %d groupes \n"%(i+1,natt,ngro))

  attide = MEDINT(natt)
  attval = MEDINT(natt)
# TODO :
# TODO : MEDCHAR(n+1) en non MEDCHAR(n), il faut modifier le comportement de la bibliothèque
# vis à vis des tableaux de tableaux de caractères.
  attdes = MEDCHAR(MED_COMMENT_SIZE*natt+1)
  gro    = MEDCHAR(MED_LNAME_SIZE*ngro+1)

  nomfam,numfam,attide,attval,attdes,gro = MEDfamily23Info(fid,maa,i+1,attide,attval,attdes,gro)


  print("Famille de nom %s et de numero %d : \n"%(nomfam,numfam))
  print("Attributs : \n")
  for j in range(0,natt):
      print("ide = %d - val = %d - des = %s\n"%( attide[j], attval[j], attdes[j*MED_COMMENT_SIZE,j*MED_COMMENT_SIZE+MED_COMMENT_SIZE]))

  print("Groupes : \n")
  for j in range(0,ngro):
      print("gro = %s\n"%(gro[j*MED_LNAME_SIZE:j*MED_LNAME_SIZE+MED_LNAME_SIZE]))


MEDfileClose(fid)
