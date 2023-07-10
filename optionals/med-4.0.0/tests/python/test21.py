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
#  * - Nom du fichier : test21.py
#  *
#  * - Description : ecriture de valeurs scalaires numeriques dans un fichier MED
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medparameter import *

nom_scalaire1 = "VariableEntiere"
description1  = "Une premiere description"
nom_scalaire2 = "VariableFlottante"
description2  = "Une seconde description"
vali1 = 56
vali2 = -789
valr1 = 67.98

MODE_ACCES = MED_ACC_CREAT

fid = MEDfileOpen("test21.med",MODE_ACCES)

# /* Creation d'un variable scalaire entiere */
MEDparameterCr(fid,nom_scalaire1,MED_INT,description1,"ms")
print("Creation d'une variable scalaire entiere \n")

# /* Ecriture d'un valeur sans pas de temps et sans numero d'ordre*/
MEDparameterValueWr(fid,nom_scalaire1,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MEDINT([vali1]))
print("Ecriture d'une valeur entiere avec pas de temps \n")

# /* Ecriture d'une valeur entiere avec 1 pas de temps et sans numero d'ordre */
MEDparameterValueWr(fid,nom_scalaire1,1,MED_NO_IT,5.5,MEDINT([vali2]))
print("Ecriture d'une valeur entiere avec pas de temps \n")

# /* Creation d'un variable scalaire flottante */
MEDparameterCr(fid,nom_scalaire2,MED_FLOAT64,description2,"ms")
print("Creation d'une variable scalaire flottante \n")

# /* Ecriture d'une valeur reelle avec 1 pas de temps et 1 numero d'ordre */
MEDparameterValueWr(fid, nom_scalaire2, 1, 2, 5.5, MEDFLOAT([valr1]))
print("Ecriture d'une valeur reelle avec pas de temps et numero d'ordre \n")

# /* Fermeture du fichier */
MEDfileClose(fid)
