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
#  * - Nom du fichier : test22.py
#  *
#  * - Description : lecture de valeurs scalaires numeriques crees dans test21.
#  *
#  *****************************************************************************/

from med.medfile import *
from med.medparameter import *

# /* Ouverture du fichier test21.med en lecture seule */
fid = MEDfileOpen("test21.med",MED_ACC_RDONLY)


# /* Lecture du nombre de variable scalaire */
n = MEDnParameter(fid)
print("Nombre de variables scalaires dans test21.med = %d\n"%(n))

# /* Lecture des infos sur les variables (type,description) */
for i in range(1,n+1):

    #TODO : Réflechir à la possibilité de renvoyer directement le typearray
    nom_scalaire, type, description, dt_unit, npdt = MEDparameterInfo(fid, i)
    print("- Scalaire n°%d de nom %s \n"%(i,nom_scalaire))
    print("Type du paramètre : ",type)
    # if (type == MED_FLOAT64): print "  Type flottant. \n"
    # else:                     print "  Type entier. \n"
    print("  Description associee : [%s] \n"%(description))
    print("  Nombre de pas de temps : %d \n"%(npdt))

    for j in range(1,npdt+1):

        numdt, numo, dt = MEDparameterComputationStepInfo(fid,nom_scalaire,j)
        print("   Valeur n°%d : \n"%(j))
        if (numdt == MED_NO_DT): print("   - Aucun de pas de temps \n")
        else:                    print("   - Pas de de temps de numero %d de valeur %f [%s] \n"%(numdt,dt,dt_unit))
        if (numo == MED_NO_IT) : print("   - Aucun numero d'ordre \n")
        else:                    print("   - Numero d'ordre : %d \n"%(numo))

        if (type.val == MED_FLOAT64): val=MEDFLOAT(1)
        else:                         val=MEDINT(1)

        # /* Lecture de la valeur flottante associee au pas de temps */
        MEDparameterValueRd(fid,nom_scalaire,numdt,numo, val)
        print("    - Valeur : %s"%(val))

# /* Fermeture du fichier */
MEDfileClose(fid)
