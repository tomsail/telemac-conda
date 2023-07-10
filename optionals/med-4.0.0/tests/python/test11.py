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
#from __future__ import division 

# /******************************************************************************
#  * - Nom du fichier : test11.py
#  *
#  * - Description : lecture de champs de resultats MED
#  *
#  *****************************************************************************/
import os
import sys

from med.medfile import *
from med.medmesh import *
from med.medfield import *
from med.medenum import *
from med.medprofile import *
from med.medlocalization import *
from med.medlink import *


exec(compile(open(os.path.join(os.path.dirname(__file__)+'/tests_params.py')).read(), os.path.join(os.path.dirname(__file__)+'/tests_params.py'), 'exec'),locals(),globals())
# filename='test10'+PARAM_ID+'.med'
if (len(sys.argv) > 1): filename=sys.argv[1]
# else: filename=""
# if ( filename == ""): filename='test10.med'


# USER_MODE=MED_COMPACT_STMODE
# USER_INTERLACE=MED_FULL_INTERLACE
MODE_ACCES=MED_ACC_RDONLY
IFORMAT="%d"

def getFieldsOn(fid,nomcha,typcha, ncomp, entite, stockage,ncstp):

    dt_unit="unknown"
    for (type_geo,type_geoname) in MED_GET_GEOTYPE[entite]:
        
        #/* Combien de (PDT,NOR) a lire */
        nbpdtnor = ncstp;
        if (nbpdtnor < 1 ): continue
        
        for j in range(nbpdtnor):
            try:
                numdt, numo, dt, nmesh, meshname,localmesh, meshnumdt, meshnumit = MEDfield23ComputingStepMeshInfo(fid,nomcha,j+1)
            except: continue
            
            for i in range(nmesh):
                try: # verifier meshname == prec meshname
                    _nprofile, meshname, pflname, locname = MEDfield23nProfile(fid,nomcha,numdt,numo,entite,type_geo, i+1 )
                except: continue

                for l in range(_nprofile):
                    nval,pflname, pflsize, locname, ngauss = MEDfield23nValueWithProfile(fid, nomcha, numdt, numo,  entite, type_geo, meshname, l+1,  USER_MODE )

                    print("\n  +Pas de Temps n.%d (%f) [%s], n. d'ordre %d, avec %d valeur(s) par entité.\n"%(numdt,dt,dt_unit,numo,ngauss))
                    print("\t- Il y a %d entités qui portent des valeurs en mode %i. Chaque entite %s\
 de type geometrique %s associes au profile |%s| a %d valeurs associées \n"%(nval,USER_MODE,MED_ENTITY_TYPE(entite),type_geoname,pflname,ngauss))

                    #TODO : les API renvoient des objets enum et non des int
                    #TODO: c'est une source d'erreur pour la comparaison .val
                    #ex: MED_FIELD_TYPE
                    if   (typcha.val == MED_FLOAT64): val=MEDFLOAT64(ncomp*nval*ngauss)
                    elif (typcha.val == MED_FLOAT32): val=MEDFLOAT32(ncomp*nval*ngauss)
                    elif (typcha.val == MED_INT32  ): val=MEDINT32(ncomp*nval*ngauss)
                    elif (typcha.val == MED_INT64  ): val=MEDINT64(ncomp*nval*ngauss)
                    else:                             val=MEDINT  (ncomp*nval*ngauss)
                    MEDfieldValueWithProfileRd(fid, nomcha, numdt,numo, entite,type_geo,
					 USER_MODE, pflname, stockage, MED_ALL_CONSTITUENT, val)
          
                    if len(locname):
                        print("\t- Modèle de localisation des points de Gauss de nom |%s|\n"%(locname))
                    print("%s\n"%(val))

                    #/*Lecture du profil associe */
                    if ( pflname == MED_NO_PROFILE ):
                        print("\t- Profil : MED_NO_PROFILE")
                    else:
                        pflsize = MEDprofileSizeByName(fid,pflname)
                        print("\t- Profil : |%s| de taille %d"%(pflname,pflsize))

                        pflval = MEDINT(pflsize)
                        MEDprofileRd(fid,pflname,pflval)
                        print("\t");print(pflval);print("\n")

# /* ouverture du fichier */
fid=MEDfileOpen(filename,MODE_ACCES)

maa, sdim, mdim, meshtype, desc, dtunit, sort, nstep,  repere, axisname, axisunit = MEDmeshInfo(fid, 1)

print("\nMaillage de nom : |%s| , de dimension : %d , et de type %s\n"%(maa,mdim,meshtype))
print("\t -Dimension de l'espace : %d\n"%(sdim))
print("\t -Description du maillage : |%s|\n"%(desc))
print("\t -Noms des axes : |%s|\n"%(axisname))
print("\t -Unités des axes : |%s|\n"%(axisunit))
print("\t -Type de repère : %s\n"%(repere))
print("\t -Nombre d'étapes de calcul : %d\n"%(nstep))
print("\t -Unité des dates : |%s|\n"%(dtunit))

ncha = MEDnField(fid)
print("Nombre de champs : %d \n"%(ncha))

for i in range(ncha):

    print("\nChamp numero : %d \n"%(i+1))

    # /* Lecture du nombre de composantes */
    ncomp = MEDfieldnComponent(fid,i+1)

    # /* Lecture du type du champ, des noms des composantes et du nom de l'unite*/

    nomcha,_meshname,_local,typcha,comp,unit,_dtunit,_ncstp = MEDfieldInfo(fid,i+1)
    
    print("Nom du champ : |%s| de type %s\n"%(nomcha,typcha))
    print("Nombre de composantes : |%d|\n"%(ncomp))
    print("Nom des composantes : |%s|\n"%(comp))
    print("Unites des composantes : |%s| \n"%(unit))
    print("Unites des dates  : |%s| \n"%(_dtunit))
    print("Le maillage associé est |%s|\n"%(_meshname))
    print("Nombre de séquences de calcul |%d|\n"%(_ncstp))

    # /* Le maillage reference est-il porte par un autre fichier */
    if not(_local):
        # inutile en python : lnsize=MEDlinkInfoByName(fid,_meshname)
        lien = MEDlinkRd(fid,_meshname)
        print("\tLe maillage |%s| est porte par un fichier distant |%s|\n"%(_meshname,lien))
    

    for (entitype,entitypename) in MED_GET_ENTITY_TYPE:      
        getFieldsOn(fid, nomcha, typcha, ncomp, entitype, USER_INTERLACE,_ncstp )  

# getFieldsOn(fid, nomcha, typcha, ncomp, MED_NODE, USER_INTERLACE,_ncstp )  
# getFieldsOn(fid, nomcha, typcha, ncomp, MED_CELL, USER_INTERLACE,_ncstp )
# getFieldsOn(fid, nomcha, typcha, ncomp, MED_DESCENDING_FACE,USER_INTERLACE,_ncstp)
# getFieldsOn(fid, nomcha, typcha, ncomp, MED_DESCENDING_EDGE,USER_INTERLACE,_ncstp)
# getFieldsOn(fid, nomcha, typcha, ncomp, MED_NODE_ELEMENT,USER_INTERLACE,_ncstp)
# getFieldsOn(fid, nomcha, typcha, ncomp, MED_STRUCT_ELEMENT,USER_INTERLACE,_ncstp)


# /* Interrogation des profils */
npro = MEDnProfile(fid)

print("\nNombre de profils stockes : %d\n"%(npro))
for i in range(1,npro+1):
    pflname, nval = MEDprofileInfo(fid, i)
    print("\t- Profil n°%i de nom |%s| et de taille %d"%(i,pflname,nval))
    pflval = MEDINT(nval)
    MEDprofileRd(fid, pflname, pflval)
    print("\t%s"%(pflval))
    
# /* Interrogation des liens */
nln = MEDnLink(fid)
print("\nNombre de liens stockes : %d\n\n"%(nln))
for i in range(1,nln+1):
    nomlien, nval = MEDlinkInfo(fid, i)
    print("\t- Lien n°%i de nom |%s| et de taille %d\n"%(i,nomlien,nval))

    lien = MEDlinkRd(fid, nomlien )
    print("\t\t|%s|\n\n"%(lien))
 
# /* Interrogation des localisations des points de GAUSS */
nloc = MEDnLocalization(fid)

print("\nNombre de localisations stockees : %d\n"%(nloc))
for i in range(1,nloc):
    locname, type_geo, locsdim, ngauss, geointerpname, ipointstructmeshname, nsectionmeshcell, sectiongeotype = MEDlocalizationInfo(fid, i)
    print("\t- Loc. n°%i de nom |%s| de dimension |%d| avec %d pts de GAUSS \n"%(i,locname,locsdim,ngauss))
    t1 = (type_geo%100)*(type_geo//100)
    t2 = ngauss*(type_geo//100)
    t3 = ngauss
    geodim,nnode = MEDmeshGeotypeParameter(fid,type_geo)
    if ( t1 != geodim*nnode): print("Erreur de cohérence entre T,geodim et nnode")
    refcoo = MEDFLOAT(t1)
    gscoo  = MEDFLOAT(t2)
    wg     = MEDFLOAT(t3)

    MEDlocalizationRd(fid, locname, USER_INTERLACE, refcoo, gscoo, wg  )
    print("\t  Coordonnees de l'element de reference de type %s :\n\t\t"%(MEDmeshGeotypeName(fid,type_geo)))
    print(refcoo); print("\n")
    print("\t  Localisation des points de GAUSS : \n\t\t")
    print(gscoo);print("\n")
    print("\t  Poids associes aux points de GAUSS :\n\t\t")
    print(wg);print("\n\n")

MEDfileClose(fid)
