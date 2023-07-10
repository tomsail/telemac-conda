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

_a=0.446948490915965
_b=0.091576213509771
_p1=0.11169079483905
_p2=0.0549758718227661

# /******************************************************************************
#  * - Nom du fichier : test10.py
#  *
#  * - Description : ecriture de champs de resultats MED 
#  *
#  *****************************************************************************/
import os

from med.medfile import *
from med.medmesh import *
from med.medfield import *
from med.medenum import *
from med.medprofile import *
from med.medlocalization import *
from med.medlink import *

exec(compile(open(os.path.join(os.path.dirname(__file__),'test10_params_f32_i64.py')).read(), os.path.join(os.path.dirname(__file__),'test10_params_f32_i64.py'), 'exec'),locals(),globals())
#cf test10.atpy qui lance les différents test10_TEST_PARAMS_PY.py et sauvegarde le fichier généré
filename='test10'+PARAM_ID+'.med'

# USER_MODE=MED_COMPACT_STMODE
# USER_INTERLACE=MED_FULL_INTERLACE
# MEDARRAY_TYPEF=MEDFLOAT
# FIELD_TYPEF=MED_FLOAT64

# /* Maillage support aux champs*/
# /* Ces maillages sont vides*/
maa1="maa1"
maa2="maa2"
lien_maa2 = "./testfoo.med"
maa3= "maa3"


# /* Caractéristiques du champ n°1 sur TRIA6 */
nomcha1 = "champ reel"
comp1   = "comp1           comp2           "

      # /*12345678901234561234567890123456*/
unit1  = "unit1           unit2           "
ncomp1 = 2
# /* Caractéristiques du model n° 1 de localisation des points de gauss pour le champ n°1*/
ngauss1_1 = 6;
gauss1_1  = "Model n1";
refcoo1=MEDFLOAT([ -1.0,1.0, -1.0,-1.0, 1.0,-1.0, -1.0,0.0, 0.0,-1.0, 0.0,0.0 ])

# /* Constantes */

gscoo1_1= MEDFLOAT([ 2*_b-1, 1-4*_b, 2*_b-1, 2*_b-1, 1-4*_b,
                     2*_b-1, 1-4*_a, 2*_a-1, 2*_a-1, 1-4*_a, 2*_a-1, 2*_a-1 ])
wg1_1   = MEDFLOAT([ 4*_p2, 4*_p2, 4*_p2, 4*_p1, 4*_p1, 4*_p1 ])

nval1_1  = 1*6; #/*1 valeurs et 6 points de gauss par valeur */
_nent1_1 = 1;   #/*1 valeurs et 6 points de gauss par valeur */
valr1_1  = MEDARRAY_TYPEF([0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0, 20.0,21.0, 22.0,23.0]) #/* 2 composantes*/
# /* Caractéristiques du model n° 2 de localisation des points de gauss pour le champ n°1*/
ngauss1_2= 3
gauss1_2 = "Model n2"
gscoo1_2 = MEDFLOAT([ -2.0/3,1.0/3, -2.0/3,-2.0/3, 1.0/3,-2.0/3 ])
wg1_2    = MEDFLOAT([ 2.0/3, 2.0/3, 2.0/3 ])
nval1_2  = 2*3  #/*2 valeurs et 3 points de gauss par valeur */
_nent1_2 = 2    #/*2 valeurs et 3 points de gauss par valeur */
valr1_2  = MEDARRAY_TYPEF([ 0.0,1.0, 2.0,3.0, 10.0,11.0,   12.0,13.0, 20.0,21.0, 22.0,23.0 ])  #/* 2 composantes*/
valr1_2p = MEDARRAY_TYPEF([                                12.0,13.0, 20.0,21.0, 22.0,23.0 ])  #/* 2 composantes*/
# /* Caractéristiques du model n° 3 sans points de gauss pour le champ n°1*/
nval1_3  = 6 #/*6 valeurs et pas de points de gauss */
_nent1_3 = 6 #/*6 valeurs et pas de points de gauss */
valr1_3  = MEDARRAY_TYPEF([ 0.0,1.0, 2.0,3.0, 10.0,11.0, 12.0,13.0, 20.0,21.0, 22.0,23.0]) #/* 2 composantes*/
valr1_3p = MEDARRAY_TYPEF([         2.0,3.0, 10.0,11.0                                  ]) #/* 2 composantes profil1 */

# /* Caractéristiques du champ n° 2 */
nomcha2 = "champ entier"
comp2   = "comp1           comp2           comp3           "
       # /*123456789012345612345678901234561234567890123456*/
unit2   = "unit1           unit2           unit3           ";
ncomp2  = 3
nval2   = 5  #/*5 valeurs */
valr2  = MEDARRAY_TYPEI([ 0,1,2, 10,11,12, 20,21,22, 30,31,32, 40,41,42]) #              /* 3 composantes*/
valr2p = MEDARRAY_TYPEI([ 0,1,2,           20,21,22,           40,41,42]) #              /* 3 composantes*/

# /* Profils utilisés */
nomprofil1  = "PROFIL(champ(1))"
nomprofil1b = "PROFIL(champ(1b))"
nomprofil2  = "PROFIL(champ2)"
profil1 = MEDINT([ 2, 3 ])
profil2 = MEDINT([ 1, 3, 5 ])

# /* Caractéristiques du champ n° 3 */
nomcha3 = "champ entier 3"
comp3   = "comp1           comp2           "
     # /*123456789012345612345678901234561234567890123456*/
unit3  = "unit1           unit2           ";
dtunit = "s"
ncomp3 = 2
nval3  = 5*4    #/*5 valeurs et 4 noeuds par element*/
_nent3 = 5      #/*5 valeurs et 4 noeuds par element*/
valr3 = MEDARRAY_TYPEI([   0,1, 10,11, 20,21, 30,31,
                  40,41, 50,51, 60,61, 70,71,
                  80,81, 90,91, 100,101, 110,111,
                  120,121, 130,131, 140,141, 150,151,
                  160,161, 170,171, 180,181, 190,191])    #/* 2 composantes*/
valr3p = MEDARRAY_TYPEI([  0,1, 10,11, 20,21, 30,31,
                   80,81, 90,91, 100,101, 110,111,
                   160,161, 170,171, 180,181, 190,191])   #/* 2 composantes*/


nomcoo = "x               y               z               "
unicoo = "cm              cm              cm              "


# /* ouverture du fichier */
fid=MEDfileVersionOpen(filename,MED_ACC_CREAT,MED_NUM_MAJEUR,MED_NUM_MINEUR,MED_NUM_RELEASE)

# /* creation de maa1 de dimension 3*/
MEDmeshCr( fid, maa1, 3, 3, MED_UNSTRUCTURED_MESH,
           "Maillage vide","s", MED_SORT_DTIT,
           MED_CARTESIAN, nomcoo, unicoo)

# /* creation de maa3 de dimension 3*/
MEDmeshCr( fid, maa3, 3, 3, MED_UNSTRUCTURED_MESH,
           "Maillage vide","s", MED_SORT_DTIT,
           MED_CARTESIAN, nomcoo, unicoo)

# /* creation du champ réel n°1 */
MEDfieldCr(fid,nomcha1,FIELD_TYPEF,ncomp1,comp1,unit1,dtunit,maa1)

# /* creation du champ entier n°2 */
MEDfieldCr(fid,nomcha2,FIELD_TYPEI,ncomp2,comp2,unit2,dtunit,maa2)

# /* creation du lien au fichier distant contenant maa2 */
MEDlinkWr(fid,maa2,lien_maa2)


# /* creation de la localisation des points de Gauss modèle n°1 */
MEDlocalizationWr(fid, gauss1_1, MED_TRIA6, MED_TRIA6//100, refcoo1,
                  USER_INTERLACE, ngauss1_1, gscoo1_1, wg1_1,
                  MED_NO_INTERPOLATION, MED_NO_MESH_SUPPORT )
      
# /* creation de la localisation des points de Gauss modèle n°2 */
MEDlocalizationWr(fid, gauss1_2, MED_TRIA6, MED_TRIA6//100, refcoo1,
                  USER_INTERLACE,ngauss1_2, gscoo1_2, wg1_2,
                  MED_NO_INTERPOLATION, MED_NO_MESH_SUPPORT)


# /* ecriture du champ n°1*/
# /* enregistre uniquement les composantes n°2 de valr1_1, et n'utilise ni pas de temps ni n° d'ordre*/

MEDfieldValueWithProfileWr(fid, nomcha1,MED_NO_DT,MED_NO_IT,0.0,MED_CELL,MED_TRIA6,
                           USER_MODE,MED_ALLENTITIES_PROFILE, gauss1_1,
                           USER_INTERLACE, 2, _nent1_1, valr1_1 )


# /* enregistre uniquement les composantes n°1 de valr1_1, et n'utilise ni pas de temps ni n° d'ordre */

MEDfieldValueWithProfileWr(fid, nomcha1,MED_NO_DT,MED_NO_IT,0.0,MED_CELL,MED_TRIA6,
                           USER_MODE,MED_ALLENTITIES_PROFILE,
                           gauss1_1,USER_INTERLACE, 1, _nent1_1, valr1_1 )

# /* enregistre uniquement les composantes n°1 de valr1_2, au pas de temps n°1(5.5), n'utilise pas de n°d'ordre*/
# /* ce champ repose sur le maillage maa2 qui est distant */

MEDfieldValueWithProfileWr(fid, nomcha1,1,MED_NO_IT,5.5,MED_CELL,MED_TRIA6,
                           USER_MODE,MED_ALLENTITIES_PROFILE,
                           gauss1_2,USER_INTERLACE, 1, _nent1_2, valr1_2 )

# /*Ce test utilise un deuxième maillag pour un même champ, ceci n'existe plus en 3.0*/
# /* enregistre uniquement les composantes n°2 de valr1_2, au pas de temps n°1(5.5), n'utilise pas de n°d'ordre*/
# /* ce champ repose sur le maillage maa1 qui est local */

MEDfieldValueWithProfileWr(fid, nomcha1,1,1,5.5,MED_CELL,MED_TRIA6,USER_MODE,MED_ALLENTITIES_PROFILE,
		       gauss1_1,USER_INTERLACE, 2, _nent1_1, valr1_1 )

# /* enregistre uniquement les composantes n°1 de valr1_1, au pas de temps n°1(5.5), et n°d'itération n°2*/
# /* ce champ repose sur le maillage maa3 qui est local */

MEDfieldValueWithProfileWr(fid, nomcha1,1,2,5.5,MED_CELL,MED_TRIA6,USER_MODE,MED_ALLENTITIES_PROFILE,
		       gauss1_2,USER_INTERLACE, 1, _nent1_2, valr1_2 )
 
# /* Creation d'un profil (selection  du deuxieme élément de valr1_1) */
# /* On n'utilise que la première valeur (2) du profil */
MEDprofileWr(fid,nomprofil1,1,profil1)
MEDprofileWr(fid,nomprofil1b,1,profil1)

# /* enregistre toutes les composantes du deuxième élèment de valr1_1 (premier élément en stockage compact de valr1p),
#  au pas de temps n°2(5.6), et n°d'itération n°2 */

MEDfieldValueWithProfileWr(fid, nomcha1,2,2,5.6,MED_CELL,MED_TRIA6,USER_MODE,nomprofil1,
                           MED_NO_LOCALIZATION,USER_INTERLACE, MED_ALL_CONSTITUENT, nval1_3, valr1_3p )

# /* enregistre toutes les composantes du deuxième élément de valr1_1 (premier élément en stockage compact de valr1p),
#      au pas de temps n°2(5.6), et n°d'itération n°2 */

MEDfieldValueWithProfileWr(fid, nomcha1,2,2,5.6,MED_CELL,MED_TRIA6,USER_MODE,nomprofil1b,
		       gauss1_2,USER_INTERLACE, MED_ALL_CONSTITUENT, _nent1_2, valr1_2p )


MEDfieldValueWithProfileWr(fid, nomcha1,3,2,5.7,MED_CELL,MED_TRIA6,USER_MODE,nomprofil1,
                           MED_NO_LOCALIZATION,USER_INTERLACE, 2, _nent1_3,valr1_3p )

# /* Ecriture du champ n° 2 */
    

MEDfieldValueWr(fid, nomcha2,MED_NO_DT,MED_NO_IT,0,
                MED_DESCENDING_EDGE,MED_SEG2,
                USER_INTERLACE, 1, nval2, valr2 )

MEDfieldValueWr(fid, nomcha2,MED_NO_DT,MED_NO_IT,0,MED_NODE,MED_NONE,
				  USER_INTERLACE, 2, nval2, valr2 )

MEDfieldValueWr(fid, nomcha2,MED_NO_DT,MED_NO_IT,0,MED_DESCENDING_FACE,MED_TRIA6,
                USER_INTERLACE, 3, nval2, valr2 )

# /* Creation d'un profil (selection  des éléments 1,3,5 de valr2) */
# /* On utilise les trois valeurs du profil */
MEDprofileWr(fid,nomprofil2,3,profil2)

MEDfieldValueWithProfileWr(fid, nomcha2,MED_NO_DT,MED_NO_IT,0,MED_CELL,MED_TRIA6,USER_MODE,nomprofil2,
                           MED_NO_LOCALIZATION,USER_INTERLACE, 3, nval2, valr2p )

# /* creation du champ entier n°3 */
MEDfieldCr(fid,nomcha3,FIELD_TYPEI,ncomp3,comp3,unit3,dtunit,maa1)

 # /* Ecriture du champ n° 3 */

MEDfieldValueWr(fid, nomcha3,MED_NO_DT,MED_NO_IT,0,MED_CELL,MED_QUAD4,
                USER_INTERLACE, 1, nval3, valr3 )

MEDfieldValueWr(fid, nomcha3,MED_NO_DT,MED_NO_IT,0,MED_NODE_ELEMENT,MED_QUAD4,
                USER_INTERLACE, MED_ALL_CONSTITUENT, _nent3, valr3 )

MEDfieldValueWithProfileWr(fid, nomcha3,MED_NO_DT,MED_NO_IT,0,MED_NODE_ELEMENT,MED_QUAD4,USER_MODE,nomprofil2,
                           MED_NO_LOCALIZATION,USER_INTERLACE, MED_ALL_CONSTITUENT, _nent3, valr3p )


MEDfileClose(fid)
