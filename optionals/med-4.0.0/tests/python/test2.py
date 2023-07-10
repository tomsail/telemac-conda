#! /usr/bin/env python
# -*- coding:utf-8 -*-

from med.medfile import *
from med.medmesh import *

axisname="x               "+"y               "+"z               "
axisunit="cm              "+"cm              "+"cm              "

fileexist, accessok = MEDfileExist( "test1.med", MED_ACC_RDONLY )
if (not fileexist):  print("Le fichier test1.med n'existe pas.")
if (not accessok ):  print("Le fichier test1.med ne peut pas être ouvert selon le mode d'accès demandé .")

# Verification de la conformite du format med du fichier test1.med 
hdfok,medok = MEDfileCompatibility("test1.med");
print("Vérification de la compatibilité du fichier avec HDF : hdfok=",hdfok," ; avec MED : medok=",medok)

fid     = MEDfileOpen("test1.med",MED_ACC_RDONLY)
comment = MEDfileCommentRd(fid)
print("En-tete du fichier ",MEDfileName(fid),":",comment)
MEDfileClose(fid)

fileexist, accessok = MEDfileExist( "test2.med", MED_ACC_CREAT )
if (not fileexist):  print("Le fichier test2.med n'existe pas.")
if (not accessok ):  print("Le fichier test2.med ne peut pas être ouvert selon le mode d'accès demandé .")

fid = MEDfileOpen("test2.med",MED_ACC_CREAT)

MEDmeshCr(fid,"maa1",3,3,MED_UNSTRUCTURED_MESH,"un premier maillage","s",MED_SORT_DTIT,MED_CARTESIAN,axisname,axisunit)
MEDmeshUniversalNameWr(fid,"maa1")


MEDmeshCr(fid,"maa2",3,2,MED_UNSTRUCTURED_MESH,"un second maillage","s",MED_SORT_DTIT,MED_CARTESIAN,axisname,axisunit)

for ndtnit in [ (MED_NO_DT, MED_NO_IT, 0), (1 , 3, 1.1), (0 , 0, 1.1), (0, -1, 1.1), (-1 ,20, 1.1) ]:
    MEDmeshComputationStepCr(fid,"maa2",MED_NO_DT,MED_NO_IT,*(ndtnit)) 

cstp = 0
for ndtnit in  [(MED_NO_DT,MED_NO_IT, 20,-1, 1.1), (0,0,  20,-1, 1.1), (20,-1, 20,-1, 1.1), (1,3, 20,-1, 1.1), (20,-1, 20,10, 1.1), (20,5, 20,5, 1.1), (20,-1, 20,20, 1.1) ]:
    try: cstp=cstp+1;MEDmeshComputationStepCr(fid,"maa2",*(ndtnit));
    except  RuntimeError as ex: print("Exception attendue (cstp: "+str(cstp)+") ",ex)


MEDmeshCr(fid,"maa3",3,1,MED_UNSTRUCTURED_MESH,	"un troisieme maillage","s",MED_SORT_DTIT,MED_CARTESIAN,axisname,axisunit)
      
MEDfileClose(fid)

