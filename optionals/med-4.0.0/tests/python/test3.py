#! /usr/bin/env python
# -*- coding:utf-8 -*-
# import sys, dl
# sys.setdlopenflags(sys.getdlopenflags() | dl.RTLD_GLOBAL)


from med.medfile import *
from med.medmesh import *

fid=MEDfileOpen("test2.med",MED_ACC_RDONLY)

def myMeshExist(meshname):
    if MEDfileObjectExist(fid,MED_MESH, meshname): print("Le maillage "+meshname+" existe.")
    else: print("Le maillage "+meshname+" n'existe pas.")

myMeshExist("maa1");myMeshExist("maa2");myMeshExist("maa3");myMeshExist("maa3");

nmaa = MEDnMesh(fid)
print("- Nombre de maillage dans test2.med = %d\n"%(nmaa));

for i in range(1,nmaa+1):
    sdim=MEDmeshnAxis(fid, i)
    maa, sdim, mdim, meshtype, desc, dtunit, sort, nstep,  axistype, axisname, axisunit = MEDmeshInfo(fid, 1)
    
    try:
        nomu = MEDmeshUniversalNameRd(fid,maa)
        print("maillage %d de nom %s, de dimension %d et de nom univ. %s\n"%(i,maa,mdim,nomu))
    except:
        print("maillage %d de nom %s, de dimension %d \n"%(i,maa,mdim))

    print("La dimension de l'espace est %d \n"%(sdim));

    if (meshtype == MED_STRUCTURED_MESH):
        print("Il s'agit d'un maillage structure \n")
    else:
      print("Il s'agit d'un maillage non structure \n")
      
    print("Description associee au maillage : %s \n"%(desc))
    print("\t -Noms des axes : %s"%(axisname))
    print("\t -Unités des axes : %s"%(axisunit))
    print("\t -Type de repère : %s"%(axistype))
    print("\t -Nombre d'étape de calcul : %d"%(nstep))
    print("\t -Unité des dates : %s"%(dtunit))

MEDfileClose(fid)

