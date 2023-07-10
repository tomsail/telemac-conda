#! /usr/bin/env python
# -*- coding:utf-8 -*-

from med.medfile import *

fid = MEDfileOpen("test_medfile_1_1.med",MED_ACC_CREAT)
MEDfileCommentWr(fid,"Ceci est mon premier commentaire")
MEDfileClose(fid)

hdfok,medok = MEDfileCompatibility("test_medfile_1_1.med");
print("Vérification de la compatibilité du fichier avec HDF : hdfok=",hdfok," ; avec MED : medok=",medok)

fid = MEDfileOpen("test_medfile_1_1.med",MED_ACC_RDONLY)

mm,m,r =  MEDfileNumVersionRd(fid)
print("Version Du Fichier (num) :",mm,m,r)
version = MEDfileStrVersionRd(fid);
print("Version Du Fichier (str) :",version)

comment = MEDfileCommentRd(fid)
print(comment)

print(MEDfileName(fid));

MEDfileClose(fid)

#TODO: Tester les routines de montage/demontage
