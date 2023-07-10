#! /usr/bin/env python
# -*- coding:utf-8 -*-
# import sys, dl
# sys.setdlopenflags(sys.getdlopenflags() | dl.RTLD_GLOBAL)


from med.medfile import *
from med.medmesh import *
from med.medfield import *

# ATTENTION:  MEDCHAR(n+1) et non MEDCHAR(n)
# (sinon il faut modifier le comportement du wrapping de la bibliothèque vis à vis des tableaux de tableaux de caractères)


fid = MEDfileOpen("test_medmesh_1_1.med",MED_ACC_CREAT)

axisname="x               "+"y               "+"z               "
axisunit="cm              "+"cm              "+"cm              "
meshnames=["maa1","maa2"]
spacedim=3
meshdim =3

for meshname in meshnames:
    MEDmeshCr(fid,meshname,spacedim,meshdim,MED_UNSTRUCTURED_MESH,"maillage "+meshname,"s",MED_SORT_DTIT, MED_CARTESIAN,axisname,axisunit)
    MEDmeshUniversalNameWr(fid,meshname)

meshevol=[(MED_NO_DT,MED_NO_IT,MED_NO_DT,MED_NO_IT,0.0),
          (MED_NO_DT,MED_NO_IT,        0,MED_NO_IT,0.1)]
for (numdt1,numit1,numdt2,numit2,dt2) in meshevol:
    MEDmeshComputationStepCr(fid, meshnames[0],numdt1,numit1,numdt2,numit2,dt2)
# for evol in meshevol:
#     err=MEDmeshComputationStepCr(fid, evol)

isolatednodes, verticesnodes, cellmaxnodes = 1,2,3
#print isolatednodes, verticesnodes, cellmaxnodes
MEDmeshAttributeWr(fid,meshnames[0],isolatednodes, verticesnodes, cellmaxnodes)
 
# meshname    = str().zfill(MED_NAME_SIZE   )
# description = str().zfill(MED_COMMENT_SIZE)
# dtunit      = str().zfill(MED_SNAME_SIZE  )
# axisname    = str().zfill(MED_SNAME_SIZE*3)
# axisunit    = str().zfill(MED_SNAME_SIZE*3)

for it in range(1,MEDnMesh(fid)+1):
    naxis1=MEDmeshnAxis(fid,it)
    #print "Nombre d'axes du maillage n",it," : ",naxis1
    meshinfo1=MEDmeshInfo(fid,it)
    meshname,spacedim1,meshdim1,meshtype1,description1,dtunit1,sortingtype1,nstep1,axistype1,axisname1,axisunit1=meshinfo1
    # print meshinfo1
    del(meshinfo1[0]);aff1=list(map(str,meshinfo1)); print(aff1)
    naxis2   =MEDmeshnAxisByName(fid,meshname)
    if naxis1 != naxis2 : print("Erreur: les informations lues par MEDmeshnAxis (%s) et MEDmeshnAxisByname (%s) differes sur le maillage n%d" % (naxis1,naxis2,it));break
    meshinfo2=MEDmeshInfoByName (fid,meshname)
    spacedim2,meshdim2,meshtype2,description2,dtunit2,sortingtype2,nstep2,axistype2,axisname2,axisunit2=meshinfo2
    aff2=list(map(str,meshinfo2));#print aff2
    if aff1 != aff2 : print("Erreur: les informations lues par MEDmeshInfo et MEDmeshInfoByname different sur le maillage n",it);break
    print("\nMaillage n :",it," de nom ",meshname)
    print("\tspacedim:%d \n\tmeshdim:%d \n\tmeshtype:%s \n\tdescription:%s \n\tdtunit:%s \n\tsortingtype:%s \n\tnstep:%d \n\taxistype:%s \n\taxisname:%s \n\taxisunit:%s " % (spacedim2,meshdim2,meshtype2,description2,dtunit2,sortingtype2,nstep2,axistype2,axisname2,axisunit2))

    print(MEDmeshSortingTypeRd(fid,meshname))
    print("\n\tNom universel du maillage : ",MEDmeshUniversalNameRd(fid,meshname))
    attmesh=MEDmeshAttributeRd( fid, meshname );
    isolatednodes, verticesnodes, cellmaxnodes = attmesh
    print("\tisolatednodes: ",isolatednodes," verticesnodes: ",verticesnodes,", cellmaxnodes: ",cellmaxnodes)
    for csit in range(1,nstep1+1):
        numdt,numit,dt1 = MEDmeshComputationStepInfo(fid,meshname,csit)
        dt2 = MEDmeshComputationStepDtRd( fid, meshname,  numdt, numit )
        if dt1 != dt2:print("Erreur: la date est différente entre MEDmeshComputationStepInfo et MEDmeshComputationStepDtRd",dt1,dt2)
        print("\tEtape d'évolution n :",csit," : (numdt=",numdt,",numit=",numit,",dt=",dt1,")")

# coo=doubleArray(6)
# initcoo=[ 0.0, 0.0, 0.0, 1.1, 1.1, 1.1 ]
# for i,el in enumerate(initcoo):
#     coo[i]=el
# MEDmeshNodeCoordinateWr(fid,'maa1',MED_NO_DT,MED_NO_IT,0.0,MED_FULL_INTERLACE,2, coo.cast())

#TODO: Vérifier la taille des allocations mémoire 
ncoo1=2
coo1=MEDFLOAT(ncoo1*spacedim)
initcoo1=[ 0.0, 0.0, 0.0, 1.1, 1.1, 1.1 ]
for i,el in enumerate(initcoo1):
    coo1[i]=el
MEDmeshNodeCoordinateWr(fid,'maa1',MED_NO_DT,MED_NO_IT,0.0,
                         MED_FULL_INTERLACE,ncoo1, coo1 )

ncoo2=3
coo2=MEDFLOAT([ 10.0, 10.1, 10.2, 11.0, 11.1, 11.2, 12.0, 12.1, 12.2 ])
try:
    MEDmeshNodeCoordinateWr(fid,'maa1',0,MED_NO_IT,0.1,
                            MED_FULL_INTERLACE,ncoo2, coo2 )
except RuntimeError as ex:
    print("Une RuntimeError exception a été attrapée : ",ex)
    
coo1_rd=MEDFLOAT(ncoo1*spacedim)
MEDmeshNodeCoordinateRd(fid,'maa1',MED_NO_DT,MED_NO_IT,MED_FULL_INTERLACE,coo1_rd)
print(coo1_rd)

coo2_rd=MEDFLOAT(ncoo2*spacedim)
MEDmeshNodeCoordinateRd(fid,'maa1',0,MED_NO_IT,MED_FULL_INTERLACE,coo2_rd)
print(coo2_rd)

nconn1=2
conn1=MEDINT([1,2,1,2])
MEDmeshElementConnectivityWr(fid,'maa1',MED_NO_DT,MED_NO_IT,0.0,MED_CELL,MED_SEG2,MED_NODAL,MED_FULL_INTERLACE,nconn1,conn1)
conn1_rd=MEDINT(nconn1*(MED_SEG2%100))
MEDmeshElementConnectivityRd(fid,'maa1',MED_NO_DT,MED_NO_IT,MED_CELL,MED_SEG2,MED_NODAL,MED_FULL_INTERLACE,conn1_rd)
if conn1 != conn1_rd : print("Erreur: les informations lues par MEDmeshElementConnectivityRd sur le maillage <%s>(%d,%d) sont inexactes."%('maa1',MED_NO_DT,MED_NO_IT))


nconn2=10000
conn2=MEDINT([i for i in range(nconn2*(MED_SEG2%100))])
# param2=(fid,'maa1',0,MED_NO_IT,0.0,MED_CELL,MED_SEG2,MED_NODAL,MED_FULL_INTERLACE)
# MEDmeshElementConnectivityWr(*param2,nconn2,conn2)
param2=(fid,'maa1',0,MED_NO_IT,0.0,MED_CELL,MED_SEG2,MED_NODAL,MED_FULL_INTERLACE)
MEDmeshElementConnectivityWr( *(param2 + (nconn2, conn2)) )
conn2_rd=MEDINT(nconn2*(MED_SEG2%100))
MEDmeshElementConnectivityRd( fid,'maa1',0,MED_NO_IT,MED_CELL,MED_SEG2,MED_NODAL,MED_FULL_INTERLACE, conn2_rd )
if conn2 != conn2_rd : print("Erreur: les informations lues par MEDmeshElementConnectivityRd sur le maillage <%s>(%d,%d) sont inexactes."%('maa1',0,MED_NO_IT))

nnam1=2
 # "".join( [str(i).zfill(MED_SNAME_SIZE) for i in range(nnam1)] )
nam1=MEDCHAR(  "".join( [str(i).zfill(MED_SNAME_SIZE) for i in range(nnam1)] )+'\0' )
MEDmeshEntityNameWr(fid,'maa1',MED_NO_DT,MED_NO_IT,MED_CELL,MED_SEG2,nnam1,nam1)
nam1_rd=MEDCHAR(nnam1*MED_SNAME_SIZE+1)
print("----------- 1a ---------------")
print(MEDmeshEntityNameRd(fid,'maa1',MED_NO_DT,MED_NO_IT,MED_CELL,MED_SEG2,nam1_rd))
print("----------- 2a ---------------")
#print [x for x in nam1_rd]
if nam1 != nam1_rd : print("Erreur: les informations lues par MEDmeshEntityNameRd sur le maillage < %s >( %d , %d ) sont inexactes."%('maa1',MED_NO_DT,MED_NO_IT))

nnam2=10
# "".join( [str(i).zfill(MED_SNAME_SIZE) for i in range(nnam2)] )
nam2=MEDCHAR(  "".join( [str(i).zfill(MED_SNAME_SIZE) for i in range(nnam2)] )+'\0' )
MEDmeshEntityNameWr(fid,'maa1',0,MED_NO_IT,MED_CELL,MED_SEG2,nnam2,nam2)
nam2_rd=MEDCHAR(nnam2*MED_SNAME_SIZE+1)
MEDmeshEntityNameRd(fid,'maa1',0,MED_NO_IT,MED_CELL,MED_SEG2,nam2_rd)
if nam2 != nam2_rd : print("Erreur: les informations lues par MEDmeshEntityNameRd sur le maillage <%s>(%d,%d) sont inexactes."%('maa1',0,MED_NO_IT))

#TODO : Tester les attributes MESH

# fieldname1="field1".ljust(MED_NAME_SIZE,'_')
# ncomp_field1=1
# comp_field1='comp1'.ljust(MED_SNAME_SIZE,'_')
# unit_field1='unit1'.ljust(MED_SNAME_SIZE,'_')
# dtunit_field1='s'.ljust(MED_SNAME_SIZE,'_')
# meshname_field1='maa1'
# type_field1=MED_FLOAT64

field1={
         'fieldname'    : "field1".ljust(MED_NAME_SIZE,'_'),
         'fieldtype'    : MED_FLOAT64,
         'ncomponent'   : 1,
         'componentname': 'comp1'.ljust(MED_SNAME_SIZE,'_'),
         'componentunit': 'unit1'.ljust(MED_SNAME_SIZE,'_'),
         'dtunit'       : 's'.ljust(MED_SNAME_SIZE,'_'),
         'meshname'     : 'maa1',   
#        'mesh_dt_it'   : (MED_NO_DT,MED_NO_IT)
         # 'create'       : lambda fid:MEDfieldCr(fid,**self)
         }

field_keys=    ("fid", "fieldname","fieldtype","ncomponent","componentname","componentunit","dtunit","meshname")
fieldinfo_keys=('meshname', 'localmesh', 'fieldtype',  'componentname', 'componentunit', 'dtunit', 'ncstp')

#vérif enum sur MED_FIELD_TYPE
#L'ordre des paramètres dans le tuple paramètre est important.
#Si on préfixe les cléfs d'un numéro correspondant à l'ordre d'apparition des paramètres ds MEDfieldCR :
# a=tuple(sorted(field1.values()))
# b=map(lambda x:field1[x],a)
# MEDfieldCr(*b)
  
# MEDfieldCr(fid,
#            fieldname1,
#            type_field1,
#            ncomp_field1,
#            comp_field1,
#            unit_field1,
#            dtunit_field1,
#            meshname_field1)
#Si la taille du dictionnaire dépasse le nombre de paramètre : PB
MEDfieldCr(fid,**field1)

##Générer automatiquement une classe avec les attributs provenant d'un dictionnaire
##Générer automatiquement les méthodes d'accès.
# class FIELD(object):
#     __setattr__ = lambda self, name, value: _swig_setattr(self, FIELD, name, value)
#     __getattr__ = lambda self, name: _swig_getattr(self, FIELD, name)
# field1=FIELD(**field1_info) <=> field1=FIELD("field1",MED_FLOAT64...)
# field1.fieldname  field1.ncomponent 
# field1.create ne peut pas fonctionner
# field1.create() idem
# field1['create'](fid) écriture pas très pratique --> class

#X=type('X',(),{'foo':lambda self:'foo'})
#FIELD=type('FIELD',(),field1)
# a=FIELD()
# print a
# dir(a)
 
# err,meshname_field1_rd,lmesh_field1_rd,type_field1_rd,comp_field1_rd,unit_field1_rd,dtunit_field1_rd,ncstp_field1_rd=MEDfieldInfoByName(fid,fieldname1)
field1_info=MEDfieldInfoByName(fid,field1['fieldname'])
#Les fonctions MED ne renvoient pas de dictionnaire (ou d'objets)
#On crée un dictionnaire à partir d'une liste de clés ordonnées.
field1_info2=dict(list(zip(fieldinfo_keys,field1_info)))
#On supprime les clés qui ne sont pas a comparer (on pourrait écire une fonction qui compare que les élements communs)
del field1_info2['ncstp']
del field1_info2['localmesh']
field1_info2['fieldname'] =field1['fieldname']
field1_info2['fieldtype'] =field1['fieldtype']
field1_info2['ncomponent']=field1['ncomponent']
field1_info2['meshname']=field1['meshname']

#En python 2.7 il existe des dict compréhension
field1_info3=dict([ (x,field1_info2[x]) for x in fieldinfo_keys if x in list(field1.keys())] )
 
  
if field1 != field1_info2 : print("Erreur: les informations lues par MEDfieldInfoByName sur le champ <%s> sont inexactes."% field1['fieldname'])

nentity_field1=10
value_field1=MEDFLOAT(list(range(nentity_field1)))
# value_field1=MEDFLOAT(range(nentity_field1))

MEDfieldValueWr(fid,
                field1['fieldname'],
                MED_NO_DT,MED_NO_IT,0.0,
                MED_CELL,MED_SEG2,
                MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
                nentity_field1,value_field1);

it=value_field1.begin()
while it!=value_field1.end():
    print(it.value());it+=1

value_field1_rd=MEDFLOAT(nentity_field1*field1['ncomponent'])
MEDfieldValueRd(fid,
                field1['fieldname'],
                MED_NO_DT,MED_NO_IT,
                MED_CELL,MED_SEG2,
                MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
                value_field1_rd);

if value_field1 != value_field1_rd : print("Erreur: les informations lues par MEDfieldValueRd sur le champ <%s>(%d,%d) sont inexactes."%('fieldname1',MED_NO_DT,MED_NO_IT))


#Opérations algébrique sur les tableaux stl (cf algo.) vs numpy

fieldname2="field2".ljust(MED_NAME_SIZE,'_')
ncomp_field2=1
comp_field2='comp1'.ljust(MED_SNAME_SIZE,'_')
unit_field2='unit1'.ljust(MED_SNAME_SIZE,'_')
dtunit_field2='s'.ljust(MED_SNAME_SIZE,'_')
meshname_field2='maa1'
type_field2=MED_INT32

#vérif enum sur MED_FIELD_TYPE

MEDfieldCr(fid,
           fieldname2,
           type_field2,
           ncomp_field2,
           comp_field2,
           unit_field2,
           dtunit_field2,
           meshname_field2)

#Ecrire Un champ unsigned char *

meshname_field2_rd,lmesh_field2_rd,type_field2_rd,comp_field2_rd,unit_field2_rd,dtunit_field2_rd,ncstp_field2_rd=MEDfieldInfoByName(fid,fieldname2)

nentity_field2=10
value_field2=MEDINT(list(range(nentity_field2)))
# value_field2=MEDFLOAT(range(nentity_field2))

MEDfieldValueWr(fid,
                fieldname2,
                MED_NO_DT,MED_NO_IT,0.0,
                MED_CELL,MED_SEG2,
                MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
                nentity_field2,value_field2);

value_field2_rd=MEDINT(nentity_field2*ncomp_field2)
MEDfieldValueRd(fid,
                fieldname2,
                MED_NO_DT,MED_NO_IT,
                MED_CELL,MED_SEG2,
                MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,
                value_field2_rd);

if value_field2 != value_field2_rd : print("Erreur: les informations lues par MEDfieldValueRd sur le champ <%s>(%d,%d) sont inexactes."%('fieldname2',MED_NO_DT,MED_NO_IT))

MEDfileClose(fid)
