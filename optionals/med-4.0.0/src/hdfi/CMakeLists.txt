SET(hdfi_SOURCES 
    MEDattrFermer.c
    _MEDattributeExist.c
    _MEDattributeNumRdByName.c
    _MEDattributeNumWrByName.c
    _MEDattributeNumWr.c
    _MEDattributeStringRdByName.c
    _MEDattributeStringWrByName.c
    _MEDattributeStringWr.c
    MEDattrNumLire.c
    MEDattrOuvrir.c
    MEDattrStringLire.c
    _MEDcheckAttributeStringFin.c
    _MEDcheckAttributeStringFunc.c
    _MEDcheckAttributeStringInit.c
    _MEDchecknSublinkFunc.c
    _MEDcopyName.c
    MEDdatagroupCreer.c
    _MEDdatagroupCrOrderCr.c
    _MEDdatagroupExist.c
    MEDdatagroupFermer.c
    MEDdatagroupLienCreer.c
    MEDdatagroupLienSupprimer.c
    _MEDdatagroupOpen.c
    MEDdatagroupOuvrir.c
    _MEDdatasetExistByMedtype.c
    _MEDdatasetExist.c
    MEDdatasetFermer.c
    MEDdatasetOuvrir.c
    _MEDdatasetRd.c
    _MEDdatasetWr.c
    MEDfichierDemonter.c
    MEDfichierFermer.c
    MEDfichierMonter.c
    MEDfichierNo.c
    _MEDfileCreate.c
    _MEDfileName.c
    _MEDfileOpen.c
    _MEDfilterBlockOfEntityFullICompactCr.c
    _MEDfilterBlockOfEntityFullIGlobalCr.c
    _MEDfilterBlockOfEntityNoICompactCr.c
    _MEDfilterBlockOfEntityNoIGlobalCr.c
    _MEDfilterEntityFullICompactCr.c
    _MEDfilterEntityFullIGlobalCr.c
    _MEDfilterEntityNoICompactCr.c
    _MEDfilterEntityNoIGlobalCr.c
    _MEDgetComputationStepName.c
    _MEDgetDatasetChgt.c
    _MEDisasoftlink.c
    _MEDiterate.c
    _MEDlinkObjs.c
    _MEDmemFileOpen.c
    MEDmodeErreurVerrouiller.c
    _MEDnObjects.c
    _MEDobjectCrOrderGetName.c
    __MEDobjectGetName.c
    _MEDobjectGetName.c
    _MEDobjectGetRank.c
    MEDobjetsOuverts.c
    _MEDselectAllEntitiesFullI.c
    _MEDselectAllEntitiesNoI.c
    _MEDsoftlinkDel.c
    _MEDvisit.c
)

IF(MEDFILE_USE_MPI)
  LIST(APPEND hdfi_SOURCES _MEDparFileOpen.c _MEDparFileCreate.c)
ENDIF(MEDFILE_USE_MPI)
  
ADD_LIBRARY(_hdfi OBJECT ${hdfi_SOURCES})
 
ADD_LIBRARY(_hdfi_static OBJECT ${hdfi_SOURCES})
MED_SET_DEFINITIONS(_hdfi_static MED3_USESTATIC)
