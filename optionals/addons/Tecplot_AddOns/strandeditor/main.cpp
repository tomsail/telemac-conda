#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ADKUTIL.h"
#include <vector>
#include <string>
#include "strandeditor.h"
#include "macro.h"

AddOn_pa  AddOnID;
Boolean_t ZoneListUpdatePending;
GlobalSettings_s GlobalSettings;

void STDCALL UpdateZoneListOnIdle(ArbParam_t ClientData);

const LgIndex_t TECUTIL_NO_NEIGHBORING_ZONE = 0;
const LgIndex_t TECUTIL_NO_NEIGHBORING_ELEM = 0;

inline bool IsLinearZone(ZoneType_e ZoneType,
                         LgIndex_t  IMax,
                         LgIndex_t  JMax,
                         LgIndex_t  KMax)
{
  return (ZoneType == ZoneType_FELineSeg ||
          (ZoneType == ZoneType_Ordered  &&
           (JMax == 1 && KMax == 1 ||   // I-Ordered
            IMax == 1 && KMax == 1 ||   // J-Ordered
            IMax == 1 && JMax == 1)) ); // K-Ordered); 
}

static bool IsLinearZone(EntIndex_t ZoneNumber)
{
  LgIndex_t IMax = 0;
  LgIndex_t JMax = 0;
  LgIndex_t KMax = 0;
  TecUtilZoneGetInfo(ZoneNumber,
                     &IMax, &JMax, &KMax,
                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL); // Field data handles
  ZoneType_e ZoneType = TecUtilZoneGetType(ZoneNumber);

  return IsLinearZone(ZoneType, IMax, JMax, KMax);    
}

static bool Is2DZone(EntIndex_t ZoneNumber)
{
  LgIndex_t IMax = 0;
  LgIndex_t JMax = 0;
  LgIndex_t KMax = 0;
  TecUtilZoneGetInfo(ZoneNumber,
                     &IMax, &JMax, &KMax,
                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL); // Field data handles
  ZoneType_e ZoneType = TecUtilZoneGetType(ZoneNumber);

  return (ZoneType == ZoneType_FEQuad         ||
          ZoneType == ZoneType_FETriangle     ||
          (ZoneType == ZoneType_Ordered &&
           (IMax>1 && JMax>1 && KMax==1 ||    // IJ-Ordered
            IMax>1 && JMax==1 && KMax>1 ||    // IK-Ordered
            IMax==1 && JMax>1 && KMax>1 )) ); // JK-Ordered); 
}

inline bool CanShareConnectivity(FaceNeighborMode_e  FaceNeighborMode)
{
  return(FaceNeighborMode != FaceNeighborMode_GlobalOneToOne &&
         FaceNeighborMode != FaceNeighborMode_GlobalOneToMany);
}


//
//  Copy the connectivity list from the source zone to the destination zone.
//
Boolean_t CopyConnectivityAndShiftedNeighbors(EntIndex_t     SourceZone,
                                              EntIndex_t     DestZone,
                                              const Set_pa   SelectedZones)
{
  REQUIRE(SourceZone > 0);
  REQUIRE(DestZone > SourceZone);

  Boolean_t  IsOk           = TRUE;
  ZoneType_e ZoneType       = TecUtilZoneGetType(SourceZone);
  Boolean_t  SkipNeighbors  = FALSE;
  ArgList_pa ArgList        = TecUtilArgListAlloc();
  LgIndex_t  NodesPerElem   = 0;
  LgIndex_t  FacesPerElem   = 0;
  LgIndex_t  NumElems       = 0;
  LgIndex_t  IMax = 0;
  LgIndex_t  JMax = 0;
  LgIndex_t  KMax = 0;

  TecUtilZoneGetInfo(SourceZone,
                     &IMax, &JMax, &KMax,
                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL); // Field data handles
  switch (ZoneType)
    {
      case ZoneType_Ordered:
        {
          NumElems = IMax*JMax*KMax;
          if (IsLinearZone(SourceZone))
            SkipNeighbors = TRUE;
          else if (Is2DZone(SourceZone))
            FacesPerElem = 4;
          else
            FacesPerElem = 6;
        } break;
      case ZoneType_FELineSeg:
        {
          NumElems = JMax;
          NodesPerElem  = 2;
          SkipNeighbors = TRUE;
        } break;
      case ZoneType_FETriangle:
        {
          NumElems = JMax;
          NodesPerElem = 3;
          FacesPerElem = 3;
        } break;
      case ZoneType_FEQuad:
        {
          NumElems = JMax;
          NodesPerElem = 4;
          FacesPerElem = 4;
        } break;
      case ZoneType_FETetra:
        {
          NumElems = JMax;
          NodesPerElem = 4;
          FacesPerElem = 4;
        } break;
      case ZoneType_FEBrick:
        {
          NumElems = JMax;
          NodesPerElem = 8;
          FacesPerElem = 6;
        } break;
      default:
        CHECK(FALSE);
        IsOk = FALSE;
        break;
    }

  if (ZoneType != ZoneType_Ordered)
    {
      /* Copy the connectivity. */
      NodeMap_pa      SourceNodeMap = NULL;
      Set_pa          ZoneList      = TecUtilSetAlloc(TRUE);

      if (ZoneList)
        TecUtilSetAddMember(ZoneList,
                            DestZone,
                            TRUE);
      else
        IsOk = FALSE;
      if (IsOk && ArgList)
        {
          TecUtilArgListAppendInt(ArgList, SV_STATECHANGE, StateChange_NodeMapsAltered);
          TecUtilArgListAppendSet(ArgList, SV_ZONELIST,    ZoneList);
        }
      else
        IsOk = FALSE;

      if (IsOk)
        {
          SourceNodeMap = TecUtilDataNodeGetRef(SourceZone);

          NodeMap_t *NodeMapArray   = NULL;
          LgIndex_t  NodeMapSize    = NumElems * NodesPerElem;
          NodeMap_pa TargetNodeMap  = NULL;

          TargetNodeMap = TecUtilDataNodeGetRef(DestZone);
          if (TargetNodeMap)
            {
              NodeMapArray = (NodeMap_t *)TecUtilStringAlloc(NodeMapSize * sizeof(NodeMap_t), "NodeMapArray");
              if (NodeMapArray)
                {
                  TecUtilDataNodeArrayGetByRef(SourceNodeMap,
                                               1,             /* SourceOffset */
                                               NodeMapSize,
                                               NodeMapArray);
                  TecUtilDataNodeArraySetByRef(TargetNodeMap,
                                               1,             /* SourceOffset */
                                               NodeMapSize,
                                               NodeMapArray);
                  TecUtilStringDealloc((char **)&NodeMapArray);
                }
              else
                IsOk = FALSE;
            }
          else
            IsOk = FALSE;

          if (IsOk)
            TecUtilStateChangedX(ArgList);
        }
      if (ZoneList)
        TecUtilSetDealloc(&ZoneList);
      if (ArgList)
        TecUtilArgListDealloc(&ArgList);
    }

  /* Copy the face neighbors but shift zone numbers by offset. */
  if (IsOk && !SkipNeighbors)
    {
      LgIndex_t EIndex;
      LgIndex_t FIndex;
      FaceNeighbor_pa FaceNeighbor = NULL;

      FaceNeighbor = TecUtilDataFaceNbrGetRef(SourceZone);
      IsOk = (FaceNeighbor != NULL);

      ArgList = TecUtilArgListAlloc();
      TecUtilArgListAppendInt(ArgList, SV_ZONE, DestZone);
      IsOk = IsOk && TecUtilDataFaceNbrBeginAssignX(ArgList);
      TecUtilArgListDealloc(&ArgList);

      for (EIndex = 1; IsOk && (EIndex <= NumElems); EIndex++)
        {
          for (FIndex = 1; IsOk && (FIndex <= FacesPerElem); FIndex++)
            {
              std::vector<LgIndex_t>  NeighborElems;
              std::vector<EntIndex_t> NeighborZones;
              Boolean_t   FaceIsObscurred = FALSE;
              LgIndex_t   NumNeighbors = TecUtilDataFaceNbrGetNumNByRef(FaceNeighbor,
                                                                        EIndex,
                                                                        FIndex);

              for (LgIndex_t NIndex = 1; NIndex <= NumNeighbors; NIndex++)
                {
                  LgIndex_t  NeighborElem = 0;
                  EntIndex_t NeighborZone = 0;
                  LgIndex_t  ZoneShift    = 0;

                  TecUtilDataFaceNbrGetNbrByRef(FaceNeighbor,
                                                EIndex,
                                                FIndex,
                                                NIndex,
                                                &NeighborElem,
                                                &NeighborZone);
                 // If the Neighbor zone is not in the SelectedZoneSet, it will
                 // have no corresponding redefined neighbor zone, so we can't set up a
                 // connection to it
                 if (NeighborZone == TECUTILINVALIDZONE ||
                     (SourceZone != NeighborZone && // This will short-circuit the below lookup for most cases
                      !TecUtilSetIsMember(SelectedZones, NeighborZone)))
                   {
                     // Do nothing. The connection is broken because the redefined neighbor zone does not exist
                   }
                 else
                   {
                     NeighborElems.push_back(NeighborElem);
                     // Add the shifted zone number to the face neighbor zone list
                     // We may be missing intermediate zones if the user chose not to
                     // redefine zones between the Source and Neighbor. In those cases
                     // we need to alter ZoneShift, which represents the offset between
                     // the source and the neighbor.
                     ZoneShift = NeighborZone - SourceZone;
                     if (ZoneShift>0)
                       {
                         for (int ii = SourceZone+1; ii < NeighborZone; ii++)
                           if (!TecUtilSetIsMember(SelectedZones, ii))
                             ZoneShift--;
                       }
                     else if (ZoneShift<0)
                       {
                         for (int ii = NeighborZone+1; ii < SourceZone; ii++)
                           if (!TecUtilSetIsMember(SelectedZones, ii))
                             ZoneShift++;
                       }
                     NeighborZones.push_back(DestZone+ZoneShift);

                    }
                 if (NeighborElem == TECUTIL_NO_NEIGHBORING_ELEM)
                   FaceIsObscurred = TRUE;
                }

              NumNeighbors = NeighborElems.size();
              if (NumNeighbors > 0)
                {
                  CHECK(NeighborZones.size() == NeighborElems.size());
                  IsOk = TecUtilDataFaceNbrAssign(EIndex,
                                                  FIndex,
                                                  FaceIsObscurred,
                                                  NumNeighbors,
                                                  &(NeighborElems[0]),
                                                  &(NeighborZones[0]));
                }
            }
        }
      if (IsOk)
        IsOk = TecUtilDataFaceNbrEndAssign();
    }


  ENSURE(VALID_BOOLEAN(IsOk));
  return (IsOk);
}

//
//  Copy all zone auxiliary data from the source zone to the destination zone.
//
void CopyZoneAuxData(EntIndex_t  SourceZone,
                     EntIndex_t  DestZone)
{
  REQUIRE(SourceZone > 0);
  REQUIRE(DestZone > 0);
  
  AuxData_pa  SourceAuxDataRef = TecUtilAuxDataZoneGetRef(SourceZone);
  AuxData_pa  DestAuxDataRef   = TecUtilAuxDataZoneGetRef(DestZone);

  if (SourceAuxDataRef != NULL &&
      DestAuxDataRef   != NULL)
    {
      LgIndex_t   NumItems   = TecUtilAuxDataGetNumItems(SourceAuxDataRef);

      for (LgIndex_t ii = 1; ii <= NumItems; ii++)
        {
          char          *Name     = NULL;
          ArbParam_t     Value;
          AuxDataType_e  Type     = AuxDataType_Invalid;
          Boolean_t      Retain   = FALSE;

          TecUtilAuxDataGetItemByIndex(SourceAuxDataRef,
		                                   ii,
                                       &Name,
                                       &Value,
                                       &Type,
                                       &Retain);
          TecUtilAuxDataSetItem(DestAuxDataRef,
		                            Name,
		                            Value,
		                            Type,
		                            Retain);
          // Clean up
          TecUtilStringDealloc(&Name);
          if (Type == AuxDataType_String) // currently the only type supported
            {
              Name = (char*)Value;
              TecUtilStringDealloc(&Name);
            }
          else
            {
              // Fill in when other aux data types are supported by Tecplot            
            }
        }
    }
}

//
//  Recreate the specified zone with the given StrandID, Solution Time,
//  and Parent Zone parameters. The original zone is left in the data set.
//  No state changes are broadcast; this is the responsibility of the caller.
//  Return TRUE if zone was redefined, FALSE otherwise.
//
static Boolean_t RedefineSingleZone(EntIndex_t  ZoneNumber,
                                    Boolean_t   AssignStrandID,
                                    Strand_t    StrandID,
                                    Boolean_t   AssignSolutionTime,
                                    double      SolutionTime,
                                    Boolean_t   AssignParentZone,
                                    EntIndex_t  ParentZone,
                                    Boolean_t  &ConnectivityWasShared)
{
  REQUIRE(ZoneNumber > 0);
  REQUIRE(IMPLICATION(AssignStrandID,   StrandID >= 0));
  REQUIRE(IMPLICATION(AssignParentZone, ParentZone >= 0));

  TecUtilLockStart(AddOnID);

  EntIndex_t        NumVars  = 0;
  EntIndex_t        NumZones = 0;
  Boolean_t         IsOk     = TRUE;
  TecUtilDataSetGetInfo(NULL, // Title 
                        &NumZones,
                        &NumVars);

  IsOk = ZoneNumber <= NumZones;

  if (IsOk)
    {
      LgIndex_t           IMax              = 0;
      LgIndex_t           JMax              = 0;
      LgIndex_t           KMax              = 0;
      ArgList_pa          ArgList           = TecUtilArgListAlloc();
      FaceNeighborMode_e  FaceNeighborMode  = FaceNeighborMode_Invalid;
      char               *ZoneName          = NULL;

      std::vector<FieldDataType_e>  DataTypeArray;
      std::vector<ValueLocation_e>  ValueLocationArray;
      std::vector<EntIndex_t>       ZoneShareArray;
      
      // Get information to inherit from the old zone, and build the argument list
      // for creating the new zone

      // If any parameter is not assigned, it should be inherited
      if (!AssignStrandID)      StrandID      = TecUtilZoneGetStrandID(ZoneNumber);
      if (!AssignSolutionTime)  SolutionTime  = TecUtilZoneGetSolutionTime(ZoneNumber);
      if (!AssignParentZone)    ParentZone    = TecUtilZoneGetParentZone(ZoneNumber);
      TecUtilArgListAppendInt   (ArgList, SV_PARENTZONE,   ParentZone);
      TecUtilArgListAppendInt   (ArgList, SV_STRANDID,     StrandID);
      TecUtilArgListAppendDouble(ArgList, SV_SOLUTIONTIME, SolutionTime);
      
      // Zone name
      TecUtilZoneGetName(ZoneNumber, &ZoneName);
      TecUtilArgListAppendString(ArgList, SV_NAME, ZoneName);

      // Zone type
      ZoneType_e ZoneType = TecUtilZoneGetType(ZoneNumber);
      TecUtilArgListAppendInt(ArgList, SV_ZONETYPE, ZoneType);

      // Zone dimensions
      TecUtilZoneGetInfo(ZoneNumber,
                         &IMax, &JMax, &KMax,
                         NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL); // Field data handles
      TecUtilArgListAppendInt(ArgList, SV_IMAX, IMax);
      TecUtilArgListAppendInt(ArgList, SV_JMAX, JMax);
      TecUtilArgListAppendInt(ArgList, SV_KMAX, KMax);

      // Variable data types and locations
      for (EntIndex_t ii = 1; ii <= NumVars; ii++)
        {
          DataTypeArray.push_back(TecUtilDataValueGetType(ZoneNumber, ii));
          ValueLocationArray.push_back(TecUtilDataValueGetLocation(ZoneNumber, ii));
        }
      TecUtilArgListAppendArray(ArgList, SV_VARDATATYPE,   &(DataTypeArray[0]));
      TecUtilArgListAppendArray(ArgList, SV_VALUELOCATION, &(ValueLocationArray[0]));

      // Variable sharing
      ZoneShareArray.assign(NumVars, ZoneNumber); // Share all variables with the old zone
      TecUtilArgListAppendArray(ArgList, SV_VARSHAREZONELIST, &(ZoneShareArray[0]));

      // Face neighbor mode
      // Several problems here:
      // 1. Tecplot allows Linear zones (FE-LineSeg or I-Ordered) to have a Global face neighbor
      //    mode (even though face neighbors are irrelevant in those zone types).
      // 2. You can't change the face neighbor mode of a zone (only can be assigned at creation).
      // 3. A bug in Tecplot (11-0.4 and lower) causes it to crash if you call TecUtilDataFaceNbrGetModeByRef
      //    on such a zone.
      // For these reasons, we can never reliably share connectivity/face neighbors for these zone types.
      // Instead, we have to copy the connectivity afterwards.
      if (IsLinearZone(ZoneType, IMax, JMax, KMax))
        {
          FaceNeighborMode = FaceNeighborMode_LocalOneToOne;
          ConnectivityWasShared = FALSE;
        }
      else
        {
          FaceNeighborMode = TecUtilDataFaceNbrGetModeByRef(TecUtilDataFaceNbrGetRef(ZoneNumber));
          // If possible, share connectivity
          if (CanShareConnectivity(FaceNeighborMode))
            {
              TecUtilArgListAppendInt(ArgList, SV_CONNECTSHAREZONE,  ZoneNumber);
              ConnectivityWasShared = TRUE;
            }
        }
      TecUtilArgListAppendInt(ArgList, SV_FACENEIGHBORMODE,  FaceNeighborMode);
      
      // Create a new zone with the same parameters except the time aware info
      IsOk = TecUtilDataSetAddZoneX(ArgList);

      // Clean up
      if (ArgList)  TecUtilArgListDealloc(&ArgList);
      if (ZoneName) TecUtilStringDealloc(&ZoneName);

      if (IsOk)
        CopyZoneAuxData(ZoneNumber, NumZones+1);
    }

  TecUtilLockFinish(AddOnID);
  ENSURE(VALID_BOOLEAN(IsOk));
  return (IsOk);
}

Boolean_t RedefineMultipleZones(const Set_pa                   ZoneSet,
                                const DataSettings_s          &DataSettings,
                                const StrandIDSettings_s      &StrandIDSettings,
                                const SolutionTimeSettings_s  &SolutionTimeSettings,
                                const StrandIDSettings_s      &ParentZoneSettings)
{
  REQUIRE(VALID_REF(ZoneSet));

  TecUtilLockStart(AddOnID);
  TecUtilDataSetSuspendMarking(TRUE);

  LgIndex_t NumSelectedZones = TecUtilSetGetMemberCount(ZoneSet);
  Boolean_t IsOk             = TRUE;

  // Sanity checks on the options
  if (DataSettings.MultipleZonesPerTimestep)
    {
      // The number of selected zones must be a multiple of the group size
      IsOk = (NumSelectedZones % DataSettings.GroupSize) == 0;
      if (!IsOk)
        TecUtilDialogErrMsg(ADDON_NAME": The number of selected zones must be a multiple of the group size.");
    }

  if (IsOk)
    {

      double      CurSolutionTime        = 0.0;
      LgIndex_t   CurStrandID            = 0;
      Set_pa      AddedZones             = TecUtilSetAlloc(FALSE);
      Set_pa      DeletedZones           = TecUtilSetAlloc(FALSE);
      EntIndex_t  NumVars                = 0;
      EntIndex_t  NumZones               = 0;
      Boolean_t   ConnectivityWasShared  = FALSE;

      TecUtilDataSetGetInfo(NULL, // Title 
                            &NumZones,
                            &NumVars);

      if (!DataSettings.MultipleZonesPerTimestep)
        {
          // Simple loop over all selected zones.
          // All zones get the same StrandID.
          CurStrandID = StrandIDSettings.Value;
          for (LgIndex_t CurZoneIndex = 0; CurZoneIndex < NumSelectedZones; CurZoneIndex++)
            {
              LgIndex_t CurZoneNumber   = TecUtilSetGetMember(ZoneSet, CurZoneIndex+1);
              CurSolutionTime = SolutionTimeSettings.Value;
              if (SolutionTimeSettings.Option == TimeOptions_ConstantDelta)
                CurSolutionTime += SolutionTimeSettings.Delta*CurZoneIndex;
              Boolean_t ZoneRedefined = RedefineSingleZone(CurZoneNumber,
                                                           StrandIDSettings.Assign,
                                                           CurStrandID,
                                                           SolutionTimeSettings.AssignSolutionTime,
                                                           CurSolutionTime,
                                                           ParentZoneSettings.Assign,
                                                           ParentZoneSettings.Value,
                                                           ConnectivityWasShared);
              if (ZoneRedefined)
                {
                  TecUtilSetAddMember(DeletedZones,   CurZoneNumber, FALSE);
                  TecUtilSetAddMember(AddedZones,     NumZones+CurZoneIndex+1, FALSE);
                  if (!ConnectivityWasShared)
                    CopyConnectivityAndShiftedNeighbors(CurZoneNumber,
                                                        NumZones+CurZoneIndex+1,
                                                        ZoneSet);
                }
            }
        }
      else
        {
          // Multiple zones per timestep
          LgIndex_t NumGroups = NumSelectedZones / DataSettings.GroupSize;

          for (LgIndex_t CurGroup = 0; CurGroup < NumGroups; CurGroup++)
            {
              if (DataSettings.ZoneGrouping == ZoneGrouping_ByTimeStep)
                {
                  // All zones in each group get the same solution time
                  CurSolutionTime = SolutionTimeSettings.Value;
                  if (SolutionTimeSettings.Option == TimeOptions_ConstantDelta)
                    CurSolutionTime += SolutionTimeSettings.Delta*CurGroup;
                }
              else
                {
                  // All zones in each group get the same StrandID
                  CurStrandID = StrandIDSettings.Value + CurGroup;
                }

              for (LgIndex_t ZoneInGroup = 0; ZoneInGroup < DataSettings.GroupSize; ZoneInGroup++)
                {
                  // Get the zone number to change
                  LgIndex_t CurZoneIndex  = CurGroup*DataSettings.GroupSize + ZoneInGroup; // Index into the ZoneSet
                  LgIndex_t CurZoneNumber = TecUtilSetGetMember(ZoneSet, CurZoneIndex+1);

                  // Determine the missing parameter from the outer loop
                  if (DataSettings.ZoneGrouping == ZoneGrouping_ByTimeStep)
                    {
                      // Varying StrandIDs within the group
                      CurStrandID = StrandIDSettings.Value + ZoneInGroup;
                    }
                  else
                    {
                      // Varying Solution Times within the group
                      CurSolutionTime = SolutionTimeSettings.Value;
                      if (SolutionTimeSettings.Option == TimeOptions_ConstantDelta)
                        CurSolutionTime += SolutionTimeSettings.Delta*ZoneInGroup;
                    }

                  Boolean_t ZoneRedefined = RedefineSingleZone(CurZoneNumber,
                                                               StrandIDSettings.Assign,
                                                               CurStrandID,
                                                               SolutionTimeSettings.AssignSolutionTime,
                                                               CurSolutionTime,
                                                               ParentZoneSettings.Assign,
                                                               ParentZoneSettings.Value,
                                                               ConnectivityWasShared);
                  if (ZoneRedefined)
                    {
                      TecUtilSetAddMember(DeletedZones, CurZoneNumber, FALSE);
                      TecUtilSetAddMember(AddedZones,   NumZones+CurZoneIndex+1, FALSE);
                      if (!ConnectivityWasShared)
                        CopyConnectivityAndShiftedNeighbors(CurZoneNumber,
                                                            NumZones+CurZoneIndex+1,
                                                            ZoneSet);
                    }
                }
            }
        }

      // Enable the added zones
      TecUtilZoneSetActive(AddedZones, AssignOp_PlusEquals);

      // Send a state change about the added zones
      TecUtilStateChanged(StateChange_ZonesAdded,   (ArbParam_t)AddedZones);
      TecUtilSetDealloc(&AddedZones);
      
      // Delete the original zones (state change provided automatically)
      TecUtilDataSetDeleteZone(DeletedZones);
      TecUtilSetDealloc(&DeletedZones);
    }

  if (IsOk && TecUtilDataSetJournalIsValid())
    {
      std::string journalString;
      BuildMacroCommand(ZoneSet, journalString);
      TecUtilDataSetAddJournalCommand(ADDON_NAME,
                                      journalString.c_str(),
                                      NULL); // Raw data
    }
  TecUtilDataSetSuspendMarking(FALSE);

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


static void STDCALL StateChangeCallback(StateChange_e StateChange)
{
  if (TecGUIDialogIsUp(Dialog1Manager))
    {
      switch (StateChange)
        {
          case StateChange_ZonesDeleted:
          case StateChange_ZonesAdded:
          case StateChange_ZoneName:
            {
              if (ZoneListUpdatePending == FALSE)
                {
                  TecUtilOnIdleQueueAddCallback(UpdateZoneListOnIdle, (ArbParam_t)NULL);
                  ZoneListUpdatePending = TRUE;
                }
            } break;

          case StateChange_FrameDeleted: 
          case StateChange_NewTopFrame:
            {
              if (TecUtilDataSetIsAvailable())
                {
                  if (ZoneListUpdatePending == FALSE)
                    {
                      TecUtilOnIdleQueueAddCallback(UpdateZoneListOnIdle, (ArbParam_t)NULL);
                      ZoneListUpdatePending = TRUE;
                    }
                }
              else
                TecGUIDialogDrop(Dialog1Manager);
            } break;

          case StateChange_DataSetReset: 
          case StateChange_NewLayout:    
          case StateChange_CompleteReset:
            {
              TecGUIDialogDrop(Dialog1Manager);
            } break;
          default: break;
        } /* end switch */
    }
}

static void STDCALL MenuCallback(void)
{
  TecUtilLockStart(AddOnID);
  if (TecUtilDataSetIsAvailable())
    {
      BuildDialog1(MAINDIALOGID);
      TecGUIDialogLaunch(Dialog1Manager);
    }
  else
    TecUtilDialogErrMsg("The Strand Editor is unavailable.\nThere is no dataset attached to the active frame.");
  TecUtilLockFinish(AddOnID);
}

/**
 * When Tecplot first loads an add-on, it makes a 
 * call to initialize the add-on. This function
 * must be named InitTecAddOn, as shown below.
 */
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  TecUtilLockOn();

  AddOnID = TecUtilAddOnRegister(110,
                                 ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "Tecplot, Inc.");

  /*
   * Initialize the Tecplot GUI Builder libraries.
   */
  InitTGB();

  TecUtilMacroAddCommandCallback(ADDON_NAME,
                                 MacroCommandCallback);
  {
    ArgList_pa ArgList;
    ArgList = TecUtilArgListAlloc();
    TecUtilArgListAppendFunction(ArgList, SV_CALLBACKFUNCTION,       (const void *)StateChangeCallback);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGEMODE,        StateChangeMode_v100);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGECALLBACKAPI, StateChangeCallbackAPI_ChangeOnly);
    TecUtilStateChangeAddCallbackX(ArgList);
    TecUtilArgListDealloc(&ArgList);
  }
  TecUtilMenuAddOption("Tools",
                       "&Strand Editor",
                       TECUTILAUTOMNEMONIC,
                       MenuCallback);
  InitGlobalSettings();
  ZoneListUpdatePending = FALSE;

  TecUtilLockOff();
}

