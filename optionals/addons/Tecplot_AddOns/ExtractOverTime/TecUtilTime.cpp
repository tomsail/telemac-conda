// TecUtilTime.cpp: implementation of the CTecUtilTime class.
//
//////////////////////////////////////////////////////////////////////
#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TecUtilTime.h"
#include "TecUtilSet.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CTecUtilTime::CTecUtilTime()
{

}

CTecUtilTime::~CTecUtilTime()
{

}

Strand_t CTecUtilTime::GetMaxStrandID()
{
  REQUIRE(TecUtilDataSetIsAvailable());
  return TecUtilDataSetGetMaxStrandID();
}


void CTecUtilTime::GetSolutionTimes(vector<double> &SolutionTimes)
{  
  Set_pa set;
  TecUtilZoneGetEnabled(&set);
  CTecUtilSet ZoneSet(set);

  if ( !ZoneSet.IsEmpty() )
    {      
      EntIndex_t Zone;     
      TecUtilSetForEachMember(Zone, ZoneSet.GetSet())
        {
          double Time = TecUtilZoneGetSolutionTime(Zone);
          SolutionTimes.push_back(Time);          
        }
      sort(SolutionTimes.begin(), SolutionTimes.end());
#if 0
      unique(SolutionTimes.begin(), SolutionTimes.end());
#else
      // Make *really* unique
      int ii = 0;
      vector<double> uniqueSolutionTimes;
      uniqueSolutionTimes.push_back(SolutionTimes[0]);
      for ( ii = 1; ii < SolutionTimes.size(); ii++ )
        {
          double Time = SolutionTimes[ii];
          if ( Time != SolutionTimes[ii-1] )
            uniqueSolutionTimes.push_back(Time);
        }
      SolutionTimes.clear();
      for ( ii = 0; ii < uniqueSolutionTimes.size(); ii++ )
        {
          SolutionTimes.push_back(uniqueSolutionTimes[ii]);
        }
#endif      
    }
}


void CTecUtilTime::GetSolutionTimes(Strand_t        StrandID,
                                    vector<double> &SolutionTimes)
{
  REQUIRE(StrandID > 0);
  Set_pa set;
  TecUtilZoneGetEnabled(&set);
  CTecUtilSet ZoneSet(set);

  if ( !ZoneSet.IsEmpty() )
    {      
      EntIndex_t Zone;     
      TecUtilSetForEachMember(Zone, ZoneSet.GetSet())
        {
          Strand_t CurStrandID = TecUtilZoneGetStrandID(Zone);
          if ( CurStrandID == StrandID )
            {
              double Time = TecUtilZoneGetSolutionTime(Zone);
              SolutionTimes.push_back(Time);
            }
        }
      sort(SolutionTimes.begin(), SolutionTimes.end());
#if 0
      unique(SolutionTimes.begin(), SolutionTimes.end());
#else
      // Make *really* unique
      int ii = 0;
      vector<double> uniqueSolutionTimes;
      uniqueSolutionTimes.push_back(SolutionTimes[0]);
      for ( ii = 1; ii < SolutionTimes.size(); ii++ )
        {
          double Time = SolutionTimes[ii];
          if ( Time != SolutionTimes[ii-1] )
            uniqueSolutionTimes.push_back(Time);
        }
      SolutionTimes.clear();
      for ( ii = 0; ii < uniqueSolutionTimes.size(); ii++ )
        {
          SolutionTimes.push_back(uniqueSolutionTimes[ii]);
        }
#endif      
    }
}

/**
 * This is unlike TecUtilZoneGetRelevant() since it takes a solution time
 * as a parameter rather than relying on the solution time in the frame.
 * It returns the set of zones that exactly match the specified solution
 * time and the results may be different that those returned by TecUtilZoneGetRelevant()
 */
CTecUtilSet *CTecUtilTime::GetZonesAtSolutionTime(Strand_t StrandID,
                                                  double   SolutionTime)
{
  REQUIRE(StrandID > 0);
  REQUIRE("SolutionTime can be any value");

  CTecUtilSet *ZoneSet = new CTecUtilSet;  
  Set_pa set;
  TecUtilZoneGetEnabled(&set);
  CTecUtilSet *EnabledZones = new CTecUtilSet(set);

  if ( !EnabledZones->IsEmpty() &&
       !ZoneSet->IsEmpty() )
    {      
      EntIndex_t Zone;
      int ii = 0;
      TecUtilSetForEachMember(Zone, EnabledZones->GetSet())
        {
          Strand_t CurStrandID = TecUtilZoneGetStrandID(Zone);
          if ( CurStrandID == StrandID )
            {
              double Time = TecUtilZoneGetSolutionTime(Zone);
              if ( Time == SolutionTime )
                ZoneSet->Add(Zone);
            }
        }      
      delete EnabledZones;
      if ( ZoneSet->Count() == 0 )
        {
          delete ZoneSet;
          ZoneSet = NULL;
        }
    }  
  ENSURE(VALID_REF_OR_NULL(ZoneSet));
  return ZoneSet;
}


static Boolean_t IsLinearZone(EntIndex_t ZoneNumber)
{
  LgIndex_t IMax = 0;
  LgIndex_t JMax = 0;
  LgIndex_t KMax = 0;
  TecUtilZoneGetInfo(ZoneNumber,
                     &IMax, &JMax, &KMax,
                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL); // Field data handles
  ZoneType_e ZoneType = TecUtilZoneGetType(ZoneNumber);

  Boolean_t IsLinear = ( ZoneType == ZoneType_FELineSeg ||
                         (ZoneType == ZoneType_Ordered &&
                          (JMax == 1 && KMax == 1 ||   // I-Ordered
                           IMax == 1 && KMax == 1 ||   // J-Ordered
                           IMax == 1 && JMax == 1)) ); // K-Ordered

  ENSURE(VALID_BOOLEAN(IsLinear));
  return IsLinear;    
}

//
//  Recreate the specified zone with the given StrandID, Solution Time,
//  and Parent Zone parameters. The original zone is left in the data set.
//  No state changes are broadcast; this is the responsibility of the caller.
//  Return TRUE if zone was redefined, false otherwise.
//
static Boolean_t RedefineSingleZone(EntIndex_t  ZoneNumber,
                                    Boolean_t   AssignStrandID,
                                    Strand_t    StrandID,
                                    Boolean_t   AssignSolutionTime,
                                    double      SolutionTime,
                                    Boolean_t   AssignParentZone,
                                    EntIndex_t  ParentZone)
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
      char               *ZoneName          = NULL;

      std::vector<FieldDataType_e>  DataTypeArray;
      std::vector<ValueLocation_e>  ValueLocationArray;
      std::vector<EntIndex_t>       ZoneShareArray;

      // If any parameter is not assigned, it should be inherited
      if (!AssignStrandID)
        StrandID = TecUtilZoneGetStrandID(ZoneNumber);
      if (!AssignSolutionTime)
        SolutionTime = TecUtilZoneGetSolutionTime(ZoneNumber);
      if (!AssignParentZone)
        ParentZone = TecUtilZoneGetParentZone(ZoneNumber);
      
      // Get information to inherit from the old zone
      TecUtilZoneGetInfo(ZoneNumber,
                         &IMax, &JMax, &KMax,
                         NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL); // Field data handles
      TecUtilZoneGetName(ZoneNumber, &ZoneName);
      ZoneType_e ZoneType = TecUtilZoneGetType(ZoneNumber);
      for (EntIndex_t ii = 1; ii <= NumVars; ii++)
        {
          DataTypeArray.push_back(TecUtilDataValueGetType(ZoneNumber, ii));
          ValueLocationArray.push_back(TecUtilDataValueGetLocation(ZoneNumber, ii));
          ZoneShareArray.push_back(ZoneNumber); // Share all variables
        }      

      // Build the arglist of information for the new zone
      TecUtilArgListAppendString  (ArgList, SV_NAME,              ZoneName);
      TecUtilArgListAppendInt     (ArgList, SV_PARENTZONE,        ParentZone);
      TecUtilArgListAppendInt     (ArgList, SV_STRANDID,          StrandID);
      TecUtilArgListAppendDouble  (ArgList, SV_SOLUTIONTIME,      SolutionTime);
      TecUtilArgListAppendInt     (ArgList, SV_ZONETYPE,          ZoneType);
      TecUtilArgListAppendInt     (ArgList, SV_IMAX,              IMax);
      TecUtilArgListAppendInt     (ArgList, SV_JMAX,              JMax);
      TecUtilArgListAppendInt     (ArgList, SV_KMAX,              KMax);
      TecUtilArgListAppendArray   (ArgList, SV_VARDATATYPE,       &(DataTypeArray[0]));
      TecUtilArgListAppendArray   (ArgList, SV_VALUELOCATION,     &(ValueLocationArray[0]));
      TecUtilArgListAppendArray   (ArgList, SV_VARSHAREZONELIST,  &(ZoneShareArray[0]));

      // TecUtilDataFaceNbrGetRef has a bug in it (fixed in 11.0-4-112 and 11.2-0-223 ) that 
      // will crash Tecplot if called with any of the following zone types.
      FaceNeighborMode_e  FaceNeighborMode = FaceNeighborMode_Invalid;

      if ( !IsLinearZone(ZoneNumber) ) //      && 
//           ZoneType != ZoneType_FEPolygon &&
//           ZoneType != ZoneType_FEPolyhedron )
        {
          FaceNeighbor_pa     FaceNeighborRef  = TecUtilDataFaceNbrGetRef(ZoneNumber);          
          if ( FaceNeighborRef )
            {          
              FaceNeighborMode = TecUtilDataFaceNbrGetModeByRef(FaceNeighborRef);
              TecUtilArgListAppendInt(ArgList, SV_FACENEIGHBORMODE,  FaceNeighborMode);
            }
        }

      // If possible, share connectivity. Ordered zones can have face neighbor
      // connectivity, so that's why we're not checking the ZoneType
      if (FaceNeighborMode != FaceNeighborMode_GlobalOneToOne &&
          FaceNeighborMode != FaceNeighborMode_GlobalOneToMany)
        {          
          TecUtilArgListAppendInt(ArgList, SV_CONNECTSHAREZONE,  ZoneNumber);
        }

      // Create a new zone with the same parameters except the time aware info
      IsOk = TecUtilDataSetAddZoneX(ArgList);

      // Clean up
      if (ArgList)  TecUtilArgListDealloc(&ArgList);
      if (ZoneName) TecUtilStringDealloc(&ZoneName);
    }

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

Boolean_t CTecUtilTime::ZoneAddCopyToStrand(EntIndex_t Zone,
                                            Strand_t   StrandID,
                                            double     SolutionTime)
{
  REQUIRE(StrandID > 0);
  REQUIRE(TecUtilDataSetIsAvailable());
  REQUIRE(TecUtilZoneIsEnabled(Zone));
  REQUIRE("Solution Time can be any value");

  //
  // NOTE: RedefineSingleZone() was lifted from the StrandEditor addon
  //       by Ben Medina-Orton, which is available on www.tecplottalk.com/addons
  //
  return RedefineSingleZone(Zone,
                            TRUE, //AssignStrandID,
                            StrandID,
                            TRUE, //AssignSolutionTime,
                            SolutionTime,
                            FALSE, //AssignParentZone,
                            0);
}


Boolean_t CTecUtilTime::ConcatenateZonesIntoStrand(Strand_t        StrandID,
                                                   vector<double> &SolutionTimes,
                                                   vector<CTecUtilSet*> &ZoneSets)
{
  REQUIRE(StrandID > 0);
  REQUIRE(SolutionTimes.size() > 0);
  REQUIRE(ZoneSets.size() > 0);
  REQUIRE(SolutionTimes.size() == ZoneSets.size());

  Boolean_t Result = TRUE;

  for ( int ii = 0; ii < SolutionTimes.size(); ii++ )
    {      
      EntIndex_t Zone;      
      TecUtilSetForEachMember(Zone, (ZoneSets[ii])->GetSet())
        {
          ZoneAddCopyToStrand(Zone, StrandID, SolutionTimes[ii]);          
        }
    }


  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}


Boolean_t CTecUtilTime::FrameHasTransientData(UniqueID_t FrameID)
{
  TecUtilLockStart(AddOnID);

  Boolean_t Result = FALSE;

  TecUtilFrameLightweightPopStart();
  do
    {      
      if ( TecUtilFrameGetUniqueID() == FrameID &&
           TecUtilDataSetIsAvailable() &&           
           (TecUtilFrameGetPlotType() == PlotType_Cartesian2D ||
            TecUtilFrameGetPlotType() == PlotType_Cartesian3D) )
        {
          Set_pa EnabledZones = NULL;
          TecUtilZoneGetEnabled(&EnabledZones);

          if ( EnabledZones )
            {
              EntIndex_t Zone;
              TecUtilSetForEachMember(Zone, EnabledZones)
                {
                  if ( TecUtilZoneGetStrandID(Zone) > 0 )
                    {
                      Result = TRUE;
                      break;
                    }
                }
              TecUtilSetDealloc(&EnabledZones);
            }
        }
    } while ( TecUtilFrameLightweightPopNext() );
  TecUtilFrameLightweightPopEnd();  

  TecUtilLockFinish(AddOnID);

  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}
