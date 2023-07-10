#include "TECADDON.h"
#include "ADDGLBL.h"
#include <string.h>

// STL
#include <vector>
#include <algorithm>
using namespace std;

AddOn_pa AddOnID;

static UniqueID_t S_lastFrameUsed = BAD_SET_VALUE;
static Boolean_t  S_probedFrameAlwaysOnTop = FALSE;

static Boolean_t FrameHasTransientData(void)
{
  REQUIRE(TecUtilDataSetIsAvailable());
  REQUIRE(TecUtilFrameGetPlotType() == PlotType_Cartesian2D ||
          TecUtilFrameGetPlotType() == PlotType_Cartesian3D);

  Boolean_t Result = FALSE;

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

  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}

static void GetSolutionTimes(Strand_t        StrandID,
                             vector<double> &SolutionTimes)
{
  REQUIRE(StrandID > 0);
  Set_pa ZoneSet = NULL;
  TecUtilZoneGetEnabled(&ZoneSet);

  if ( ZoneSet )
    {      
      EntIndex_t Zone;     
      TecUtilSetForEachMember(Zone, ZoneSet)
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

      TecUtilSetDealloc(&ZoneSet);
    }
}

static Set_pa GetZonesAtSolutionTime(Strand_t StrandID,
                                     double   SolutionTime)
{
  REQUIRE(StrandID > 0);
  REQUIRE("SolutionTime can be any value");

  Set_pa ZoneSet = TecUtilSetAlloc(TRUE);
  Set_pa EnabledZones = NULL;
  TecUtilZoneGetEnabled(&EnabledZones);

  if ( EnabledZones &&
       ZoneSet )
    {      
      EntIndex_t Zone;
      int ii = 0;
      TecUtilSetForEachMember(Zone, EnabledZones)
        {
          Strand_t CurStrandID = TecUtilZoneGetStrandID(Zone);
          if ( CurStrandID == StrandID )
            {
              double Time = TecUtilZoneGetSolutionTime(Zone);
              if ( Time == SolutionTime )
                TecUtilSetAddMember(ZoneSet, Zone, TRUE);
            }
        }      
      TecUtilSetDealloc(&EnabledZones);
      if ( TecUtilSetGetMemberCount(ZoneSet) == 0 )
        {
          TecUtilSetDealloc(&ZoneSet);
          ZoneSet = NULL;
        }
    }  
  ENSURE(VALID_REF_OR_NULL(ZoneSet));
  return ZoneSet;
}

static void ClearFrame()
{
  double X, Y, W, H;
  TecUtilFrameGetPosAndSize(&X,&Y,&W,&H);
  TecUtilFrameDeleteTop();
  TecUtilFrameCreateNew(TRUE, X,Y,W,H);
  S_lastFrameUsed = TecUtilFrameGetUniqueID();
}

static void InstallTimeSeriesPlotProbeCallback(void);

static void DumpTimeSeriesDataIntoFrame(vector<double> &TimeSeriesPlotData,
                                        int             NumVars)
{  
  REQUIRE(TimeSeriesPlotData.size() > 0);

  Boolean_t IsOk = TRUE;
  StringList_pa VarNames = TecUtilStringListAlloc();
  if ( VarNames )
    {
      TecUtilStringListAppendString(VarNames, "Solution Time");
      for ( int ii = 1; ii < NumVars; ii++ )
        {
          char *VarName;
          TecUtilVarGetName(ii, &VarName);
          CHECK(VALID_REF(VarName));
          TecUtilStringListAppendString(VarNames, VarName);
          TecUtilStringDealloc(&VarName);
        }
    }
  else
    {
      IsOk = FALSE;
    }

  if ( IsOk )
    {
      MouseButtonMode_e LastMouseMode = TecUtilMouseGetCurrentMode();
      // We probed to get here so the mouse mode should be Probe,
      // but we'll get the mouse mode just to be safe
      CHECK(LastMouseMode == Mouse_Probe);

      // Save off the UniqueID for the frame we just probed. If we want
      // to keep this frame always on top, then we use this to pop
      // the frame later.
      UniqueID_t ProbedFrameID = TecUtilFrameGetUniqueID();

      // Use the contouring variable for the dependent variable in the
      // resulting XY plot
      EntIndex_t CVar = BAD_SET_VALUE;
      CVar = TecUtilVarGetNumByAssignment('C');

      // Try popping the last frame we used to dump the data into,
      // if it isn't popped then create a new frame using Tecplot
      // defaults for frame size.
      if ( !TecUtilFramePopByUniqueID(S_lastFrameUsed) )
        {
          TecUtilFrameCreateNew(FALSE, 0,0,0,0);
          S_lastFrameUsed = TecUtilFrameGetUniqueID();          
        }
      
      if ( TecUtilDataSetIsAvailable() )
        {
          // If there's already a dataset in the frame, make sure that
          // it has the proper number of variables
          EntIndex_t NumVarsInThisDataSet = 0;
          TecUtilDataSetGetInfo(NULL, NULL, &NumVarsInThisDataSet);
          if ( NumVarsInThisDataSet > NumVars )
            {
              Set_pa VarSet = TecUtilSetAlloc(TRUE);
              while ( NumVarsInThisDataSet > NumVars )
                {
                  TecUtilSetAddMember(VarSet, NumVarsInThisDataSet--, TRUE);
                }
              TecUtilDataSetDeleteVar(VarSet);
              TecUtilSetDealloc(&VarSet);
            }
          else if ( NumVarsInThisDataSet < NumVars )
            {
              while ( NumVarsInThisDataSet < NumVars )
                {
                  TecUtilDataSetAddVar("Dummy", NULL);
                  NumVarsInThisDataSet++;
                }
            }
          for ( int ii = 0; ii < NumVars; ii++ )
            {
              char *VarName = TecUtilStringListGetString(VarNames, ii+1);
              TecUtilVarRename(ii+1, VarName);
              TecUtilStringDealloc(&VarName);
            }
        }
      else
        {
          TecUtilDataSetCreate("Time Series Plot",
                               VarNames,
                               TRUE); // ResetStyle
        }
      
      
      // Now, create the zone that contains the time series data.        
      LgIndex_t NumTimeSteps = TimeSeriesPlotData.size()/NumVars;

      ArgList_pa ArgList;
      ArgList = TecUtilArgListAlloc();
      TecUtilArgListAppendString(ArgList, SV_NAME, "Time Series Plot Zone");
      TecUtilArgListAppendInt(ArgList, SV_ZONE, 1);      
      TecUtilArgListAppendInt(ArgList, SV_IMAX, NumTimeSteps);      

      if ( TecUtilDataSetAddZoneX(ArgList) )
        {
           Set_pa zones_added = TecUtilSetAlloc(TRUE);
           // new zone is always last zone 
           EntIndex_t newzone;
           TecUtilDataSetGetInfo(NULL, &newzone, NULL);

           // Add the data to the new zone
           for ( int CurVar = 0; CurVar < NumVars; CurVar++ )
             {
               FieldData_pa fd = TecUtilDataValueGetWritableRef(newzone, CurVar+1);
               for ( int CurTimeStep = 0; CurTimeStep < NumTimeSteps; CurTimeStep++ )
                 {
                   // Data is stored in the array by variable then by time:
                   // e.g. V1t1 V2t1, V3t1, V1t2, V2t2, V3t3
                   double Value = TimeSeriesPlotData[NumVars*CurTimeStep+CurVar];
                   TecUtilDataValueSetByRef(fd,
                                            CurTimeStep+1,
                                            Value);
                 }
             }       
       
           TecUtilSetAddMember(zones_added, newzone, TRUE);
           TecUtilStateChanged(StateChange_ZonesAdded,
                               (ArbParam_t)zones_added);
           TecUtilSetDealloc(&zones_added);
        }
      TecUtilArgListDealloc(&ArgList);

      // Switch the plot type to XY Line and setup the proper line map
      // to be active. The one we activate is the one that matches the
      // contouring variable in the probed frame.
      TecUtilFrameSetPlotType(PlotType_XYLine);
      if ( CVar >= 1 )
        {
          int NumLineMaps = TecUtilLineMapGetCount();
          EntIndex_t LineMapToActivate = BAD_SET_VALUE;
          for ( int ii = 0; ii < NumLineMaps; ii++ )
            {
              EntIndex_t YVar;
              TecUtilLineMapGetAssignment(ii+1,
		                                      NULL,  //Zone
		                                      NULL,  //XVar
		                                      &YVar,
		                                      NULL,  //XAxisNum
		                                      NULL,  //YAxisNum
		                                      NULL); //FunctionDependency
              // Add 1 to CVar because the dataset in this frame (the TimeSeries frame)
              // has an addition variable (solution time) which comes before any other variables
              if ( YVar == CVar+1 )
                {
                  LineMapToActivate = ii+1;
                  break;
                }
            }
          
          if ( LineMapToActivate != BAD_SET_VALUE )
            {
              Set_pa LineMapSet = TecUtilSetAlloc(TRUE);
              TecUtilSetAddMember(LineMapSet, LineMapToActivate, TRUE);
              TecUtilLineMapSetActive(LineMapSet, AssignOp_Equals);
              TecUtilSetDealloc(&LineMapSet);
            }
        }
      TecUtilViewDataFit();

      // We may have changed the mouse mode by creating a frame or some other
      // action, be sure to put the mouse mode back.
      if ( TecUtilMouseGetCurrentMode() != LastMouseMode )
        TecUtilMouseSetMode(LastMouseMode);

      // If we want the Transient frame (probed frame) always on top,
      // pop it now.
      if ( S_probedFrameAlwaysOnTop )
        {
          TecUtilFramePopByUniqueID(ProbedFrameID);
          // We need to reinstall the probe callback, since it's just
          // a one shot deal.
          InstallTimeSeriesPlotProbeCallback();
        }
    }
  if ( VarNames )
    TecUtilStringListDealloc(&VarNames);
}



static Boolean_t ExtractDataOverTime(Strand_t   StrandID,
                                     double     XPos,
                                     double     YPos,
                                     double     ZPos,
                                     Boolean_t  GetNearestPoint)
{
  REQUIRE(StrandID > 0);
  REQUIRE("XPos, YPos, ZPos can be any value");
  REQUIRE(VALID_BOOLEAN(GetNearestPoint));
  REQUIRE(TecUtilDataSetIsAvailable());
  REQUIRE(TecUtilFrameGetPlotType() == PlotType_Cartesian2D ||
          TecUtilFrameGetPlotType() == PlotType_Cartesian3D);  

  vector<double> SolutionTimes;  
  GetSolutionTimes(StrandID, SolutionTimes);

  Boolean_t isSuccess = TRUE;

  Boolean_t Interrupted = FALSE;

  if ( SolutionTimes.size() > 0 )
    {      
      TecUtilProbeAtPosSequenceBeginX(NULL);
      int ii = 0;

      EntIndex_t NumVars;
      TecUtilDataSetGetInfo((char **)NULL,
                            (EntIndex_t *)NULL,
                             &NumVars);
      // Store the solution time and probed data for each solution time in a vector.
      // The vector is organized in "point" format:
      //   V1t1, V2t1, V3t1, V1t2, V2t2, V3t3...
      vector<double> TimeSeriesPlotData;

      TecUtilStatusStartPercentDone("Extracting Data Over Time",
                                    TRUE,
                                    TRUE);

      int NumSolutionTimes = SolutionTimes.size();
      for ( ii = 0; ii < NumSolutionTimes; ii++ )
        {    
          if ( TecUtilInterruptCheck() )
            {
              Interrupted = TRUE;
              break;
            }

          int PercentDone = (int)((double)(((double)ii+1)/(double)NumSolutionTimes)*100);
          TecUtilStatusCheckPercentDone(PercentDone);
                      
          Set_pa SourceZones = GetZonesAtSolutionTime(StrandID, SolutionTimes[ii]);          

          if ( SourceZones )
            {              
              char Msg[4096];
              sprintf(Msg, "Collecting Data at Time %f...", SolutionTimes[ii]);
              TecUtilStatusSetPercentDoneText(Msg);
              // We can't handle the multiple zone case yet. Probably need to
              // create a ProbeData array for each Strand.
              CHECK(TecUtilSetGetMemberCount(SourceZones) == 1);              

              double *VValues;
              VValues = (double *)malloc(NumVars*sizeof(double));
              Boolean_t AddVarData = TRUE;
   
              if ( VValues )
                {
                  // Not a Nearest Point probe, so we maintain the X,Y,Z position
                  // rather than following a particular point index through space.
                  LgIndex_t    ICell, JCell, KCell;
                  IJKPlanes_e  Plane;                  
                  EntIndex_t   SourceZone;

                  TecUtilDataLoadBegin();
                  AddVarData = TecUtilProbeAtPosition(XPos, YPos, ZPos,
                                                      &ICell,
                                                      &JCell,
                                                      &KCell,
                                                      &Plane,
                                                      &SourceZone,
                                                      FALSE,
                                                      VValues,
                                                      SourceZones,
                                                      TecUtilFrameGetPlotType() == PlotType_Cartesian3D, // SearchVolume
                                                      FALSE,
                                                      FALSE); // GetNearestPoint - Set to FALSE since the point
                                                              // has already been determined by the orginal probe.
                  TecUtilDataLoadEnd();
                }                          

              if ( AddVarData )
                {
                  // Adding the solution time into this array as well, so add 1 to NumVars.
                  TimeSeriesPlotData.push_back(SolutionTimes[ii]);
                  for ( int CurVar = 0; CurVar < NumVars; CurVar++ )
                    TimeSeriesPlotData.push_back(VValues[CurVar]);
                }
              TecUtilSetDealloc(&SourceZones);
              free(VValues);              
            }
        }      
      TecUtilProbeAtPosSequenceEnd();

      if ( !Interrupted && TimeSeriesPlotData.size() > 0 )
        {
          // Add one to NumVars to make room for the solution time
          DumpTimeSeriesDataIntoFrame(TimeSeriesPlotData, NumVars+1);
        }
      else
        {
          isSuccess = FALSE;
        }
      TecUtilStatusFinishPercentDone();
    }
  else
    {
      isSuccess = FALSE;
    }

  ENSURE(VALID_BOOLEAN(isSuccess));
  return isSuccess;
}



static void STDCALL TimeSeriesPlotProbeCallback(Boolean_t IsNearestPoint)
{
  TecUtilLockStart(AddOnID);
  REQUIRE(TecUtilDataSetIsAvailable());
  REQUIRE(TecUtilFrameGetPlotType() == PlotType_Cartesian2D ||
          TecUtilFrameGetPlotType() == PlotType_Cartesian3D);
  REQUIRE(FrameHasTransientData());

  PlotType_e PlotType = TecUtilFrameGetPlotType();
  
  EntIndex_t XVarNum = TecUtilVarGetNumByAssignment('X');
  EntIndex_t YVarNum = TecUtilVarGetNumByAssignment('Y');
  EntIndex_t ZVarNum = BAD_SET_VALUE;
  if ( PlotType == PlotType_Cartesian3D )
    ZVarNum = TecUtilVarGetNumByAssignment('Z');

  double theXValue = TecUtilProbeFieldGetValue(XVarNum);
  double theYValue = TecUtilProbeFieldGetValue(YVarNum);
  double theZValue = 0;
  if ( PlotType == PlotType_Cartesian3D )
    theZValue = TecUtilProbeFieldGetValue(ZVarNum);

  // Just in case we hit a COB, probe again at this point to get
  // the Zone we care about
  LgIndex_t   ICell, JCell, KCell;
  IJKPlanes_e Plane;                  
  EntIndex_t  SourceZone;
  EntIndex_t  NumVars;
  TecUtilDataSetGetInfo((char **)NULL,
                        (EntIndex_t *)NULL,
                         &NumVars);
  double *VValues;
  VValues = (double *)malloc(NumVars*sizeof(double));

  TecUtilDataLoadBegin();
  TecUtilProbeAtPosition(theXValue, theYValue, theZValue,
                         &ICell,
                         &JCell,
                         &KCell,
                         &Plane,
                         &SourceZone,
                         FALSE, //StartWithLocalCell
                         VValues,
                         NULL, // SourceZones - NULL means to check all zones
                         TecUtilFrameGetPlotType() == PlotType_Cartesian3D, // SearchVolume
                         TRUE, // GetZoneOnly
                         IsNearestPoint);
  TecUtilDataLoadEnd();
  free(VValues);

  Strand_t Strand = TecUtilZoneGetStrandID(SourceZone);
  
  Boolean_t IsOk = FALSE;
  if ( Strand > 0 )
    IsOk = ExtractDataOverTime(Strand,
                               theXValue, 
                               theYValue,
                               theZValue,
                               IsNearestPoint);

  TecUtilLockFinish(AddOnID);
}


static Boolean_t OkToCreateTimeSeriesPlot()
{
  Boolean_t Result = TecUtilDataSetIsAvailable() &&
                     (TecUtilFrameGetPlotType() == PlotType_Cartesian2D ||
                      TecUtilFrameGetPlotType() == PlotType_Cartesian3D) &&
                     FrameHasTransientData();
  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}



static void InstallTimeSeriesPlotProbeCallback(void)
{  
  TecUtilLockStart(AddOnID);

  REQUIRE(OkToCreateTimeSeriesPlot());
  
  TecUtilProbeInstallCallback(TimeSeriesPlotProbeCallback,
                              "Click to create time series plot. Use <Ctrl>+click for nearest point probe.");
  TecUtilProbeAllowCOBs();

  TecUtilLockFinish(AddOnID);
}


static void ResetLastFrameUsed(void)
{
  S_lastFrameUsed = BAD_SET_VALUE;
}

static void STDCALL KeepProbedFrameOnTopMenuCallback(ArbParam_t ClientData)
{
  S_probedFrameAlwaysOnTop = !S_probedFrameAlwaysOnTop;
  ENSURE(VALID_BOOLEAN(S_probedFrameAlwaysOnTop));
}

static Boolean_t STDCALL GetKeepProbedFrameOnTopStateCallback(ArbParam_t ClientData)
{
  ENSURE(VALID_BOOLEAN(S_probedFrameAlwaysOnTop));
  return S_probedFrameAlwaysOnTop;
}

static Boolean_t STDCALL OkToSendTimeSeriesDataToNewFrameSensitivityCallback(ArbParam_t ClientData)
{
  TecUtilLockStart(AddOnID);
  //
  // If the S_lastFrameUsed UniqueID exists in the current layout, then
  // the user should have the ability to press them menu option, which
  // resets the S_lastFrameUsed value so a new frame is created when
  // creating a time series plot.
  //
  Boolean_t LastFrameUsedExists = FALSE;
  TecUtilFrameLightweightPopStart();
  do 
    {
      if ( !LastFrameUsedExists &&
           TecUtilFrameGetUniqueID() == S_lastFrameUsed )
        {
          LastFrameUsedExists = TRUE;
        }
    } while ( TecUtilFrameLightweightPopNext() );
  TecUtilFrameLightweightPopEnd();        

  TecUtilLockFinish(AddOnID);

  ENSURE(VALID_BOOLEAN(LastFrameUsedExists));
  return LastFrameUsedExists;
}

static void STDCALL SendTimeSeriesDataToNewFrameMenuCallback(ArbParam_t ClientData)
{
  ResetLastFrameUsed();
}

static Boolean_t STDCALL OkToCreateTimeSeriesPlotSensitivityCallback(ArbParam_t ClientData)
{
  return OkToCreateTimeSeriesPlot();
}

static void STDCALL CreateTimeSeriesPlotMenuCallback(ArbParam_t ClientData)
{
  TecUtilLockStart(AddOnID);
  REQUIRE(OkToCreateTimeSeriesPlot());

  InstallTimeSeriesPlotProbeCallback();
  
  TecUtilLockFinish(AddOnID);
}

/**
 * This function is called when the
 * $!ADDONCOMMAND macro command is
 * processed.
 */
static Boolean_t STDCALL MacroCommandCallback(char *MacroCommandString,  /* IN */
                                              char **ErrMsg)             /* OUT (only if returning FALSE) */
{

  Boolean_t IsOk = TRUE;

  /* 
   * MacroCommandString is the add-on macro command string needing processing.
   *
   * *ErrMsg is an error message string which must be allocated and set by this
   * function if and only if the return value is FALSE.
   */
  
  TecUtilLockStart(AddOnID);
  
  /*
   * TODO: Process the macro command.
   *
   * Example:
   *
   * $!ADDONCOMMAND ADDONID='Time Series Plot' COMMAND='MYCOMMAND'
   */  

  if (!IsOk)
    {
      /*
       * Some kind of error, so inform the user about it.
       */
  
      *ErrMsg = TecUtilStringAlloc(1000,"String for Error Message");
      strcpy(*ErrMsg,"Error processing macro command");
    }
  else
    {
      /* Ignore the *ErrMsg parameter */
    }

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static void STDCALL StateChangeCallback(StateChange_e StateChange)
{
  
  switch (StateChange)
    {
   /*
    * This function will be called by Tecplot
    * each time a state change occurs.
    *
    *
    * NOTE:
    *
    * Some State changes also have some supplemental "state"
    * information that can be retrieved if you desire.
    * Comments in the case statements below identify these
    * state changes.  To retrieve the supplemental information
    * use the functions TecUtilStateChangeGetXXXXX. You may
    * only call these functions during the scope of this
    * callback.  Once control returns from this call the
    * supplemental information will become unaccessible.
    *
    */

      /*   State Change                Supplemental information */
      case StateChange_VarsAltered:     /* set of altered variables */
      case StateChange_VarsAdded:       /* set of added variables */
      case StateChange_ZonesDeleted:    /* set of deleted zones */
      case StateChange_ZonesAdded:      /* set of added zones */
      case StateChange_NodeMapsAltered: /* set of node maps altered */                
      case StateChange_MouseModeUpdate: /* the new mouse mode */                
      case StateChange_Style:           /* Style Parameters P1,P2,P3,P4,P5,P6 */
      case StateChange_View:            /* View action (View_e) */
      case StateChange_Streamtrace:     /* Streamtrace action (Streamtrace_e) */
      case StateChange_AuxDataAltered:  /* Name, Auxiliary Location (AuxDataLocation_e), Var/Zone/or Map Num */
      case StateChange_AuxDataAdded:    /* Name, Auxiliary Location (AuxDataLocation_e), Var/Zone/or Map Num */
      case StateChange_AuxDataDeleted:  /* Name, Auxiliary Location (AuxDataLocation_e), Var/Zone/or Map Num */
      case StateChange_VarsDeleted:     /* set of deleted variables (zero based set) */
      case StateChange_VariableLockOn:  /* Locker name, Variable Num, VarLockMode */
      case StateChange_VariableLockOff: /* Unlocker name, Variable Num */
      case StateChange_DataSetLockOn:   /* Locker name */
      case StateChange_DataSetLockOff:  /* Unlocker name */

    /* State changes which do not have any supplemental "state" information. */
      case StateChange_TecplotIsInitialized:/* Tecplot is finished initializing */
      case StateChange_FrameDeleted:        /* A frame was delete */        
      case StateChange_NewTopFrame:         /* A new frame has become the current frame */        
      case StateChange_Text:                /* One or more text elements has changed */
      case StateChange_Geom:                /* One or more geometry elements has changed */
      case StateChange_DataSetReset:        /* A new dataset has been loaded */
      case StateChange_NewLayout:           /* The current layout has been cleared and reset */
      case StateChange_CompleteReset:       /* Anything could have happened */
      case StateChange_LineMapAssignment:   /* A line mapping definition has been altered (includes zone and axis information) */
      case StateChange_ContourLevels:       /* The contour levels have been altered */
      case StateChange_ModalDialogLaunch:   /* A modal dialog has been launched */
      case StateChange_ModalDialogDismiss:  /* A modal dialog has been dismissed */
      case StateChange_QuitTecplot:         /* Tecplot is about to exit */
      case StateChange_ZoneName:            /* The name of a zone has been altered */
      case StateChange_VarName:             /* The name of a variable has been altered */
      case StateChange_LineMapName:           /* The name of an X-Y mapping has been altered */
      case StateChange_LineMapAddDeleteOrReorder: /* The set of existing X-Y mappings has been altered */
      case StateChange_ColorMap:            /* The color mapping has been altered */
      case StateChange_ContourVar:          /* The contour variable has been reassigned */
      case StateChange_NewAxisVariables:    /* The axis variables have been reassigned */
      case StateChange_PickListCleared:     /* All picked objects are unpicked */
      case StateChange_PickListGroupSelect: /* A group of objects has been added to the pick list */
      case StateChange_PickListSingleSelect:/* A single object has been added to or removed from the pick list */
      case StateChange_PickListStyle:       /* An action has been performed on all of the objects in the pick list */
      case StateChange_DataSetFileName:     /* The current data set has been saved to a file */
      case StateChange_DataSetTitle:        /* The current data set title has been changed */
      case StateChange_DrawingInterrupted:  /* The user has interrupted the drawing */
      case StateChange_ImageExported:       /* An image frame was exported */


    /* Version 9 and later Note: If you are using modeless dialogs, you should
       trap the following state changes and take appropriate
       action when print preview is launched and dismissed.

       Usually you will either disable or close your dialog
       when print preview is launched. */

      case StateChange_PrintPreviewLaunch:  /* Modeless dialogs should close or disable themselves */
      case StateChange_PrintPreviewDismiss: /* Modeless dialogs can re-launch or enable themselves */


      case StateChange_SuspendInterface:    /* Replaces StateChange_DrawGraphicsOn */
      case StateChange_UnsuspendInterface:  /* Replaces StateChange_DrawGraphicsOff */
        {
          /* TODO: Add code to handle state changes.... */
        } break;
      default: break;
    } /* end switch */
}




/**
 * When Tecplot first loads an add-on, it makes a 
 * call to initialize the add-on. This function
 * must be named InitTecAddOn, as shown below.
 */
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{


  /*
   * NOTE:  TecUtilLockOn MUST be used for InitTecAddOn instead
   *        of TecUtilLockStart because AddonID has yet to be
   *        established.  TecUtilLockOn is in effect an "anonymous"
   *        locking of Tecplot (old style).
   */

  TecUtilLockOn();

  /*
   * The function TecUtilAddOnRegister() is the
   * only function that is REQUIRED to be called from
   * the initialization function.
   *
   * The information you give Tecplot by calling
   * this function will show up in the Help/About Add-ons
   * dialog box.
   */

  /*
   * Note that if your add-on requires a specific version of Tecplot,
   * you would check for that here using TecUtilGetTecplotVersion()
   */

  AddOnID = TecUtilAddOnRegister(110,
                                 ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "Tecplot, Inc.");
  
  TecUtilMacroAddCommandCallback(ADDON_NAME,
                                 MacroCommandCallback);

  ArgList_pa ArgList;
  ArgList = TecUtilArgListAlloc();
  TecUtilArgListAppendFunction(ArgList, SV_CALLBACKFUNCTION,       (const void *)StateChangeCallback);
  TecUtilArgListAppendInt(ArgList,      SV_STATECHANGEMODE,        StateChangeMode_v100);
  TecUtilArgListAppendInt(ArgList,      SV_STATECHANGECALLBACKAPI, StateChangeCallbackAPI_ChangeOnly);
  TecUtilStateChangeAddCallbackX(ArgList);
  TecUtilArgListDealloc(&ArgList);
  
  Menu_pa ToolsMenu = TecUtilMenuGetStandard(StandardMenu_Tools);

  if ( ToolsMenu )
    {
      Menu_pa TimeSeriesMenu = TecUtilMenuInsertSubMenu(ToolsMenu,
                                                        MENU_POSITION_LAST,
                                                        "Time Series Plot");
      Menu_pa CurMenu = TecUtilMenuInsertOption(TimeSeriesMenu,
                                                MENU_POSITION_LAST,
                                                "Probe To Create Time Series Plot",
                                                CreateTimeSeriesPlotMenuCallback,
                                                0);
      TecUtilMenuRegisterSensitivityCallback(CurMenu,
		                                         OkToCreateTimeSeriesPlotSensitivityCallback,
		                                         0);
      CurMenu = TecUtilMenuInsertOption(TimeSeriesMenu,
                                        MENU_POSITION_LAST,
                                        "Send Time Series Data To New Frame",
                                        SendTimeSeriesDataToNewFrameMenuCallback,
                                        0);
      TecUtilMenuRegisterSensitivityCallback(CurMenu,
                                             OkToSendTimeSeriesDataToNewFrameSensitivityCallback,
                                             0);
      TecUtilMenuInsertToggle(TimeSeriesMenu,
                              MENU_POSITION_LAST,
                              "Keep Probed Frame On Top",
                              KeepProbedFrameOnTopMenuCallback,
                              0,
                              GetKeepProbedFrameOnTopStateCallback,
                              0);
    }



  /*
   * See note on TecUtilLockOn at start of this function.
   */
  TecUtilLockOff();
}

