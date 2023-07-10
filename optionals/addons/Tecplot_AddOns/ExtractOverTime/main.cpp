#include "TECADDON.h"
#include "ADDGLBL.h"
#include <string.h>
#include "TecUtilTime.h"
#include "TecUtilSet.h"

AddOn_pa AddOnID;


typedef Boolean_t (STDCALL *ExtractionFunction_pf)(void);

static Boolean_t STDCALL ExtractStream(void)
{
  return TecUtilCreateStreamZones(TRUE);
}

const int DefaultNumPtsToExtractAlongPolyLine = 100;
static int NumPtsToExtractAlongPolyline = DefaultNumPtsToExtractAlongPolyLine;
static Boolean_t STDCALL ExtractFromGeom(void)
{
  return TecUtilExtractFromGeom(FALSE, // ExtractOnlyPointsOnPolyline,
		                            FALSE, // IncludeDistanceVariable,
		                            NumPtsToExtractAlongPolyline,
		                            FALSE, // ExtractToFile,
		                            NULL); // ExtractFName
}

static Boolean_t ExtractOverTime(const char *ObjectName,
                                 ExtractionFunction_pf ExtractionFunction)
{
  REQUIRE(VALID_REF(ObjectName));
  REQUIRE(VALID_REF(ExtractionFunction));
  Boolean_t Result = FALSE;
  TecUtilLockStart(AddOnID);  

  if ( TecUtilDataSetIsAvailable() )
    {
      TecUtilWorkAreaSuspend(TRUE);
      TecUtilInterfaceSuspend(TRUE);

      CTecUtilTime Time;
      vector<double> SolutionTimes;
      Time.GetSolutionTimes(SolutionTimes);
                  
      vector<double> UsedSolutionTimes;
      vector<CTecUtilSet*> ZoneSets;

      TecUtilStatusStartPercentDone("",
                                    TRUE,
                                    TRUE);
      double OrigSolutionTime = TecUtilSolutionTimeGetCurrent();

      Boolean_t Interrupted = FALSE;

      for ( int ii = 0; ii < SolutionTimes.size() && !Interrupted; ii++ )
        {                      
          int PercentDone = (int)((double)(((double)ii+1)/(double)SolutionTimes.size())*100);
          TecUtilStatusCheckPercentDone(PercentDone);
          char Msg[4096];
          sprintf(Msg, "Collecting %ss at Time %f...", ObjectName, SolutionTimes[ii]);          
          TecUtilStatusSetPercentDoneText(Msg);
          // Suspend the status line so Tecplot doesn't take it over
          TecUtilStatusSuspend(TRUE);

          TecUtilDataLoadBegin();

          TecUtilSolutionTimeSetCurrent(SolutionTimes[ii]);          
          EntIndex_t LastNumZones;
          TecUtilDataSetGetInfo(NULL, &LastNumZones, NULL);
          
          if ( ExtractionFunction() )
            {
              EntIndex_t CurNumZones;
              TecUtilDataSetGetInfo(NULL, &CurNumZones, NULL);
              CTecUtilSet *CurZoneSet = new CTecUtilSet;
              for ( int jj = LastNumZones; jj < CurNumZones; jj++ )
                {
                  CurZoneSet->Add(jj+1);
                }              
              UsedSolutionTimes.push_back(SolutionTimes[ii]);
              ZoneSets.push_back(CurZoneSet);
            }          
          TecUtilDataLoadEnd();
          Interrupted = TecUtilInterruptCheck();

          // Don't forget to unsuspend so we can write to it again.
          TecUtilStatusSuspend(FALSE);
        }

      if ( ZoneSets.size() > 0 )
        {
          if ( !Interrupted )
            {
              char Msg[4096];
              //sprintf(Msg, "Concatenating %s Zones Into a Single Strand...", ObjectName, SolutionTimes[ii]);
              sprintf(Msg, "Concatenating %s Zones Into a Single Strand...", ObjectName);
              TecUtilStatusSetPercentDoneText(Msg);
              Time.ConcatenateZonesIntoStrand(Time.GetMaxStrandID()+1,
                                              UsedSolutionTimes,
                                              ZoneSets);
              Result = TRUE;
            }
          // Always delete the zones that were created, especially if the action was interrupted
          CTecUtilSet ZonesToDelete;
          for ( int ii = 0; ii < ZoneSets.size(); ii++ )
            {
              EntIndex_t Zone;
              TecUtilSetForEachMember(Zone, (ZoneSets[ii])->GetSet())
                {
                  ZonesToDelete.Add(Zone);
                }
              delete ZoneSets[ii];
            }
          TecUtilDataSetDeleteZone(ZonesToDelete.GetSet());
        }
      TecUtilSolutionTimeSetCurrent(OrigSolutionTime);

      TecUtilStatusFinishPercentDone();

      TecUtilInterfaceSuspend(FALSE);
      TecUtilWorkAreaSuspend(FALSE);
    }
  TecUtilLockFinish(AddOnID);
  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}

const char *MacroCmd_ExtractGeomOverTime       = "ExtractGeomOverTime";
const char *MacroCmd_ExtractStreamOverTime     = "ExtractStreamOverTime";
const char *MacroCmd_ExtractSliceOverTime      = "ExtractSliceOverTime";
const char *MacroCmd_ExtractIsoSurfaceOverTime = "ExtractIsoSurfaceOverTime";

static Boolean_t Action_ExtractGeomOverTime(void)
{
  Boolean_t Result = ExtractOverTime("Geom", ExtractFromGeom);
  if ( Result && TecUtilMacroIsRecordingActive() )
    {
      char Cmd[1024];
      sprintf(Cmd, "%s:%d", MacroCmd_ExtractGeomOverTime, NumPtsToExtractAlongPolyline);
      TecUtilMacroRecordAddOnCommand(ADDON_NAME, Cmd);
    }
    
  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}

static Boolean_t Action_ExtractStreamOverTime(void)
{
  Boolean_t Result = (TecUtilFrameGetPlotType() == PlotType_Cartesian3D ||
                      TecUtilFrameGetPlotType() == PlotType_Cartesian2D) &&
                     ExtractOverTime("Streamtrace", ExtractStream);
  if ( Result && TecUtilMacroIsRecordingActive() )
    {
      TecUtilMacroRecordAddOnCommand(ADDON_NAME, MacroCmd_ExtractStreamOverTime);
    }
  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}

static Boolean_t Action_ExtractSliceOverTime(void)
{
  Boolean_t Result = TecUtilFrameGetPlotType() == PlotType_Cartesian3D &&
                     ExtractOverTime("Slice", TecUtilCreateSliceZones);
  if ( Result && TecUtilMacroIsRecordingActive() )
    {
      TecUtilMacroRecordAddOnCommand(ADDON_NAME, MacroCmd_ExtractSliceOverTime);
    }
  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}

static Boolean_t Action_ExtractIsoSurfaceOverTime(void)
{
  Boolean_t Result = TecUtilFrameGetPlotType() == PlotType_Cartesian3D &&
                     ExtractOverTime("Iso-Surface", TecUtilCreateIsoZones);
  if ( Result && TecUtilMacroIsRecordingActive() )
    {
      TecUtilMacroRecordAddOnCommand(ADDON_NAME, MacroCmd_ExtractIsoSurfaceOverTime);
    }
  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}

static Boolean_t PromptForNumPointsToExtractFromPolyline(void)
{
  char DefaultNumPts[256];
  sprintf(DefaultNumPts, "%d", NumPtsToExtractAlongPolyline);

  char *StrNumPts = NULL;
  Boolean_t IsOk = TecUtilDialogGetSimpleText("Enter the number of points to extract along the line.",
                                              DefaultNumPts,
                                              &StrNumPts);
  if ( IsOk )
    {
      int NewNumPts = atoi(StrNumPts);
      if ( NewNumPts >= 2 )
        NumPtsToExtractAlongPolyline = NewNumPts;
      else
        {
          TecUtilDialogErrMsg("Invalid value. You must enter a number >= 2");
          IsOk = PromptForNumPointsToExtractFromPolyline();
        }
    }
  ENSURE(VALID_BOOLEAN(IsOk));
  return IsOk;
}

static void STDCALL ExtractGeomOverTimeMenuCallback(void)
{
  TecUtilLockStart(AddOnID);
  
  if ( PromptForNumPointsToExtractFromPolyline() )
    {
      if ( !Action_ExtractGeomOverTime() )
        {
          TecUtilDialogErrMsg("No zones were extracted. You must have a polyline geometry selected."); 
        }
    }
  TecUtilLockFinish(AddOnID);
}

static void STDCALL ExtractStreamOverTimeMenuCallback(void)
{  
  TecUtilLockStart(AddOnID);
  if ( !Action_ExtractStreamOverTime() )
    {
      TecUtilDialogErrMsg("No streamtraces were extracted. You must be in 2D or 3D and have at least one streamtrace plotted.");
    }
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void STDCALL ExtractSliceOverTimeMenuCallback(void)
{ 
  TecUtilLockStart(AddOnID);
  if ( !Action_ExtractSliceOverTime() )
    {
      TecUtilDialogErrMsg("No slices were extracted. You must be in 3D and have at least one slice plotted.");
    }
  TecUtilLockFinish(AddOnID);
}

static void STDCALL ExtractIsoSurfaceOverTimeMenuCallback(void)
{
  TecUtilLockStart(AddOnID);
  if ( !Action_ExtractIsoSurfaceOverTime() )
    {
      TecUtilDialogErrMsg("No iso-surfaces were extracted. You must be in 3D and have at least one iso-surface plotted.");
    }
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
   * $!ADDONCOMMAND ADDONID='Extract Over Time' COMMAND='MYCOMMAND'
   */
  
  //if ( _strnicmp(MacroCommandString, MacroCmd_ExtractGeomOverTime, strlen(MacroCmd_ExtractGeomOverTime)) == 0 )
  if ( strncasecmp(MacroCommandString, MacroCmd_ExtractGeomOverTime, strlen(MacroCmd_ExtractGeomOverTime)) == 0 )
    {
      NumPtsToExtractAlongPolyline = DefaultNumPtsToExtractAlongPolyLine;

      // Search for the NumPts entry, the form of the command is:
      //  "ExtractGeomOverTime:nnn"
      char *CPtr = MacroCommandString;
      // Advance to the colon
      while ( *CPtr && *CPtr != ':' )
        CPtr++;

      Boolean_t FoundColon = FALSE;
      if ( *CPtr == ':' )
        {
          FoundColon = TRUE;
          CPtr++;
        }

      if ( CPtr && *CPtr && FoundColon )
        {          
          int NewNumPts = atoi(CPtr);
          if ( NewNumPts >= 2 )
            NumPtsToExtractAlongPolyline = NewNumPts;
        }
      IsOk = Action_ExtractGeomOverTime();
    }
  else if ( strcasecmp(MacroCommandString, MacroCmd_ExtractStreamOverTime) == 0 )
    {
      IsOk = Action_ExtractStreamOverTime();
    }
  else if ( strcasecmp(MacroCommandString, MacroCmd_ExtractSliceOverTime) == 0 )
    {
      IsOk = Action_ExtractSliceOverTime();
    }
  else if ( strcasecmp(MacroCommandString, MacroCmd_ExtractIsoSurfaceOverTime) == 0 )
    {
      IsOk = Action_ExtractIsoSurfaceOverTime();
    }

  if (!IsOk)
    {
      /*
       * Some kind of error, so inform the user about it.
       */
  
      *ErrMsg = TecUtilStringAlloc(1000,"String for Error Message");
      sprintf(*ErrMsg,"Error processing %s command", MacroCommandString);
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
  {
    ArgList_pa ArgList;
    ArgList = TecUtilArgListAlloc();
    TecUtilArgListAppendFunction(ArgList, SV_CALLBACKFUNCTION,       (const void *)StateChangeCallback);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGEMODE,        StateChangeMode_v100);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGECALLBACKAPI, StateChangeCallbackAPI_ChangeOnly);
    TecUtilStateChangeAddCallbackX(ArgList);
    TecUtilArgListDealloc(&ArgList);
  }

  if ( !TecUtilMacroIsBatchModeActive() )
    {
      TecUtilMenuAddSeparator("Data\nExtract");
      TecUtilMenuAddOption("Data\nExtract",
                           "Extract Slices Over Time",
                           '\0',
                           ExtractSliceOverTimeMenuCallback);
      TecUtilMenuAddOption("Data\nExtract",
                           "Extract IsoSurfaces Over Time",
                           '\0',
                           ExtractIsoSurfaceOverTimeMenuCallback);
      TecUtilMenuAddOption("Data\nExtract",
                           "Extract Geom Over Time",
                           '\0',
                           ExtractGeomOverTimeMenuCallback);
      TecUtilMenuAddOption("Data\nExtract",
                           "Extract Streamtraces Over Time",
                           '\0',
                           ExtractStreamOverTimeMenuCallback);
    }

  /*
   * See note on TecUtilLockOn at start of this function.
   */
  TecUtilLockOff();
}

