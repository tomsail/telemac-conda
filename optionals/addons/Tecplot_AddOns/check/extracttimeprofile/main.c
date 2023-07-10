#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include <string.h>


AddOn_pa AddOnID;

static Boolean_t IsTransientDataSet()
{
  return (TecUtilDataSetGetMaxStrandID()>0);
}
static Boolean_t CheckIfExtractionIsOk()
{
   Boolean_t Ok ;
   Ok = TecUtilDataSetIsAvailable() &&
        (TecUtilFrameGetPlotType() == PlotType_Cartesian2D ||
        TecUtilFrameGetPlotType() == PlotType_Cartesian3D);
   if(Ok) Ok = IsTransientDataSet();
   ENSURE(VALID_BOOLEAN(Ok));
   return Ok; 
}


/**
 * This function is called when the
 * $!ADDONCOMMAND macro command is
 * processed.
 */
static Boolean_t STDCALL MacroLineCommandCallback(char *MacroCommandString,  /* IN */
                                              char **ErrMsg)             /* OUT (only if returning FALSE) */
{

  Boolean_t IsOk = TRUE;
  double X[2],Y[2],Z[2];
  int NbPoints;
  Boolean_t extraction();
  Boolean_t ResIsTransient;
  char str[70];

  TecUtilLockStart(AddOnID);
  *ErrMsg = TecUtilStringAlloc(1000,"String for Error Message");
  
  IsOk = FALSE;
  if(sscanf(MacroCommandString," %d %d %lf %lf %lf %lf %lf %lf",
	      &ResIsTransient,&NbPoints,&X[0],&Y[0],&Z[0],&X[1],&Y[1],&Z[1]))
  {
        sprintf(str," Nb Points : %d \n %lf %lf %lf \n %lf %lf %lf \n",
	      NbPoints,X[0],Y[0],Z[0],X[1],Y[1],Z[1]);
        TRACE(str);
      if(CheckIfExtractionIsOk())
       IsOk = extraction(&X,&Y,&Z,NbPoints,ResIsTransient);
      sprintf(str,"ok? %d \n",IsOk);
      TRACE(str);
  }
  else
  {
      strcpy(*ErrMsg,"Bad Parameters for ExtractTimeProfileFromLine");
  }
  

  if(IsOk) TecUtilStringDealloc(ErrMsg);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


static Boolean_t STDCALL MacroPointCommandCallback(char *MacroCommandString,  /* IN */
                                              char **ErrMsg)             /* OUT (only if returning FALSE) */
{

  Boolean_t IsOk = TRUE;
  double X,Y,Z;
  char Message[50];
  Boolean_t extraction();

  TecUtilLockStart(AddOnID);
  *ErrMsg = TecUtilStringAlloc(1000,"String for Error Message");
  
  IsOk = FALSE;
  if(sscanf(MacroCommandString,"%lf %lf %lf",&X,&Y,&Z))
  {
//      TRACE("macro command : \n");
//      TRACE(MacroCommandString);
//      sprintf(Message,"coord : %lf %lf %lf\n ",X,Y,Z);
//      TRACE(Message);
      if(CheckIfExtractionIsOk())
      {

       IsOk = extraction(&X,&Y,&Z,1,FALSE);

       if(!IsOk) strcpy(*ErrMsg,"Error executing macro ExtractTimeProfileFromPoint");
      }
      else
      {
      strcpy(*ErrMsg,"Bad Plottype or data set is not transient.");
      }
  }
  else
  {
      strcpy(*ErrMsg,"Bad Parameters for ExtractTimeProfileFromPoint");
  }

  TecUtilLockFinish(AddOnID);
  if(IsOk) TecUtilStringDealloc(ErrMsg);
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
 */
static void STDCALL MenuCallback(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
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
                                 "EDF R&D");

  /*
   * Initialize the Tecplot GUI Builder libraries.
   */
  InitTGB();


  TecUtilMacroAddCommandCallback("ExtractTimeProfileFromPoint",
                                 MacroPointCommandCallback);
  TecUtilMacroAddCommandCallback("ExtractTimeProfileFromLine",
                                 MacroLineCommandCallback);


  {
    ArgList_pa ArgList;
    ArgList = TecUtilArgListAlloc();
    TecUtilArgListAppendFunction(ArgList, SV_CALLBACKFUNCTION,       (const void *)StateChangeCallback);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGEMODE,        StateChangeMode_v100);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGECALLBACKAPI, StateChangeCallbackAPI_ChangeOnly);
    TecUtilStateChangeAddCallbackX(ArgList);
    TecUtilArgListDealloc(&ArgList);
  }


  Menu_pa ToolsMenu, ExtractMenu;
  ToolsMenu = TecUtilMenuGetStandard(StandardMenu_Tools);
  ExtractMenu = TecUtilMenuInsertOption(ToolsMenu,
                                       MENU_POSITION_LAST,
				       "Extract Time Profile",
				       MenuCallback, 0);

   TecUtilMenuRegisterSensitivityCallback(ExtractMenu,
	   CheckIfExtractionIsOk, 0);


  /*
   * See note on TecUtilLockOn at start of this function.
   */
  TecUtilLockOff();
}

