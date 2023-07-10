#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include <string.h>


AddOn_pa AddOnID;

extern void UpdateExtprecDialogSensitivities(void)
{
  FrameMode_e FrameMode;

  FrameMode = TecUtilFrameGetMode();
    
  if (FrameMode == Frame_TwoD)
    {
      TecGUISetSensitivity(EndZ_TF_D1, FALSE);
      TecGUISetSensitivity(StartZ_TF_D1, FALSE);
      TecGUISetSensitivity(Z_LBL_D1, FALSE);
      TecGUISetSensitivity(VolumeOrSu_RADIO_D1,  FALSE);

    }
  if (FrameMode == Frame_ThreeD)
    {
      TecGUISetSensitivity(EndZ_TF_D1, TRUE);
      TecGUISetSensitivity(StartZ_TF_D1, TRUE);
      TecGUISetSensitivity(Z_LBL_D1, TRUE);
      TecGUISetSensitivity(VolumeOrSu_RADIO_D1, TRUE);
 
    }
  if (FrameMode == Frame_XY || FrameMode == Frame_Sketch)
    {
      TecGUIDialogDrop(Dialog1Manager);
    }
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
   * $!ADDONCOMMAND ADDONID='Extract Precise Polyline' COMMAND='MYCOMMAND'
   */
  
  if (!strcmp(MacroCommandString,"MYCOMMAND")) /* For example */
    {
      /* IsOk = ProcessMacroCommand_MYCOMMAND(); */
    }

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
static void STDCALL StateChangeCallback(StateChange_e StateChange,
                                        ArbParam_t    CallData)
{
  TecUtilLockStart(AddOnID);
  /* only listen to events if the dialog is up */
  if (TecGUIDialogIsUp(Dialog1Manager))
  {

    if (StateChange == StateChange_Style 
        || StateChange == StateChange_NewTopFrame
        || StateChange == StateChange_FrameDeleted
        || StateChange == StateChange_NewLayout)
    {
      UpdateExtprecDialogSensitivities();
      
    }
  }
  TecUtilLockFinish(AddOnID);                        
  return;
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

  AddOnID = TecUtilAddOnRegister(100,
                                 ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "Tecplot, Inc");

  /*
   * Initialize the Tecplot GUI Builder libraries.
   */
  InitTGB();
  TecUtilStateChangeAddCallback(StateChangeCallback);

  TecUtilMacroAddCommandCallback(ADDON_NAME,
                                 MacroCommandCallback);
  {
    ArgList_pa ArgList;
    ArgList = TecUtilArgListAlloc();
    TecUtilArgListAppendFunction(ArgList, SV_CALLBACKFUNCTION,       (const void *)StateChangeCallback);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGEMODE,        StateChangeMode_v100);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGECALLBACKAPI, StateChangeCallbackAPI_ChangeOnly);
/*    TecUtilStateChangeAddCallbackX(ArgList); */
    TecUtilArgListDealloc(&ArgList);
  }
  TecUtilMenuAddOption("Tools",
                       "Extract Precise Polyline",
                       '\0',
                       MenuCallback);



  /*
   * See note on TecUtilLockOn at start of this function.
   */
  TecUtilLockOff();
}

