#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUI.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"
#include "EXTPREC.h"
#include "MAIN.h"

char FileName[500];

extern Boolean_t SetupExtract(void);

static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilHelp("extprec.html", FALSE, 0);
  TecUtilLockFinish(AddOnID);
}


static void Dialog1CloseButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}


static void Dialog1Init_CB(void)
{
  FrameMode_e FrameMode;

  TecUtilLockStart(AddOnID);
  FrameMode = TecUtilFrameGetMode();

  if (FrameMode == Frame_XY || FrameMode == Frame_Sketch)
    {
      TecUtilDialogErrMsg("Can only extract in 2D or 3D Mode");
      TecGUIDialogDrop(Dialog1Manager);
    }
  else
    {
#ifdef _DEBUG
      /* Test bug 6602: Create a 10x10x10, then try to extract to a zone
       * with these settings.
       */
      TecGUITextFieldSetString(StartX_TF_D1, "0.2");
      TecGUITextFieldSetString(StartY_TF_D1, "0.1");
      TecGUITextFieldSetString(StartZ_TF_D1, "1.0");

      TecGUITextFieldSetString(EndX_TF_D1, "0.2");
      TecGUITextFieldSetString(EndY_TF_D1, "0.9");
      TecGUITextFieldSetString(EndZ_TF_D1, "1.0");

      TecGUITextFieldSetString(PtsToExtract_TF_D1, "20");
#endif  
      UpdateExtprecDialogSensitivities();
    }
  TecUtilLockFinish(AddOnID);
}


static int  StartX_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (StartX_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


static int  StartY_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (StartY_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


static int  StartZ_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (StartZ_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


static int  EndX_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (EndX_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


static int  EndZ_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (EndZ_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


static int  EndY_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (EndY_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


static void VolumeOrSu_RADIO_D1_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("RadioBox (VolumeOrSu_RADIO_D1) Value Changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}


static void ZoneOrFile_RADIO_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);

  if (TecGUIRadioBoxGetToggle(ZoneOrFile_RADIO_D1) == 1)
  {
    TecGUISetSensitivity(SaveFileName_TF_D1, FALSE);
    TecGUISetSensitivity(Filename_LBL_D1, FALSE);
    TecGUISetSensitivity(PickFile_BTN_D1, FALSE);
  }
  if (TecGUIRadioBoxGetToggle(ZoneOrFile_RADIO_D1) == 2)
  {
    TecGUISetSensitivity(SaveFileName_TF_D1, TRUE);
    TecGUISetSensitivity(Filename_LBL_D1, TRUE);
    TecGUISetSensitivity(PickFile_BTN_D1, TRUE);
  }


  TRACE1("RadioBox (ZoneOrFile_RADIO_D1) Value Changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}


static void Extract_BTN_D1_CB(void)
{
  FrameMode_e FrameMode;

  TecUtilLockStart(AddOnID);

  if (strlen(FileName) < 1 && TecGUIRadioBoxGetToggle(ZoneOrFile_RADIO_D1) == 2)
    TecUtilDialogErrMsg("Please enter a file name");

  else
  {
      FrameMode = TecUtilFrameGetMode();

    if (FrameMode == Frame_ThreeD || FrameMode == Frame_TwoD)
      extractpts();
    else
      TecUtilDialogErrMsg("Frame must be in 2D or 3D Mode");
  }

  TecUtilLockFinish(AddOnID);
}


static int  PtsToExtract_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (PtsToExtract_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


static void PickFile_BTN_D1_CB(void)
{
   char        *fname            = NULL;
   static char *previousFileName = NULL;
   char        *type             = "Tecplot ASCII";
   char        *filter           = "*.dat";

  TecUtilLockStart(AddOnID);
  if (TecUtilDialogGetFileName(SelectFileOption_WriteFile,
                               &fname, 
                               type, 
                               NULL, /* Default file name */
                               filter))
    {
      strcpy (FileName, fname);
      if (previousFileName != NULL)
        {
          TecUtilStringDealloc(&previousFileName);
        }
      previousFileName = fname;
      TecGUITextFieldSetString(SaveFileName_TF_D1, FileName);
    }
    
   TecUtilLockFinish(AddOnID);
}


static int  SaveFileName_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  strcpy(FileName,S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


#include "guibld.c"
