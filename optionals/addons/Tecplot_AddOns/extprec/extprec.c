#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"
#include <stdlib.h>

Boolean_t   IsOk;

extern char FileName[500];
Boolean_t   IsNumber(char *val);

static Boolean_t GetDoubleValue(int         TextFieldID,
                                double     *DValue,
                                double      Min,
                                double      Max,
                                const char *ParamName,
                                Boolean_t   ThreeDSurf);

static Boolean_t GetIntValue(int         TextFieldID,
                             int        *IValue,
                             int         Min,
                             int         Max,
                             const char *ParamName);




static void STDCALL xtractMenuCB(void)
{
  FrameMode_e FrameMode;

  TecUtilLockStart(AddOnID);
  if (TecUtilDataSetIsAvailable())
  {
    BuildDialog1(MAINDIALOGID);
    TecGUISetSensitivity(SaveFileName_TF_D1, FALSE);
    TecGUISetSensitivity(Filename_LBL_D1, FALSE);
    TecGUISetSensitivity(PickFile_BTN_D1, FALSE);
    TecGUITextFieldSetString(StartX_TF_D1, "0");
    TecGUITextFieldSetString(EndX_TF_D1, "0");
    TecGUITextFieldSetString(StartY_TF_D1, "0");
    TecGUITextFieldSetString(EndY_TF_D1, "0");
    TecGUITextFieldSetString(StartZ_TF_D1, "0");
    TecGUITextFieldSetString(EndZ_TF_D1, "0");
    TecGUITextFieldSetString(PtsToExtract_TF_D1, "10");

    FrameMode = TecUtilFrameGetMode();
  
    if (FrameMode == Frame_TwoD)
    {
      TecGUISetSensitivity(EndZ_TF_D1, FALSE);
      TecGUISetSensitivity(StartZ_TF_D1, FALSE);
      TecGUISetSensitivity(Z_LBL_D1, FALSE);
      TecGUISetSensitivity(VolumeOrSu_RADIO_D1, FALSE);

      TecGUIDialogLaunch(Dialog1Manager);

    }
    else if (FrameMode == Frame_ThreeD)
    {
      TecGUISetSensitivity(EndZ_TF_D1, TRUE);
      TecGUISetSensitivity(StartZ_TF_D1, TRUE);
      TecGUISetSensitivity(Z_LBL_D1, TRUE);
      TecGUISetSensitivity(VolumeOrSu_RADIO_D1, TRUE);

      TecGUIDialogLaunch(Dialog1Manager);
    }
    else if (FrameMode == Frame_XY || FrameMode == Frame_Sketch)
       TecUtilDialogErrMsg("Can only extract in 2D or 3D Mode");

  }
  else
    TecUtilDialogErrMsg("No dataset is available");

  TecUtilLockFinish(AddOnID);
}
 
 


Boolean_t extractpts(void)
{
  double      xes[2];
  double      yes[2];
  double      zes[2];
  double      XMin, XMax;
  double      YMin, YMax;
  double      ZMin, ZMax;
  const LgIndex_t   ptsinline = 2;
  LgIndex_t   ptsalongline;
  Boolean_t   volume = TRUE; 
  Boolean_t   tofile;
  Boolean_t   IsOk = TRUE;
  FrameMode_e FrameMode;
  EntIndex_t  XVarNum;
  EntIndex_t  YVarNum;
  EntIndex_t  ZVarNum;
  Boolean_t   ThreeDSurf = FALSE;
  
  FrameMode = TecUtilFrameGetMode();
  if (FrameMode == Frame_ThreeD)
    {
      int Source = TecGUIRadioBoxGetToggle(VolumeOrSu_RADIO_D1);

      if (Source == 1)
        {
          volume = FALSE;
          ThreeDSurf = TRUE;
        }
      else
        volume = TRUE;
    }

  if (FrameMode == Frame_TwoD)
    volume = FALSE;

  XVarNum = TecUtilVarGetNumByAssignment('X');
  TecUtilVarGetMinMax(XVarNum, &XMin, &XMax);

  YVarNum = TecUtilVarGetNumByAssignment('Y');
  TecUtilVarGetMinMax(YVarNum, &YMin, &YMax);

  IsOk = IsOk && GetDoubleValue(StartX_TF_D1,
                                &xes[0],
                                XMin,
                                XMax,
                                "X",
                                ThreeDSurf);
  IsOk = IsOk && GetDoubleValue(EndX_TF_D1,
                                &xes[1],
                                XMin,
                                XMax,
                                "X",
                                ThreeDSurf);
  IsOk = IsOk && GetDoubleValue(StartY_TF_D1,
                                &yes[0],
                                YMin,
                                YMax,
                                "Y",
                                ThreeDSurf);
  IsOk = IsOk && GetDoubleValue(EndY_TF_D1,
                                &yes[1],
                                YMin,
                                YMax,
                                "Y",
                                ThreeDSurf);
  if (FrameMode == Frame_ThreeD)
  {
    ZVarNum = TecUtilVarGetNumByAssignment('Z');
    TecUtilVarGetMinMax(ZVarNum, &ZMin, &ZMax);
    IsOk = IsOk && GetDoubleValue(StartZ_TF_D1,
                                  &zes[0],
                                  ZMin,
                                  ZMax,
                                  "Z",
                                  ThreeDSurf);
    IsOk = IsOk && GetDoubleValue(EndZ_TF_D1,
                                  &zes[1],
                                  ZMin,
                                  ZMax,
                                  "Z",
                                  ThreeDSurf);
  }


    if (!IsOk)
      TecUtilDialogErrMsg("Please check that valid numbers make up the endpoints.\nNo new zone was extracted.");

    if (IsOk)
    {
          IsOk = IsOk && GetIntValue(PtsToExtract_TF_D1,
                          &ptsalongline,
                          1,
                          10000,
                          "Number of Points");
    
      if (IsOk)
      {
        tofile = (TecGUIRadioBoxGetToggle(ZoneOrFile_RADIO_D1) == 2);

        if (!TecUtilExtractFromPolyline(xes, yes, zes,
                                        ptsinline,
                                        volume,
                                        FALSE, /* ExtractOnlyPointsOnPolyline */
                                        FALSE, /* IncludeDistanceVariable     */
                                        ptsalongline, 
                                        tofile, 
                                        tofile?FileName:NULL))
        {
          TecUtilDialogErrMsg("Please check that your line falls \nwithin the bounds of the data.");
          IsOk = FALSE;
          if (FrameMode == Frame_ThreeD)
            TecUtilDialogErrMsg("Check that there is volume data.");
        }
      }
    }

  return IsOk;
}

Boolean_t IsNumber(char *NumberString)
{
  int       i;
  int       length = strlen(NumberString);
  Boolean_t IsNum = TRUE;

  for (i=0;i<length;i++)
  {
    if (!strchr("0123456789.+-Ee",NumberString[i]))
    {
      IsOk =  FALSE;
      IsNum = FALSE;
      break;
    }
  }
  return IsNum;
}

static Boolean_t GetDoubleValue(int         TextFieldID,
                                double     *DValue,
                                double      Min,
                                double      Max,
                                const char *ParamName,
                                Boolean_t   ThreeDSurf)
{
  Boolean_t Result = TRUE;
  char *DString = TecGUITextFieldGetString(TextFieldID);
  if (strlen(DString) > 0 && IsNumber(DString))
    {
      double D;
      D = atof(DString);
      TecUtilStringDealloc(&DString);
      if (!ThreeDSurf && ((D < Min) || (D > Max)))
        {
          char ErrMsgString[128];
          Result = FALSE;
          sprintf(ErrMsgString,
                  "Invalid %s parameter.  %s must be between %G and %G",
                  ParamName,ParamName,Min,Max);
          TecUtilDialogErrMsg(ErrMsgString);
        }
      else
        *DValue = D;
    }
  else if (strlen(DString) == 0)
    {
      char ErrMsgString[128];
      TecUtilStringDealloc(&DString);
      Result = FALSE;
      sprintf(ErrMsgString,
              "%s parameter is missing.",ParamName);
      TecUtilDialogErrMsg(ErrMsgString);
    }
  else
  {
    TecUtilStringDealloc(&DString);
    Result = FALSE;
  }

  return Result;
}


static Boolean_t GetIntValue(int         TextFieldID,
                             int        *IValue,
                             int         Min,
                             int         Max,
                             const char  *ParamName)
{
  Boolean_t Result = TRUE;
  char *IString = TecGUITextFieldGetString(TextFieldID);
  if (strlen(IString) > 0 && IsNumber(IString)) 
    {
      int I;
      I = atoi(IString);
      TecUtilStringDealloc(&IString);
      if ((I < Min) || (I > Max))
        {
          char ErrMsgString[128];
          Result = FALSE;
          sprintf(ErrMsgString,
                  "Invalid %s parameter.  %s must be between %d and %d",
                  ParamName,ParamName,Min,Max);
          TecUtilDialogErrMsg(ErrMsgString);
        }
      else
        *IValue = I;
    }
  else if (strlen(IString) == 0)
    {
      char ErrMsgString[128];
      TecUtilStringDealloc(&IString);

      Result = FALSE;
      sprintf(ErrMsgString,
              "%s parameter is missing.",ParamName);
      TecUtilDialogErrMsg(ErrMsgString);
    }
  else
  {
    TecUtilStringDealloc(&IString);
    Result = FALSE;
  }

  return Result;
}

