#include "TECADDON.h"
#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif

#include "GUIDEFS.h"
#include "ENGINE.h"
Boolean_t StopReadFile = FALSE;



/* Update sensitivities of dialog components.  */

static void UpdateMainDialogSensitivities(void)
{
  int LoadMethod = TecGUIRadioBoxGetToggle(LoadMethod_RADIO_D1); 
  Boolean_t SensitiveVars = FALSE;
  Boolean_t SensitiveFunction = FALSE;

  switch (LoadMethod)
    {
      case 1:
        {
          SensitiveVars = FALSE;
          SensitiveFunction = FALSE;
        } break;
      case 2:
        {
          SensitiveVars = TRUE;
          SensitiveFunction = FALSE;
        } break;
      case 3:
        {
          SensitiveVars = FALSE;
          SensitiveFunction = FALSE;
        } break;
      case 4:
        {
          SensitiveVars = FALSE;
          SensitiveFunction = FALSE;
        } break;
      case 5:
        {
          SensitiveVars = FALSE;
          SensitiveFunction = TRUE;
        } break;

      default:
        {
          SensitiveVars = FALSE;
          SensitiveFunction = FALSE;
        } break;
    }

  TecGUISetSensitivity(SpecifyVar_RADIO_D1, SensitiveVars);
  TecGUISetSensitivity(SpecFunction_BTN_D1, SensitiveFunction);

}




/**
 */
static void Dialog1Init_CB(void)
{
  TecUtilLockStart(AddOnID);
  UpdateMainDialogSensitivities();


}

/**  Creates a stringlist in STANDARDSYNTAX
 *   with the instructions needed to load a binary file:
 *   FileName, LoadMethod
 *   Then LoaderCallback is called with these instructions.  
 */
static void Dialog1OkButton_CB(void)
{
  LgIndex_t DataFormat;
  FILE     *MyFile; 
  Boolean_t IsOk = TRUE;
  Boolean_t Result;
  LgIndex_t LoadMethod;
  char     *FileName; 
  char      LoadMethodString[32];

  /* First check for a valid file name. */
  FileName = TecGUITextFieldGetString(FileName_TF_D1);
  if (VALID_NON_ZERO_LEN_STR(FileName))
    {
      MyFile = fopen(FileName, "rb");
      if (MyFile == NULL)
        {
          TecUtilDialogErrMsg("Bad file name from OK button.");
          IsOk = FALSE;
        }
      else
        {
          fread (&DataFormat, sizeof(int), 1, MyFile); 
          if (MyFile != NULL)
            fclose(MyFile);
        }
    }
  else
    {
      TecUtilDialogErrMsg("Enter a file name.");
      IsOk = FALSE;
    }


  LoadMethod = TecGUIRadioBoxGetToggle(LoadMethod_RADIO_D1);
  sprintf(LoadMethodString,"%d",(int)LoadMethod);

  if (IsOk == TRUE)
    {
      StringList_pa  Instructions;
      Instructions = TecUtilStringListAlloc();
      TecUtilStringListAppendString(Instructions,"STANDARDSYNTAX");
      TecUtilStringListAppendString(Instructions,"1.0");
      TecUtilStringListAppendString(Instructions,"FILENAME");
      TecUtilStringListAppendString(Instructions,FileName);
      TecUtilStringListAppendString(Instructions,"LoadMethod");
      TecUtilStringListAppendString(Instructions,LoadMethodString);

      Result = LoaderCallback(Instructions);
      TecUtilStringListDealloc(&Instructions);
      TecUtilStringDealloc(&FileName);
      if (Result)
        {
          TecGUIDialogDrop(Dialog1Manager);
        }
      else 
        TecUtilDialogErrMsg("Error loading the file." );
      TecUtilLockFinish(AddOnID);

    }

}

/**
 */
static void Dialog1CancelButton_CB(void)
{
  /* Only unlock tecplot here because a modal dialog was launched. */
  StopReadFile = FALSE;
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilHelp("LoadMethod.htm", FALSE, 0);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Browse_BTN_D1_CB(void)
{
  char *FName = NULL;
  TecUtilLockStart(AddOnID);
  if (TecUtilDialogGetFileName(SelectFileOption_ReadSingleFile,
                               &FName,
                               "Binary",
                               "",
                               "*.*"))
    {
      TecGUITextFieldSetString(FileName_TF_D1,FName);
      TecUtilStringDealloc(&FName);
    }
  
  TRACE("... Button Pushed\n");
  TecUtilLockFinish(AddOnID);
}


/**
 */
static LgIndex_t  FileName_TF_D1_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (FileName_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static void SpecFunction_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TRACE("Specify Function ... Button Pushed\n");
  TecUtilLockFinish(AddOnID);
}




/**
 */
static void DataFormat_RADIO_D2_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("RadioBox (DataFormat_RADIO_D1) Value Changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}


/**
 */
static LgIndex_t  NumPoints_TF_D2_CB(const char *S)
{
  
  LgIndex_t NumPoints; 
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (NumPoints_TF_D2) Value Changed,  New value is: %s\n",S);
  if (TecGUITextFieldGetLgIndex(NumPoints_TF_D2, &NumPoints) != TRUE)
    {
      TecUtilDialogErrMsg  ("Invalid number.");
      TecGUITextFieldSetLgIndex(NumPoints_TF_D2, 10, TRUE);
    }

    if ((NumPoints <= 0) || (NumPoints >= 2000000000))
    {
      TecUtilDialogErrMsg  ("Invalid number.");
      TecGUITextFieldSetLgIndex(NumPoints_TF_D2, 10, TRUE);
    }

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  NumVars_TF_D2_CB(const char *S)
{
  LgIndex_t NumVars; 
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (NumVars_TF_D1) Value Changed,  New value is: %s\n",S);
  if (TecGUITextFieldGetLgIndex(NumVars_TF_D2, &NumVars) != TRUE)
    {
      TecUtilDialogErrMsg  ("Invalid number.");
      TecGUITextFieldSetLgIndex(NumVars_TF_D2, 10, TRUE);
    }
  if ((NumVars <= 0)  || (NumVars >= 327000))
    {
      TecUtilDialogErrMsg  ("Invalid number.");
      TecGUITextFieldSetLgIndex(NumVars_TF_D2, 10, TRUE);
    }

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static LgIndex_t  NumZones_TF_D2_CB(const char *S)
{
  LgIndex_t NumZones; 
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (NumZones_TF_D2) Value Changed,  New value is: %s\n",S);
  if (TecGUITextFieldGetLgIndex(NumZones_TF_D2, &NumZones) != TRUE)
    {
      TecUtilDialogErrMsg  ("Invalid number.");
      TecGUITextFieldSetLgIndex(NumZones_TF_D2, 10, TRUE);
    }

  if ((NumZones <= 0) || (NumZones >= 32700))
    {
      TecUtilDialogErrMsg  ("Invalid number.");
      TecGUITextFieldSetLgIndex(NumZones_TF_D2, 10, TRUE);
    }
  TecUtilLockFinish(AddOnID);
  return (IsOk);

}



/**
 */
static void DataType_RADIO_D2_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("RadioBox (SizeOfVars_RADIO_D2) Value Changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}


 

/**
 */
static void CreateFile_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TRACE("Create Test File ... Button Pushed\n");
  BuildDialog2(Dialog1Manager);
  TecGUIDialogLaunch(Dialog2Manager);
  TecUtilLockFinish(AddOnID);
}



/**
 */
static void LoadMethod_RADIO_D1_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("RadioBox (LoadMethod_RADIO_D1) Value Changed,  New value is: %d\n",*I);
  UpdateMainDialogSensitivities();
  TecUtilLockFinish(AddOnID);
}


/**
 */
static void SpecifyVar_RADIO_D1_CB(const LgIndex_t *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("RadioBox (SpecifyVar_RADIO_D1) Value Changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}



/**
 */
static void Dialog2HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}


/**
 */
static void Dialog2CancelButton_CB(void)
{
  TecGUIDialogDrop(Dialog2Manager);

  TecUtilLockFinish(AddOnID);
}


/**  Creates a binary test file with a name that reflects the instructions
 *   needed to loaded it :  Format, NumPoints, NumVars, NumZones, DataType.
 *   The file header also begins with this information.  
 */

static void Dialog2OkButton_CB(void)
{
  int       PointIndex;
  int       VarIndex;
  int       ZoneIndex;
  LgIndex_t NumZones;
  LgIndex_t NumVars;
  LgIndex_t NumPoints;
  LgIndex_t DataType;
  LgIndex_t DataFormat;
  FILE     *MyFile; 

  char      FileNameString[35];
  char      DataTypeString[5];
  char      DataFormatString[5];
  char      NumPointsString[12];
  char      NumVarsString[7];
  char      NumZonesString[7];
  
  if (TecGUITextFieldGetLgIndex(NumPoints_TF_D2, &NumPoints) != TRUE)
    {
      NumPoints = 100;
    }
  sprintf(NumPointsString, "%d", NumPoints);

  if (TecGUITextFieldGetLgIndex(NumVars_TF_D2, &NumVars) != TRUE)
    {
      NumVars = 3;
    }
  sprintf(NumVarsString, "%d", NumVars);
  
  if (TecGUITextFieldGetLgIndex(NumZones_TF_D2, &NumZones) != TRUE)
    {
      NumZones = 1;
    }
  sprintf(NumZonesString, "%d", NumZones);

  DataFormat = TecGUIRadioBoxGetToggle(DataFormat_RADIO_D2);
  switch (DataFormat)
    {
    case 1:
      strcpy(DataFormatString, "BK");
      break;
    case 2:
      strcpy(DataFormatString, "PT");
      break;
   default:
      TecUtilDialogErrMsg("No data format.  This shouldn't happen.");
      break;
 
    }

  DataType = TecGUIRadioBoxGetToggle(DataType_RADIO_D2);
  switch (DataType)
    {
    case 1:
      strcpy(DataTypeString, "int");
      break;
    case 2:
      strcpy(DataTypeString, "flt");
      break;
    case 3:
      strcpy(DataTypeString, "dbl");
      break;
    default:
      TecUtilDialogErrMsg("No data type.  This shouldn't happen.");
      break;
    }

  strcpy(FileNameString, DataFormatString);
  strcat(FileNameString, "_");
  strcat(FileNameString, NumPointsString);
  strcat(FileNameString, "_");
  strcat(FileNameString, NumVarsString);
  strcat(FileNameString, "_");
  strcat(FileNameString, NumZonesString);
  strcat(FileNameString, "_");
  strcat(FileNameString, DataTypeString);
  strcat(FileNameString, ".bin");

  MyFile = fopen(FileNameString,"wb");
  fwrite(&DataFormat, sizeof(int),1,MyFile);
  fwrite(&NumPoints,  sizeof(int),1,MyFile);
  fwrite(&NumVars,    sizeof(int),1,MyFile);
  fwrite(&NumZones,   sizeof(int),1,MyFile);
  fwrite(&DataType,   sizeof(int),1,MyFile);


  if (DataFormat == 1)  /* Block format */
    {
      TRACE("Writing block file\n");
      if (DataType == 1)  /* Integer values */
        {
          for (ZoneIndex=0;ZoneIndex<NumZones;ZoneIndex++)
            {
              for (VarIndex=0;VarIndex<NumVars;VarIndex++)
                {
                  for ( PointIndex=0;PointIndex<NumPoints;PointIndex++ )
                    {
                      int Value = (int)(((VarIndex -1) + (PointIndex -1)) * ZoneIndex);
                      fwrite(&Value,sizeof(int),1,MyFile);
                    }
                }
            }
        }
      else if (DataType == 2) /* Float values */
        {
           for (ZoneIndex=0;ZoneIndex<NumZones;ZoneIndex++)
              {
                TRACE1("Zone %d \n", ZoneIndex+1);
                 for (VarIndex=0;VarIndex<NumVars;VarIndex++)
                   {
                      for ( PointIndex=0;PointIndex<NumPoints;PointIndex++ )
                        {
                          float Value = 2.0F * (float) ZoneIndex - (float)(sin(PointIndex + 2*VarIndex + ZoneIndex)*10);
                          fwrite(&Value,sizeof(float),1,MyFile);
                        }
                      
                   }
              }
        }
      else if (DataType == 3) /* Double values. */
        {
           for (ZoneIndex=0;ZoneIndex<NumZones;ZoneIndex++)
              {
                 for (VarIndex=0;VarIndex<NumVars;VarIndex++)
                   {
                      for ( PointIndex=0;PointIndex<NumPoints;PointIndex++ )
                        {
                          double Value = (double) ZoneIndex + (double)(sin(PointIndex + 2*VarIndex)*10);
                          fwrite(&Value,sizeof(double),1,MyFile);
                        }
                   }
              }
        }
  
    }


  else if (DataFormat == 2) /* Point format */
    {
      TRACE("Writing point file\n");
      if (DataType == 1) /* Integer values */
        {
        
          for (ZoneIndex=0;ZoneIndex<NumZones;ZoneIndex++)
            {
              for ( PointIndex=0;PointIndex<NumPoints;PointIndex++ )
                {
                  for (VarIndex=0;VarIndex<NumVars;VarIndex++)
                    {
                      int Value = (int)(((VarIndex -1) + (PointIndex -1)) * ZoneIndex);
                      fwrite(&Value,sizeof(int),1,MyFile);
                    }
                }
            }

        }
      else if (DataType ==2) /* Float values */
        {

          for (ZoneIndex=0;ZoneIndex<NumZones;ZoneIndex++)
            {
              for ( PointIndex=0;PointIndex<NumPoints;PointIndex++ )
                {
                  for (VarIndex=0;VarIndex<NumVars;VarIndex++)
                    {
                      float Value = 2.0F * (float) ZoneIndex - (float)(sin(PointIndex + 2*VarIndex + ZoneIndex)*10);
                      
                      fwrite(&Value,sizeof(float),1,MyFile);
                    }
                  
                }

            }
        }
      else if (DataType == 3) /* Double values */ 
        {
          for (ZoneIndex=0;ZoneIndex<NumZones;ZoneIndex++)
            {
              for ( PointIndex=0;PointIndex<NumPoints;PointIndex++ )
                {
                  for (VarIndex=0;VarIndex<NumVars;VarIndex++)
                    {
                      double Value = (double) ZoneIndex + (double)(sin(PointIndex + 2*VarIndex)*10);
                      fwrite(&Value,sizeof(double),1,MyFile);
                    }
                }

            }
          
        }
      else 
        TecUtilDialogErrMsg("Wrong data type.  This shouldn't happen.");

      }

   else
      TecUtilDialogErrMsg("Wrong data format.  This shouldn't happen.");
  if (MyFile != NULL)
    fclose(MyFile);
  TecUtilDialogMessageBox( "File was created.", MessageBox_Information); 
  TecGUIDialogDrop(Dialog2Manager);

  TecUtilLockFinish(AddOnID);
}


/**
 */
static void Dialog2Init_CB(void)
{

  TecUtilLockStart(AddOnID);

}





#include "guibld.c"

