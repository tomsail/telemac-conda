#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "GUIDEFS.h"
#include "ENGINE.h"
#include "ADKUTIL.h"

#include <string.h>



/*********************************************************************
 * Functions used with Custom Load on Demand to load, unload and 
 * cleanup variables from Block format.  Tecplot passes a field data
 * pointer containing specific client data to each function each 
 * time a variable is loaded or unloaded. 
 * The client data contains the following: the file name, the offset 
 * to the first value for the current variable, the number of points, 
 * and the data type.
 * 
 *********************************************************************/

/**
 * LoadOnDemandVarLoadBlock retrieves all values for one variable
 * from a file in Block format.  As soon as a value is read, it is 
 * added to the Tecplot dataset.
 */
static Boolean_t STDCALL LoadOnDemandVarLoadBlock(FieldData_pa FieldData)
{
  LgIndex_t                PointIndex = 1;
  int                      NumValuesRead; 
  BlockClientDataValues_s *MyClientData; 
  FILE                    *MyFile;
  Boolean_t                IsOk = TRUE;

  REQUIRE(FieldData != NULL);
  REQUIRE(VALID_REF((BlockClientDataValues_s *)TecUtilDataValueGetClientData(FieldData)));

  TecUtilLockStart(AddOnID);

  MyClientData = (BlockClientDataValues_s *)TecUtilDataValueGetClientData(FieldData);
  MyFile = fopen(MyClientData->FileName, "rb");
  IsOk = (MyFile != NULL);
  if (IsOk)
    /* Go to the position in the file for the first value of the variable. */
    IsOk = (fseek(MyFile, MyClientData->CurOffset, SEEK_SET) == 0);
  if (IsOk)
    {
      FieldDataType_e DataType  = MyClientData->DataType;
      int             NumPoints = MyClientData->NumPoints;

      if (DataType == FieldDataType_Int32)  /* Integer */
        {
          int Value;

          for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
            {
              NumValuesRead = (int)fread (&Value, sizeof(int), 1, MyFile); 
              if (NumValuesRead == 1)
                {
                  TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
                }
              else
                {
                  IsOk = FALSE;
                  TecUtilDialogErrMsg("Binary Value not read correctly.");
                }
            }

        }
      else if (DataType == FieldDataType_Float)  /* Float */
        {
          float Value;

          for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
            {
              NumValuesRead = (int)fread (&Value, sizeof(float), 1, MyFile); 
              if (NumValuesRead == 1)
                {
                  TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
                }
              else
                {
                  IsOk = FALSE;
                  TecUtilDialogErrMsg("Binary Value not read correctly.");
                }
            }
        }

      else if (DataType == FieldDataType_Double) /* Double */
        {
          double Value;

          for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
            {

              NumValuesRead = (int)fread (&Value, sizeof(double), 1, MyFile); 
              if (NumValuesRead == 1)
                {
                  TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
                }
              else
                {
                  IsOk = FALSE;
                  TecUtilDialogErrMsg("Binary Value not read correctly.");
                }
            }
        }
      else
        TecUtilDialogErrMsg("Invalid data type.");
    }

  if (MyFile != NULL)
    fclose(MyFile);

  TecUtilLockFinish(AddOnID);

  ENSURE(VALID_BOOLEAN(IsOk));
  return IsOk;
}
      
/**
 */
static void STDCALL LoadOnDemandVarCleanupBlock(FieldData_pa FieldData)
{
  BlockClientDataValues_s *MyClientData;

  REQUIRE(FieldData != NULL);
  REQUIRE(VALID_REF((BlockClientDataValues_s *)TecUtilDataValueGetClientData(FieldData)));

  TecUtilLockStart(AddOnID);

  MyClientData = (BlockClientDataValues_s *)TecUtilDataValueGetClientData(FieldData);
  free(MyClientData->FileName);
  free(MyClientData);
  TRACE("Load on Demand Var Cleanup callback.\n");

  TecUtilLockFinish(AddOnID);
}

/*********************************************************************
 * Functions used with Custom Load on Demand to load, unload and 
 * cleanup variables from Point format.  Tecplot passes a field data
 * pointer containing specific client data to each function each 
 * time a variable is loaded or unloaded. 
 * The client data contains the following: the file name, the offset 
 * to the first value for the current variable, the distance between 
 * values for a variable (stride), the number of points, and the 
 * data type.
 *********************************************************************/

/**
 * LoadOnDemandVarLoadPoint retrieves all values for one variable
 * from a file in Point format.  As soon as a value is read, it is 
 * added to the Tecplot dataset.
 */ 
static Boolean_t STDCALL LoadOnDemandVarLoadPoint(FieldData_pa FieldData)
{
  int                      PointIndex;
  int                      NumValuesRead; 
  int                      CurOffset;
  PointClientDataValues_s *MyClientData;
  FILE                    *MyFile;
  Boolean_t                IsOk = TRUE;

  REQUIRE(FieldData != NULL);
  REQUIRE(VALID_REF((PointClientDataValues_s *)TecUtilDataValueGetClientData(FieldData)));

  TecUtilLockStart(AddOnID);

  MyClientData = (PointClientDataValues_s *)TecUtilDataValueGetClientData(FieldData);
  MyFile  = fopen(MyClientData->FileName, "rb");
  IsOk = (MyFile != NULL);
  CurOffset = MyClientData->CurOffset;

  /* Go to the file position for the first value of the variable. */
  IsOk = IsOk && (fseek(MyFile, CurOffset, SEEK_SET) == 0);
  if (IsOk)
    {
      FieldDataType_e DataType    =  MyClientData->DataType;
      int             NumPoints   =  MyClientData->NumPoints;
      int             PointOffset =  MyClientData->PointOffset;

      if (DataType == FieldDataType_Int32) /* integer values */
        {
          int Value;

          for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
            {
              NumValuesRead = (int)fread (&Value, sizeof(int), 1, MyFile); 
              if (NumValuesRead == 1)
                {
                  TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
                  /* Move to the location in the file for the next value 
                   * of the variable.  Current position + 
                   * sizeof(int) * (NumVars -1)
                   */
                  fseek(MyFile, PointOffset, SEEK_CUR);

                }
              else
                {
                  TecUtilDialogErrMsg("Incorrect reading of binary file.");
                }
            }
        }
      else if (DataType == FieldDataType_Float) /* float */
        {
          float Value;

          for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
            {
              NumValuesRead = (int)fread (&Value, sizeof(float), 1, MyFile); 
              if (NumValuesRead == 1)
                {
                  TecUtilDataValueSetByRef(FieldData,PointIndex,Value);

                  /* Move to the location in the file for the next value 
                   * of the variable.  Current position + 
                   * sizeof(float) * (NumVars - 1)
                   */
                  fseek(MyFile, PointOffset, SEEK_CUR);
                }
              else
                TecUtilDialogErrMsg("Incorrect reading of binary file.");
            }
        }
      else if (DataType == FieldDataType_Double) /* double */
        {
          double Value;
 
          for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
            {
              NumValuesRead = (int)fread (&Value, sizeof(double), 1, MyFile); 
              if (NumValuesRead == 1)
                {
                  TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
                  /* Move to the location in the file for the next value 
                   * of the variable.  Current position + 
                   * sizeof(double) * (NumVars - 1)
                   */
                  fseek(MyFile, PointOffset, SEEK_CUR);
                }
              else
                TecUtilDialogErrMsg("Incorrect reading of binary file.");
            }
        }
      else
        TecUtilDialogErrMsg("Invalid data type.");  
    }

  if (MyFile != NULL)
    fclose(MyFile);

  TecUtilLockFinish(AddOnID);

  ENSURE(VALID_BOOLEAN(IsOk));
  return IsOk;
}

/**
 */
static void STDCALL LoadOnDemandVarCleanupPoint(FieldData_pa FieldData)
{
  PointClientDataValues_s *MyClientData;

  REQUIRE(FieldData != NULL);
  REQUIRE(VALID_REF((PointClientDataValues_s *)TecUtilDataValueGetClientData(FieldData)));

  TecUtilLockStart(AddOnID);

  MyClientData = (PointClientDataValues_s *)TecUtilDataValueGetClientData(FieldData);
  free(MyClientData->FileName);
  free(MyClientData);
  TRACE("Load on Demand Var Cleanup callback.\n");

  TecUtilLockFinish(AddOnID);
}


void DeferredLoadBlock(FILE            *MyFile,
                       FileOffset_t     CurOffset,
                       FieldDataType_e  DataType,
                       LgIndex_t        NumPoints,
                       EntIndex_t       ZoneIndex,
                       EntIndex_t       VarIndex)
{
  LgIndex_t     PointIndex = 1;
  int           NumValuesRead; 
  FieldData_pa  FieldData;

  TecUtilLockStart(AddOnID);
  TecUtilDataValueAlloc(ZoneIndex, VarIndex);
  FieldData = TecUtilDataValueGetWritableRef(ZoneIndex, VarIndex);
  /* Go to the file position for the first value of the variable. */
  fseek(MyFile, (long)CurOffset, 0);

  if (DataType == FieldDataType_Int32)
    {
      int Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(int), 1, MyFile); 
          TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
        }       
    }
  else if (DataType == FieldDataType_Float)
    {
      float Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(float), 1, MyFile); 
          TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
        }
    }
  else if (DataType == FieldDataType_Double)
    {
      double Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(double), 1, MyFile); 
          TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
        }
    }
  else
    TecUtilDialogErrMsg("Invalid data type.");
  TecUtilLockFinish(AddOnID);
}



void DeferredLoadPoint(FILE            *MyFile,
                       FileOffset_t     CurOffset,
                       FieldDataType_e  DataType,
                       int              PointOffset,
                       LgIndex_t        NumPoints,
                       EntIndex_t       ZoneIndex,
                       EntIndex_t       VarIndex)
{
  LgIndex_t     PointIndex = 1;
  int           NumValuesRead; 
  FieldData_pa  FieldData;
  TecUtilLockStart(AddOnID);
  TecUtilDataValueAlloc(ZoneIndex, VarIndex);
  FieldData = TecUtilDataValueGetWritableRef(ZoneIndex, VarIndex);
  /* Go to the file position for the first value of the variable. */
  fseek(MyFile, (long)CurOffset, 0);

  if (DataType == FieldDataType_Int32)
    {
      int Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(int), 1, MyFile); 
          TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
          fseek(MyFile, PointOffset, SEEK_CUR);

        }       
    }
  else if (DataType == FieldDataType_Float)
    {
      float Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(float), 1, MyFile); 
          TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
          fseek(MyFile, PointOffset, SEEK_CUR);

        }
    }
  else if (DataType == FieldDataType_Double)
    {
      double Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(double), 1, MyFile); 
          TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
          fseek(MyFile, PointOffset, SEEK_CUR);

        }
    }
  else
    TecUtilDialogErrMsg("Invalid data type.");
  TecUtilLockFinish(AddOnID);
}







/**
 * LoadBlock is used for immediate loading a variable from a file 
 * in Block format without load on demand.
 */
Boolean_t LoadBlock(FILE            *MyFile, 
                    FileOffset_t     CurOffset, 
                    FieldDataType_e  DataType, 
                    LgIndex_t        NumPoints, 
                    FieldData_pa     FieldData)
{
  LgIndex_t PointIndex = 1;
  int       NumValuesRead; 
  Boolean_t IsOk = TRUE; 

  /* Go to the file position for the first value of the variable. */
  fseek(MyFile, (long)CurOffset, 0);

  if (DataType == FieldDataType_Int32)
    {
      int Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(int), 1, MyFile); 
          TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
        }       
    }
  else if (DataType == FieldDataType_Float)
    {
      float Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(float), 1, MyFile); 
          TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
        }
    }
  else if (DataType == FieldDataType_Double)
    {
      double Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(double), 1, MyFile); 
          TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
        }
    }
  else
    TecUtilDialogErrMsg("Invalid data type.");
  return IsOk; 
}


/**
 * ReadVarValuesFromPointFile is used for loading a variable from a file 
 * in Point format without load on demand.
 */
Boolean_t LoadPoint(FILE            *MyFile, 
                                     FileOffset_t     CurOffset, 
                                     FieldDataType_e  DataType, 
                                     int              PointOffset,
                                     int              NumPoints, 
                                     FieldData_pa     FieldData)
{
  int       PointIndex;
  int       NumValuesRead; 
  Boolean_t IsOk;

  /* Go to the file position for the first value of the variable. */
  fseek(MyFile, (long) CurOffset, SEEK_SET);
  if (DataType == FieldDataType_Int32)
    {
      int Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(int), 1, MyFile); 
          if (NumValuesRead == 1)
            {
              TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
              /* Move to the location in the file for the next value 
               * of the variable.  Current position + 
               * (the size of the variable) * (NumVars - 1)
               */
              fseek(MyFile, PointOffset, SEEK_CUR);
            }
          else
            { 
              IsOk = FALSE;
              TecUtilDialogErrMsg("Incorrect reading of binary file.");
            }
        }
    }
  else if (DataType == FieldDataType_Float)
    {
      float Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(float), 1, MyFile); 
          if (NumValuesRead == 1)
            {
              TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
              /* Move to the location in the file for the next value 
               * of the variable.  Current position + 
               * (the size of the variable) * (NumVars - 1)
               */
              fseek(MyFile, PointOffset, SEEK_CUR);
            }
          else
            {
              IsOk = FALSE;
              TecUtilDialogErrMsg("Incorrect reading of binary file.");
            }
        }
    }
  else if (DataType == FieldDataType_Double)
    {
      double Value;

      for (PointIndex = 1; PointIndex <= NumPoints; PointIndex++)
        {
          NumValuesRead = (int)fread (&Value, sizeof(double), 1, MyFile); 
          if (NumValuesRead == 1)
            {
              TecUtilDataValueSetByRef(FieldData,PointIndex,Value);
              /* Move to the location in the file for the next value 
               * of the variable.  Current position + 
               * (the size of the variable) * (NumVars - 1)
               */
              fseek(MyFile, PointOffset, SEEK_CUR);
            }
          else
            {
              IsOk = FALSE;
              TecUtilDialogErrMsg("Incorrect reading of binary file.");
            }
        }
    }
  else
    TecUtilDialogErrMsg("Invalid data type.");   
  return IsOk; 
}

/**
 */
Boolean_t STDCALL LoaderCallback(StringList_pa Instructions) /* IN */
{
  char *Name = NULL;
  char *ValueString = NULL;
  char FileName[120]; 
  char DataTypeString[20];
  char DataFormatString[20];
  char MyVar[20];

  LgIndex_t       NumZones;
  LgIndex_t       NumVars;
  LgIndex_t       NumPoints;
  LgIndex_t       DataType;
  LgIndex_t       LoadMethod;
  EntIndex_t      ZoneIndex;
  EntIndex_t      VarIndex;
  FieldDataType_e FDataType;
  DataPacking_e   DataFormat;

  int             SizeOfVar;
  int             InstrIndex;
  FILE           *MyFile;

  StringList_pa   VarNames = TecUtilStringListAlloc();
  Boolean_t IsOk = TRUE;
  LgIndex_t Count;
  REQUIRE(Instructions != NULL);

  TecUtilLockStart(AddOnID);
  /* Check that Instructions are in STANDARDSYNTAX. 
  */
  Count = TecUtilStringListGetCount(Instructions);
  Name = TecUtilStringListGetString(Instructions, 1);
  if (Str_ustrcmp(Name, "STANDARDSYNTAX") == 0)
    {
      IsOk = TRUE;
    }
  TecUtilStringDealloc(&Name);

  /* Get pairs of instructions and convert the value string to the form
   * needed to load the data.
   */
  InstrIndex = 3;
  while(IsOk && InstrIndex < Count)
    {
      Name  = TecUtilStringListGetString(Instructions, InstrIndex);
      ValueString = TecUtilStringListGetString(Instructions, InstrIndex + 1);
      InstrIndex += 2;

      if(Str_ustrcmp(Name, "FILENAME") == 0)
        {
          strcpy(FileName,ValueString);
          REQUIRE(VALID_NON_ZERO_LEN_STR(FileName));
        }
      else if (Str_ustrcmp(Name, "LoadMethod") == 0)
        {
          LoadMethod = atoi(ValueString);
          REQUIRE(VALID_ENUM(LoadMethod,LoadMethod_e));
        }

      else
        {
          break;
        }

      TecUtilStringDealloc(&Name);
      TecUtilStringDealloc(&ValueString);
    }


  /* Get header info : Format, NumPoints, NumVars, NumZones, Data type */
  MyFile = fopen(FileName, "rb");
  if (MyFile == NULL)
    {
      TecUtilDialogErrMsg("Invalid file name.");
      IsOk = FALSE;
    }
  fread (&DataFormat, sizeof(int), 1, MyFile); 
  fread (&NumPoints,  sizeof(int), 1, MyFile);
  fread (&NumVars,    sizeof(int), 1, MyFile);
  fread (&NumZones,   sizeof(int), 1, MyFile);
  fread (&DataType,   sizeof(int), 1, MyFile);
  if (MyFile != NULL)
    fclose(MyFile); 


  /* Determine data type and size of values. */
  switch (DataType)
    {
      case 1:
      FDataType = FieldDataType_Int32;
      SizeOfVar = sizeof(int);
      strcpy(DataTypeString, "INT");
      break;
      case 2:
      FDataType = FieldDataType_Float; 
      SizeOfVar = sizeof(float);
      strcpy(DataTypeString, "FLOAT");
      break;

      case 3:
      FDataType = FieldDataType_Double;
      SizeOfVar = sizeof(double);
      strcpy(DataTypeString, "DOUBLE");
      break;
      default:
      TecUtilDialogErrMsg("Inappropriate data type.");
      IsOk = FALSE;
      break;
    }


  /* DataFormat radio options are 1-based, but the enumerated options are 0-based.*/
  DataFormat -= 1; 

  if (DataFormat == DataPacking_Block) 
    {
      strcpy(DataFormatString, "BLOCK");
    }
  else if (DataFormat == DataPacking_Point) 
    {
      strcpy(DataFormatString, "POINT");
    }
  else
    {
      TecUtilDialogErrMsg("Inappropriate data format.");
      IsOk = FALSE;
    }
  if (IsOk == TRUE)
    {
      Boolean_t        DeferVarCreation;
      Int64_t          Stride;
      FieldData_pa     FieldData;
      FieldDataType_e *VarDataTypes; 
      FileOffset_t     CurOffset = 0;
      int              ZoneOffset;
      int              DataOffset = 5*sizeof(int);
      char             DataSetString[50];

      if (LoadMethod == LoadMethod_Immediate) /* Immediate Load */
        {
          DeferVarCreation = FALSE;
          strcpy(DataSetString, "Data loaded immediately.");
        }
      else if (LoadMethod == LoadMethod_Deferred)
        {
          DeferVarCreation = TRUE;
          strcpy(DataSetString, "Data loading deferred.");
        }
      else if (LoadMethod == LoadMethod_AutoLOD) /* Auto LOD */
        {
          DeferVarCreation = TRUE;
          strcpy(DataSetString, "Data with auto load on demand");
        }
      else if (LoadMethod == LoadMethod_CustomLOD)  /* Custom LOD */
        {
          DeferVarCreation = TRUE; 
          strcpy(DataSetString, "Data with custom load on demand");
        }

      VarDataTypes = (FieldDataType_e *)TecUtilStringAlloc(sizeof(FieldDataType_e)*NumVars,
                                                           "Var Data Types");
      for ( VarIndex=1;VarIndex<=NumVars;VarIndex++ )
        {
          sprintf(MyVar, "MyVar %d", VarIndex);
          TecUtilStringListAppendString(VarNames, MyVar);
          VarDataTypes[VarIndex - 1] = FDataType;
        }
      /* Create the data set in Tecplot. */ 
      TecUtilDataSetCreate(DataSetString,  
                           VarNames,  
                           TRUE ); 
      for ( ZoneIndex=1;ZoneIndex<=NumZones;ZoneIndex++ )
        {
          ArgList_pa ArgList;
          char       ZoneTitle[20];

          if (((NumPoints * NumVars) > 100000) && (ZoneIndex > 1) && (LoadMethod == LoadMethod_Immediate))
            StopReadFile = TecUtilDialogMessageBox("Stop loading file?", MessageBox_YesNo);
          if (StopReadFile == FALSE) 
            {
              ZoneOffset = ((ZoneIndex -1) * (NumPoints*NumVars)* SizeOfVar);
              sprintf(ZoneTitle, "Zone %d", ZoneIndex); 
              ArgList = TecUtilArgListAlloc();
              TecUtilArgListAppendString(ArgList, SV_NAME,                ZoneTitle);
              TecUtilArgListAppendInt   (ArgList, SV_IMAX,                NumPoints);
              TecUtilArgListAppendInt   (ArgList, SV_DEFERVARCREATION,    DeferVarCreation);
              TecUtilArgListAppendArray (ArgList, SV_VARDATATYPE, (void *)VarDataTypes);
              IsOk = TecUtilDataSetAddZoneX(ArgList);
              if (VarDataTypes)
                TecUtilStringDealloc((char **)&VarDataTypes);

              TecUtilArgListDealloc(&ArgList);
              if (DataFormat == DataPacking_Block)  /* File is in Block format. */
                {
                  switch (LoadMethod)
                    {
                      case 1:

                      /*  if (LoadMethod == LoadMethod_Immediate)   No load on demand. */
                        {
                          MyFile = fopen(FileName, "rb");
                          if (MyFile == NULL)
                            {
                              TecUtilDialogErrMsg("Cannot open file for LoadBlock.  ");
                              IsOk = FALSE;
                            }

                          else for (VarIndex = 1; VarIndex <= NumVars; VarIndex++)
                            {

                              CurOffset = SizeOfVar*(VarIndex - 1)*(NumPoints) + ZoneOffset + DataOffset;
                              FieldData = TecUtilDataValueGetWritableRef(ZoneIndex, VarIndex);
                              LoadBlock(MyFile, 
                                        CurOffset, 
                                        FDataType, 
                                        NumPoints, 
                                        FieldData);
                            }
                          if (MyFile != NULL)
                            fclose(MyFile);
                        } break;
                      case 2:  /* Deferred loading */
                        {
                          int WhichVars = TecGUIRadioBoxGetToggle(SpecifyVar_RADIO_D1);


                          MyFile = fopen(FileName, "rb");

                          if (MyFile == NULL)

                            {
                              TecUtilDialogErrMsg("Cannot open file for DeferredLoadBlock.");
                              IsOk = FALSE;
                            }

                          else 
                            for (VarIndex = 1; VarIndex <= NumVars; VarIndex++)
                              {

                                double DVarIndex = (double) VarIndex;
                                double EvenOdd = (DVarIndex/2.0 - floor(DVarIndex/2.0)) * 2;
                                if (((WhichVars == 2) && ((int)EvenOdd == 1)) ||
                                    ((WhichVars == 3) && ((int)EvenOdd == 0)) ||
                                    (WhichVars == 1))
                                  {
                                    CurOffset = SizeOfVar*(VarIndex - 1)*(NumPoints) + ZoneOffset + DataOffset;
                                    DeferredLoadBlock(MyFile, 
                                                      CurOffset,
                                                      FDataType,
                                                      NumPoints,
                                                      ZoneIndex,
                                                      VarIndex);
                                  }

                              }
                        } break;

                      case 3:
                      /* else if (LoadMethod == LoadMethod_AutoLOD) */ /* Auto Load on demand */
                        {
                          Boolean_t IsNativeByteOrder = TRUE;

                          /* TODO - Consider Byte Order for platforms. */
                          for (VarIndex = 1; VarIndex <= NumVars; VarIndex++)
                            {
                              Stride = 1;
                              CurOffset = SizeOfVar*(VarIndex - 1)*(NumPoints) + ZoneOffset + DataOffset;
                              IsOk = TecUtilDataValueAutoLOD (ZoneIndex,
                                                              VarIndex,
                                                              DataValueStructure_ClassicPlus,
                                                              FileName,
                                                              CurOffset, 
                                                              Stride,
                                                              IsNativeByteOrder);
                            }

                        } break; 
                      case 4: 
                      /* else if (LoadMethod == LoadMethod_CustomLOD) */ /* Custom Load on demand */
                        {
                          /* Place necessary information for Custom Load on Demand in Block 
                           * format files into the structure BlockClientData.  This includes:
                           * file name, the number of points, the data type, and the offset 
                           * in the file for the first value of the current variable.
                           */

                          for (VarIndex = 1; VarIndex <= NumVars; VarIndex++)
                            {
                              BlockClientDataValues_s *BlockClientData = (BlockClientDataValues_s *)malloc(sizeof(BlockClientDataValues_s));
                              BlockClientData->FileName  = (char *) malloc(sizeof(char) * (strlen(FileName) +1));
                              strcpy(BlockClientData->FileName, FileName);
                              BlockClientData->CurOffset = SizeOfVar*(VarIndex -1)*(NumPoints) + ZoneOffset + DataOffset;
                              BlockClientData->DataType  = FDataType;
                              BlockClientData->NumPoints = NumPoints;
                              IsOk = TecUtilDataValueCustomLOD(ZoneIndex, 
                                                               VarIndex,
                                                               LoadOnDemandVarLoadBlock,
                                                               NULL, /* ...instructs Tecplot to unload and subsequently reload the variable */
                                                               LoadOnDemandVarCleanupBlock,
                                                               NULL, /* GetFunction */
                                                               NULL, /* SetFunction */
                                                               (ArbParam_t)BlockClientData);
                            }
                        }  break; 
                      default: 
                        {
                          TecUtilDialogErrMsg("Not yet implemented.");
                        } break; 
                    }
                }

              else if (DataFormat == DataPacking_Point) /* File is in Point format. */
                {
                  int              PointOffset;

                  switch (LoadMethod)
                    {
                      case 1:

                      /* LoadMethod_Immediate No load on demand. */
                        {
                          MyFile = fopen(FileName, "rb");
                          if (MyFile == NULL)
                            {
                              TecUtilDialogErrMsg("Cannot open file for LoadPoint.  ");
                              IsOk = FALSE;
                            }
                          else
                            {
                              PointOffset = SizeOfVar*(NumVars - 1);
                              TRACE("Reading from file in Point format. \n"); 

                              for (VarIndex = 1; VarIndex <= NumVars; VarIndex++)
                                {
                                  CurOffset = SizeOfVar*(VarIndex - 1) + ZoneOffset + DataOffset;
                                  FieldData = TecUtilDataValueGetWritableRef(ZoneIndex, VarIndex);

                                  LoadPoint(MyFile, 
                                            CurOffset, 
                                            FDataType,
                                            PointOffset,
                                            NumPoints, 
                                            FieldData );
                                }
                            }
                          if (MyFile != NULL)
                            fclose(MyFile); 
                        } break;

                      case 2:  /* Deferred loading */
                        {
                          int WhichVars = TecGUIRadioBoxGetToggle(SpecifyVar_RADIO_D1);
                          MyFile = fopen(FileName, "rb");
                          if (MyFile == NULL)
                            {
                              TecUtilDialogErrMsg("Cannot open file for DeferredLoadPoint.");
                              IsOk = FALSE;
                            }
                          else
                            {
                              PointOffset = SizeOfVar*(NumVars - 1);
                              for (VarIndex = 1; VarIndex <= NumVars; VarIndex++)
                                {
                                  double DVarIndex = (double) VarIndex;
                                  double EvenOdd = (DVarIndex/2.0 - floor(DVarIndex/2.0)) * 2;
                                  if (((WhichVars == 2) && ((int)EvenOdd == 1)) ||
                                      ((WhichVars == 3) && ((int)EvenOdd == 0)) ||
                                      (WhichVars == 1))
                                    {
                                      CurOffset = SizeOfVar*(VarIndex - 1) + ZoneOffset + DataOffset;
                                      DeferredLoadPoint(MyFile, 
                                                        CurOffset,
                                                        FDataType,
                                                        PointOffset,
                                                        NumPoints,
                                                        ZoneIndex,
                                                        VarIndex);
                                    }
                                }
                            } 
                        } break;
                      case 3:  /*LoadMethod_AutoLOD)  Auto Load on demand for point format. */
                        {

                          Boolean_t IsNativeByteOrder = TRUE;

                          /* TODO - Consider Byte Order for platforms. */
                          for (VarIndex = 1; VarIndex <= NumVars; VarIndex++)
                            {
                              Stride = NumVars; 
                              CurOffset = SizeOfVar*(VarIndex - 1) + ZoneOffset + DataOffset;
                              IsOk = TecUtilDataValueAutoLOD (ZoneIndex,
                                                              VarIndex,
                                                              DataValueStructure_ClassicPlus,
                                                              FileName,
                                                              CurOffset, 
                                                              Stride,
                                                              IsNativeByteOrder);
                            }
                        } break;
                      case 4:     /*   LoadMethod_CustomLOD Custom Load On Demand  for Point*/
                        {
                          /* Pass these parameters for Client Data for Point format:
                           * PointOffset (distance between values of the variable),
                           * the file name, NumPoints, the data type and the offset in the file 
                           * for the first value of the current variable.
                           */

                          TRACE("Reading from file in Point format. \n"); 

                          for (VarIndex = 1; VarIndex <= NumVars; VarIndex++)
                            {
                              /* Place necessary information for Custom Load on Demand with Point 
                               * format files into the structure PointClientData.  This includes:
                               * the file name, the offset for the variable between its first value
                               * and its second value (stride), the offset in the file for the first 
                               * value of the current variable, the number of points, and the data
                               * type.
                               */

                              PointClientDataValues_s *PointClientData = (PointClientDataValues_s *)malloc(sizeof(PointClientDataValues_s)); 
                              PointClientData->FileName = (char *) malloc(sizeof(char) * (strlen(FileName) +1));
                              strcpy(PointClientData->FileName, FileName);
                              PointClientData->PointOffset = SizeOfVar*(NumVars-1);
                              CurOffset = SizeOfVar*(VarIndex-1) + ZoneOffset + DataOffset;
                              PointClientData->CurOffset = (long) CurOffset;
                              PointClientData->DataType = FDataType;
                              PointClientData->NumPoints = NumPoints;

                              /* With TecUtilDataValueCustomLOD, Tecplot stores a field data pointer
                               * and the PointClientData for each variable of each zone. Then it will 
                               * call one of the listed functions each time it needs to load or
                               * unload a variable, passing the field data pointer and the client 
                               * data for that variable.
                               */
                              TecUtilDataValueCustomLOD(ZoneIndex, 
                                                        VarIndex,
                                                        LoadOnDemandVarLoadPoint,
                                                        NULL, /* ...instructs Tecplot to unload and subsequently reload the variable */
                                                        LoadOnDemandVarCleanupPoint,
                                                        NULL, /* GetFunction */
                                                        NULL, /* SetFunction */
                                                        (ArbParam_t)PointClientData);
                            }
                        } break;
                      default:
                        {
                          TecUtilDialogErrMsg("Inappropriate load type. ");
                        } break;
                    }
                }
              else 
                TecUtilDialogErrMsg("Inappropriate data format.");
            }
        } /* End zone loop */
      if (DeferVarCreation == TRUE)
        TecUtilDataSetDefVarLoadFinish(IsOk);
      TecUtilFrameSetPlotType(PlotType_Cartesian2D); 
      TecUtilRedraw(TRUE);
      TecUtilImportSetLoaderInstr(ADDON_NAME,Instructions);

    } /* End if (IsOk == TRUE)  */

  TecUtilStringListDealloc(&VarNames);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
void STDCALL LoaderSelectedCallback(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}
