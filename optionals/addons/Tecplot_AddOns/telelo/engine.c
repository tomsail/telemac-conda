#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "GUIDEFS.h"
#include "ENGINE.h"
#include "UTIL.h"
#include "ADKUTIL.h"


/*******************************************************************/
Boolean_t STDCALL LoaderCallback(StringList_pa Instructions) /* IN */
/*******************************************************************/
{

  FILE *DataFile;
  LgIndex_t Count;
  LgIndex_t InstrIndex;
  char      *Name = NULL;
  char      *Value = NULL;
  Boolean_t IsOk;
  char *FileName=NULL;
  int i,n,NbFiles;
  int StartFileList;
  Boolean_t AppendData = FALSE;
  Boolean_t RetainPlotStyle = FALSE;
  Boolean_t LoadFile();
  char MessageString[100];
  char *ParsePtr;
/*******************************************************************/

  /*
   *  Tecplot will call this function any time it needs to load 
   *  data using this custom loader. 
   *
   *    IMPORTANT: After the data has been successfully loaded,
   *    be sure to call TecUtilImportSetLoaderInstr(). This will allow
   *    the user to save a layout file which references this 
   *    custom loader.
   */

  TecUtilLockStart(AddOnID);
  TRACE("entered loadercallback\n");
  IsOk = TRUE;
  Count = TecUtilStringListGetCount(Instructions);
  InstrIndex = 1;
      TRACE("------\n");
  while(IsOk && InstrIndex <= Count )
  {
      Name = TecUtilStringListGetString(Instructions,InstrIndex);
      Value = TecUtilStringListGetString(Instructions,InstrIndex+1);
      TRACE(Name);
      TRACE("\n");
      TRACE(Value);
      TRACE("\n");
      TRACE("------\n");
      if(strcmp(Name,"FILENAME_TOLOAD") == 0 )
      {
	 // Load a single file. The file name is the value of
	 // the instruction.
         NbFiles = 1;
	 StartFileList = InstrIndex+1;
      } // end FILENAME_TOLOAD
      else if(strcmp(Name,"FILELIST_TOLOAD") == 0 )
      {
	 // load a file list. For the Filelist, the value of
	 // the instruction is the number of files to load,
	 // than follow the NbFile file names to load in the
	 // instructions stringlist.
	 NbFiles = 0;
	 ParsePtr = Value;
         IsOk = Str_ScanForLgIndex(&ParsePtr,(LgIndex_t*)&NbFiles);
	 sprintf(MessageString,
		 "Number of files to read : %d\n",NbFiles);
	 TRACE(MessageString);
	 if( IsOk == TRUE) 
	 {
	     sprintf(MessageString,
		     "Number of files to read : %d\n",NbFiles);
	     TRACE(MessageString);
	 }
	 else
	 {
	     TRACE("impossible to read the number of files\n");
	     IsOk = FALSE;
	 }
	 StartFileList = InstrIndex+2;
	 InstrIndex += NbFiles;
      } // end FILENAME_TOLOAD
      else if (strcmp(Name,"APPEND_DATA") == 0 )
      {
	  if(strcmp(Value,"YES") ==0 ) AppendData = TRUE;
      }
      else if (strcmp(Name,"RETAIN_PLOTSTYLE") == 0 )
      {
	  if(strcmp(Value,"YES") ==0) RetainPlotStyle = TRUE;
      }
      TRACE("step1\n");
      InstrIndex += 2;
      TRACE("step2\n");
      TecUtilStringDealloc(&Name);
      TRACE("step3\n");
      TecUtilStringDealloc(&Value);
      TRACE("step4\n");
  }  // end while over instructions
  // dealloc name and value string for instructions
  TRACE("End recovering instructions. \n");

  if(NbFiles == 0 ) 
  {
     TRACE("No files to load !!!\n");
     IsOk = FALSE;
  }
  else  
  {
      TRACE("enter here ...\n");
  // Load the NbFile data files :
  for(i=0;i<NbFiles;i++)
  {
      // the position of the file name in the Instructions
      // string list
      n = StartFileList + i;
      // get the file name from the instructions list
      sprintf(MessageString,"Try to get file no %d \n",n);
      FileName = TecUtilStringListGetString(Instructions,n);
      // check if file name is valid
      if ( FileName != NULL && strlen(FileName) != 0 ) 
      {
      TRACE("Try to open file <\n");
      TRACE(FileName);
      TRACE(">\n");
      DataFile = fopen(FileName,"rb");
      if ( DataFile == NULL ) 
      {
          TecUtilDialogErrMsg("Invalid File Name ");
          IsOk = FALSE;
      } // end error open file
      if(IsOk) 
      {
	  TRACE("try to load the data file ... \n");
          IsOk = LoadFile(DataFile,FileName,&AppendData);
          fclose(DataFile);
	  if(IsOk != TRUE )
	      TecUtilDialogErrMsg("Error on loading file");
      }
      }
      // in case of multiple file loads, set AppendData to
      // TRUE after the first load ...
      if(IsOk) AppendData = TRUE;
      // we should deallocate the file name, recovered from
      // the string list :
      TecUtilStringDealloc(&FileName);
      FileName = NULL;
  }
  }

  TRACE("End of file load ...\n");

  if( IsOk == TRUE )
  {
    TecUtilViewAxisNiceFit('X',1);
    TecUtilViewAxisNiceFit('Y',1);
    TecUtilViewAxisNiceFit('Z',1);
    TecUtilViewCenter();
  }
  //IsOk = TRUE;
  TecUtilImportSetLoaderInstr("Serafin2DataLoader",Instructions);
  TecUtilLockFinish(AddOnID);
  TRACE("end of loader call back\n");
  return(IsOk);

}

/*******************************************************************/
void STDCALL LoaderSelectedCallback(void)
/*******************************************************************/
{
  /*
   * This function is called when
   * the user selects this dataset
   * loader from the list of importers
   * in the File/Import dialog.
   *
   * TODO:
   *
   * 1. Launch a dialog which collects settings
   *    for how the file should be loaded.
   *    (filename, skip, etc.)
   *
   * 2. In the OK callback of the dialog, close the dialog
   *    and call a function to load the data using the
   *    indicated settings. Note: you may want to use
   *    the same function to load the data from the
   *    LoaderCallback() function above.
   *
   * 3. IMPORTANT: After the data has been successfully loaded,
   *    be sure to call TecUtilImportSetLoaderInstr(). This will allow
   *    the user to save a layout file which references this 
   *    custom loader.
   */
  extern int LoadMode;
  extern StringList_pa FileNames;
  Boolean_t IsOk;
  extern Boolean_t DoneWithDialog2;
  char message[100];
  char *FName;
  int i, NbFiles;
  StringList_pa Instructions;



  TecUtilLockStart(AddOnID);

  TRACE("enter loaderselectcallback");
  
  // If there is a dataset in the current frame, start with
  // dialog1.
  if( TecUtilFrameGetPlotType() != PlotType_Sketch )
  {
      DoneWithDialog2 = FALSE;
      BuildDialog2(MAINDIALOGID);
      TecGUIDialogLaunch(Dialog2Manager);
      TecGUIBlockForModalDialog(&DoneWithDialog2);
  }
  
      IsOk = TecUtilDialogGetFileNames(SelectFileOption_AllowMultiFileRead,
                                   &FileNames, "Data Files.", NULL, "*");

  NbFiles = TecUtilStringListGetCount(FileNames);
     
  sprintf(message,"load mode : %d\n",LoadMode);
  TRACE(message);
  // Construction of the Loader Instructions:
  

    Instructions = TecUtilStringListAlloc();
    TecUtilStringListAppendString(Instructions,"APPEND_DATA");
    if ( LoadMode == 1 || LoadMode == 2 ) 
        TecUtilStringListAppendString(Instructions,"NO");
    else
        TecUtilStringListAppendString(Instructions,"YES");
    TecUtilStringListAppendString(Instructions,"RETAIN_PLOTSTYLE");
    if ( LoadMode == 2 || LoadMode == 3 ) 
        TecUtilStringListAppendString(Instructions,"YES");
    else
         TecUtilStringListAppendString(Instructions,"NO");

	TecUtilStringListAppendString(Instructions,"STANDARDSYNTAX");
	TecUtilStringListAppendString(Instructions,"1.0");
	// add the number of files to load to the stringlist :
	DLoad_AddLgIndexArg(Instructions,"FILELIST_TOLOAD",NbFiles);
	// add the filenames
	for(i=1;i<=NbFiles;i++)
	{
           FName = TecUtilStringListGetString(FileNames,(LgIndex_t)i);
	   TecUtilStringListAppendString(Instructions,FName);
           TecUtilStringDealloc(&FName);
	}

	TRACE("Call to loadercallback from dialog1okbutton\n");
        IsOk = LoaderCallback(Instructions);
	//TecUtilStringListDealloc(&Instructions);
	if ( IsOk != TRUE ) 
	{
            TecUtilDialogErrMsg("Error loading the file");
	}
  TecUtilLockFinish(AddOnID);
  return;
}





