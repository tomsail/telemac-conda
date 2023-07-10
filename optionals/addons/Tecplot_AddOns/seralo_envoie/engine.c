#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "GUIDEFS.h"
#include "ENGINE.h"
#include "SERAFIN2.h"
#include "UTIL.h"
#include "ADKUTIL.h"
#include <string.h>

// The two types of serafin file format.
//
#define SERAFIN 1
#define SERAFIN2 2

#define FileLoad_Telemac 1
#define FileLoad_Geom 3
#define FileLoad_TelDel 2


void trim( char *str ) 
{
    /* Remove trailing and heading white spaces.
     */
        int i;                                     
        int j=0;                                   
	int len=strlen(str);
        for(i=0;i<len-1;i++)   
	{
          if(str[i]=='\0'||str[i]!=' ') break;
	  j++;
	}
	if(j>0)
	{
	    for(i=0;i<len-j;i++) str[i]=str[i+j];                       
            for(i=len-j;i<len-2;i++) str[i]=' ';                            
	    str[len-1]=0;
	}
        for(i=len-2;(str[i]==' ')&&i>0;i--)     
	{
            str[i]='\0';                           
	}
	return;
}
/*******************************************************************/
static Boolean_t AppendVariable( char *VName, // name of var to add
	                         int *n)      // Id var in data set
/*******************************************************************/
{
  Boolean_t IsOk;
  EntIndex_t NumVars;
  ArgList_pa ArgList;
  Set_pa VarsAdded;

/*******************************************************************/
  IsOk = FALSE;
  *n = -1;

  // Create argument list for variable to add.
  // For all zones present, the variable will be passive, so
  // no need to specify variable location or type (us default
  // types).
  ArgList = TecUtilArgListAlloc();
  TecUtilArgListAppendString(ArgList,SV_NAME,VName);
  // defere var creation (autolod)
  TecUtilArgListAppendInt(ArgList,SV_DEFERVARCREATION,TRUE);
  // add variable to the data set
  TecUtilDataSetAddVarX(ArgList);
  TecUtilArgListDealloc(&ArgList);

  // notify the stat change to tecplot.
  VarsAdded = TecUtilSetAlloc(FALSE);
  TecUtilDataSetGetInfo((char **)NULL,
                        (EntIndex_t *)NULL,
                         &NumVars);
  TecUtilSetAddMember(VarsAdded,NumVars,FALSE);
  TecUtilStateChanged(StateChange_VarsAdded,
                     (ArbParam_t)VarsAdded);
  TecUtilSetDealloc(&VarsAdded);
  *n = (int)NumVars;
  
  
  return(IsOk);
}
/*******************************************************************/
static Boolean_t LoadFile(FILE *DataFile,
	                  char *FileName,
	                  Boolean_t GeomOnly,
	                  Boolean_t AppendData,
	                  Boolean_t AddDelwaq,
                          FILE *DelwaqDataFile,
	                  char *DelwaqFileName)
/*******************************************************************/
/* Function : Read a data file of the SERAFIN2 data format and
 * load the data into tecplot using AutoLoadOnDemand.
 * For each data array (ie the data for a variable for a given
 * zone) should be provided as a position in the file to
 * Tecplot.
 * Zones should be created by TecUtilDataSetAddZoneX(), using
 * DEFERVARCREATION option.
 * The data read may be appended to the existing data set.
 */
{



  // SERAFIN2 or SERAPHIN file format
  int FileFormat;
  // Return values for tecplot calls 
  Boolean_t IsOk;
  // invert byte order in source file or not
  Boolean_t Invert;
  Boolean_t InvertDelwaq;
  // header bytes and expected value for fortran blocks in
  // file
  int FHeader;
  int nelem, npoin,nplan,ndp,npoinDelwaq;
  // File header informations
  char title[81];
  char TitleDelwaq[81];
  char key_title[9];
  // Number of variables in files
  int NbVar1,NbVar2, NbVarTot, NbVarDelwaq;
  // Total number of variables in data set after adding (or
  // not) the variables from the file.
  EntIndex_t NbVarDataSet;
  // loop counter
  int i,j;
  int ivar;
  long int fpos;
  // Variable names from stringlist
  char *VName1, *VName2;
  // Number of variables in actual data set
  EntIndex_t NumVars_orig;
  // Correlation between variables in file and variables in
  // data set.
  EntIndex_t *VarCorr;
  // Error and debug informations
  char MessageString[100];
  // Variable names
  StringList_pa VarNames1, VarNames2, VarNamesDelwaq ;
  //StringList_pa VarNames;
  // Zone informations : number of zones in original data set
  // Number of zones after reading the file and the set of
  // zones added when reading the file.
  EntIndex_t StartNewZones;
  EntIndex_t EndNewZones;
  Set_pa NewZoneSet;

  Boolean_t ResetPlotStyle;

  // external functions for reading the fortran blocks.
  Boolean_t ReadFHead();
  Boolean_t CheckReadBlock();
  Boolean_t GetVarNamesSera();
  Boolean_t GetVarNamesDelwaq();
  Boolean_t GetVarNamesSera2();
  Boolean_t GetMeshInfo();
  Boolean_t LoadDataSera();
  Boolean_t LoadDataSera2();
  Boolean_t LoadDataDelwaq();
  Boolean_t ReadDelwaqHeader();

/*******************************************************************/

    
/* Read the file header informations. There we will find the
 * variable names, the kind of data in the file,
 * the variable locations. All data in the file are double
 * precision values.
 * The number of points may vary from zone to zone.
 * The total number of zones in the file is unknown.
 *
 * The file is written by fortran. We should add to each block
 * written in the file 4 header and 4 trailing bytes.
 * NB : This will allow us to define the byte order in the file.
 */

  // The first thing to read are the 4 head bytes for the
  // title of the data file. This title is 80 charcters
  // long, so the first valu to read is 80. If this is not
  // 80, try to invert byte order and check again. If the
  // header byte of the title is not 80, error.
  fread(&FHeader,sizeof(int),1,DataFile);

  sprintf(MessageString,"Read in file :%d \n",FHeader);

  TRACE(MessageString);

  if ( FHeader != 80 )
  {
      swapbytes32(FHeader) ;
      Invert = TRUE;
  }
  else
      Invert = FALSE;

  if ( FHeader == 80 ) 
  {
      IsOk = TRUE;
  }
  else
  {
       TecUtilDialogErrMsg("Invalid File Format");
       IsOk = FALSE;
       return(IsOk);
  }
  sprintf(MessageString,"Read in file :%d \n",FHeader);
  TRACE(MessageString);
   if(IsOk == TRUE)
   {
       fread(&title,80,1,DataFile);
       IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);
   }
   else
   {
       TRACE("error reading data file header \n");
   }
   title[80]=0;
   TRACE("title of the dataset in file :\n <");
   TRACE(title);
   TRACE(">\n");
   sprintf(key_title,"");
   TRACE(key_title);
   TRACE("\n");

// if(strlen(title)==79)
   {
       Str_CopySubString(key_title,title,72,9);
       TRACE("<");
       TRACE(key_title);
       TRACE(">\n");
   }
      
   // Get the file format. There may be two different versions
   // of the file format : 
   // Serafin classic or Serafin2.
   
   if(strcmp(key_title,"SERAFIN2")==0)
   {
       FileFormat = SERAFIN2;
   }
   else// if(strcmp(key_title,"SERAPHIN")==0 || strcmp(key_title,"SERAFIN ")==0 )
       // try serafin, this may be a matisse mesh file ...
   {
       // anyhow, try serafin. in case of geometry files, the
       // keyword may not be present.
       FileFormat = SERAFIN;
   }


   // There are two types of variable names.
   // Allocate the string lists here.

   VarNames1 = TecUtilStringListAlloc();
   VarNames2 = TecUtilStringListAlloc();

   // Get the variable names and the number of variables in
   // the file. 
   
   TRACE("Recuperation noms des variables \n");
   if(FileFormat == SERAFIN2)
   {
       IsOk = GetVarNamesSera2(DataFile,Invert,&NbVar1,&NbVar2,
	                       &VarNames1,&VarNames2);
   }
   else if (FileFormat == SERAFIN)
   {
       IsOk = GetVarNamesSera(DataFile,Invert,&NbVar1,&VarNames1);
   }
   else
   {
       IsOk = FALSE;
       TecUtilDialogErrMsg("Bad File Format");
   }
   TRACE("fin recuperation nom des variables\n");
   if(IsOk != TRUE) TRACE("echec recuperation nom des variables\n");

   /* For delwaq data files : 
    * Get the title of the delwaq file and read the number of
    * variables (and the number of points).
    * We will need to know the numb er of points, in order to
    * check the byte order.
    */
   NbVarDelwaq= 0;
   VarNamesDelwaq = TecUtilStringListAlloc();
   if ( AddDelwaq == TRUE)
   {
       // Read the delwaq data file header with the title, the
       // number of variables and the number of data points.
       IsOk = ReadDelwaqHeader(DelwaqDataFile,TitleDelwaq,
	                       &npoinDelwaq,&NbVarDelwaq);

       // Forward the telemac data file until reading the
       // number of data points in the telemac file.
       // Actually, it is not possible to load serafin2 and
       // telemac data.
       
       if (FileFormat == SERAFIN )
       {
	   TRACE("check for delwaq byte order ...\n");
          fpos = ftell(DataFile);
          IsOk = GetMeshInfo(DataFile,Invert,&nelem,&npoin,
		             &ndp,&nplan,MessageString) ;
	  fseek(DataFile,fpos,0);
	  TRACE("check for compatibility betwenn telemac and delwaq ...\n");
          if ( npoin == npoinDelwaq ) 
	  {
              InvertDelwaq = FALSE;
	      TRACE("delwaq vs telemac ok\n");
	      IsOk = TRUE;
	  }
	  else
	  {
              swapbytes32(npoinDelwaq);
	      if(npoinDelwaq == npoin)
	      {
		  InvertDelwaq = TRUE;
	          TRACE("delwaq vs telemac ok\n");
	          IsOk = TRUE;
	      }
	      else
	      {
                 // Delwaq and telemac data files are
		 // incompatible.

	        TRACE("delwaq vs telemac KO \n");
		IsOk = FALSE;
	      }
	  }
	  if(IsOk == TRUE)
	  {
              IsOk = GetVarNamesDelwaq(DelwaqDataFile,
	                        NbVarDelwaq,&VarNamesDelwaq);
	  }
       }
       else
       {
	   // impossible to load serafin2 + delwaq, actually.
	   // sorry.
       }

   }
   TRACE("Fin recuperation variables \n");
   TecUtilStringListAppend(VarNames1,VarNamesDelwaq);
   NbVarTot = NbVar1+NbVarDelwaq;
     

 /* Find the correlation between the variables in the
  * original data set and the variables in the new data
  * set.
  */
  if(IsOk)
  {
      VarCorr = (EntIndex_t*)calloc(NbVarTot,sizeof(EntIndex_t));
      if ( AppendData == TRUE ) 
      {
	  /* Get the variable names of the original data set.
	   */
	  TecUtilDataSetGetInfo(NULL,NULL,&NumVars_orig);
	  // Array indicating the Id of the variable in the 
	  // data set.
	  for(i=0;i<NbVarTot;i++)VarCorr[i]=0;
	  // Variable Id's start at 1. Set to 0 for indicating
	  // "invalid".
	  ivar = 0;
	  // First check : for the variables already in the
	  // data set, find out the corresponding variables in
	  // the file.
	  // --------------------------------------------------
	  // Loop over all variables in the data set already
	  // present :
	  for(j=1;j<=NbVarTot;j++)
	  {
	      VName2 = TecUtilStringListGetString(VarNames1,j);
	      // Remove trailing and heading white spaces
	      // from the variable name.
	      trim(VName2);
	      // Get the variable no i
	      // no correspondance.
	      ivar = -1;
	      // Loop over variables in the file :
              for(i=1;i<=NumVars_orig;i++)
	      {
                  TecUtilVarGetName(i,&VName1);
		  // Compare the name of the variable in the
		  // data set and the file.
		  if(Str_ustrcmp(VName2,VName1) == 0 )
		  {
		      // if the variable is already present,
		      // note it's Id.
		      ivar = j-1;
                      VarCorr[ivar]=i;
		      // We should deallocate the String
		      // coming from tecplot.
                      TecUtilStringDealloc(&VName1);    
		      // exit loop over variables in file.
		      break;
		  }
                  TecUtilStringDealloc(&VName1);    
	      } // end loop over variables in file
	      // Deallocate variable name coming from
	      // tecplot.
	      // If for this variable in the file no
	      // corresponding variable exists in the data
	      // set, add the variable to the data set.
	      if( ivar == -1 ) 
	      {
		  IsOk = AppendVariable(VName2,&i);
		  VarCorr[j-1]=i;
		  /// if variables are added to the data set,
		  // we should loop over all zones and add it,
		  // by giving a type and a precision to the
		  // variable.
		  //
	      }
              TecUtilStringDealloc(&VName2);    
	  }
      }
      else // Create a new data set (AppendData == FALSE)
      {
          TRACE("Create a new data set : \n");
	  // If delwaq data are added, add the variable names
	  // too ...
	  // concatenate the string lists, we don't need them
	  // anymore ...
          TecUtilDataSetCreate(title,VarNames1,TRUE);
	  for(i=0;i<NbVarTot;i++)VarCorr[i]=i+1;
	  VName1 = TecUtilStringListToNLString(VarNames1);
	  TRACE("Variable names : \n");
	  TRACE("-----------------\n");
	  TRACE(VName1);
	  TRACE("\n");
	  TRACE("-----------------\n");
	  TecUtilStringDealloc(&VName1);
      }
      for(i=0;i<NbVarTot;i++)
      {
          sprintf(MessageString,"VarCorr[%d]=%d\n",i,VarCorr[i]);
          TRACE(MessageString);
      }

      // Free the variable name string lists.
      TecUtilStringListDealloc(&VarNames1);
      TecUtilStringListDealloc(&VarNames2);
      TecUtilStringListDealloc(&VarNamesDelwaq);

      // Get the number of the first zone in the data set.
      // If there is no zone actually in the data set, this
      // will be zero.
      TecUtilDataSetGetInfo(NULL, &StartNewZones , NULL);
      sprintf(MessageString,"Start new zone %d\n",StartNewZones);
      TRACE(MessageString);
      TecUtilDataSetGetInfo(NULL,NULL,&NbVarDataSet);

      // Load the data corresponding to the file format. 

      if(FileFormat == SERAFIN2)
      {
          IsOk = LoadDataSera2(DataFile,FileName,Invert,
		               NbVarDataSet,NbVar1,NbVar2,&(VarCorr[0]),
			       StartNewZones );
      }
      else if (FileFormat == SERAFIN)
      {
	  TRACE("Call to load data serafin ...\n");
          IsOk = LoadDataSera(DataFile,FileName,Invert,
		              NbVarDataSet,NbVar1,
		              AddDelwaq,DelwaqDataFile,
			      DelwaqFileName,InvertDelwaq,
			      NbVarDelwaq,
   	                      &(VarCorr[0]),
		              StartNewZones,
			      title,
			      GeomOnly);
      }
      else
      {
	  IsOk = FALSE;
	  TecUtilDialogErrMsg("Bad File Format");
      }
      free(VarCorr);
     
      // Notify the state change "zones added". First, create
      // the set of the zones added.
      //
      // Get the number of zones in the data set after loading the
      // file. 
      TecUtilDataSetGetInfo(NULL, &EndNewZones , NULL);

      // allocate the set of zones added. 
      NewZoneSet = TecUtilSetAlloc(TRUE); 

      // Fill into the set the zones added 
      for(i=StartNewZones+1;i<= EndNewZones;i++)
          TecUtilSetAddMember(NewZoneSet, i, FALSE);

      // notify the state change.
      TecUtilStateChanged(StateChange_ZonesAdded,
	                  (ArbParam_t)NewZoneSet);
  }
  TRACE("end of loadFile\n");
  return (IsOk);
}

/*******************************************************************/
Boolean_t STDCALL LoaderCallback(StringList_pa Instructions) /* IN */
/*******************************************************************/
{

  FILE *DataFile;
  FILE *DelwaqDataFile;
  LgIndex_t Count;
  LgIndex_t InstrIndex;
  char      *Name = NULL;
  char      *Value = NULL;
  Boolean_t IsOk;
  char *FileName=NULL;
  char *DelwaqFileName=NULL;
  int i,n,NbFiles, NbFilesDelwaq;
  int ndelwaq;
  int StartFileList;
  int StartFileListDelwaq;
  Boolean_t AppendData = FALSE;
  Boolean_t AddDelwaq = FALSE;
  Boolean_t GeomOnly = FALSE;
  Boolean_t RetainPlotStyle = FALSE;
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
      else if(strcmp(Name,"DELWAQ_FILELIST_TOLOAD") == 0 )
      {
	 // load a file list. For the Filelist, the value of
	 // the instruction is the number of files to load,
	 // than follow the NbFile file names to load in the
	 // instructions stringlist.
	 NbFilesDelwaq = 0;
	 ParsePtr = Value;
         IsOk = Str_ScanForLgIndex(&ParsePtr,(LgIndex_t*)&NbFilesDelwaq);
	 sprintf(MessageString,
		 "Number of files to read : %d\n",NbFilesDelwaq);
	 TRACE(MessageString);
	 if( IsOk == TRUE) 
	 {
	     sprintf(MessageString,
		     "Number of files to read : %d\n",NbFilesDelwaq);
	     TRACE(MessageString);
	 }
	 else
	 {
	     TRACE("impossible to read the number of files\n");
	     IsOk = FALSE;
	 }
	 StartFileListDelwaq = InstrIndex+2;
	 InstrIndex += NbFilesDelwaq;
      } // end FILENAME_TOLOAD
      else if (strcmp(Name,"APPEND_DATA") == 0 )
      {
	  if(strcmp(Value,"YES") ==0 ) AppendData = TRUE;
      }
      else if (strcmp(Name,"RETAIN_PLOTSTYLE") == 0 )
      {
	  if(strcmp(Value,"YES") ==0) RetainPlotStyle = TRUE;
      }
      else if (strcmp(Name,"ADD_DELWAQ") == 0 )
      {
	  if(strcmp(Value,"YES") ==0) AddDelwaq = TRUE;
      }
      else if (strcmp(Name,"GEOM_ONLY") == 0 )
      {
	  if(strcmp(Value,"YES") ==0) GeomOnly = TRUE;
      }
      InstrIndex += 2;
      TecUtilStringDealloc(&Name);
      TecUtilStringDealloc(&Value);
  }  // end while over instructions
  // dealloc name and value string for instructions
  //TRACE("End recovering instructions. \n");

  if(NbFiles == 0 ) 
  {
     TRACE("No files to load !!!\n");
     IsOk = FALSE;
  }
  else if(AddDelwaq && NbFiles != NbFilesDelwaq)
  {
     TRACE("the number of telemac and delwaq files should be the same!!\n");
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
      n       = StartFileList + i;
      ndelwaq = StartFileListDelwaq + i;
      // get the file name from the instructions list
      sprintf(MessageString,"Try to get file no %d \n",i);
      TRACE(MessageString);
      FileName = TecUtilStringListGetString(Instructions,n);
      // Get the associated delwaq file name
      if(AddDelwaq == TRUE)
          DelwaqFileName = TecUtilStringListGetString(Instructions,ndelwaq);
      // check if file name is valid
      if ( FileName != NULL && strlen(FileName) != 0 ) 
      {
          TRACE("Try to open file < \n");
          TRACE(FileName);
          TRACE(">\n");
          DataFile = fopen(FileName,"rb");
          if ( DataFile == NULL ) 
          {
             TecUtilDialogErrMsg("Invalid Telemac File Name ");
             IsOk = FALSE;
          } // end error open file
      }
      if ( DelwaqFileName != NULL && strlen(DelwaqFileName) != 0 && IsOk == TRUE ) 
      {
          TRACE("Try to open file <\n");
          TRACE(DelwaqFileName);
          TRACE(">\n");
          DelwaqDataFile = fopen(DelwaqFileName,"rb");
          if ( DelwaqDataFile == NULL ) 
          {
              TecUtilDialogErrMsg("Invalid Delwaq File Name ");
              IsOk = FALSE;
          } // end error open file
      }
      else
      {
	  DelwaqDataFile = NULL;
          if(AddDelwaq == TRUE )
	  {
              TecUtilDialogErrMsg("You should provide a Delwaq data file. ");
              IsOk = FALSE;
	  }
	  else
	  {
	  }
      }
      if(IsOk) 
      {
	  TRACE("try to load the data file ... \n");
          IsOk = LoadFile(DataFile,FileName,
		          GeomOnly,
			  AppendData,
			  AddDelwaq,
			  DelwaqDataFile,
			  DelwaqFileName);
	  TRACE("astep1\n");
          fclose(DataFile);
	  TRACE("astep2\n");
          if( DelwaqDataFile != NULL) { fclose(DelwaqDataFile);}
	  TRACE("astep3\n");
	  if(IsOk != TRUE ){ TecUtilDialogErrMsg("Error on loading file");}
	  TRACE("astep4\n");
      }
      
      // in case of multiple file loads, set AppendData to
      // TRUE after the first load ...
      if(IsOk == TRUE) AppendData = TRUE;
      // we should deallocate the file name, recovered from
      // the string list :
      TecUtilStringDealloc(&FileName);
      FileName = NULL;
      DelwaqFileName = NULL;
  } // end loop files
  }

  TRACE("End of file load ...\n");

  if( IsOk == TRUE )
  {
  TRACE("data load ok\n");
    TecUtilViewAxisNiceFit('X',1);
    TRACE("step11\n");
    TecUtilViewAxisNiceFit('Y',1);
    TRACE("step12\n");
    if(TecUtilFrameGetPlotType() == PlotType_Cartesian3D)
    {
    TRACE("step13\n");
        //TecUtilViewAxisNiceFit('Z',1);
        TecUtilReset3DScaleFactors();
        TecUtilReset3DOrigin();
    }
    TRACE("step14\n");
    TecUtilViewDataFit();
    TRACE("step2\n");
    TecUtilViewCenter();
    TRACE("step3\n");
    TecUtilRedraw(TRUE);
    TRACE("step4\n");
    TecUtilImportSetLoaderInstr("Telemac Data Loader",Instructions);
    TRACE("step5\n");
  }
  else
  TRACE("pb during data load\n");
  //IsOk = TRUE;
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
  extern int FileLoadMode;
  extern int DataSetLoadMode;
  extern StringList_pa FileNames;
  extern StringList_pa DelFileNames;
  Boolean_t IsOk;
  extern Boolean_t DoneWithDialog3;
  extern Boolean_t DoneWithDialog2;
  extern Boolean_t DoneWithDialog1;
  extern Boolean_t IsOkDialog3;
  char message[100];
  char *FName, *Name;
  int i;
  int NbTelFiles;
  int NbDelFiles;
  StringList_pa Instructions;

/********************************************************************/

  TecUtilLockStart(AddOnID);
  
  // If there is a dataset in the current frame, start with
  // dialog1.
  if( TecUtilFrameGetPlotType() != PlotType_Sketch )
  {
      DoneWithDialog2 = FALSE;
      BuildDialog2(MAINDIALOGID);
      TecGUIDialogLaunch(Dialog2Manager);
      TRACE("before blockdialog2\n");
      TecGUIBlockForModalDialog(&DoneWithDialog2);
      TRACE("end of dialog2\n");
  }
  
  if ( DataSetLoadMode >  0 )
  {
      DoneWithDialog1 = FALSE;
      BuildDialog1(MAINDIALOGID);
      TecGUIDialogLaunch(Dialog1Manager);
      TRACE("before blockdialog1\n");
      TecGUIBlockForModalDialog(&DoneWithDialog1);
      TRACE("end of dialog1\n");
  }
  else
      FileLoadMode = -1;


  if (FileLoadMode == FileLoad_Telemac ||
      FileLoadMode == FileLoad_Geom      ) 
  {
      IsOk =TecUtilDialogGetFileNames(
	               SelectFileOption_AllowMultiFileRead,
                       &FileNames, "Data Files.", NULL, "*");
      if(IsOk == FALSE ) FileLoadMode = -1;
  }
  else if (FileLoadMode == FileLoad_TelDel )
  {
      // dialog with choice for tecplot and delwaq files
      DoneWithDialog3 = FALSE;
      BuildDialog3(MAINDIALOGID);
      TecGUIDialogLaunch(Dialog3Manager);
      TecGUIBlockForModalDialog(&DoneWithDialog3);
      IsOk = IsOkDialog3;
      if(IsOk == FALSE ) FileLoadMode = -1;
  }

  if (FileLoadMode > 0 )
  {

  TRACE("check for telemac file names ...\n");
  NbTelFiles = TecUtilStringListGetCount(FileNames);
  sprintf(message,"number of telemac files : %d\n",NbTelFiles);
  TRACE(message);

  if(NbTelFiles <= 1 ) 
  {
        TRACE("check for valid telemac file names ...\n");
        Name = TecUtilStringListGetString(FileNames,1);
	TRACE("<");TRACE(Name);TRACE(">\n");
	if(Name == "")
	{
	    TecUtilDialogErrMsg("You should provide as least one valid file name.");
	    IsOk = FALSE;
	}
	TecUtilStringDealloc(&Name);
    }
  if ( IsOk == TRUE ) 
  {

     
    sprintf(message,"load mode : %d\n",FileLoadMode);
    TRACE(message);
    // Construction of the Loader Instructions:
  

    Instructions = TecUtilStringListAlloc();
    TecUtilStringListAppendString(Instructions,"APPEND_DATA");
    if ( DataSetLoadMode == 1 || DataSetLoadMode == 2 ) 
        TecUtilStringListAppendString(Instructions,"NO");
    else
        TecUtilStringListAppendString(Instructions,"YES");
    TecUtilStringListAppendString(Instructions,"RETAIN_PLOTSTYLE");
    if ( DataSetLoadMode == 2 || DataSetLoadMode == 3 ) 
        TecUtilStringListAppendString(Instructions,"YES");
    else
         TecUtilStringListAppendString(Instructions,"NO");

    TecUtilStringListAppendString(Instructions,"GEOM_ONLY");
    if ( FileLoadMode == 3 ) 
        TecUtilStringListAppendString(Instructions,"YES");
    else
        TecUtilStringListAppendString(Instructions,"NO");
    TecUtilStringListAppendString(Instructions,"ADD_DELWAQ");
    if ( FileLoadMode == 2 ) 
        TecUtilStringListAppendString(Instructions,"YES");
    else
        TecUtilStringListAppendString(Instructions,"NO");

    TecUtilStringListAppendString(Instructions,"STANDARDSYNTAX");
    TecUtilStringListAppendString(Instructions,"1.0");

    // add the number of files to load to the stringlist :
    DLoad_AddLgIndexArg(Instructions,"FILELIST_TOLOAD",NbTelFiles);

    // add the Telemac data filenames
    TecUtilStringListAppend(Instructions,FileNames);

    // If delwaq data too, add the name of the data files
    if ( FileLoadMode == 2 ) 
    {
	TRACE("get number of delwaq files ... \n");
        NbDelFiles = TecUtilStringListGetCount(DelFileNames);
        // add the number of files to load to the stringlist :
        DLoad_AddLgIndexArg(Instructions,"DELWAQ_FILELIST_TOLOAD",NbDelFiles);

        // add the Delwaq data filenames
        TecUtilStringListAppend(Instructions,DelFileNames);
    }

    FName = TecUtilStringListToNLString(Instructions);
    TRACE("Instructions : \n");
    TRACE(FName);
    TRACE("\n");
    TecUtilStringDealloc(&FName);
    TecUtilStringListDealloc(&FileNames);

    if(NbDelFiles <= 1 && FileLoadMode == 2 ) 
    {
	TRACE("check delwaq file names (zero names?) \n");
        Name = TecUtilStringListGetString(DelFileNames,1);
	if(Name == "")
	{
	    TecUtilDialogErrMsg("You should provide at least one valid delwaq file name.");
	    IsOk = FALSE;
	}
	TecUtilStringDealloc(&Name);
    }
    if ( FileLoadMode == 2 ) 
        TecUtilStringListDealloc(&DelFileNames);

    TRACE("Call to loadercallback from dialog1okbutton\n");
    if(IsOk == TRUE)
    {
    IsOk = LoaderCallback(Instructions);

    if ( IsOk != TRUE ) 
    {
       TecUtilDialogErrMsg("Error loading the file");
    }
    }

  } // end ok selected file.


    TecUtilStringListDealloc(&Instructions);
  } // if FileLoadMode > 0
  TecUtilLockFinish(AddOnID);
  return;

}





