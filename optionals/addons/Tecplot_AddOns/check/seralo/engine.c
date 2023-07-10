#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "GUIDEFS.h"
#include "ENGINE.h"
#include "SERAFIN2.h"
#include "UTIL.h"

Boolean_t STDCALL LoaderCallback(StringList_pa Instructions) /* IN */
{

  EntIndex_t var;
  int i, ret;
  Boolean_t IsOk;
  Boolean_t ArgOk;
  Boolean_t EndOfFile;
  Boolean_t Invert;  // invert byte order in source file or not
  int FHeader, RefVal;
  ArgList_pa ArgList ;
  // for tecplot AUTOLOAD on demand
  Boolean_t IsNativeByteOrder; 
  int Stride;
  LgIndex_t Count;
  LgIndex_t InstrIndex;
  char      *Name = NULL;
  char      *Value = NULL;
  // File header informations
  char FileName[250];
  char title[80];
  int NbVar, NbVarAux, NumVars;
  FILE *DataFile;
  char MessageString[100];
  StringList_pa VarNames;
  StringList_pa AuxVarNames;
  char VName[30];     // variable name
  int *VarLocation;   // variable location (P0 or P1 for FE data)
  int DataType;
  int NData ;
  // integers for file positioning
  long int VarPos ;
  long int SkipValue ;
  long int FileHeadSize;
  // zone header informations
  int izone;
  double time;        // solutiontime for a zone
  int *PtrIkle;       // connectivity
  int *VarShareList;    // variable share list
  int *PassiveVarList; // array for passive variable list
  int *PtrInt;
  double *AuxData; // array for auxdata storing
  int IPARAM[20]; // serafin2 iparam section

  int idisc; // type of FE discretization
  int nelem, npoin, nelem_p, npoin_p; // number of points, elements
  int ndp, ndp_p;
  ZoneType_e ZTyp;
  EntIndex_t CurZone;
  int ivar;
  // tecplot connectivity :
  NodeMap_pa NodeMap;
  int ShareConnect;
  Set_pa ZoneSet;

  FieldDataType_e *VarPrecision;

  Boolean_t ReadFHead();
  Boolean_t CheckReadBlock();

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
  while(IsOk && InstrIndex <= Count )
  {
      Name = TecUtilStringListGetString(Instructions,InstrIndex);
      Value = TecUtilStringListGetString(Instructions,InstrIndex+1);
      TRACE("------\n");
      TRACE(Name);
      TRACE("\n");
      TRACE(Value);
      TRACE("\n");
      InstrIndex += 2;
      if(strcmp(Name,"FILENAME") == 0 )
      {
         strcpy(FileName,Value);
	 TRACE(FileName);
	 TRACE("\n");
	 if ( FileName != NULL && strlen(FileName) != 0 ) 
	 {
	     TRACE("Try to open file \n");
             DataFile = fopen(FileName,"rb");
	     if ( DataFile == NULL ) 
	     {
		 TecUtilDialogErrMsg("Invalid File Name ");
		 IsOk = FALSE;
	     } // end error open file
	 } // end valid file name
      } // end FILENAME_TOLOAD
  }  // end while over instructions

  // dealloc name and value string for instructions
  TecUtilStringDealloc(&Name);
  TecUtilStringDealloc(&Value);
  TRACE("End recovering instructions. \n");

  // END OF parsing instructions 
  // File is open now, if IsOk is true.
  if ( IsOk == TRUE ) 
  {
    
/* Read the file header informations. There we will find the
 * variable names, the kind of data in the file,
 * the variable locations. All data in the file are double
 * precision values.
 * The number of points may vary from zone to zone.
 * The total number of zones in the file is unknown.
 *
 * The file is written by fortran. We should add to each block
 * written in the file 4 header and trailing bytes.
 * This will allow us to define the byte order in the file.
 */

      // The first thing to read are the 4 head bytes for the
      // title of the data file. This title is 80 charcters
      // long, so the first valu to read is 80. If this is not
      // 80, try to invert byte order and check again. If the
      // header byte of the title is not 80, error.
      fread(&FHeader,sizeof(int),1,DataFile);
      if ( FHeader != 80 ) 
      {
	  swapbytes32(FHeader) ;
	  if ( FHeader == 80 ) 
	  {
	      Invert = TRUE;
	  }
	  else
	  {
	       TecUtilDialogErrMsg("Invalid File Format");
               IsOk = FALSE;
	  }
      }
      if(IsOk == TRUE)
      {
          fread(&title,80,1,DataFile);
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);
      }
      TRACE("title of the dataset in file :\n");
      TRACE(title);
      TRACE("\n");
     
      // next entry in the file : two integers, giving the
      // number of variables and the number of aux variables.
      RefVal = 8;
      IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,TRUE,
	               &MessageString,&EndOfFile);
      fread(&NbVar,sizeof(int),1,DataFile) ;
      fread(&NbVarAux,sizeof(int),1,DataFile) ;
      if(Invert)
      {
	  swapbytes32(NbVar);
	  swapbytes32(NbVarAux);
      }
      IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);

      NumVars = NbVar + NbVarAux;

      // skip the units of the calculation. 
      // add 4 header and 4 trailer bytes. a variable unit is
      // 4 characters long.
      SkipValue = 8 + 8;
      fseek(DataFile,SkipValue,1);
      // Read the variable names and the auxvariable names.
      VarNames = TecUtilStringListAlloc();
      AuxVarNames = TecUtilStringListAlloc();
      RefVal = 30*NumVars;
      IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,TRUE,
	               &MessageString,&EndOfFile);

      TRACE("Variable names \n");
      for (i=0;i<NbVar;i++)
      {
          fread(&VName,30,1,DataFile) ;
	  TRACE(VName);
	  TRACE("\n");
	  TecUtilStringListAppendString(VarNames,VName);
      }
      TRACE("AUX Variable names \n");
      for (i=0;i<NbVarAux;i++)
      {
          fread(&VName,30,1,DataFile) ;
	  TRACE(VName);
	  TRACE("\n");
	  TecUtilStringListAppendString(AuxVarNames,VName);
      }
      
      IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);

      // skip the variable units section 
      SkipValue = 8 + NumVars*10;
      SkipValue += 2*8 + 2*12 ;
      // skip date and time informations

      ret = fseek(DataFile,SkipValue,1);


      // Read the data type in the file :
      RefVal = 4;
      IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,TRUE,
	               &MessageString,&EndOfFile);

      fread(&DataType,sizeof(int),1,DataFile) ;

      if(Invert) swapbytes32(DataType);
      sprintf(MessageString,"data type : %d\n",DataType);
      TRACE(MessageString);
      IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);

      
      // read the variable locations 
      // first allocate the table 

      VarLocation    = (int*)calloc(NbVar,sizeof(int));
      PassiveVarList = (int*)calloc(NbVar,sizeof(int));
      VarShareList   = (int*)calloc(NbVar,sizeof(int));
      VarPrecision   = (FieldDataType_e*)calloc(NbVar,
	                                sizeof(FieldDataType_e));
      for(i=0;i<NbVar;i++)VarPrecision[i]=FieldDataType_Double;

      RefVal = 4*NbVar;
      IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,TRUE,&MessageString,&EndOfFile);

      for(i=0;i<NbVar;i++)
      {
          fread(&VarLocation[i],sizeof(int),1,DataFile) ;
	  if(Invert) swapbytes32(VarLocation[i]);
      }
      IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);


      // end of file header section.
      // Get the size of the position of the fileheader :
      FileHeadSize = ftell(DataFile);


      // create the data set :
      
      TRACE("Create data set : \n");
      TecUtilDataSetCreate(title,VarNames,TRUE);

      TRACE("Create data set ok\n");

      // read the different zones. 
      // For each zone, we need to know the zone parameters
      // and the data value positions in the file.
      izone = 0;
      
      if(Invert) {IsNativeByteOrder = FALSE;}
      else { IsNativeByteOrder = TRUE;}

      // allocate the variable share list and the passive
      // variable list :
      AuxData  = (double*)calloc(NbVar,sizeof(double));

      nelem = 0;
      npoin = 0;
      ndp   = 0;
      PtrIkle = NULL;

      TRACE("Create argument list \n");
      ArgList = TecUtilArgListAlloc();

      // Loop over all zones in the file :
      
      IsOk = TRUE;
      TRACE("start reading the zones \n");
      while (IsOk == TRUE )
      {
	  TecUtilArgListClear(ArgList);
	  TRACE("Read zone : \n");
          // Read the zone title (80 chars)
          RefVal = 80;
	  TRACE(" zone title \n");
          IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,TRUE,
		           &MessageString,&EndOfFile);
	  if ( EndOfFile == TRUE ) break;
          fread(&title,RefVal,1,DataFile) ;
	  TRACE(title);
	  TRACE("\n");
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);
	  ArgOk = TecUtilArgListAppendString(ArgList,SV_NAME,title);
	 
  	  // Solution time (double precision)
	  TRACE(" zone solution time \n");
          RefVal = 8;
          IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
          fread(&time,sizeof(double),1,DataFile) ;

          if(Invert)
          {
	      swapbytes64(time);
          }
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);
	  ArgOk = TecUtilArgListAppendDouble(ArgList,
		                             SV_SOLUTIONTIME,time);

	  // iparam section (20 integers)
	  TRACE(" zone iparam \n");
          RefVal = 80;
          IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
          fread(&IPARAM,sizeof(IPARAM),1,DataFile) ;

          if(Invert)
          {
	      for(i=0;i<20;i++) swapbytes32(IPARAM[i]);
          }
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);

	  // Read in the iparam section the number of nodes,
	  // points ....
	  nelem_p = nelem;
	  npoin_p = npoin;
	  ndp_p   = ndp;
	  // C tables are indexed starting with 0 !!!!
	  nelem   = IPARAM[ID_NELEM-1];
	  npoin   = IPARAM[ID_NPOIN-1];
	  ndp     = IPARAM[ID_NDP-1];
	  idisc   = IPARAM[ID_DISC-1];

	  ShareConnect = IPARAM[ID_SHARE_CONNECT-1];
	  TRACE(" zone iparam end. \n");
	  sprintf(MessageString,"nelem %d, npoin %d, idisc %d shareconnect %d \n",nelem,npoin,idisc,ShareConnect);
	  TRACE(MessageString);

	  ArgOk = TecUtilArgListAppendInt(ArgList,SV_IMAX,npoin);
	  i = 1 ;
	  ArgOk = TecUtilArgListAppendInt(ArgList,SV_KMAX,i);
	  i = MAX(nelem,i) ;
	  ArgOk = TecUtilArgListAppendInt(ArgList,SV_JMAX,i);

          // get the type of the discretization
          switch(idisc)
          {
	      case POINTS :  ZTyp = ZoneType_Ordered;
	    		     TRACE("Point Like Data \n");
			     break;
	      case FE_TRI :  ZTyp = ZoneType_FETriangle;
		  	     TRACE("FE Triangle\n");
			     break;
    	      case FE_TETRA: ZTyp = ZoneType_FETetra;
			     TRACE("FE Tetra\n");
			     break;
	      default : IsOk  = FALSE ; 
			     TRACE("Unknown discretizations.\n");
			break;
           }
	  ArgOk = TecUtilArgListAppendInt(ArgList,SV_ZONETYPE,ZTyp);
	  // automatic stranding does not work??
	  ArgOk = TecUtilArgListAppendInt(ArgList,SV_STRANDID,1);

	  // In case of finite element data :
	  // check for the dimension of the arrays.
	  if ( DataType == DATA_FE &&
	      (nelem_p != nelem || npoin_p != npoin || ndp_p != ndp)  ) 
	  {
	      TRACE("allocation of connectivity table\n");
	     if ( PtrIkle != NULL) free(PtrIkle);  
             PtrIkle = (int*)calloc(nelem*ndp,sizeof(int)); 
	  }
          
	  TRACE("alloc Connectivity ok\n");
	  // variable share list (NbVar integers)
          RefVal = NbVar*4;
          IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
          fread(VarShareList,RefVal,1,DataFile) ;

          if(Invert)
          {
	      for(i=0;i<NbVar;i++) swapbytes32(VarShareList[i]);
          }
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);
// ArgOk = TecUtilArgListAppendArray(ArgList,SV_VARSHAREZONELIST,
//                            VarShareList);
	  TRACE("Variable share list ok.\n");
	  
	  // Passive variable list (NbVar integers)
          RefVal = NbVar*4;
          IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
          fread(PassiveVarList,RefVal,1,DataFile) ;
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);
          if(Invert)
          {
	      for(i=0;i<NbVar;i++) swapbytes32(PassiveVarList[i]);
          }
	  /*ArgOk = TecUtilArgListAppendArray(ArgList,SV_PASSIVEVARLIST,
	                            PassiveVarList);
				    */
	  TRACE("passive variable list ok\n");
	  // variable location (cell centered or node
	  // centered)
	  ArgOk = TecUtilArgListAppendArray(ArgList,SV_VALUELOCATION,
		                            VarLocation);
	  // Aux data (NbVarAux double precision values)
          RefVal = NbVarAux*8;
          IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
	  TRACE(MessageString);
          fread(AuxData,RefVal,1,DataFile) ;
          TRACE("reading aux data ok.\n");
          if(Invert)
          {
	      for(i=0;i<NbVarAux;i++) swapbytes64(AuxData[i]);
          }
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);

          TRACE("Reading auxdata ok\n");
          // Zone header completed, create the tecplot zone.
	  
	  ArgOk = TecUtilArgListAppendInt(ArgList,SV_DEFERVARCREATION,
		                            TRUE);
	  ArgOk = TecUtilArgListAppendArray(ArgList,SV_VARDATATYPE,
		                            VarPrecision);
	  IsOk = TecUtilDataSetAddZoneX(ArgList);

          TecUtilFrameSetPlotType(PlotType_Cartesian3D);

	  // get the handle of the created zone :
	  TecUtilDataSetGetInfo(NULL,&CurZone,NULL);
	  sprintf(MessageString,"zone no %d\n",CurZone);
	  TRACE(MessageString);
	  izone += 1;

	  TRACE("read connectivity?\n");
	  sprintf(MessageString,"ShareConnect %d, DataType %d, DATA_FE %d\n",ShareConnect,DataType,DATA_FE);
	  TRACE(MessageString);
	  // if finite element data read the connectivity
	  if ( (ShareConnect == 0) && (DataType == DATA_FE) ) 
	  {
	      TRACE("Read connectivity.\n");
              RefVal = nelem*ndp*4;
              IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
              fread(PtrIkle,RefVal,1,DataFile) ;

              if(Invert)
              {
	          for(i=0;i<nelem*ndp;i++) swapbytes32(PtrIkle[i]);
              }
              IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);
	      // to communicate the node map to tecplot, we
	      // need first a writable reference to the node
	      // map. This is done by the following :
	      TRACE("step 1\n");
	      TecUtilZoneGetInfo(izone,
		      NULL,NULL,NULL,NULL,NULL,NULL,
		      &NodeMap,
		      NULL,NULL,NULL,NULL,NULL,NULL);
	      // THIS DOES **NOT** WORK. should define
	      // plottype first, but this is not possible,
	      // actually ...
	      TRACE("step 2\n");
	      TecUtilDataNodeArraySetByRef(NodeMap,
		      1,nelem*ndp,(NodeMap_t*)PtrIkle);
	      TRACE("step ok\n");
	  }
	  

	  // than comes the data arrays.
	  // If variable is not declared to be passive or
	  // shared, get the address in the file of the
	  // variable and communicate this value to tecplot.
	  
	  Stride = 1;
	  for(ivar=0;ivar<NbVar;ivar++)
	  {
	      TRACE("read variable \n");
	      var = (ivar + 1);
	      SkipValue = 0;
	      if(PassiveVarList[ivar] == 0 && VarShareList[ivar] == 0 ) 
	      { 
	          // add the header bytes 
		  SkipValue += 4;
	          VarPos += SkipValue;
		  // the number of data points depends on the
		  // variable location cell centered or nodal)
		  NData = IPARAM[VarLocation[ivar]];
                  RefVal = NData*8;
                  IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
	          VarPos = ftell(DataFile);
		  if(IsOk == FALSE)
		  {
		      TRACE("error reading header bytes\n");
		      TRACE(MessageString);
		      return(IsOk);
		  }
		  IsOk = TecUtilDataValueAutoLOD(izone,var,
			  DataValueStructure_Classic,FileName,
			  VarPos,Stride,IsNativeByteOrder);
		  fseek(DataFile,RefVal,1);
                  IsOk = CheckReadBlock(DataFile,Invert,FHeader,
			                &MessageString);
		  if(IsOk == FALSE)
		  {
		      TRACE("error reading trail bytes\n");
		      TRACE(MessageString);
		      return(IsOk);
		  }
	      }
	      else if(VarShareList[ivar] >= 0 )
	      {
                  TecUtilDataValueShare(VarShareList[ivar],CurZone,var);
	      }
	      IsOk = TRUE;
	  }
      }
      close(DataFile);
    

   // choose the first zone to display
   
   

   // nice fit for x,y,z
   //
   TecUtilViewAxisNiceFit('X',1);
   TecUtilViewAxisNiceFit('Y',1);
   TecUtilViewAxisNiceFit('Z',1);


   if ( PtrIkle != NULL)  free(PtrIkle);
   free(VarNames);
   free(PassiveVarList);
   free(VarLocation);
   free(VarShareList);
  
  }
  IsOk = TRUE;
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

void STDCALL LoaderSelectedCallback(void)
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
  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}





