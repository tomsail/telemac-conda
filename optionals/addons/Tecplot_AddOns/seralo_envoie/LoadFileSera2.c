#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "GUIDEFS.h"
#include "ENGINE.h"
#include "SERAFIN2.h"
#include "UTIL.h"
#include "ADKUTIL.h"

/*******************************************************************/
Boolean_t GetVarNamesSera2(FILE          *DataFile,
                           Boolean_t      Invert,
			   int           *NbVar,
			   int           *NbVarAux,
			   StringList_pa *VarNames,
			   StringList_pa *AuxVarNames)
/*******************************************************************/
/* Function :
 * Read the names of the variables and of the aux variables
 * and put them into the stringlists, allocated by the calling
 * procedure.
 * Skip the variable units.
 */
{
  int i;
  Boolean_t IsOk;
  Boolean_t EndOfFile;
  int FHeader, RefVal;
  // File header informations
  int NumVars;
  char MessageString[100];
  char VName[30];     // variable name
  // integers for file positioning
  long int SkipValue ;

  Boolean_t ReadFHead();
  Boolean_t CheckReadBlock();

/*******************************************************************/
    
  // next entry in the file : two integers, giving the
  // number of variables and the number of aux variables.
  RefVal = 8;
  IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,TRUE,
	               &MessageString,&EndOfFile);
  if ( IsOk ) 
  {
      fread(NbVar,sizeof(int),1,DataFile) ;
      fread(NbVarAux,sizeof(int),1,DataFile) ;

      if(Invert)
      {
          swapbytes32(*NbVar);
          swapbytes32(*NbVarAux);
      }
      IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);

      NumVars = *NbVar + *NbVarAux;

      // skip the units of the calculation. 
      // add 4 header and 4 trailer bytes. a variable unit is
      // 4 characters long.
      SkipValue = 8 + 8;
      fseek(DataFile,SkipValue,1);
      // Read the variable names and the auxvariable names.
      RefVal = 30*NumVars;
      IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,TRUE,
	               &MessageString,&EndOfFile);
  } // if (IsOk)


  if (IsOk) 
  {

      TRACE("Variable names \n");
      for (i=0;i<*NbVar;i++)
      {
          fread(&VName,30,1,DataFile) ;
          VName[29]=0;
          TRACE(VName);
          TRACE("\n");
          TecUtilStringListAppendString(*VarNames,VName);
      }
      TRACE("AUX Variable names \n");
      for (i=0;i<*NbVarAux;i++)
      {
          fread(&VName,30,1,DataFile) ;
          VName[29]=0;
          TRACE(VName);
          TRACE("\n");
          TecUtilStringListAppendString(*AuxVarNames,VName);
      }
  }
      
  IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);

  if(IsOk)
  {
      // skip the variable units section 
      SkipValue = 8 + NumVars*10;
      SkipValue += 2*8 + 2*12 ;
      // skip date and time informations

      fseek(DataFile,SkipValue,1);
  }

  return(IsOk);
}


/*******************************************************************/
Boolean_t LoadDataSera2(FILE *DataFile,
                        char *FileName,
		        Boolean_t Invert,
			int NbVarTot,
		        int NbVar,
		        int NbVarAux,
		        EntIndex_t *VarCorr,
		        EntIndex_t StartNewZones)
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
   
  EntIndex_t var;
  int i;
  Boolean_t IsOk;
  Boolean_t ArgOk;
  Boolean_t EndOfFile;
  int FHeader, RefVal;
  ArgList_pa ArgList ;
  // for tecplot AUTOLOAD on demand
  Boolean_t IsNativeByteOrder; 
  int Stride;
  // File header informations
  char MessageString[100];
  char title[80];
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
  EntIndex_t VarShare;    // variable share list
  int *PassiveVarList; // array for passive variable list

  double *AuxData;
  double *PtrAuxData;
  int IPARAM[20]; // serafin2 iparam section

  int idisc; // type of FE discretization
  int nelem, npoin, nelem_p, npoin_p; // number of points, elements
  int ndp, ndp_p, ipoin;
  ZoneType_e ZTyp;
  EntIndex_t CurZone;
  int ivar;
  // tecplot connectivity :
  NodeMap_pa NodeMap;
  int ShareConnect;
  Strand_t strand;

  FieldDataType_e *VarPrecision;

  Boolean_t ReadFHead();
  Boolean_t CheckReadBlock();

/*******************************************************************/

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

      VarLocation    = (int*)calloc(NbVarTot,sizeof(int));
      PassiveVarList = (int*)calloc(NbVarTot,sizeof(int));
      VarShareList   = (int*)calloc(NbVarTot,sizeof(int));
      VarPrecision   = (FieldDataType_e*)calloc(NbVarTot,sizeof(FieldDataType_e));
      for(i=0;i<NbVarTot;i++)VarPrecision[i]=FieldDataType_Double;
      for(i=0;i<NbVarTot;i++)VarLocation[i]=1;
      for(i=0;i<NbVarTot;i++)PassiveVarList[i]=1;
      for(i=0;i<NbVarTot;i++)VarShareList[i]=0;

      RefVal = 4*NbVar;
      IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,TRUE,&MessageString,&EndOfFile);

      for(i=0;i<NbVar;i++)
      {
          fread(&VarLocation[VarCorr[i]-1],sizeof(int),1,DataFile) ;
	  if(Invert) swapbytes32(VarLocation[VarCorr[i]-1]);
      }
      IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);


      // end of file header section.
      // Get the size of the position of the fileheader :
      FileHeadSize = ftell(DataFile);


      // read the different zones. 
      // For each zone, we need to know the zone parameters
      // and the data value positions in the file.
      izone = StartNewZones;
      
      if(Invert) {IsNativeByteOrder = FALSE;}
      else { IsNativeByteOrder = TRUE;}

      AuxData = (double*)calloc(NbVarAux,sizeof(double));
      PtrAuxData = AuxData;

      nelem = 0;
      npoin = 0;
      ndp   = 0;
      PtrIkle = NULL;

      TRACE("Create argument list \n");
      ArgList = TecUtilArgListAlloc();

      strand = TecUtilDataSetGetMaxStrandID();
      strand += 1;
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
          title[79]=0;
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
	  if(ShareConnect != 0 )
	      ShareConnect = IPARAM[ID_SHARE_CONNECT-1]+StartNewZones;
	  TRACE(" zone iparam end. \n");
	  sprintf(MessageString,"nelem %d, npoin %d, idisc %d ndp %d shareconnect %d \n",nelem,npoin,idisc,ndp,ShareConnect);
	  TRACE(MessageString);

	  ArgOk = TecUtilArgListAppendInt(ArgList,SV_CONNECTSHAREZONE,
		  ShareConnect);
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
	
	 

          ArgOk = TecUtilArgListAppendInt(ArgList,SV_STRANDID,strand);

	  // In case of finite element data :
	  // check for the dimension of the arrays.
	  if ( DataType == DATA_FE &&
	      (nelem_p != nelem || npoin_p != npoin || ndp_p != ndp)  ) 
	  {
	      TRACE("allocation of connectivity table\n");
	     if ( PtrIkle != NULL) free(PtrIkle);  
             PtrIkle = (int*)calloc(nelem*ndp,sizeof(int)); 
	     TRACE("alloc Connectivity ok\n");
	  }
          
	  // variable share list (NbVar integers)
          RefVal = NbVar*4;
          IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
	 
          for(i=0;i<NbVar;i++)
          {
              fread(&VarShareList[VarCorr[i]-1],sizeof(int),1,DataFile) ;
    	      if(Invert) swapbytes32(VarShareList[VarCorr[i]-1]);
          }
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);

	  // Passive variable list (NbVar integers)
          RefVal = NbVar*4;
          IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
          for(i=0;i<NbVar;i++)
          {
              fread(&PassiveVarList[VarCorr[i]-1],sizeof(int),1,DataFile) ;
    	      if(Invert) swapbytes32(PassiveVarList[VarCorr[i]-1]);
          }
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);
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
	  
          fread(PtrAuxData,RefVal,1,DataFile) ;
          TRACE("reading aux data ok.\n");
          if(Invert)
          {
	      for(i=0;i<NbVarAux;i++) swapbytes64(PtrAuxData[i]);
          }
          IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);

          TRACE("Reading auxdata ok\n");
          // Zone header completed, create the tecplot zone.
	  
	  ArgOk = TecUtilArgListAppendInt(ArgList,SV_DEFERVARCREATION,
		                            TRUE);
	  TRACE("step1\n");
	  ArgOk = TecUtilArgListAppendArray(ArgList,SV_VARDATATYPE,
		                            VarPrecision);
	  TRACE("step2\n");
	  IsOk = TecUtilDataSetAddZoneX(ArgList);

	  TRACE("step3\n");
          TecUtilFrameSetPlotType(PlotType_Cartesian3D);

	  TRACE("step1\n");
	  // get the handle of the created zone :
	  TecUtilDataSetGetInfo(NULL,&CurZone,NULL);
	  sprintf(MessageString,"zone no %d\n",CurZone);
	  TRACE(MessageString);
	  izone += 1;

	  sprintf(MessageString,"ShareConnect %d, DataType %d, DATA_FE %d\n",
		  ShareConnect,DataType,DATA_FE);
	  TRACE(MessageString);
	  // if finite element data read the connectivity
	  if ( (ShareConnect == 0) && (DataType == DATA_FE) ) 
	  {
	      TRACE("Read connectivity.\n");
              RefVal = nelem*ndp*4;
              IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
              fread(PtrIkle,RefVal,1,DataFile) ;
              IsOk = CheckReadBlock(DataFile,Invert,FHeader,&MessageString);
	      sprintf(MessageString,"fhead %d , refval %d",FHeader, RefVal);
	      TRACE(MessageString);

              if(Invert)
              {
	          for(i=0;i<nelem*ndp;i++)
		  {

		      swapbytes32(PtrIkle[i]);
		  }
              }

	      // to communicate the node map to tecplot, we
	      // need first a writable reference to the node
	      // map. This is done by the following :
	      TRACE("step 1\n");
	      TecUtilZoneGetInfo(izone,
		      NULL,NULL,NULL,NULL,NULL,NULL,
		      &NodeMap,
		      NULL,NULL,NULL,NULL,NULL,NULL);
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
	      var = (ivar + 1);
	      SkipValue = 0;
	      if(PassiveVarList[VarCorr[ivar]-1] == 0 && VarShareList[VarCorr[ivar]-1] == 0 ) 
	      { 
		  // the number of data points depends on the
		  // variable location cell centered or nodal)
		  NData = IPARAM[VarLocation[VarCorr[ivar]-1]];
	          sprintf(MessageString," izone %d VarCorr[%d]=%d\n",
			                 izone,ivar,VarCorr[ivar]);
		  TRACE(MessageString);
	          sprintf(MessageString,"VarLocation[%d]=%d\n",ivar,VarLocation[VarCorr[ivar]-1]);
		  TRACE(MessageString);
	          sprintf(MessageString,"read %d variable values in file \n",NData);
		  TRACE(MessageString);
                  RefVal = NData*8;
                  IsOk = ReadFHead(DataFile,Invert,RefVal,&FHeader,
		           TRUE,&MessageString,&EndOfFile);
		  if(IsOk == FALSE)
		  {
		      TRACE("error reading header bytes\n");
		      // TRACE(MessageString);
		      return(IsOk);
		  }
	          VarPos = ftell(DataFile);
		  IsOk = TecUtilDataValueAutoLOD(izone,VarCorr[ivar],
			  DataValueStructure_ClassicPlus,FileName,
			  VarPos,Stride,IsNativeByteOrder);
		  fseek(DataFile,RefVal,1);
		  if(IsOk == FALSE)
		  {
		      TRACE("Error for AutoLOD\n");
		      return(IsOk);
		  }
	          sprintf(MessageString,"refval %d ,fheader %d\n",RefVal,FHeader);
		  TRACE(MessageString);
                  IsOk = CheckReadBlock(DataFile,Invert,FHeader,
			                &MessageString);
		  if(IsOk == FALSE)
		  {
		      TRACE("error reading trail bytes\n");
		      return(IsOk);
		  }
	      }
	      else if(VarShareList[ivar] > 0 )
	      {
   	          VarShare = VarShareList[VarCorr[ivar]-1]+(int)StartNewZones; 
		  sprintf(MessageString,"Share variable with zone %d\n",VarShare);
		  TRACE(MessageString);
                  TecUtilDataValueShare(VarShare,CurZone,VarCorr[ivar]);
	      }
	      else TRACE("Variable is passive.\n");
	      IsOk = TRUE;
	  }
      }

  if ( PtrIkle != NULL)  free(PtrIkle);
  free(PassiveVarList);
  free(AuxData);
  free(VarLocation);
  free(VarPrecision);
  free(VarShareList);
  IsOk = TRUE;
  TRACE("end of loadFile\n");
  return (IsOk);
}
