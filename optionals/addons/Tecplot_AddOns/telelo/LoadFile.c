/*********************************************************************
 * AddOn for loading files of the telemac Serafin or Volfin data
 * format
 * into tecplot.
 * written by R Nebauer EDF R&D LNHE, may 2007
 * regina.nebauer@edf.fr
 * DoConversion.c - converts a serafin or volfin datafile into a
 * temporary tecplot binary file, loaded into tecplot.
 **********************************************************************/


#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"
#include "UTIL.h"
#include "ADKUTIL.h"
#include <string.h>

void trim( char *str ) 
{
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



/************************************************************************/
Boolean_t LoadFile(
/************************************************************************/
          FILE *f           ,  /* IN - the file to read  */
	  char *FileName,
	  Boolean_t *AppendData)
/************************************************************************/
{
/* Local Variables */

  Boolean_t IsOk = TRUE  ;
  int   i , j         ;
  int   NumVars       ;
  EntIndex_t  NumVars_orig  ;
  int   NbV1 ;
  int   NbV2 ;
  int ivar;
  char  title[81]     ;
  char key_title[9]   ;
  int   ndp           ;
  int   nelem         ;
  int   npoin         ;
  int   nplan         ;
  StringList_pa VarNames;
  char MessageString[100];

  int   *PtrIkle    ; /* Connectivity Table */
  int   NDataPoints ; /* Number of data points */

  int   TimeStep;
  int FileFormat;
  Boolean_t FirstDataSet ;
  Boolean_t EndOfFile;
  Boolean_t ArgOk;
  Boolean_t IsNativeByteOrder;
  int ShareConnect;
  int Stride=1;
  ZoneType_e ZTyp;
  ArgList_pa Arguments;
  int izone;
  char *VName1,*VName2;


  int FHeader ; /* Head bytes in fortran files */
  int RefVal ;

  long int VarPosX,VarPosY,VarPos;
  EntIndex_t *VarCorr;

  int ndp_tec ; /* Number of nodes for element in tecplot */

  float time;
  double time_double;
  int kmax;
  ValueLocation_e *ValueLoc, Loc;
  FieldDataType_e *VarPrecision;


  Boolean_t Invert ;

  NodeMap_pa NodeMap;

  EntIndex_t StartNewZones;
  EntIndex_t EndNewZones;
  Set_pa NewZoneSet;
  EntIndex_t CurZone;


  Boolean_t GetVarNames();
  Boolean_t GetMeshInfo();
  Boolean_t ReadMesh();
  Boolean_t CheckReadBlock();
  Boolean_t ReadFHead();

  int DetectFormat();

  Boolean_t MeshOnly;

/************************************************************************/

  TRACE("Enterend LoadFile\n");
  Invert = FALSE ;
  IsNativeByteOrder = TRUE;
  MeshOnly = FALSE ;

  /* Telemac files are written by binary Fortran write procedures.
   * Fortran adds before each block written a 4byte integer indicating
   * the size of the following block.
   * In C, we have to read this block. We are lucky : Knowing the size
   * of the title (80 caracters), we can detect if the file is written
   * in the same byte order than the present machine. If not, use
   * inverse macros for numeric values 
   * These 4 bytes are added again at the END of the block written.
   * So there are 4 trailing bytes, which should have the same value
   * than the four header bytes!!!
   *
   * So, here read the first 4 bytes and than the title:
   * */
  
  fread(&FHeader,sizeof(int),1,f);
  if(FHeader != 80)
  {   // If the value is not the expected 80, this may have two
      // reasons : this is NOT a serafin data file, or the byte order 
      // should be inversed. First, try to onvert. 
      swapbytes32(FHeader) ;

	  if(FHeader == 80 ) 
	  {
	      Invert = TRUE;
	      IsNativeByteOrder = FALSE;
	      TRACE("Inverse byte order.\n");
	  }
	  // if not 80, even if inverted, this is not the right file!!
	  else
	  {
              strcpy(MessageString ,"Sorry, not the right file!");
              TecUtilDialogErrMsg(MessageString);
              IsOk = FALSE ;
	      TRACE("Error reading header byte.\n");
              return(IsOk);
	  }
  }
  TRACE("End of reading head bytes\n");

/* Read the title of the data set */
  if ( fread(&title,80,1,f) != 1 )
    {
      strcpy(MessageString,"Could not read the title of the data file");
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
      return(IsOk);
    }

  title[80]=0;
  TRACE(title);
  TRACE("\n");
  Str_CopySubString(key_title,title,72,9);
  TRACE("<");
  TRACE(key_title);
  TRACE(">\n");
  if(strcmp(key_title,"SERAPHIN"))
  {
      strcpy(MessageString,"Bad File format.");
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
      return(IsOk);
  }
	  
	 
  /* CHECK TODO : the title is only 72 caracters long. the last 8
   * caracters are the word "SERAPHIN".
   */

  /* Read the trailing bytes : */
  IsOk = CheckReadBlock(f,Invert,FHeader,&MessageString);
  if(!IsOk)
  {
      TecUtilDialogErrMsg(MessageString);
      TRACE("Error Reading trail bytes of title\n");
      return(IsOk);
  }
//----------------------------------------------------------------------------
/* Read the number of variables in the data set 
 * These are two 4 byte integers, so head and trail should be 2*4=8 :
 */

  RefVal = 2 * sizeof(int);

  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,&MessageString,&EndOfFile);
  if(!IsOk)
  {
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      return(IsOk);
  }
  if(EndOfFile) 
  {
      strcpy(MessageString,"Unexpected End Of File ");
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
      return(IsOk);
  }
  // read the number of variables
  if ( fread(&NbV1,sizeof(int),1,f) != 1)
  {
      strcpy(MessageString,"Could not read the data file");
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
      return(IsOk);
  }
  if(Invert) swapbytes32(NbV1) ;
  // read the number of variables 2 :
  if ( fread(&NbV2,sizeof(int),1,f) != 1)
  {
      strcpy(MessageString,"Could not read the data file");
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
      return(IsOk);
  }
  if(Invert) swapbytes32(NbV2) ;

  sprintf(MessageString,"NbV1 : %d NbV2 : %d \n",NbV1,NbV2);
  TRACE(MessageString);

  TRACE("Got he number of variables to read.\n");
  // check the trailer bytes
  IsOk = CheckReadBlock(f,Invert,FHeader,&MessageString);
  if(!IsOk)
  {
    TecUtilDialogErrMsg(MessageString);
    return(IsOk);
  }

/* The total number of variables in the file */

  NumVars = NbV1 + NbV2 ;


  /* Read the variable names */
  TRACE("Try to get the variable names \n");
  VarNames = TecUtilStringListAlloc();

  IsOk = GetVarNames(f,Invert,NumVars,&VarNames,&MessageString) ;
  if (!IsOk)
  {
    TecUtilDialogErrMsg(MessageString);
    return(IsOk);
  }
  i = TecUtilStringListGetCount(VarNames);
  sprintf(MessageString,"NumVars %d count %d\n",NumVars,i);
  TRACE(MessageString);
  NumVars = TecUtilStringListGetCount(VarNames);


  VarCorr = (EntIndex_t*)calloc(NumVars,sizeof(EntIndex_t));
  if ( *AppendData == TRUE ) 
  {
      TRACE("Append data to original data set : \n");
      TecUtilDataSetGetInfo(NULL,NULL,&NumVars_orig);
      for(i=0;i<NumVars;i++)VarCorr[i]=0;
	  ivar = 0;
          for(i=1;i<=NumVars_orig;i++)
	  {
              TecUtilVarGetName(i,&VName1);
	      ivar = -1;
	      for(j=1;j<=NumVars;j++)
	      {
		  VName2 = TecUtilStringListGetString(VarNames,j);
		  trim(VName2);
		  if(Str_ustrcmp(VName2,VName1) == 0 )
		  {
		      ivar = j-1;
                      VarCorr[ivar]=i+2;
                      TecUtilStringDealloc(&VName2);    
		      break;
		  }
                  TecUtilStringDealloc(&VName2);    
	      }
              TecUtilStringDealloc(&VName1);    
	      if(ivar == -1 || VarCorr[ivar] == 0 ) 
	      {

		  TecUtilDataSetAddVarX();
                  strcpy(MessageString,"Different variables in the data sets. Stop.\n");           
		  TecUtilDialogErrMsg(MessageString);
		  free(VarCorr);
		  TecUtilStringListDealloc(&VarNames);
		  return(FALSE);
           }
       }
   }
   else
  {
      TRACE("Create data set : \n");
      TecUtilDataSetCreate(title,VarNames,TRUE);
      for(i=0;i<NumVars;i++)VarCorr[i]=i+2;
  }

  for(i=0;i<NumVars;i++)
  {
      sprintf(MessageString,"VarCorr[%d]=%d\n",i,VarCorr[i]);
      TRACE(MessageString);
  }
  TecUtilDataSetGetInfo(NULL, &StartNewZones , NULL);
  TecUtilStringListDealloc(&VarNames);


 TRACE("Try to get mesh info .. \n");
  /* Read the mesh info */
  IsOk = GetMeshInfo(f,Invert,&nelem,&npoin,&ndp,&nplan,&MessageString) ;
  if (!IsOk)
  {
    TecUtilDialogErrMsg(MessageString);
    return(IsOk);
  }

  if( ndp == 6 ) ndp_tec = 8; else ndp_tec = ndp ;
  PtrIkle = (int*)  calloc(nelem*ndp_tec,sizeof(int));

  /* Read the connectivity and the coordinates in X an Y.
   * PtrIkle contains the connectivity table in Tecplot format
   * (conversion telemac to tecplot done while reading ...)
   */

  TRACE("Read the mesh ..\n");
  IsOk = ReadMesh(f,Invert,nelem,npoin,ndp,nplan,PtrIkle,
	          &VarPosX,&VarPosY,&MessageString) ;
  TRACE("end of reading mesh\n");
  if (!IsOk)
  {
      TRACE(MessageString);
    TecUtilDialogErrMsg(MessageString);
    return(IsOk);
  }

  TRACE("step1\n");
  if(feof(f))
  {
      MeshOnly = TRUE;
  }


  /* Detect the file format : serafin or volfin.  */

  TRACE("step2\n");
  if(!MeshOnly)
  FileFormat = DetectFormat(f,Invert,&EndOfFile,nelem,npoin);

  TRACE("step3\n");
  if(EndOfFile)
  {
      strcpy(MessageString,"Unexpected end of file");
      TRACE(MessageString);
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE ;
  }
  
  TRACE("step4\n");
  switch ( FileFormat )
  {
      case 1 : NDataPoints = nelem ;
	       TRACE("Dataformat volfin\n");
	       break;

      case 2 : NDataPoints = npoin ;
	       TRACE("Dataformat serafin\n");
	       break;

      default : IsOk = FALSE ;
                strcpy(MessageString,"Unknown File Format");
		TRACE(MessageString);
                TecUtilDialogErrMsg(MessageString);
                IsOk = FALSE;
    break;
  }
  
  TRACE("step5\n");
/* Allocate the memory for the data set to read */

  i = NumVars;
  if (i==0) i=1;

  // The tecplot FE type : 
  switch (ndp_tec )
  {
      case 3 : ZTyp = ZoneType_FETriangle;
	       break;
      case 4 : ZTyp = ZoneType_FEQuad;
               break ;
      case 8 : ZTyp = ZoneType_FEBrick ;
               break ;
      default : strcpy(MessageString,"Unknown FE Format");
      TecUtilDialogErrMsg(MessageString);
      		IsOk = FALSE;
                break ;
	      
  }
  TRACE("step6\n");
  // the number ot use for the KMAX parameter :
  kmax = 1;
  /* Valuelocation SV_VALUELOCATION ValueLocation_e*
   * default value : Null, all data are nodal.
   * otherwise array containing the lcation of the variables :
   * ValueLocation_CellCentered or ValueLocation_Nodal
   */
  ValueLoc = (ValueLocation_e*)calloc(NumVars+2,sizeof(int));

  TRACE("step7\n");
  if(NDataPoints == npoin ) {Loc = ValueLocation_Nodal;}
  else if(NDataPoints == nelem) { Loc = ValueLocation_CellCentered;}
  else
  {
      strcpy(MessageString,"Unconsistent number of data points");
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE ;
  }

  TRACE("step8\n");
  for(i=2;i<NumVars+2;i++) { ValueLoc[i] = Loc; }
  ValueLoc[0] = ValueLocation_Nodal ;
  ValueLoc[1] = ValueLocation_Nodal ;
  
  VarPrecision   = (FieldDataType_e*)calloc(NumVars+2,sizeof(FieldDataType_e));
  for(i=0;i<NumVars+2;i++)VarPrecision[i]=FieldDataType_Float;

  TRACE("step9\n");
  /* Allocate the argument list */
  Arguments = TecUtilArgListAlloc();
  if(Arguments == NULL)
  {
      strcpy(MessageString,"Failed allocation of Arglist");
      TecUtilDialogErrMsg(MessageString);
      IsOk = FALSE;
   }

  TRACE("step10\n");
  /* Read the data sets and write them to the temp tecfile. */
  TimeStep     = 0;
  izone = StartNewZones+1;
  FirstDataSet = TRUE ;
  EndOfFile    = FALSE ;
  RefVal       = NDataPoints * sizeof(float);

  TRACE("step11\n");
  if(IsOk)
{
  while (!EndOfFile)
  {

      /* Read header for the time ... */
      RefVal = sizeof(float);
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,&MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      if(EndOfFile)
      {
	  TRACE("End of fil reached.\n");
          break;
      }

      /* Read the time ... */
      if(fread(&time,sizeof(float),1,f)!=1)
      {
          strcpy(MessageString,"Error reading time");
          TecUtilDialogErrMsg(MessageString);
          IsOk = FALSE;
          break;
      }
      if(Invert){ swapbytes32(time);  }
      time_double = (double)time;
  
      /* Read trailer for the time ... */
      IsOk = CheckReadBlock(f,Invert,FHeader,&MessageString);
      if(!IsOk)
      {
          strcpy(MessageString,"Error reading trail bytes time");
          TecUtilDialogErrMsg(MessageString);
          IsOk = FALSE;
          break;
      }

      if(!EndOfFile ) 
      {
        /* The zone title SV_TITLE char* 
         * use the solution time as zone title */
      
        sprintf(title,"Zone t= %f ",time);
	TRACE(title);
	TRACE("\n");

	TecUtilArgListClear(Arguments);
        ArgOk = TecUtilArgListAppendString(Arguments,SV_NAME,title);
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_ZONETYPE,ZTyp);
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_DEFERVARCREATION, TRUE);
        ArgOk = TecUtilArgListAppendArray(Arguments,SV_VARDATATYPE,
                                                VarPrecision);


        /* Number of nodes SV_IMAX LgIndex_t  */
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_IMAX,npoin);

        /* Number of elements SV_JMAX LgIndex_t */
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_JMAX,nelem);

        ArgOk = TecUtilArgListAppendInt(Arguments,SV_KMAX,kmax);

        /* Solution Time SV_SOLUTIONTIME double */
        ArgOk = TecUtilArgListAppendDouble(Arguments,SV_SOLUTIONTIME,
		                           time_double);

        /* StrandId  SV_STRANDID LgIndex_t
         * 0 for static 
         * > 1 for transient ( +1 for each timestep )
         * -1 for automatic based on solution time */

        ArgOk = TecUtilArgListAppendInt(Arguments,SV_STRANDID,1);

        ArgOk = TecUtilArgListAppendArray(Arguments,SV_VALUELOCATION,ValueLoc);

        /* Share connectivity from zone : SV_CONNECTSHAREZONE LgIndex_t
         * 0 if connectivity not shared, otherwise no of the zone
         * defining the connectivity to share */
	if(FirstDataSet){ShareConnect = 0;}
	else {ShareConnect = StartNewZones+1;} 
        ArgOk = TecUtilArgListAppendInt(Arguments,SV_CONNECTSHAREZONE,
		                        ShareConnect);


        /* Create the zone with the argument list */

        IsOk = TecUtilDataSetAddZoneX(Arguments);
        if ( !IsOk )
        {
            strcpy(MessageString,"Error creating zone");
            TecUtilDialogErrMsg(MessageString);
            IsOk = FALSE ;
            break;
        }
        TecUtilDataSetGetInfo(NULL,&CurZone,NULL);
        TecUtilFrameSetPlotType(PlotType_Cartesian3D);
        sprintf(MessageString,"zone no %d\n",izone);
        TRACE(MessageString);
     
        ///// END OF CREATING TECPLOT ZONE
      }


      if(FirstDataSet)
      {
	  TRACE("write connectivity\n");
	  // get a writable handle to the node map and write
	  // it.
          TecUtilZoneGetInfo(izone,
                 NULL,NULL,NULL,NULL,NULL,NULL,
                 &NodeMap,
                 NULL,NULL,NULL,NULL,NULL,NULL);
	  TecUtilDataNodeArraySetByRef(NodeMap,
	                               1,nelem*ndp_tec,(NodeMap_t*)PtrIkle);
	  free(PtrIkle);

	  // The X and Y coordinates does not change in the
	  // file. Load the data here (first data set).
	  TRACE("get node coordinates\n");
          TecUtilDataValueAutoLOD(izone,1,
		  DataValueStructure_ClassicPlus,FileName,
		  VarPosX,1,IsNativeByteOrder);
          TecUtilDataValueAutoLOD(izone,2,
		  DataValueStructure_ClassicPlus,FileName,
		  VarPosY,1,IsNativeByteOrder);
	  TRACE("end node coordinates\n");
      }
      else
      {
	  // If this is not the first time step,
	  //share connectivity and X,Y coordinates
	  TecUtilDataValueShare(StartNewZones+1,CurZone,1);
	  TecUtilDataValueShare(StartNewZones+1,CurZone,2);
      }


      if (!MeshOnly)
      {
      for(i=0;i<NumVars;i++)
      {
	  sprintf(MessageString,"Variable no %d\n",i);
	  TRACE(MessageString);
          /* Read header for datapoints of one variable ... */
	  RefVal = NDataPoints*4;
          IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,&MessageString,&EndOfFile);
          if(!IsOk) break;
          if(EndOfFile)
          {
              strcpy(MessageString,"Unexpected End Of File");
	      TRACE(MessageString);
              TecUtilDialogErrMsg(MessageString);
	      break;
          }
          /* Read datapoints of one variable ... */
	  VarPos = ftell(f);
	  // CALL TECUTILDATAVALUEAUTOLOD
          TecUtilDataValueAutoLOD(izone,VarCorr[i],
		  DataValueStructure_ClassicPlus,FileName,
		  VarPos,Stride,IsNativeByteOrder);
	  fseek(f,RefVal,1);
          /* Read trailer for datapoints of one variable ... */
          IsOk = CheckReadBlock(f,Invert,FHeader,&MessageString);
	  if(!IsOk)
          {
              strcpy(MessageString,"Error reading trail bytes\n");
	      TRACE(MessageString);
              TecUtilDialogErrMsg(MessageString);
	      break;
          }
      }
      }

      FirstDataSet = FALSE;
      TimeStep++;
      izone++;
      
  }
} // if(IsOk)

  // Notify the state change "zones added". First, create
  // the set of the zones added.
  TecUtilDataSetGetInfo(NULL, &EndNewZones , NULL);
  NewZoneSet = TecUtilSetAlloc(TRUE);
  for(i=StartNewZones+1;i<= EndNewZones;i++)
         TecUtilSetAddMember(NewZoneSet, i, FALSE);
  TecUtilStateChanged(StateChange_ZonesAdded,(ArbParam_t)NewZoneSet);

  free(ValueLoc);
  free(VarPrecision);
  TecUtilArgListDealloc(&Arguments);
  
  return (IsOk);
}
/************************************************************************/


