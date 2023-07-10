#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"
#include "UTIL.h"

/************************************************************************/
Boolean_t DoConversion(
/************************************************************************/
          FILE *f           ,  /* IN - the file to read  */
          char *TempFName   ,  /* IN - the temp file to write */
          char **MessageString) /* OUT - error message                  */
/************************************************************************/
{
/* Local Variables */

  Boolean_t IsOk = TRUE  ;
  int   i             ;
  int   NumVars       ;
  int   NbV1 ;
  int   NbV2 ;
  char  title[81]     ;
  int   ndp           ;
  int   nelem         ;
  int   npoin         ;
  int   nplan         ;
  char  VarNames[5000];

  int   Debug = 1;      /* Debug for tecplot writing */
  int   VIsDouble = 0 ; /* Serafin and Volfin are simple precision */
  int   TecOk         ; /* Return value for TecUtilXxx calls */

  int   *PtrIkle    ; /* Connectivity Table */
  float *PtrX, *PtrY; /* Coordinates */
  float *PtrValues  ; /* Values in the data set */
  int   NDataPoints ; /* Number of data points */

  int   TimeStep;
  float Time;
  int FileFormat;
  Boolean_t FirstDataSet ;
  float *PtrFloat ;
  Boolean_t EndOfFile;

  int FHeader ; /* Head bytes in fortran files */
  int RefVal ;
  char string[50];

  int ndp_tec ; /* Number of nodes for element in tecplot */

  Boolean_t Invert ;

  Boolean_t GetVarNames();
  Boolean_t GetMeshInfo();
  Boolean_t ReadMesh();
  Boolean_t ReadDataSet();
  Boolean_t CheckReadBlock();
  Boolean_t ReadFHead();

  Boolean_t WriteTecData();
  int DetectFormat();

/************************************************************************/

  Invert = FALSE ;

  /* Telemac files are written by binary Fortran write procedures.
   * Fortran adds before each block written a 4byte integer indicating
   * the size of the following block.
   * In C, we have to read this block. We are lucky : Knowing the size
   * of the title (80 caracters), we can detect if the file is written
   * in the same byte order than the present machine. If not, use
   * inverse macros for numeric values (TODO)
   * These 4 bytes are added again at the END of the block written.
   * So there are 4 trailing bytes, whih should have the same value
   * than the four header bytes!!!
   *
   * So, here read the first 4 bytes and than the title:
   * */
  
  fread(&FHeader,sizeof(int),1,f);
  if(FHeader != 80)
  {
      swapbytes32(FHeader) ;

	  if(FHeader == 80 ) 
	  {
	      Invert = TRUE;
	  }
	  else
	  {
              strcpy(*MessageString ,"Sorry, not the right file!");
              IsOk = FALSE ;
              return(IsOk);
	  }
  /* TODO : check with inverted bytes if 80... */
  }

/* Read the title of the data set */
  if ( fread(&title,FHeader,1,f) != 1 )
    {
      strcpy(*MessageString,"Could not read the title of the data file");
      IsOk = FALSE;
      return(IsOk);
    }

  /* CHECK TODO : the title is only 72 caracters long. the last 8
   * caracters are the word "SERAPHIN".
   */

  /* Read the trailing bytes : */
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return(IsOk);

  title[80]=0; 


/* Read the number of variables in the data set 
 * These are two 4 byte integers, so head and trail should be 8 :
 */

  RefVal = 2 * sizeof(int);

  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  if(EndOfFile) 
  {
      strcpy(*MessageString,"Unexpected End Of File ");
      IsOk = FALSE;
      return(IsOk);
  }

  if ( fread(&NbV1,sizeof(int),1,f) != 1)
  {
      strcpy(*MessageString,"Could not read the data file");
      IsOk = FALSE;
      return(IsOk);
  }
  if(Invert) swapbytes32(NbV1) ;

  if ( fread(&NbV2,sizeof(int),1,f) != 1)
  {
      *MessageString = "Could not read the data file";
      IsOk = FALSE;
      return(IsOk);
  }
  if(Invert) swapbytes32(NbV2) ;


  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return(IsOk);

/* The total number of variables in the file */

  NumVars = NbV1 + NbV2 ;


  /* Read the variable names */
  IsOk = GetVarNames(f,Invert,NumVars,VarNames,MessageString) ;
  if (!IsOk) return(IsOk);


  /* Read the mesh info */
  IsOk = GetMeshInfo(f,Invert,&nelem,&npoin,&ndp,&nplan,MessageString) ;
  if (!IsOk) return(IsOk);

  /* Open the tecplot temp file */

  TecOk = TecUtilTecIni(title, VarNames, TempFName,
	                ".", &Debug, &VIsDouble);
  if ( TecOk != 0 )
  {
      strcpy(*MessageString,"Error opening tecplot temp file");
      IsOk = FALSE ;
      return(IsOk) ;
  }

  PtrX    = (float*)calloc(npoin,sizeof(float));
  PtrY    = (float*)calloc(npoin,sizeof(float));

  if( ndp == 6 ) ndp_tec = 8; else ndp_tec = ndp ;
  PtrIkle = (int*)  calloc(nelem*ndp_tec,sizeof(int));

  /* Read the connectivity and the coordinates in X an Y.
   * PtrIkle contains the connectivity table in Tecplot format
   * (conversion telemac to tecplot done while reading ...)
   */

  IsOk = ReadMesh(f,Invert,nelem,npoin,ndp,nplan,PtrIkle,PtrX,PtrY,MessageString) ;
  if (!IsOk) return(IsOk);


  /* Detect the file format : serafin or volfin.  */

  FileFormat = DetectFormat(f,Invert,&EndOfFile,nelem,npoin);

  if(EndOfFile)
  {
      strcpy(*MessageString,"Unexpected end of file");
      return FALSE ;
  }
  
  switch ( FileFormat )
  {
      case 1 : NDataPoints = nelem ;
	       break;

      case 2 : NDataPoints = npoin ;
	       break;

      default : IsOk = FALSE ;
		     strcpy(*MessageString,"Unknown File Format");
		     break;
  }
  
/* Allocate the memory for the data set to read */

  i = NumVars;
  if (i==0) i=1;

  PtrValues = (float*)calloc(NDataPoints*i,sizeof(float));

  /* Read the data sets and write them to the temp tecfile. */
  TimeStep     = 0;
  FirstDataSet = TRUE ;
  EndOfFile    = FALSE ;

  TecUtilStatusStartPercentDone("Please Wait While Loading the data ...",FALSE,FALSE);

  while (!EndOfFile)
  {
      sprintf(*MessageString,"Please wait, loading data set no %d .. ",TimeStep);
      TecUtilStatusSetPercentDoneText(*MessageString);
      IsOk = ReadDataSet(f,Invert,NumVars,NDataPoints,PtrValues,&Time,
	        MessageString,&EndOfFile);

      if(!IsOk)
      {
          strcat(*MessageString," - Error reading Data Set");
          return(IsOk);
      }
  
      if(!EndOfFile ) 
      {
      sprintf(*MessageString,"Loading data ... ",TimeStep);
      TecUtilStatusSetPercentDoneText(*MessageString);
	  IsOk = WriteTecData(FirstDataSet,nelem,npoin,ndp_tec,
	                           PtrIkle, PtrX, PtrY,
	                           NumVars,NDataPoints,
				   PtrValues, &Time, TimeStep,
				   MessageString);

	  if(!IsOk) { return(IsOk); }
	  
      }

      FirstDataSet = FALSE;
      TimeStep++;
  }
  TecUtilStatusFinishPercentDone();

  TecOk = TecUtilTecEnd();
  
  return (IsOk);
}
/************************************************************************/


