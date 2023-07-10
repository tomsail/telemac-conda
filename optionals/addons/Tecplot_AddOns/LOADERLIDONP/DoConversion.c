#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"
#include "UTIL.h"

/************************************************************************/
Boolean_t ReadFHead(
	FILE *f,
	Boolean_t Invert,
	int RefVal,
	int *FHeader,
	Boolean_t Compare,
	char **MessageString,
	Boolean_t *EndOfFile)
{
/************************************************************************ 
 *  Function : Read the fortran header of the block in the file.
 *  Check the header value with a reference value (expected block
 *  size). If bad block size, exit with error.
 ************************************************************************/
 Boolean_t IsOk ;
 int dummy;

/************************************************************************/
 *EndOfFile = FALSE;

 if(fread(FHeader,sizeof(int),1,f) != 1)
 {
     /*
  sprintf(*MessageString,"Reached end of file.");
  TecUtilDialogMessageBox(*MessageString,MessageBox_Information);
  */
     *EndOfFile = TRUE; IsOk = TRUE;  return(IsOk); 
 }

 if(Invert) {dummy = *FHeader;swapbytes32(dummy);*FHeader=dummy;}

 if (Compare  && (*FHeader != RefVal))
 {
      sprintf(*MessageString,"Bad block size. Read %d ",*FHeader);
      IsOk = FALSE ;
      return(IsOk);
 }
 IsOk = TRUE;
 return(IsOk);
}

/************************************************************************/
Boolean_t CheckReadBlock(
	        FILE *f, 
               	Boolean_t Invert,
		int FHeader,
		char **MessageString)
/************************************************************************ 
 * Read the trail bytes of a block in the file. Check this against the
 * provided Header value. If they are not equal, error.
 ************************************************************************/
{
 Boolean_t IsOk;
 int FTrail;
/************************************************************************/

 if(fread(&FTrail,sizeof(int),1,f) != 1)
 {
     strcpy(*MessageString,"Unexpected End Of File (while reading Trail)");
     return FALSE;
 }

 if(Invert) swapbytes32(FTrail);

 if(FHeader != FTrail )
 {
     strcpy(*MessageString,"Error while reading file!");
     IsOk = FALSE;
 }
 return(IsOk);
}
/************************************************************************/

/************************************************************************/
Boolean_t DoConversion(
/************************************************************************/
         FILE *f            ,  /* IN - the file to read                 */
         char *TempFName    ,  /* IN - the temp file to write           */
         char **MessageString) /* OUT - error message                   */
/************************************************************************/
{
  /* Local Variables */

  Boolean_t    IsOk;
  int          i;
  ArgList_pa   ArgList;
  int          percent;

  int          NBV1,NBV2;
  int          nbval_stat;
  int          nbval_trans;
  int          nbval_old;
  int          tstep;
  float        time ;
  double       time_double;
  int          itab2[2];
  float        ttab2[2];
  int          *PtrITab;
  float        *PtrDataStat;
  float        *PtrDataTrans;
  float        *PtrFloat;
  char         c4[4];
  char         c5[5];
  int          *VarShareList;

  char         str[100];

  Boolean_t EndOfFile;
  Boolean_t Invert;
  Boolean_t FirstPrint;
  Boolean_t ArgOk;


  char  title[73],title1[72] ;
  int   FHeader   ; /* Head bytes in fortran files */
  int   RefVal    ;

  char  VarNames[5000];

  int   Debug = 1;      /* Debug for tecplot writing */
  int   IsDouble = 0 ;  /* read simple precision values */
  int   TecOk         ; /* Return value for TecUtilXxx calls */



/************************************************************************/


  // Read the dataset. We suppose there is only one permanent
  // variable, which is the space variable.
  // The others are dependent of time.

  EndOfFile = FALSE;
  Invert = FALSE ;

  TRACE("Reading LIDO NP data file ... \n");

  fread(&FHeader,sizeof(int),1,f);
  if(FHeader != 72)
  {
      swapbytes32(FHeader) ;
      if(FHeader == 72 )
      { Invert = TRUE; }
      else
      {
         strcpy(*MessageString ,"Sorry, not the right file!");
         IsOk = FALSE ;
         return(IsOk);
      }
  }

  //TRACE("Invert data : ");
  //if(Invert)TRACE("TRUE \n"); else TRACE("FALSE \n");

  //strcpy(title,"");
  //TRACE("read header");
  /* Read the title of the data set */
  if ( fread(&title,FHeader,1,f) != 1 )
  {
     strcpy(*MessageString,"Could not read the title of the data file");
     IsOk = FALSE;
     return(IsOk);
  }
  title[72]=0;
  //TRACE(title);
  //TRACE("\n");

  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return(IsOk);


  // subtitle 1
  strcpy(title1,"");
  RefVal = 72;
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  fread(&title1,FHeader,1,f);
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return(IsOk);
  title1[71]=0;
  //TRACE(title1);
  //TRACE("\n");

  // subtitle 2
  strcpy(title1,"");
  RefVal = 72;
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  fread(&title1,FHeader,1,f);
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return(IsOk);
  title1[71]=0;
  //TRACE(title1);
  //TRACE("\n");

  strcpy(c4,"    ");
  RefVal = 4;

  // variables generiques
  //TRACE("read generic variables \n");
  while(strcmp(c4,"FIN ")!= 0 )
  {
      //TRACE(" Variable generique : ");
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      fread(&c4,FHeader,1,f);
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return(IsOk);
      //TRACE(c4);
      //TRACE(" \n");
  }

  //TRACE("read unused ints (2) \n");
  RefVal = 8;
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  if (fread(&itab2,FHeader,1,f) != 1 ) return FALSE;
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return(IsOk);
  if(itab2[0] != itab2[1]) return FALSE;
  nbval_stat = itab2[0];
  if(Invert) swapbytes32(nbval_stat);

  //TRACE("read unused  tabs(2x2) \n");
  // read unused tab
  RefVal = nbval_stat*4;
  PtrITab = (int*)calloc(itab2[1],sizeof(int));

  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  if (fread(PtrITab,FHeader,1,f) != 1 ) return FALSE;
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return(IsOk);

  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  if (fread(PtrITab,FHeader,1,f) != 1 ) return FALSE;
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return(IsOk);
  free(PtrITab);

  // The names of the variables, one list, comma separated


  // read the names of the timeindependent variables
  

/*------------------------------------------------------------------------------
 * Read static variables : these variables are independent of the
 * time.
 * ----------------------------------------------------------------------------*/

  strcpy(VarNames,"");
  //TRACE("read static variables \n");
  NBV1 = 0; // the number of static variables
  strcpy(c4,"    ");
  RefVal = 4;
  strcpy(VarNames,"");
  i=4;
  while(i!=0)
  {
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      fread(&c4,FHeader,1,f);
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return(IsOk);
      strcpy(c5,c4);
      c5[4]=0;
      i =strcmp(c5,"FIN ") ;
      if(i != 0)
      {
	  if(NBV1 != 0) strcat(VarNames,", ");
          strcat(VarNames,c5);
          NBV1++;
      }
      //TRACE(c5);
      //TRACE("\n");
  }
  //TRACE("List of variable names : \n");
  //TRACE(VarNames);
  //TRACE("\n");
  //TRACE("END read static variables. \n");

  // read static variable values 
  //TRACE("read static variables : number of values ..\n");
  RefVal = 8;
  IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
  if(!IsOk) return(IsOk);
  if (fread(&itab2,FHeader,1,f) != 1 ) return FALSE;
  IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
  if(!IsOk) return(IsOk);
  if(itab2[0] != itab2[1]) return FALSE;
  nbval_stat = itab2[0];
  if(Invert)swapbytes32(nbval_stat);
  sprintf(str," nb val %d \n",nbval_stat);
  //TRACE(str);

  //TRACE("read static variable values \n");
  // the size of the data sets :
  RefVal = nbval_stat*4;
  // allocate table for the dataset : NBV1*size
  PtrDataStat = (float*)calloc(nbval_stat*NBV1,sizeof(float));

  PtrFloat = PtrDataStat;

  for(i=0;i<NBV1;i++)
  {
      sprintf(str,"data set no %i ... \n",i);
      //TRACE(str);
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      if (fread(PtrFloat,FHeader,1,f) != 1 ) return FALSE;
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return(IsOk);
      PtrFloat += nbval_stat;
      sprintf(str,"OK data set no %i \n",i);
      //TRACE(str);
  }
  if(Invert)for(i=0;i<nbval_stat*NBV1;i++)swapbytes32(PtrDataStat[i]);



/*------------------------------------------------------------------------------
 * Read transient variable names.
 * ----------------------------------------------------------------------------*/

  //TRACE("read transient variables \n");
  NBV2   = 0; // the number of static variables
  RefVal = 4;
  i = 4;
  while( i != 0 )
  {
      strcpy(c4,"");
      //TRACE("read 1 transient variable name ... \n");
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      fread(&c4,FHeader,1,f);
      strcpy(c5,c4);
      c5[4]=0;
      //TRACE(c5);
      //TRACE("\n");
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      //TRACE("check block ok \n");
      if(!IsOk) return(IsOk);
      i = strcmp(c5,"FIN ") ;
      if(i != 0)
      {
	  strcat(VarNames,", ");
          strcat(VarNames,c5);
          NBV2++;
      }
      sprintf(str,"res of strcmp : %d \n",i);
      //TRACE(str);
  }
  //TRACE("Transient variable names OK\n");
  //TRACE("List of variable names : \n");
  //TRACE(VarNames);


  /*--------------------------------------------------------------------------
   * Create the temp data file. 
   *
   *
   *------------------------------------------------------------------------*/

  //TRACE("Create temp data file \n");
  TecOk = TecUtilTecIni(title, VarNames, TempFName, ".", &Debug, &IsDouble);
  if ( TecOk != 0 )
  {
      strcpy(*MessageString,"Error opening tecplot temp file");
      IsOk = FALSE ;
      return(IsOk) ;
  }
  

  FirstPrint = TRUE;
  nbval_old = 0;
  EndOfFile = FALSE;
  ArgList = TecUtilArgListAlloc();

  VarShareList = (int*)calloc(NBV1+NBV2,sizeof(int));
  for(i=0;i<NBV1+NBV2;i++) { VarShareList[i] = 0; }

  while(!EndOfFile)
  {
      // read transient data set : time step, time, nbval_trans and
      // the data.
      

      //TRACE("Read data set \n");
      // time step : 
      RefVal = 8;
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
      if(EndOfFile){TRACE("Reached EOF \n"); break ;}
      if(!IsOk) return(IsOk);
      if (fread(&itab2,FHeader,1,f) != 1 ) return FALSE;
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return(IsOk);
      if(itab2[0] != itab2[1]) return FALSE;
      tstep = itab2[0];
      if(Invert) swapbytes32(tstep);
      sprintf(str,"time step %d \n",tstep);
      //TRACE(str);

      // time
      RefVal = 8;
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      if (fread(&ttab2,FHeader,1,f) != 1 ) return FALSE;
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return(IsOk);
      if(ttab2[0] != ttab2[1]) return FALSE;
      time = ttab2[0];
      if(Invert) swapbytes32(time);
      sprintf(str,"time %f \n",time);
      //TRACE(str);
     
      // size of the following NBV2 arrays :
      RefVal = 8;
      IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
      if(!IsOk) return(IsOk);
      if (fread(&itab2,FHeader,1,f) != 1 ) return FALSE;
      IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
      if(!IsOk) return(IsOk);
      if(itab2[0] != itab2[1]) return FALSE;
      nbval_trans = itab2[0];
      if(Invert) swapbytes32(nbval_trans);

      //TRACE("End reading data set : creating teczone \n");
      if(FirstPrint && nbval_trans != nbval_stat)
      {
	  strcpy(*MessageString,"Number of variables incompatible (transient and static data)");
	  return FALSE;
      }
      sprintf(title,"t= %f ",time);
      ArgOk = TecUtilArgListAppendString(ArgList,SV_TITLE,title);
      ArgOk = TecUtilArgListAppendInt(ArgList,SV_ZONETYPE,ZoneType_Ordered);
      ArgOk = TecUtilArgListAppendInt(ArgList,SV_IMAX,nbval_trans);
      ArgOk = TecUtilArgListAppendInt(ArgList,SV_JMAX,1);
      ArgOk = TecUtilArgListAppendInt(ArgList,SV_KMAX,1);
      ArgOk = TecUtilArgListAppendInt(ArgList,SV_ISBLOCK,1);
      time_double = time ;
      ArgOk = TecUtilArgListAppendDouble(ArgList,SV_SOLUTIONTIME,time_double);
      ArgOk = TecUtilArgListAppendInt(ArgList,SV_STRANDID,-1);
      if(!FirstPrint) for(i=0;i<NBV1;i++) VarShareList[i]  = 1 ;
      ArgOk = TecUtilArgListAppendArray(ArgList,SV_VARSHAREZONELIST,VarShareList);

      TecOk = TecUtilTecZneX(ArgList);
      if (TecOk != 0 )
      {
          strcpy(*MessageString,"Error creating zone");
          IsOk = FALSE ;
          return(IsOk);
      }



      TecUtilArgListClear(ArgList);
      /* if this is the first print, write out the static data values.
       * they are shared with the other zones.
       */
      if(FirstPrint)
      {
	  PtrFloat = PtrDataStat;
	  for(i=0;i<NBV1;i++)
	  {
              TecOk = TecUtilTecDat((LgIndex_t*)&nbval_stat,(void*)PtrFloat,&IsDouble);
              PtrFloat += nbval_stat;
	  }
          free(PtrDataStat);
      }

      /* Allocate the memory for the transient data set.*/
       
      if((nbval_trans != nbval_old) || FirstPrint)
      {
	  if(!FirstPrint)free(PtrDataTrans);
          PtrDataTrans = (float*)calloc(nbval_trans,sizeof(float));
      }
      PtrFloat = PtrDataTrans;
      RefVal = nbval_trans*4;
      // Read the transient data set.
      for(i=0;i<NBV2;i++)
      {
          IsOk = ReadFHead(f,Invert,RefVal,&FHeader,TRUE,MessageString,&EndOfFile);
          if(!IsOk) return(IsOk);
          if (fread(PtrFloat,FHeader,1,f) != 1 ) return FALSE;
          IsOk = CheckReadBlock(f,Invert,FHeader,MessageString);
          if(!IsOk) return(IsOk);
          if(Invert)for(i=0;i<nbval_trans;i++)swapbytes32(PtrDataTrans[i]);
          TecOk = TecUtilTecDat((LgIndex_t*)&nbval_trans,(void*)PtrFloat,&IsDouble);
      }
      FirstPrint = FALSE;
      nbval_old = nbval_trans;
      //TRACE("End creating tec zone \n");
  }

  free(PtrDataTrans);
  
  //TRACE("close binary data file \n");
  TecUtilTecEnd();

  //TecUtilArgListDealloc(&ArgList);

  IsOk = TRUE;
  return(IsOk);
}
