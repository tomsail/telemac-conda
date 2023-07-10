/************************************************************************/
#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"

/************************************************************************/
Boolean_t WriteTecData (
        Boolean_t FirstDataSet,
	int nelem,
	int npoin,
	int ndp_tec,
	int *Ikle,
	float *x,
	float *y,
	int NumVars,
	int NDataValues,
	float *DataValues,
	float *Time,
	int TimeStep,
	char **MessageString)
/************************************************************************/
{
  Boolean_t IsOk = TRUE ;

  ArgList_pa Arguments ;
  Boolean_t  ArgOk;
  int i;
  int Ret;
  char       Title[50];        /* Zone title */
  LgIndex_t  *VarShareList ; /* List of variables to share */
  LgIndex_t  ShareConnect;     /* Share connectivity from zone no ... */
  ZoneType_e ZTyp ;            /* Zone (element) type */
  Boolean_t  IsBlock;          /* Block format or not */
  ValueLocation_e *ValueLoc, Loc;
  LgIndex_t IsDouble = 0;
  float *PtrData;
  LgIndex_t kmax;
  double time_double;

/************************************************************************/

  /*---------------------------------------------------------------------
   * First create the Zone information. Use the advanced function
   * TecUtilTecZneX for this. Required Input : a list of arguments, to
   * create as an ArgList_pa type. 
   *---------------------------------------------------------------------
   */

  /* Allocate the argument list */
  Arguments = TecUtilArgListAlloc();
  if(Arguments == NULL)
  {
      strcpy(*MessageString,"Failed allocation of Arglist");
      IsOk = FALSE;
      return(IsOk);
  }
  

  /* The zone title SV_TITLE char* 
   * use the solution time as zone title */

  sprintf(Title,"Zone t= %f ",*Time);

  ArgOk = TecUtilArgListAppendString(Arguments,SV_TITLE,Title);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating Zone Title Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  /* The zone type SV_ZONETYPE ZoneType_e 
   * ZoneType_FETriangle
   * ZoneType_FEBrick (or tetras ???)
   * ZoneType_FEQuad
   * no tetras in telemac file, 4 nodes means always quads! */

  switch (ndp_tec )
  {
      case 3 : ZTyp = ZoneType_FETriangle;
	  break;
      case 4 : ZTyp = ZoneType_FEQuad;
	  break ;
      case 8 : ZTyp = ZoneType_FEBrick ;
	  break ;
      default : strcpy(*MessageString,"Unknown FE Format");
		IsOk = FALSE;
		return(IsOk);
	  break ;
	      
  }
  ArgOk = TecUtilArgListAppendInt(Arguments,SV_ZONETYPE,ZTyp);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating Zone Type Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  /* Number of nodes SV_IMAX LgIndex_t  */

  ArgOk = TecUtilArgListAppendInt(Arguments,SV_IMAX,npoin);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating SV_IMAX Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  /* Number of elements SV_JMAX LgIndex_t */

  ArgOk = TecUtilArgListAppendInt(Arguments,SV_JMAX,nelem);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating SV_JMAX Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  kmax = 1;
  ArgOk = TecUtilArgListAppendInt(Arguments,SV_KMAX,kmax);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating SV_KMAX Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  /* Solution Time SV_SOLUTIONTIME double */
  time_double = *Time ;
  ArgOk = TecUtilArgListAppendDouble(Arguments,SV_SOLUTIONTIME,time_double);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating SV_SOLUTIONTIME Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  /* StrandId  SV_STRANDID LgIndex_t
   * 0 for static 
   * > 1 for transient ( +1 for each timestep )
   * -1 for automatic based on solution time */

  /*ArgOk = TecUtilArgListAppendInt(Arguments,SV_STRANDID,TimeStep);*/
  ArgOk = TecUtilArgListAppendInt(Arguments,SV_STRANDID,-1);



  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating SV_STRANDID Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  /* Write Format SV_ISBLOCK Boolean_t 
   * BLOCK format (TRUE) is more efficient than POINT Format (FALSE) */ 
  
  IsBlock = TRUE;
  ArgOk = TecUtilArgListAppendInt(Arguments,SV_ISBLOCK,IsBlock);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating SV_ISBLOCK Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  /* Passive Varlist SV_PASSIVEVARLIST LgIndex_t*
   * an array, 0 if variable is active (written) 1 if variable is
   * passive (shared with previous zone)*/

  VarShareList = (int*)calloc(NumVars+2,sizeof(int));
  for(i=0;i<NumVars+2;i++) { VarShareList[i] = 0; }

  if(!FirstDataSet) 
  {
      VarShareList[0]  = 1 ; /* Share X coordonates */
      VarShareList[1]  = 1 ; /* Share Y coordinates */
  }

  ArgOk = TecUtilArgListAppendArray(Arguments,SV_VARSHAREZONELIST,
	                            VarShareList);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating SV_VARSHAREZONELIST Argument");
      IsOk = FALSE;
      return(IsOk);
  }


  /* Valuelocation SV_VALUELOCATION ValueLocation_e*
   * default value : Null, all data are nodal.
   * otherwise array containing the lcation of the variables :
   * ValueLocation_CellCentered or ValueLocation_Nodal
   */

  ValueLoc = (ValueLocation_e*)calloc(NumVars+2,sizeof(int));

  if(NDataValues == npoin )
  {Loc = ValueLocation_Nodal;}
  else if(NDataValues == nelem)
  { Loc = ValueLocation_CellCentered;}
  else
  {
      strcpy(*MessageString,"Unconsistent number of data points");
      IsOk = FALSE ;
      return(IsOk);
  }

  for(i=0;i<NumVars+2;i++) { ValueLoc[i] = Loc; }
  ValueLoc[0] = ValueLocation_Nodal ;
  ValueLoc[1] = ValueLocation_Nodal ;

  ArgOk = TecUtilArgListAppendArray(Arguments,SV_VALUELOCATION,ValueLoc);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating SV_VALUELOCATION Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  /* Share connectivity from zone : SV_CONNECTSHAREZONE LgIndex_t
   * 0 if connectivity not shared, otherwise no of the zone
   * defining the connectivity to share */

  if(FirstDataSet){ ShareConnect = 0;} else {ShareConnect = 1;}
  ArgOk = TecUtilArgListAppendInt(Arguments,SV_CONNECTSHAREZONE,
	                          ShareConnect);
  if(!ArgOk)
  {
      strcpy(*MessageString,"Error Creating SV_CONNECTSHAREZONE Argument");
      IsOk = FALSE;
      return(IsOk);
  }

  /* Create the zone with the argument list */

  Ret = TecUtilTecZneX(Arguments);
  if (Ret != 0 )
  {
      strcpy(*MessageString,"Error creating zone");
      IsOk = FALSE ;
      return(IsOk);
  }

  TecUtilArgListDealloc(&Arguments);
  
  free(VarShareList);
  free(ValueLoc);

  /*---------------------------------------------------------------------
   * Write the data set. Coordinates and Connectivity are written only
   * once and shared.
   *---------------------------------------------------------------------
   */


  /* Node coordinates */
  if(FirstDataSet)
  {
      Ret = TecUtilTecDat((LgIndex_t*)&npoin,(void*)x,&IsDouble);
      if (Ret != 0 )
      {
          strcpy(*MessageString,"Error writing data (x coordinates)");
          IsOk = FALSE ;
          return(IsOk);
      }
      Ret = TecUtilTecDat((LgIndex_t*)&npoin,(void*)y,&IsDouble);
      if (Ret != 0 )
      {
          strcpy(*MessageString,"Error writing data (y coordinates)");
          IsOk = FALSE ;
          return(IsOk);
      }
  }

  /* Data values */

  PtrData = DataValues;
  for(i=0;i<NumVars;i++)
  {
      Ret = TecUtilTecDat((LgIndex_t*)&NDataValues,(void*)PtrData,
	                  &IsDouble);
      if (Ret != 0 )
      {
          strcpy(*MessageString,"Error writing data ");
          IsOk = FALSE ;
          return(IsOk);
      }
      PtrData += NDataValues;
  }

  /* Connectivity */
  if(FirstDataSet)
  {
      Ret = TecUtilTecNod((LgIndex_t*)Ikle);
      if (Ret != 0 )
      {
          strcpy(*MessageString,"Error writing connectivity ");
          IsOk = FALSE ;
          return(IsOk);
      }
  }


  return (IsOk);
}
/************************************************************************/

