#include "TECADDON.h"
#include "ADDGLBL.h"


/*************************************************************************
 * ExtractTimeProfile AddOn
 * --------------------------
 * Function : Extract a time profile for all variables in a given
 * point or from a polyline.
 * written by R Nebauer EDF R&D LNHE, may 2007
 * regina.nebauer@edf.fr
 ************************************************************************/


// Compare function for sorting an array of double values
int compare_double (const void *a, const void *b)
{
    const double *da = (const double *) a;
    const double *db = (const double *) b;
    return (*da > *db) - (*da < *db);
}




static Boolean_t CreateStaticDataSet(
	double *PlotData, int *DataPoints,
	int NbPoints, int NumTimeSteps, int NumVars, int ExtraVars,
	int MaxStrandID,
	Set_pa NewSet, char *ErrMsg)
{
/*************************************************************************
 * Creates a new static dataset if the option ResIsTransient is set to
 * FALSE. 
 ************************************************************************/
  Boolean_t IsOk;
  int i,j,k,n;
  ArgList_pa ArgList;
  EntIndex_t  TempZone;      // The current zone as an entity
  int percent;
  FieldData_pa FieldDataRef ;
  double DD;
  char str[100];

  //TRACE("Create a static data set ... \n");
  IsOk = TRUE ;

  ArgList = TecUtilArgListAlloc();

  for(i=0;i<MaxStrandID;i++)
  {
  //TRACE("dataset for strand no ?? \n");

      if(DataPoints[i] == 0) continue;

      //sprintf(str,"Create new zone for strand id %d \n",i+1);
      //TRACE(str);
      //sprintf(str,"Data points in timestrand : %d \n",DataPoints[i]);
      //TRACE(str);

      /* Create a new zone.  */
      TecUtilArgListClear(ArgList);
      TecUtilArgListAppendInt(ArgList, SV_ZONE, i+1);
      TecUtilArgListAppendInt(ArgList, SV_IMAX, NumTimeSteps);
      TecUtilArgListAppendInt(ArgList, SV_JMAX, NbPoints);
      strcpy(str,"");
      sprintf(str,"Time Series for Strand %d",i+1);
      //TRACE(str);
      //TRACE("\n");
      TecUtilArgListAppendString(ArgList, SV_NAME, str);
      IsOk = TecUtilDataSetAddZoneX(ArgList) ;
      if(IsOk) TecUtilDataSetGetInfo(NULL, &TempZone, NULL);
      TecUtilSetAddMember(NewSet, TempZone, TRUE);
      if(IsOk)
      {
	  //sprintf(str,"new zone created : %d \n",TempZone);
	  //TRACE(str);
      }
      else
      {
	  //TRACE("New zone creation failed");
	  strcpy(ErrMsg,"New zone creation failed");
	  return( IsOk);
      }
      /* Add the zone to the set */

      /* Write the data values. Start with the solution time values,
       * than continue with the other variable values.
       */

      for(j=0;j<NumVars+ExtraVars;j++)
      {
	  FieldDataRef = TecUtilDataValueGetWritableRef(TempZone,j+1);
	  if(!FieldDataRef)
	  {
	      IsOk = FALSE;
	      break;
	  }

	  percent = j*100/(NumVars+ExtraVars)   ;
          TecUtilStatusCheckPercentDone(percent);

	  // TRACE("writing data values to zone ...");
	  for(n=0;n<NbPoints;n++)
	  {
              for(k=0;k<NumTimeSteps;k++)
	      {
	//  if(DataPointOk[(k*NbPoints+n)])
		  {
		  DD = PlotData[
		      i*(NumVars+ExtraVars)*NbPoints*NumTimeSteps+
		      (NumVars+ExtraVars)*(k*NbPoints+n)+j];
	          TecUtilDataValueSetByRef(FieldDataRef,
		                       k+NumTimeSteps*n+1,DD ); 
		  }
		      
              }
	  }
      }
  }
  TecUtilStateChanged(StateChange_ZonesAdded,(ArbParam_t)NewSet);

  TecUtilArgListClear(ArgList);
  TecUtilArgListDealloc(&ArgList);

  return(IsOk);
}
/************************************************************************/
static Boolean_t CreateTransientDataSet(
	double *PlotData, int *DataPoints,
	int NbPoints, int NumTimeSteps, int NumVars, int ExtraVars,
	int MaxStrandID,
	Set_pa NewSet, char *ErrMsg)
{
/*************************************************************************
 * Creates a new transient dataset if the option ResIsTransient is set to
 * TRUE. 
 ************************************************************************/
  Boolean_t IsOk;
  int i,j,k,n;
  ArgList_pa Arguments;
  double time, DD;
  char str[100];
  EntIndex_t  TempZone;      // The current zone as an entity
  int percent;
  FieldData_pa FieldDataRef ;
  int FirstZone ;


  TRACE("Create a transient data set ... \n");
  FirstZone = TRUE;

  TRACE("step 1 \n");

  Arguments = TecUtilArgListAlloc();

  /*
  TRACE("step 2 \n");
  Ikle = (int*)calloc(2*(NbPoints-1),sizeof(int));
  TRACE("step 3 \n");
  PtrIkle = Ikle;
  TRACE("step 4 \n");

  for(i=1;i<NbPoints;i++)
  {
      *PtrIkle++ = i; 
      *PtrIkle++ = i+1; 
  }
  for(i=0;i<NbPoints-1;i++)
  {
     sprintf(str,"Ikle el : %d, N : %d, %d \n",i+1,Ikle[2*i],Ikle[2*i+1]);
	     TRACE(str);
	  
  }
  */

  IsOk = TRUE ;
  TRACE("step 5 \n");

  for(k=0;k<NumTimeSteps;k++)
  {
      /* Write the data values. Start with the solution time values,
       * than continue with the other variable values.
       */
	  sprintf(str,"data set for time step no %d \n",k);
          TRACE(str);

      for(i=0;i<MaxStrandID;i++)
      {
	  //sprintf(str,"data set for strand %d \n",i);
          //TRACE(str);

          if(DataPoints[i] == 0) continue;

          //sprintf(str,"Create new zone for strand id %d \n",i+1);
          //TRACE(str);
          //sprintf(str,"Data points in timestrand : %d \n",DataPoints[i]);
          //TRACE(str);

          /* Create a new zone.  */
          TecUtilArgListClear(Arguments);
          //TecUtilArgListAppendInt(Arguments, SV_ZONE, i+1);
          //TecUtilArgListAppendInt(Arguments, SV_ZONETYPE, ZoneType_FELineSeg);
          TecUtilArgListAppendInt(Arguments, SV_IMAX, NbPoints);
          //TecUtilArgListAppendInt(Arguments, SV_JMAX, NbPoints-1);
	  //if(FirstZone){ ShareConnect = 0;} else{ ShareConnect = 1;}
	  //TecUtilArgListAppendInt(Arguments, SV_CONNECTSHAREZONE,ShareConnect);

	  time = PlotData[
		      i*(NumVars+ExtraVars)*NbPoints*NumTimeSteps+
		      (NumVars+ExtraVars)*(k*NbPoints)];
	  sprintf(str,"time = %lf \n",time);
	  TRACE(str);

          TecUtilArgListAppendDouble(Arguments, SV_SOLUTIONTIME, time);
	  IsOk = TecUtilArgListAppendInt(Arguments,SV_STRANDID,i+1);

          strcpy(str,"");
	  if(MaxStrandID > 1){ sprintf(str,"T =  %lf (%d/%d)",time,i+1,MaxStrandID);}
	  else{ sprintf(str,"T =  %lf ",time);}
          TRACE(str);
          TRACE("\n");
          TecUtilArgListAppendString(Arguments, SV_NAME, str);

          IsOk = TecUtilDataSetAddZoneX(Arguments) ;

          if(IsOk) TecUtilDataSetGetInfo(NULL, &TempZone, NULL);
          TecUtilSetAddMember(NewSet, TempZone, TRUE);
          if(IsOk)
          {
              sprintf(str,"new zone created : %d \n",TempZone);
              TRACE(str);
          }
          else
          {
              TRACE("New zone creation failed");
	      strcpy(ErrMsg,"New zone creation failed");
	      return( IsOk);
          }

      /* Add the zone to the set */

	  percent = k*i*100/NumTimeSteps/MaxStrandID;
          TecUtilStatusCheckPercentDone(percent);

	  // TRACE("writing data values to zone ...");
          for(j=0;j<NumVars+ExtraVars;j++)
	  {
	      FieldDataRef = TecUtilDataValueGetWritableRef(TempZone,j+1);
	      if(!FieldDataRef)
	  {
	      IsOk = FALSE;
	      break;
	  }
	      for(n=0;n<NbPoints;n++)
	      {
	//  if(DataPointOk[(k*NbPoints+n)])
		  {
		  DD = PlotData[
		      i*(NumVars+ExtraVars)*NbPoints*NumTimeSteps+
		      (NumVars+ExtraVars)*(k*NbPoints+n)+j];
		  // TODO new index into the datatable ...
	          TecUtilDataValueSetByRef(FieldDataRef,
		                       n+1,DD ); 
		  }
		      
              }
	  }
	  /*
	  if(FirstZone)
	  {
	       ret = TecUtilTecNod((LgIndex_t*)Ikle);
	       if (ret != 0 )
               {
	           strcpy(str,"Error writing connectivity ");
		   TRACE(str);
	           return(IsOk);
	         }

	      FirstZone = FALSE;
	  }
	  */
      }
  }
  TecUtilStateChanged(StateChange_ZonesAdded,(ArbParam_t)NewSet);

  //free(Ikle);
  TecUtilArgListClear(Arguments);
  // TecUtilArgListDealloc(&ArgList);

  return(IsOk);
}
/************************************************************************/

Boolean_t extraction(double *X_COOR, double *Y_COOR, double *Z_COOR,
	             int NbPoints, Boolean_t ResIsTransient)
/*************************************************************************
 * Function : Get the coordinates of the points to extract, create a
 * new zone, do the extraction and fill the data in this new zone
 * created.
 *
 * Arguments : X_COOR,Y_COOR and Z_COOR contain the coordinates of the
 * point (NbPoints = 1) or the start and the end point of the line
 * (NbPoints > 0) where to extract the data. If NbPoints > 0, it
 * defines the number of points to extract along the line. 
 * 
 * How it works :
 * 1) Order the zones of the initial dataset corresponding to the
 * solution time and the time strand.
 * 2) Extract the values of all variables at a given (X,Y,Z) point or 
 * the given number of points along a line and store them in an array
 * 3) Create a new frame, create as many new zones as we have time
 * strands, write the data in there. Display these new data.
*************************************************************************/
{

  Boolean_t IsOk ;

  // General purpose variables
  int i,j,n; 
  int percent;


  // Declarations for using Tecplot Sets
  Set_pa      EnabledZones ; // The set of enabled zones
  SetIndex_t  ZoneCount ;    // The number of zones in the set

  Set_pa      *OrderedZones;// Zones ordered by solutiontime and strandid
  Set_pa      TempSet;       // Temporary set of zones.
  Set_pa      NewZones;      // Temporary set of zones.

  // Declarations for using Zones
  EntIndex_t  CurZone ;      // The current zone as an entity
  Strand_t    StrandID;      // STRANDID of a zone

  // Aux variables for zone information / extractions ....
  IJKPlanes_e Planes ;
  LgIndex_t   ICell, JCell, KCell;

  // The variable names
  StringList_pa VarNames ;   // List of all variable names
  char         *VName;       // Name of a variable

  // Data set informations :
  char        *Title   ;     // title of the data set
  EntIndex_t   NumZones;     // Number of zones in the data set
  EntIndex_t   NumVars ;     // Number of variables in the data set
  int          NumTimeSteps; // Number of transient zones in the data set
  Strand_t     MaxStrandID;  // The max number of strands in the data set

  // Creating the new data set :
  double      *VTimes   ;    // Array containing the time values
  double       t;
  double      *PlotData ;
  double      *PtrVarValues;
  double      *PtrTimesMem;

  double      *XVals,*YVals,*ZVals; // Points of extraction
  double      *Distance;          // distance from start point
  double      DX,DY,DZ,DD;

  // Creating new zone
  ArgList_pa ArgList;

  Boolean_t Extract3D;
  Boolean_t *DataPointOk;
  Boolean_t *PtrOk;
  int *DataPoints;
  int NTot;

  char message[200];

  UniqueID_t UserFrameID;

  int ExtraVars ;
  EntIndex_t CVar ;

/************************************************************************/


  IsOk = FALSE;
  sprintf(message,"extract %d points, istransient %d ",NbPoints,ResIsTransient);
  TRACE(message);
  sprintf(message,"x,y,z %f %f %f %f %f %f ",X_COOR[0],Y_COOR[0], Z_COOR[0],X_COOR[1],Y_COOR[1], Z_COOR[1]);
  TRACE(message);

  /* Save the Frame ID of the user frame, in order to push them on the
   * top after the extraction. This allows the user to make more than
   * one extraction, without changing the dialog.
   */
   UserFrameID = TecUtilFrameGetUniqueID();

   /* Save the current contour variable and use it for the new plot 
    * to create.
    */

    CVar = BAD_SET_VALUE;
    CVar = TecUtilVarGetNumByAssignment('C');

   /* For the new data set to create, we need one or two more
    * variables. In any case, we need the solution time. in case of
    * an extraction along a line, we will also include the distance
    * from the starting point in the variable list.
    */
   if(NbPoints == 1) { ExtraVars = 1; }
   else             { ExtraVars = 3; }


  /********************************************************************  
   *  CHECK SOURCE DATA SET
   ********************************************************************/ 

  /*-------------------------------------------------------------------
   * Get the number of variables and the number of zones in the
   * current data set (this is the data set in the activated frame)
   * */

  IsOk = TecUtilDataSetGetInfo(&Title,&NumZones,&NumVars);


  /*-------------------------------------------------------------------
   * Get the Max StrandId, means how many zones declare the same time
   * step. Writing a first data set with a given time will give the
   * time strand 1 to it, writing a second data set with the same
   * time, this wil be strandid 2 and so on ... ( tested by writing
   * data with autostranding ... to be confirmed.)
   */

  MaxStrandID = TecUtilDataSetGetMaxStrandID();

  /*-------------------------------------------------------------------*/
  /* Get the set of zones enabled, means all zones loaded.
   * (in the current, ie user frame)
   */

  EnabledZones = NULL;

  IsOk = TecUtilZoneGetEnabled(&EnabledZones); 

  if( EnabledZones == NULL )
  {
      /* Error, no Zones enabled */
  }

  /* Check out the number of zones enabled */

  ZoneCount = TecUtilSetGetMemberCount(EnabledZones);
  if(ZoneCount != NumZones )
  {
      // Number of zones in data set is different from number of zones
      // enabled ..... ????
  }

  
  /*-------------------------------------------------------------------*/
  /* First test : are this transient data? And how many time strands?
   * (means : how many zones may declare the same time)
   * The max number of zones declaring the same timestrand is stored
   * in NumTZones
   */

  NumTimeSteps = 0 ;

  TecUtilSetForEachMember(CurZone,EnabledZones)
  {
      StrandID = TecUtilZoneGetStrandID(CurZone);
      /* Count the number of zones declaring strandid 1.
       * This is the number of timesteps written in the data file
       * If a second strand exists, assume it will use the same time
       * steps (solution times).
       */
      if( StrandID == 1 ) NumTimeSteps++;
  }

  if(NumTimeSteps == 0)
  {
      // error, not a transient data set.
  }

  /*-------------------------------------------------------------------*/
  /* Get the variable names of the data set 
   */

  VarNames = TecUtilStringListAlloc();
  if ( !VarNames )
  {
      // error allocating string list
  }

  for(i=0;i<NumVars;i++)
  {
      IsOk = TecUtilVarGetName(i+1,&VName);
      if ( &VName == NULL) 
      {
	  // error getting the variable name
      }
      IsOk = TecUtilStringListAppendString(VarNames, VName);
  }

  // VarNames contains now the list of all variables in the 
  // active data set.

  /********************************************************************  
   * ORDER ZONES FROM THE SOURCE DATA SET
   *********************************************************************/
  
  /*-------------------------------------------------------------------*/
  /* Get the Solutiontime for the different time steps:
   * Allocate an array of the size NumTZones.
   * Fill it with the solution times of the zones.
   * assume the different strandid's have the same solution times.
   */

  VTimes = (double*)calloc(NumTimeSteps,sizeof(double));
  PtrTimesMem = VTimes;

  i = 0;
  TecUtilSetForEachMember(CurZone,EnabledZones)
  {
      StrandID = TecUtilZoneGetStrandID(CurZone);
      if (StrandID == 1 )
      {
          VTimes[i] = TecUtilZoneGetSolutionTime(CurZone);
          i++;
      }
  }
  // ?? GNU extension function?? may not work ....
  // sort the time array.
  qsort(VTimes,NumTimeSteps,sizeof(double),compare_double);

  /* create a number of new sets, with the data ordered by strandid
   * and solution time.
   * */

  OrderedZones = (Set_pa*)calloc(MaxStrandID,sizeof(Set_pa));

  // Init to NULL the sets.

  for(i=0;i<MaxStrandID;i++) { OrderedZones[i] = TecUtilSetAlloc(TRUE); }


  /* Put the user zones into the OrderedZones array, using the
   * StrandID as array Index. 
   */
  
  i=1;
  TecUtilSetForEachMember(CurZone,EnabledZones)
  {
      StrandID = TecUtilZoneGetStrandID(CurZone);
      if(StrandID>0)
      {
	  TecUtilSetAddMember(OrderedZones[StrandID-1],CurZone,TRUE);
      }
      i++;
  }

  /* Order the zones for each time strand in time :
   * Put the zones of one strand (actually in OrderedZones) into a
   * temporary set, where they are ordered in time. Then remove the
   * zone set from OrderedZones (they are not ordered, actually) and
   * replace it by the really ordered zone set in TempSet.
   * OrderedZones contains finally for each time strand the zones
   * ordered by increasin solution time.
   */

  TempSet = TecUtilSetAlloc(TRUE);
  
  for(i=0;i<MaxStrandID;i++)
  {
      for(j=0;j<NumTimeSteps;j++)
      {
      //sprintf(message,"Timestep no %d \n",j);
      //TRACE(message);
          TecUtilSetForEachMember(CurZone,OrderedZones[i])
	  {
              t = TecUtilZoneGetSolutionTime(CurZone);    
	      if(t == VTimes[j])
	      {
		  //sprintf(message,"added zone to time %f strand %d \n",t,i);
		  //TRACE(message);
		  TecUtilSetAddMember(TempSet,CurZone,TRUE);
		  // assumed : not more than one zone per strand has
		  // the same solutiontime.
		  break; // does this work to exit macro loop???
	      }
	  }
      }
      
      // Copy the ordered temporary data set into the OrderedZones
      // data set for the current strand id
      TecUtilSetClear(OrderedZones[i]);
      TecUtilSetCopy(OrderedZones[i],TempSet,TRUE);
      TecUtilSetClear(TempSet);
  }

  // Deallocate the temporary zone set.
  TecUtilSetDealloc(&TempSet);

  /*-------------------------------------------------------------------  
   * OrderedZones contains know for each time strand the zones ordered
   * in time.
   *-------------------------------------------------------------------*/

  /********************************************************************  
   * Calculate the extraction points 
   *********************************************************************/

  if(NbPoints > 1 ) 
  {
      XVals = (double*)calloc(NbPoints,sizeof(double));
      YVals = (double*)calloc(NbPoints,sizeof(double));
      ZVals = (double*)calloc(NbPoints,sizeof(double));
      Distance = (double*)calloc(NbPoints,sizeof(double));

      XVals[0]          = X_COOR[0];
      YVals[0]          = Y_COOR[0];
      ZVals[0]          = Z_COOR[0];

      XVals[NbPoints-1] = X_COOR[1];
      YVals[NbPoints-1] = Y_COOR[1];
      ZVals[NbPoints-1] = Z_COOR[1];

      Distance[0] = 0.0;
      DX = (XVals[NbPoints-1]-XVals[0]);
      DY = (YVals[NbPoints-1]-YVals[0]);
      DZ = (ZVals[NbPoints-1]-ZVals[0]);
      Distance[NbPoints-1] = sqrt(DX*DX+DY*DY+DZ*DZ);

      DX = DX/(double)(NbPoints-1);
      DY = DY/(double)(NbPoints-1);
      DZ = DZ/(double)(NbPoints-1);
      DD = sqrt(DX*DX+DY*DY+DZ*DZ);
      //sprintf(message,"dx dy dz dd %f %f %f %f \n",DX,DY,DZ,DD);
      //TRACE(message);

      for(i=1;i<NbPoints-1;i++)
      {
          XVals[i]=XVals[i-1]+DX;
          YVals[i]=YVals[i-1]+DY;
          ZVals[i]=ZVals[i-1]+DZ;
	  Distance[i] = Distance[i-1]+DD;
      //sprintf(message,"x y z d %f %f %f %f \n",XVals[i],YVals[i],ZVals[i],Distance[i]);
      //TRACE(message);
      }
      //sprintf(message,"x y z d %f %f %f %f \n",XVals[NbPoints-1],YVals[NbPoints-1],ZVals[NbPoints-1],Distance[NbPoints-1]);
      //TRACE(message);
  }
  else
  {
      //sprintf(message,"point : x y z %f %f %f \n",*X_COOR,*Y_COOR,*Z_COOR);
      //TRACE(message);
  }
      


  /********************************************************************  
   * EXTRACT DATA FROM THE SOURCE DATA SET
   *********************************************************************/

  /* In order to create the new data set we need for each timestrand
   * the values of all variables at (X,Y,Z) for each time step.
   * Create an array where to store these values and the solution time
   * (which is the same for all time strands)
   * 
   */

  // Allocate the array for the plot data for all new zones.
  // The solutiontime is stored only once.
  
  i = (NumVars+ExtraVars)*NumTimeSteps*MaxStrandID*NbPoints ;
  PlotData = (double*)calloc(i,sizeof(double));
  for(j=0;j<i;j++)PlotData[j]=0.0;

  
  i = NumTimeSteps*MaxStrandID*NbPoints; 
  DataPointOk = (Boolean_t*)calloc(i,sizeof(Boolean_t));
  DataPoints  = (int*)calloc(MaxStrandID,sizeof(Boolean_t));

  // If initial plot type is 3D, extract through Volume. Otherwise,
  // extract only surface data.
  Extract3D = TecUtilFrameGetPlotType() == PlotType_Cartesian3D;


  /* About zones and time strands ...
   * There are two possibilities for having more than one time strand:
   * These are data from parallel computing, means the zones for a
   * given time step, with different time strands are spatially
   * disconnected. In this case, only one zone will match the point.
   * Second case : there are different zones for one time step, but
   * they share the same mesh. So there is more than one data point
   * for a given (X,Y,Z). 
   * And finally, this may be a mixture of both cases.....
   * Do a check of this in DataPointOk.
   */

  PtrOk = DataPointOk;

  TempSet = TecUtilSetAlloc(TRUE);
  TecUtilDataLoadBegin(); 

  // Pointer to the data to write 
  PtrVarValues = PlotData;

  TecUtilStatusStartPercentDone("Extracting data ...",FALSE,TRUE);

  if(NbPoints == 1 )
  { // Extraction of one data point

    for(i=0;i<MaxStrandID;i++)
    {
	DataPoints[i] = 0;
	/*
	sprintf(message,"Look for point in zones of strand %d\n",i+1);
	TRACE(message);
	*/
      // Insert the time as a first variable for this strand.
      TecUtilSetForEachMember(CurZone,OrderedZones[i])
      {
	  TecUtilSetAddMember(TempSet,CurZone,TRUE);
	  /* The first value is the solution time */
          *PtrVarValues = TecUtilZoneGetSolutionTime(CurZone);
	  PtrVarValues++;
	  // Get the NumVar values at the point (X,Y,Z)
	  IsOk = 
          TecUtilProbeAtPosition(*X_COOR,*Y_COOR,*Z_COOR,
                             &ICell,&JCell,&KCell,&Planes,
                             &CurZone,    // The current zone to extract
                             FALSE,       // garbage
                             PtrVarValues,// target Array for values
                             TempSet,     // Limit search to current zone
                             Extract3D,   // Extract 3D or 2D data
                             FALSE,       // Do extraction.
                             FALSE);      // Use interpolation
	  *PtrOk++ = IsOk;
    	   PtrVarValues += NumVars ;
	   TecUtilSetClear(TempSet);
	  /*
	  if(!IsOk)
	  {
	      TecUtilDialogMessageBox(
		    "Data points out of Domain. top.",
		     MessageBox_Information); 
	      break; 
	  }
	  */
	  if(IsOk)
	  {
	      /*
	      sprintf(message," OK strand : %d / %d time %f point 1\n",
		      i+1,MaxStrandID,
		      TecUtilZoneGetSolutionTime(CurZone));
	      TRACE(message);
	      */
	      DataPoints[i] += 1;
	  }
	  else
	  {
	      /*
	      sprintf(message," KO strand : %d / %d time %f point 1\n",
		      i+1,MaxStrandID,
		      TecUtilZoneGetSolutionTime(CurZone));
	      TRACE(message);
	      */
	  }
      } // end loop over time ordered zones of a strand
      //if(!IsOk) break;
    } // end loop over time strands
  } 
  else // extract NbPoints along a line.
  {
    percent = 0;
    n = 0;
    TecUtilStatusCheckPercentDone(percent);
    for(i=0;i<MaxStrandID;i++)
    {
      /*
      sprintf(message,"Look for point in zones of strand %d\n",i+1);
      TRACE(message);
      */
      // Insert the time as a first variable for this strand.
      TecUtilSetForEachMember(CurZone,OrderedZones[i])
      {
          percent = n*100/NumZones  ;
          TecUtilStatusCheckPercentDone(percent);
	  n++;
	  /*
	  sprintf(message,"Check zone no %d\n",n);
	  TRACE(message);
	  */
	  TecUtilSetAddMember(TempSet,CurZone,TRUE);
	  
	  // Get the NumVar values at each point of the line.
	  for(j=0;j<NbPoints;j++)
	  {
	      /* The first value is the solution time */
              *PtrVarValues = TecUtilZoneGetSolutionTime(CurZone);
	      PtrVarValues++;
	      // The first variable is the distance
	      *PtrVarValues = Distance[j];
	      PtrVarValues++;
	      /*
      sprintf(message,"x y z d %f %f %f %f \n",XVals[j],YVals[j],ZVals[j],Distance[j]);
      TRACE(message);
      */
  	      IsOk = 
              TecUtilProbeAtPosition(XVals[j],YVals[j],ZVals[j],
                             &ICell,&JCell,&KCell,&Planes,
                             &CurZone,    // The current zone to extract
                             FALSE,       // garbage
                             PtrVarValues,// target Array for values
                             TempSet,     // Limit search to current zone
                             Extract3D,   // Extract 3D or 2D data
                             FALSE,       // Do extraction.
                             FALSE);      // Use interpolation
	      PtrVarValues += NumVars;
	     *PtrOk++ = IsOk;
	      /*
	      if(!IsOk)
	      {
		  TecUtilDialogMessageBox(
			    "Data points out of Domain. Stop.",
			     MessageBox_Information); 
		 break; 
	      }
	      */
    	      if(IsOk)
	      {
		  /*
	      sprintf(message," OK strand : %d / %d time %f point %d\n",
		      i+1,MaxStrandID,
		      TecUtilZoneGetSolutionTime(CurZone),j);
	      TRACE(message);
	      */
	      DataPoints[i] += 1;
	      // Data point is ok.
	      *PtrVarValues = 1.0;
	      PtrVarValues++;
	      }
	      else
	      {
		  /*
	      sprintf(message," KO strand : %d / %d time %f point %d\n",
		      i+1,MaxStrandID,
		      TecUtilZoneGetSolutionTime(CurZone),j);
	      TRACE(message);
	      */
	      *PtrVarValues = 0.0;
	      PtrVarValues++;
	      }
	  } // end loop points
  	  TecUtilSetClear(TempSet);
	  //if(!IsOk) break;
      } // end loop over time ordered zones of a strand
      //if(!IsOk) break;
    } // end loop over time strands
  }

  TecUtilStatusFinishPercentDone();
	      if(DataPoints == 0 )
	      {
		 TecUtilDialogMessageBox(
			    "All Data points out of Domain. Stop.",
			     MessageBox_Information); 
		 return FALSE; 

   	      }
  //TRACE("end loop extraction \n");

  TecUtilDataLoadEnd();
  TecUtilSetDealloc(&TempSet);

  //TRACE("deallocated temp set \n");
  /* Check the DataPointOk array. 
   * If there are less than MaxStrandID data points for one time step,
   * there are spatially disconnected zones for a given time strand.
   * Count the number of "real" time strands ....
   *
   */


  /*  We got all information needed from the User data set, so we can
   *  Deallocate the zone set
   */

  TecUtilSetDealloc(&EnabledZones);
  free(VTimes); free(OrderedZones);
  if(NbPoints > 1 )
  { free(XVals); free(YVals); free(ZVals); free(Distance);}


  
  /* Check if at least one data point is inside the domain. If not,
   * error and exit. */

  NTot = 0;
  for(i=0;i<MaxStrandID;i++) NTot += DataPoints[i];
  
  if(NTot == 0)
  {
      TecUtilDialogMessageBox(
	    "All points out of Domain. Stop.",
	     MessageBox_Information); 
     free(PlotData);
     return FALSE;
  }
  

  //TRACE("freed temp memory \n");

  /********************************************************************/

  /*-------------------------------------------------------------------
   * THE NEW DATA SET FOR XY Time Series plot.
   * This new data set may have more than one zone. Each zone
   * represents a TimeStrand of the original (user) data set.
   *
   * This data set is attached to a new frame, which will be popped on
   * the top of the (actually current) user frame.
   *
   *-------------------------------------------------------------------*/

  /* New Frame. The new data set will be attached to this frame.
   * The frame has the default size and position.
   */

  //TRACE("start creation of new frame \n");
  IsOk = TecUtilFrameCreateNew(FALSE,0,0,0,0);
  if(!IsOk)
  {
     TecUtilDialogMessageBox(
	    "Error creating new frame",
	     MessageBox_Information); 
     free(PlotData);
     return(IsOk);
  }

  //TRACE("created new frame \n");

  /* Create a new data set inside this frame, with the variable names
   * of the source zone plus the solution time variable.
   */
  
  IsOk = TecUtilStringListInsertString(VarNames,1,"Solution Time"); 
  if(!IsOk)
  {
     TecUtilDialogMessageBox(
	    "Error updating VarList for new zone. Stop.",
	     MessageBox_Information); 
     free(PlotData);
     return(IsOk);
  }

  if(NbPoints > 1) 
  {
      IsOk = TecUtilStringListInsertString(VarNames,2,"Distance"); 
      if(!IsOk)
      {
         TecUtilDialogMessageBox(
	    "Error updating VarList for new zone. Stop.",
	     MessageBox_Information); 
         free(PlotData);
         return(IsOk);
      }
  IsOk = TecUtilStringListAppendString(VarNames,"PointIsInDomain"); 
  }


  if(NbPoints == 1)
  {
      sprintf(message,"Extraction in point (%f, %f, %f)",
    	      *X_COOR,*Y_COOR,*Z_COOR);
  }
  else
  {
      sprintf(message,"Extraction for line (%f, %f, %f) (%f %f %f)",
	  X_COOR[0],Y_COOR[0],Z_COOR[0],X_COOR[1],Y_COOR[1],Z_COOR[1]);
  }
  IsOk = TecUtilDataSetCreate(message,
                              VarNames,
                              TRUE); // ResetStyle
  if(!IsOk)
  {
     TecUtilDialogMessageBox(
	    "Error creating new data set",
	     MessageBox_Information); 
     free(PlotData);
     return(IsOk);
  }


  /* There will be MaxStrandID Zones for the new dataset. Create a
   * Tecplot set structure for these zones.
   */
  //TRACE("start creating new data set \n");

  NewZones = TecUtilSetAlloc(TRUE);
  TecUtilSetClear(NewZones);

  ArgList = TecUtilArgListAlloc();
  TecUtilStatusStartPercentDone("Creating new data set ...",FALSE,TRUE);
  percent = 0;
  TecUtilStatusCheckPercentDone(percent);

  if( ResIsTransient == FALSE ||
	    NbPoints == 1    )
  {

      IsOk = CreateStaticDataSet(PlotData,DataPoints,
	NbPoints, NumTimeSteps, NumVars, ExtraVars, MaxStrandID,
	NewZones, message);
      if(!IsOk) TecUtilDialogMessageBox(message, MessageBox_Information); 
  }
  else if( ResIsTransient == TRUE )
  {
      IsOk = CreateTransientDataSet(PlotData,DataPoints,
	NbPoints, NumTimeSteps, NumVars, ExtraVars, MaxStrandID,
	NewZones, message);
      if(!IsOk) TecUtilDialogMessageBox(message, MessageBox_Information); 
  }


  // free plotdata array 
  free(PlotData);
  // Notify the changed state to tecplot



  // deallocate the zone
  TecUtilSetDealloc(&NewZones);
  TecUtilArgListClear(ArgList);

  TecUtilStatusFinishPercentDone();
  //TRACE("start adjusting plot style \n");

  if(NbPoints == 1)
  {
  //TRACE("avant .. \n");

      TecUtilFrameSetPlotType(PlotType_XYLine);
      if ( CVar >= 1 )
        {
          int NumLineMaps = TecUtilLineMapGetCount();
          EntIndex_t LineMapToActivate = BAD_SET_VALUE;
          for ( i = 0; i < NumLineMaps; i++ )
            {
              EntIndex_t YVar;
              TecUtilLineMapGetAssignment(i+1,
		                                      NULL,  //Zone
		                                      NULL,  //XVar
		                                      &YVar,
		                                      NULL,  //XAxisNum
		                                      NULL,  //YAxisNum
		                                      NULL); //FunctionDependency
              // Add 1 to CVar because the dataset in this frame (the TimeSeries frame)
              // has an addition variable (solution time) which comes before any other variables
              if ( YVar == CVar+1 )
                {
                  LineMapToActivate = i+1;
                  break;
                }
            }
          
          if ( LineMapToActivate != BAD_SET_VALUE )
            {
              Set_pa LineMapSet = TecUtilSetAlloc(TRUE);
              TecUtilSetAddMember(LineMapSet, LineMapToActivate, TRUE);
              TecUtilLineMapSetActive(LineMapSet, AssignOp_Equals);
              TecUtilSetDealloc(&LineMapSet);
            }
        }
      TecUtilViewDataFit();



  //TRACE("apres ... \n");
  }
  else
  {
      TecUtilFrameSetPlotType(PlotType_Cartesian2D);
      /*
      // 2D cartesian plot
      // set axis mode to independent,
      IsOk = TecUtilMacroExecuteCommand(
	       "$!TWODAXIS AXISMODE = INDEPENDENT"); 
      */

      // if transient data set => X variable to the actual contour var
      if( ResIsTransient == TRUE)
      {
          if(CVar >= 1){ CVar = CVar + 2;}
          TecUtilArgListClear(ArgList);
          TecUtilArgListAppendString(ArgList, SV_P1, SV_TWODAXIS);
          TecUtilArgListAppendString(ArgList, SV_P2, SV_XDETAIL);
          TecUtilArgListAppendString(ArgList, SV_P3, SV_VARNUM);
          TecUtilArgListAppendArbParam(ArgList,SV_IVALUE, CVar);
          IsOk = TecUtilStyleSetLowLevelX(ArgList); 
          TecUtilArgListClear(ArgList);
      }
      // if 2d contour plot (distance vs time) set axis mode
      // independent.
      // contour var is the actual contour variable.
      else
      {
//          if(CVar >= 1) TecUtilContourSetVariable(CVar);
//          IsOk = TecUtilMacroExecuteCommand(
//	       "$!FIELDMAP CONTOUR {SHOW = YES}"); 
	  // TODO : how to activate the contour plot from a macro ???
	  if ( CVar >= 1 ) 
	  {
	    CVar = CVar + 2; // added distance and time as variables.
            TecUtilArgListClear(ArgList);
            TecUtilArgListAppendInt(ArgList, SV_CONTOURGROUP, 1);
            TecUtilArgListAppendInt(ArgList, SV_VAR, CVar);
            TecUtilContourSetVariableX(ArgList);         
	    TRACE("set contour ok\n");
            TecUtilArgListClear(ArgList);
            TecUtilArgListAppendString(ArgList, SV_P1, SV_FIELDLAYERS);
            TecUtilArgListAppendString(ArgList, SV_P2, SV_SHOWCONTOUR);
            TecUtilArgListAppendArbParam(ArgList,SV_IVALUE, TRUE);
            IsOk = TecUtilStyleSetLowLevelX(ArgList); 
            TecUtilArgListClear(ArgList);
	    TRACE("Activate contour ok\n");
	  }
	 
      }
	

      TecUtilArgListClear(ArgList);
      TecUtilArgListAppendString(ArgList, SV_P1, SV_TWODAXIS);
      TecUtilArgListAppendString(ArgList, SV_P2, SV_AXISMODE);
      TecUtilArgListAppendArbParam(ArgList,SV_IVALUE, AxisMode_Independent);
      IsOk = TecUtilStyleSetLowLevelX(ArgList); 
      TecUtilArgListClear(ArgList);
      // nice fit
      TecUtilViewAxisNiceFit('X',1);
      TecUtilViewAxisNiceFit('Y',1);

  }

  IsOk = TRUE;

  // Pop the user frame on the top.
  TecUtilFramePopByUniqueID(UserFrameID);


  // free allocated memory

  //TRACE("dealloc arglist \n");
  TecUtilArgListDealloc(&ArgList);
  //TRACE("end processing \n");

return(IsOk);

}

