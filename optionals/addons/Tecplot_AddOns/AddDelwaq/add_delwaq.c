/*********************************************************************
 *  AddOn 
 **********************************************************************/


#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "UTIL.h"


/************************************************************************/
Boolean_t add_delwaq_data ( FILE *f, char **ErrMsg )
/************************************************************************ 
 * Function : Read the variable names in the serafin/volfin file.
 * Each variable name is 32 bytes long.
 * Concatenate these names, separeted by a "," as requested by tecplot
 * and add the coordinates X and Y.
 ************************************************************************/
{
  Boolean_t IsOk = TRUE ;

  char str1[40];
  char str2[40];
  char str3[40];
  char str4[40];
  char VarName[20];
  char* Title;

  Boolean_t BytesChecked;
  Boolean_t INVERT;
  int npts; // number of points in delwaq file
  int NPOIN; // number of points telemac
  int nvar_delw; // number of delwaq variables
  int NumVars; // number of variables in current data set (telemac)
  int NumZones; // number of zones in current data set (telemac)
  int i,j; // loop counter
  int i3;
  Set_pa      EnabledZones ; // The set of enabled zones
  EntIndex_t  CurZone ;      // The current zone as an entity
  FieldData_pa FieldDataRef ;

  FieldDataType_e *DataType;

  float *VTab, *PtrTab;

/************************************************************************/

  INVERT = FALSE ;
  TRACE("Entered add_delwaq_data\n");

  // Read the first lines : 
  // 4 times 40 characters.
  if(fread(&str1,40,1,f) != 1)
  {
      TRACE("Read 40 character string failed : \n");
      *ErrMsg = "Error reading first 40 characters";
      IsOk = FALSE;
      return(IsOk);
  }
  TRACE(str1);
  TRACE("\n");

  if(fread(&str2,40,1,f) != 1)
  {
      TRACE("Read 40 character string failed : \n");
      *ErrMsg = "Error reading second 40 characters";
      IsOk = FALSE;
      return(IsOk);
  }
  TRACE(str2);
      TRACE("\n");

  if(fread(&str3,40,1,f) != 1)
  {
      TRACE("Read 40 character string failed : \n");
      *ErrMsg = "Error reading third 40 characters";
      IsOk = FALSE;
      return(IsOk);
  }
  TRACE(str3);
      TRACE("\n");


  if(fread(&str4,40,1,f) != 1)
  {
      TRACE("Read 40 character string failed : \n");
      *ErrMsg = "Error reading fourth 40 characters";
      IsOk = FALSE;
      return(IsOk);
  }
  TRACE(str4);
      TRACE("\n");

  if(fread(&nvar_delw,4,1,f) != 1)
  {
      TRACE("Read number of variables - failed. \n");
      *ErrMsg = "Error Reading Number of variables";
      IsOk = FALSE;
      return(IsOk);
  }
  sprintf(str4,"number of variables delw : %d \n",nvar_delw);
  TRACE(str4);

  if(fread(&npts,4,1,f) != 1)
  {
      TRACE("Read 40 character string : \n");
      TRACE(str4);
      TRACE("\n");
      *ErrMsg = "Error Reading number of points";
      IsOk = FALSE;
      return(IsOk);
  }
  sprintf(str4,"number of points delw : %d \n",npts);
  TRACE(str4);


  

/* Get the informations about the current data set.
 * title, number of zones and number of variables.
 **/

  IsOk = TecUtilDataSetGetInfo(&Title,(EntIndex_t*)&NumZones,
	                              (EntIndex_t*)&NumVars);
  TRACE(Title);
  TRACE("Current data set informations : \n");
  TRACE("Title : " );
  TRACE(Title);
  TRACE("\n");
  // NumZones is not really the number of zones ... to check.
  // count the number of zones later.
  sprintf(str4,"number of zones : %d \n",NumZones);
  TRACE(str4);
  sprintf(str4,"number of variables : %d \n",NumVars);
  TRACE(str4);

  // get a handle to the zone set :
  TecUtilZoneGetEnabled(&EnabledZones);

  if ( EnabledZones == NULL)
  {
      TRACE("No Zones in current data set \n");
      *ErrMsg = "No Zones defined in current data set ";
      IsOk = FALSE;
  }
  else
  {
  // Loop over all zones in the current data set.
  // check if the byte order should be inverse.
  // check if all zones in the actual data set have the right number
  // of nodes.

      TRACE("Loop over all zones : \n"); 
      NumZones = 0;
      // loop over all members of the zone set :
      TecUtilSetForEachMember(CurZone,EnabledZones)
      {
	  // Get the number of points in the zone. (other info
	  // ignored)
	   TecUtilZoneGetInfo( CurZone, &NPOIN,
                               NULL, NULL, NULL, NULL, NULL, NULL,
                               NULL, NULL, NULL, NULL, NULL, NULL);
	   // count the zones in the dataset
	   NumZones++;
	  // If the number of points is not the same as in the 
	  // delwaq file, try inverse byte order. 
	  if(NPOIN != npts ) 
	  {
	      TRACE("try inverse bytes \n");
	      // Try inverse byte order :
	      if (!BytesChecked )
	      {
	          swapbytes32(npts);
		  // if ok, keep track of inverted bytes.
		  if ( npts == NPOIN )
		  {
		      INVERT = TRUE;
		      TRACE("Inverse byte order");
		  }
		  else
		  {
		      // bad file, error.
		      TRACE("bad number of points .. bye. \n");
	              *ErrMsg = "Bad Number of points in data file";
	              IsOk = FALSE;
		      break ;
		  }
	      }
	  }
      }
  }

  // if inverted byte order, swap bytes for the number of variables.
  if (INVERT ) swapbytes32(nvar_delw);
  TRACE("CHeck for no of points passed. \n");
  sprintf(str4,"number of points delw : %d \n",npts);
  TRACE(str4);
  sprintf(str4,"number of vars delw : %d \n",nvar_delw);
  TRACE(str4);
  sprintf(str4,"number of zones telemac : %d \n",NumZones);
  TRACE(str4);


 // Allocate a table of size NumZones for the type of the array.
 // Delwaq data are of simple precision type.
 // This table is needed when the variable is created (Tecplot
 // needs to know the variable type for each zone).

if(IsOk) 
{

  TRACE("Allocation of data value tab : \n");
  DataType = (FieldDataType_e*)calloc(NumZones,sizeof(FieldDataType_e));
  TRACE("passed.\n");
  for(i=0;i<NumZones;i++)
  {
      DataType[i] = FieldDataType_Float;
  }

  if ( IsOk ) // the number of points is OK.
  {
  // read the variable names and add them to the current data set :
  // if inversed byte order, invert nvar first.
  for (i=0;i < nvar_delw; i++)
  {
      if(fread(&VarName,20,1,f) != 1)
      {
          *ErrMsg = "Error reading variable names ";
	  IsOk = FALSE;
          break;
      }
      else
      {
	  // a variable name should end with a 0.
	  VarName[19] = 0;
	  TRACE("Variable name : \n");
	  TRACE(VarName);
	  TRACE("\n");
	  // Create the new variable.
	  // The type of the variable is float.
	  // data values will come later!
	  TecUtilDataSetAddVar(VarName,DataType);
	  TRACE("Added var to data set.\n");
      }
  }

  free(DataType);

  // allocate a table for storing the values :
  VTab = (float*)calloc(npts*nvar_delw,sizeof(float));

  if ( IsOk ) // the variables are added to the data set.
  {
      // Read the values in the file zone by zone. Put the values in
      // the data set while reading.
      TecUtilSetForEachMember(CurZone,EnabledZones)
      {
	  // Read an integer : (time step number ??)
	  if ( fread(&i3,4,1,f) != 1)
	  {
	      *ErrMsg = "Error while reading file ";
	      TRACE(*ErrMsg);
	      IsOk = FALSE;
	      break;
	  }
          //sprintf(str4,"header zone : %d \n",i3);
          //TRACE(str4);
	  // The number of variables in the current data set
	  // is NumVars+nvar_delw. The last nvar_delw variables
	  // have to be read.

	  for(i=0;i<npts;i++)
	  {
	    for(j=0;j<nvar_delw;j++)
	    {
		// read delwaq data values.
		// Put all points for a variable together (=>see
		// tecplot writing)
                 PtrTab = VTab + j*npts + i ;
		 fread(PtrTab,4,1,f);
		 if (INVERT) swapbytes32(*PtrTab)
	    }
	            
	  }

	  // point to the beginning of the data table (the npts values
	  // of the first variable)
	  PtrTab = VTab;
	  //TRACE("Try to add the data values : \n");
	  // loop over the nvar_delw added variables
	  for(j=NumVars;j<NumVars+nvar_delw;j++)
	  {
	    // Get a writable ref for the variable.
	    // Tecplot starts count at 1.
            FieldDataRef = TecUtilDataValueGetWritableRef(CurZone,j+1);
	    // set the array as value for the variable. 
            //sprintf(str4,"Variable no : %d \n",j+1);
            //TRACE(str4);
	    // datavalues for tecplot start at 1 :
            TecUtilDataValueArraySetByRef(FieldDataRef, // data ref
		                          1,            // point offset
		                          npts,         // no of points
					  PtrTab );     // ptr to array
	    // point to the values of the next variable
	    PtrTab += npts;
	  }
      }

      // variables are added to the current data set : send a
      // notification to tecplot:
      TecUtilStateChanged(StateChange_VarsAdded,(ArbParam_t)NULL);

  }
  // free memory of data value tab.
  free(VTab);
  
  }

}
  
  return(IsOk) ;
}
/************************************************************************/
