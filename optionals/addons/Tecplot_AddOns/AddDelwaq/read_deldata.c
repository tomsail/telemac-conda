
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

  
