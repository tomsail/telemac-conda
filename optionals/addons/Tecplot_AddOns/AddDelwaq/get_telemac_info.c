

  IsOk = TecUtilDataSetGetInfo(&Title,&NumZones,&NumVars);
  TecUtilZoneGetEnabled(&EnabledZones);

  if ( EnabledZones == NULL)
  {
      // error, no zones !
      *nzone = 0;
  }
  else
  {
    // get the number of points in the data set.
    // we suppose all zones share the same connectivity and have the
    // same number of nodes and elements.

  }
