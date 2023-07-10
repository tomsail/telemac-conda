A set of files is used by Telemac-2D as input. Some files are
optional.
The input files are the following : 
- the steering file (mandatory), containing the configuration of the
  computation, here "t2d_garonne_zonal.cas"
- the geomatry file (mandatory), containing the mesh, here "t2d_garonne.geo"
- the boundary conditions file (mandatory), containing the description of the
  type of each boundary, here "t2d_garonne.cli"
- the previous computation file, which can give the initial state of the
  computation. This is an optional file. Here "r2d_garonne_reprise_zonal.slf"
- The FORTRAN file, containing the specific programming, here "Princi.f"
 

