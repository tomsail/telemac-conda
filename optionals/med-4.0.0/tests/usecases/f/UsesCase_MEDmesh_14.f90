!*  This file is part of MED.
!*
!*  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
!*  MED is free software: you can redistribute it and/or modify
!*  it under the terms of the GNU Lesser General Public License as published by
!*  the Free Software Foundation, either version 3 of the License, or
!*  (at your option) any later version.
!*
!*  MED is distributed in the hope that it will be useful,
!*  but WITHOUT ANY WARRANTY; without even the implied warranty of
!*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!*  GNU Lesser General Public License for more details.
!*
!*  You should have received a copy of the GNU Lesser General Public License
!*  along with MED.  If not, see <http://www.gnu.org/licenses/>.
!*
!*
!*  Use case 14 : read a 2D unstructured mesh with 2 polygons
!*

program UsesCase_MEDmesh_14

  implicit none
  include 'med.hf90'

  integer cret
  integer*8 fid

  
  ! mesh name
  character*64 mname
  ! file name
  character*64 finame
  ! mesh description
  character*200 mdesc
  ! mesh dim, space dim
  integer mdim, sdim
  !sorting type
  integer stype
  ! number of computing step
  integer nstep
  ! mesh type, coordinate axis type
  integer mtype, atype
  ! axis name, unit name
  character*16 axname(2), unname(2)
  ! time step unit
  character*16 dtunit
  ! coordinates
  real*8, dimension(:), allocatable :: coords
  integer nnodes
  integer npoly
  ! index size
  integer isize
  integer, dimension(:), allocatable :: index 
  ! connectivity
  integer, dimension(:), allocatable :: conity 
  ! connectivity size
  integer cosize
  ! coordinate changement, geotransformation
  integer coocha, geotra

  parameter (mname = "2D unstructured mesh")
  parameter (finame = "UsesCase_MEDmesh_13.med")

  ! open MED file with READ ONLY access mode
  call mfiope(fid, finame, MED_ACC_RDONLY, cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : open file'
     call efexit(-1)
  endif

  ! read mesh informations : mesh dimension, space dimension ...
  call  mmhmin(fid, mname, sdim, mdim, mtype, mdesc, dtunit, stype, nstep, atype, axname, unname, cret)
  if (cret .ne. 0 ) then
     print *,'Read mesh informations'
     call efexit(-1)
  endif
  print *,"mesh name =", mname
  print *,"space dim =", sdim
  print *,"mesh dim =", mdim
  print *,"mesh type =", mtype
  print *,"mesh description =", mdesc
  print *,"dt unit = ", dtunit
  print *,"sorting type =", stype
  print *,"number of computing step =", nstep
  print *,"coordinates axis type =", atype
  print *,"coordinates axis name =", axname
  print *,"coordinates axis units =", unname

  ! read how many nodes in the mesh
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_NODE,MED_POINT1,MED_COORDINATE,MED_NO_CMODE,coocha,geotra,nnodes,cret)
  if (cret .ne. 0 ) then
     print *,'Read number of nodes ...'
     call efexit(-1)
  endif
  print *,"Number of nodes  =", nnodes

  ! we know that we only have MED_POLYGON celles in the mesh, 
  ! a real code working would check all MED geometry cell types ...

  ! How many polygon in the mesh in nodal connectivity mode
  ! For the polygons, we get the size of array index
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_POLYGON,MED_INDEX_NODE,MED_NODAL,coocha,geotra,isize,cret)
  if (cret .ne. 0 ) then
     print *,'Read number of polygon ...'
     call efexit(-1)
  endif
  npoly = isize - 1
  print *,"Number of polygons  =", npoly

  ! how many nodes for the polygon connectivity ? 
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_POLYGON,MED_CONNECTIVITY,MED_NODAL,coocha,geotra,cosize,cret)
  if (cret .ne. 0 ) then
     print *,'Read connectivity size ...'
     call efexit(-1)
  endif
  print *,"Read connectivity size ...", cosize

  ! read mesh nodes coordinates
  allocate (coords(nnodes*sdim),STAT=cret)
  if (cret .ne. 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhcor(fid,mname,MED_NO_DT,MED_NO_IT,MED_FULL_INTERLACE,coords,cret)
  if (cret .ne. 0 ) then
     print *,'Read nodes coordinates ...'
     call efexit(-1)
  endif
  print *,"Read nodes coordinates ...", coords

  deallocate(coords)

  ! read polygons connectivity mmhpgr
  allocate (index(isize),STAT=cret)
  if (cret .ne. 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  allocate (conity(cosize),STAT=cret)
  if (cret .ne. 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhpgr(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_NODAL,index,conity,cret)
  if (cret .ne. 0 ) then
     print *,'Read polygon connectivity ...'
     call efexit(-1)
  endif
  print *,"Read polygon connectivity ...", conity

  deallocate(index)
  deallocate(conity)

  ! ... we know that the family number of nodes and elements is 0, a real working would check ...

! close MED file
  call mficlo(fid,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR :  close file'
     call efexit(-1)
  endif

end program UsesCase_MEDmesh_14
