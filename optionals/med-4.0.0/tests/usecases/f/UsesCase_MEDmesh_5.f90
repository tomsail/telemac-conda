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
!*
!*  Use case 5 : read a 2D structured mesh
!*

program UsesCase_MEDmesh_5

  implicit none
  include 'med.hf90'

  integer cret
  integer*8 fid
  integer   nmesh, it, naxis, axis
  integer coocha, geotra
  character(64)  :: mname = "2D structured mesh"
  character(200) :: desc
  character(16)  :: dtunit
  integer nstep, mdim, sdim, stype, mtype, atype, asize
  integer gtype, ncell
  character(16), dimension(:), allocatable :: aname
  character(16), dimension (:), allocatable :: aunit
  real*8, dimension (:), allocatable :: cooXaxis
  real*8, dimension (:), allocatable :: cooYaxis
  character*16, dimension (:), allocatable ::  cnames

  ! open MED file
  call mfiope(fid,'UsesCase_MEDmesh_4.med',MED_ACC_RDONLY, cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : open file'
     call efexit(-1)
  endif

  ! ... we know that the MED file has only one mesh, 
  ! a real code working would check ... 

  ! read computation space dimension
  call mmhnan(fid,mname,naxis,cret)
  if (cret .ne. 0 ) then
     print *,'Read number of axis in the mesh'
     call efexit(-1)
  endif
  print *,'Number of axis in the mesh  = ',naxis

  ! read mesh informations
  allocate ( aname(naxis), aunit(naxis) ,STAT=cret )
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call  mmhmin(fid, mname, sdim, mdim, mtype, desc, dtunit, stype, nstep, atype, aname, aunit, cret)
  if (cret .ne. 0 ) then
     print *,'Read mesh informations'
     call efexit(-1)
  endif
  print *,"mesh name =", mname
  print *,"space dim =", sdim
  print *,"mesh dim =", mdim
  print *,"mesh type =", mtype
  print *,"mesh description =", desc
  print *,"dt unit = ", dtunit
  print *,"sorting type =", stype
  print *,"number of computing step =", nstep
  print *,"coordinates axis type =", atype
  print *,"coordinates axis name =", aname
  print *,"coordinates axis units =", aunit
  deallocate(aname, aunit)

  ! read grid type
  call mmhgtr(fid,mname,gtype,cret)
  if (cret .ne. 0 ) then
     print *,'Read grid type'
     call efexit(-1)
  endif
  print *,"grid type =", gtype

  ! ... we know that we the mesh is a cartesian grid,  
  ! a real code working would check  ... 

  ! read the axis coordinates (MED_CARTESIAN coordinates system)
  ! X
  axis = 1
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,MED_COORDINATE_AXIS1,MED_NO_CMODE,coocha,geotra,asize,cret)
  if (cret .ne. 0 ) then
     print *,'Read number of coordinates on X axis '
     call efexit(-1)
  endif
  print *,"Number of coordinates on X axis  =", asize
  ncell = asize-1

  allocate ( cooXaxis(asize),STAT=cret )
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhgcr(fid,mname,MED_NO_DT,MED_NO_IT,axis,cooXaxis,cret)
  if (cret .ne. 0 ) then
     print *,'Read axis X coordinates'
     call efexit(-1)
  endif
  print *,"Axis X coordinates  =", cooXaxis
  deallocate(cooXaxis)

  ! Y
  axis = 2
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,MED_COORDINATE_AXIS2,MED_NO_CMODE,coocha,geotra,asize,cret)
  if (cret .ne. 0 ) then
     print *,'Read number of coordinates on Y axis '
     call efexit(-1)
  endif
  print *,"Number of coordinates on Y axis  =", asize
  ncell = ncell * (asize-1)

  allocate ( cooYaxis(asize),STAT=cret )
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhgcr(fid,mname,MED_NO_DT,MED_NO_IT,axis,cooYaxis,cret)
  if (cret .ne. 0 ) then
     print *,'Read axis Y coordinates'
     call efexit(-1)
  endif
  print *,"Axis Y coordinates  =", cooYaxis
  deallocate(cooYaxis)

  ! optionnal : read names for nodes or elements
  print *,'ncell :', ncell
  allocate ( cnames(ncell),STAT=cret )
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhear(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,cnames,cret)
  if (cret .ne. 0 ) then
     print *,'Read names for elements'
     call efexit(-1)
  endif
  print *,'Cells names =', cnames
  deallocate(cnames)

  ! close file
  call mficlo(fid,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR :  close file'
     call efexit(-1)
  endif

end program UsesCase_MEDmesh_5

