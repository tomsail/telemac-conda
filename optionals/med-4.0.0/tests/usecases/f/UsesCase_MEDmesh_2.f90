!  This file is part of MED.
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
!*  Use case 2 read a 2D unstructured mesh with 15 nodes,
!*               8 triangular cells, 4 triangular cells
!*  - Computation step : NO
!*

program UsesCase_MEDmesh_2

  implicit none
  include 'med.hf90'

  integer cret
  integer*8 fid
  integer   nmesh, it, naxis
  character(64)  :: mname = "2D unstructured mesh"
  character(200) :: desc
  character(16)  :: dtunit
  integer nstep, mdim, sdim, stype, mtype, atype
  character(16), dimension(:), allocatable :: aname
  character(16), dimension (:), allocatable :: aunit
  real*8, dimension(:), allocatable :: ncoord
  integer coocha, geotra, nnodes, ntria3, nquad4
  integer, dimension(:), allocatable :: tricon
  integer, dimension(:), allocatable :: quacon

  ! open MED file with READ ONLY access mode **
  call mfiope(fid,'UsesCase_MEDmesh_1.med',MED_ACC_RDONLY, cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : open file'
     call efexit(-1)
  endif

  ! ... we know that the MED file has only one mesh, 
  ! a real code working would check ... 

  ! read mesh informations : computation space dimension
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

  ! read how many nodes in the mesh **
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NO_GEOTYPE,MED_COORDINATE,MED_NO_CMODE,coocha,geotra,nnodes,cret)
  if (cret .ne. 0 ) then
     print *,'Read how many nodes in the mesh'
     call efexit(-1)
  endif
  print *,"number of nodes in the mesh =", nnodes

  ! we know that we only have MED_TRIA3 and MED_QUAD4 in the mesh
  ! a real code working would check all MED geometry cell types

  ! read how many triangular cells in the mesh
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_CONNECTIVITY,MED_NODAL,coocha,geotra,ntria3,cret)
  if (cret .ne. 0 ) then
     print *,'Read how many nodes in the mesh'
     call efexit(-1)
  endif
  print *,"number of triangular cells in the mesh =", ntria3

  ! read how many quadrangular cells in the mesh
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,MED_CONNECTIVITY,MED_NODAL,coocha,geotra,nquad4,cret)
  if (cret .ne. 0 ) then
     print *,'Read how many nodes in the mesh'
     call efexit(-1)
  endif
  print *,"number of quadrangular cells in the mesh =", nquad4

  ! read mesh nodes coordinates
  allocate (ncoord(nnodes*2),STAT=cret)
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhcor(fid,mname,MED_NO_DT,MED_NO_IT,MED_FULL_INTERLACE,ncoord,cret)
  if (cret .ne. 0 ) then
     print *,'Nodes coordinates'
     call efexit(-1)
  endif
  print *,"Nodes coordinates =", ncoord
  deallocate(ncoord)

  ! read cells connectivity in the mesh
  allocate ( tricon(ntria3 * 3) ,STAT=cret )
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhcyr(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_NODAL,MED_FULL_INTERLACE,tricon,cret)
  if (cret .ne. 0 ) then
     print *,'MED_TRIA3 connectivity'
     call efexit(-1)
  endif
  print *,"MED_TRIA3 connectivity =", tricon
  deallocate(tricon)

  allocate ( quacon(nquad4*4) ,STAT=cret )
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhcyr(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,MED_NODAL,MED_FULL_INTERLACE,quacon,cret)
  if (cret .ne. 0 ) then
     print *,'MED_QUAD4 connectivity'
     call efexit(-1)
  endif
  print *,"MED_QUAD4 connectivity =", quacon
  deallocate(quacon)

  ! we know that the family number of nodes and elements is 0, a real working would check ...

  ! close file **
  call mficlo(fid,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR :  close file'
     call efexit(-1)
  endif

end program UsesCase_MEDmesh_2

