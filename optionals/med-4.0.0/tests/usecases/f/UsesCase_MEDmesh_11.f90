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
!*  Use case 11 : read a 2D unstructured mesh with 15 nodes, 8 triangular cells, 4 quadragular cells with
!*  nodes families
!*

program UsesCase_MEDmesh_11

  implicit none
  include 'med.hf90'

  integer cret
  integer*8 fid

  ! space dim, mesh dim
  integer sdim, mdim
  ! axis name, unit name
  character*16 axname(2), unname(2)
  ! time step unit
  character*16 dtunit
  ! mesh name, family name, file name
  character*64 mname, fyname, finame
  ! mesh type, sorting type, coordinate axis type
  integer mtype, stype, atype
  ! number of family, number of group, family number
  integer nfam, ngro, fnum
  ! number of computing step
  integer nstep
  ! coordinate changement, geotransformation
  integer coocha, geotra
  ! number of family numbers
  integer nfanbrs
  ! coordinates
  real*8, dimension(:), allocatable :: coords
  integer nnodes, ntria3, nquad4
  ! triangular and quadrangular cells connectivity
  ! integer tricon(24), quacon(16)
  integer, dimension(:), allocatable :: tricon, quacon
  integer n
  ! family numbers
  ! integer fanbrs(15)
  integer, dimension (:), allocatable :: fanbrs
  ! comment 1, mesh description
  character*200 cmt1, mdesc
  ! group name
  character*80, dimension (:), allocatable ::  gname

  parameter (mname = "2D unstructured mesh")
  parameter (finame = "UsesCase_MEDmesh_10.med")

  ! open MED file with READ ONLY access mode
  call mfiope(fid, finame, MED_ACC_RDONLY, cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : open file'
     call efexit(-1)
  endif

  ! ... we know that the MED file has only one mesh,
  ! a real code working would check ...

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
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NO_GEOTYPE,MED_COORDINATE,MED_NO_CMODE,coocha,geotra,nnodes,cret)
  if (cret .ne. 0 ) then
     print *,'Read number of nodes ...'
     call efexit(-1)
  endif
  print *,"Number of nodes  =", nnodes

  ! ... we know that we only have MED_TRIA3 and MED_QUAD4 in the mesh,
  ! a real code working would check all MED geometry cell types ...

  ! read how many triangular cells in the mesh
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_CONNECTIVITY,MED_NODAL,coocha,geotra,ntria3,cret)
  if (cret .ne. 0 ) then
     print *,'Read number of MED_TRIA3 ...'
     call efexit(-1)
  endif
  print *,"Number of MED_TRIA3  =", ntria3

  ! read how many quadrangular cells in the mesh
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,MED_CONNECTIVITY,MED_NODAL,coocha,geotra,nquad4,cret)
  if (cret .ne. 0 ) then
     print *,'Read number of MED_QUAD4 ...'
     call efexit(-1)
  endif
  print *,"Number of MED_QUAD4  =", nquad4

  ! read mesh nodes coordinates
  allocate ( coords(nnodes*sdim),STAT=cret )
  if (cret .ne. 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhcor(fid,mname,MED_NO_DT,MED_NO_IT,MED_FULL_INTERLACE,coords,cret)
  print *,cret
  if (cret .ne. 0 ) then
     print *,'Read nodes coordinates'
     call efexit(-1)
  endif
  print *,"Nodes coordinates =", coords
  deallocate(coords)

  ! read cells connectivity in the mesh
  allocate ( tricon(ntria3*3),STAT=cret )
  if (cret .ne. 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhcyr(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_NODAL,MED_FULL_INTERLACE,tricon,cret)
  if (cret .ne. 0 ) then
     print *,'Read MED_TRIA3 connectivity'
     call efexit(-1)
  endif
  print *,"MED_TRIA3 connectivity =", tricon
  deallocate(tricon)

  ! read cells connectivity in the mesh
  allocate ( quacon(nquad4*4),STAT=cret )
  if (cret .ne. 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mmhcyr(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,MED_NODAL,MED_FULL_INTERLACE,quacon,cret)
  if (cret .ne. 0 ) then
     print *,'Read MED_QUAD4 connectivity'
     call efexit(-1)
  endif
  print *,"MED_QUAD4 connectivity =", quacon
  deallocate(quacon)

  ! read families of entities
  call mfanfa(fid,mname,nfam,cret)
  if (cret .ne. 0 ) then
     print *,'Read number of family'
     call efexit(-1)
  endif
  print *,"Number of family =", nfam

  do n=1,nfam

     call mfanfg(fid,mname,n,ngro,cret)
     if (cret .ne. 0 ) then
        print *,'Read number of group in a family'
        call efexit(-1)
     endif
     print *,"Number of group in family =", ngro

     if (ngro .gt. 0) then
        allocate ( gname((ngro)),STAT=cret )
        if (cret .ne. 0) then
           print *,'Memory allocation'
           call efexit(-1)
        endif
        call mfafai(fid,mname,n,fyname,fnum,gname,cret)
        if (cret .ne. 0) then
           print *,'Read group names'
           call efexit(-1)
        endif
        print *,"Group name =", gname
        deallocate(gname)
     endif

  enddo

  ! read family numbers for nodes
  ! By convention, if there is no numbers in the file, it means that 0 is the family
  ! number of all nodes
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,MED_FAMILY_NUMBER,MED_NO_CMODE,coocha,geotra,nfanbrs,cret)
  if (cret .ne. 0) then
     print *,'Check family numbers nodes'
     call efexit(-1)
  endif
  allocate ( fanbrs(nnodes),STAT=cret )
  if (cret .ne. 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif
  if (nfanbrs .ne. 0) then
     call mmhfnr(fid,mname,MED_NO_DT,MED_NO_IT,MED_NODE, MED_NONE,fanbrs,cret)
     if (cret .ne. 0) then
        print *,'Read family numbers nodes'
        call efexit(-1)
     endif
  else
     do n=1,nnodes
        fanbrs(n) = 0
     enddo
  endif
  print *, 'Family numbers for nodes :', fanbrs
  deallocate(fanbrs)

  ! read family numbers for cells
  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_FAMILY_NUMBER,MED_NODAL,coocha,geotra,nfanbrs,cret)
  if (cret .ne. 0) then
     print *,'Check family numbers tria3'
     call efexit(-1)
  endif
  allocate ( fanbrs(ntria3),STAT=cret )
  if (cret .ne. 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  if (nfanbrs .ne. 0) then
     call mmhfnr(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,fanbrs,cret)
     if (cret .ne. 0) then
        print *,'Read family numbers tria3'
        call efexit(-1)
     endif
  else
     do n=1,ntria3
        fanbrs(n) = 0
     enddo
  endif
  print *, 'Family numbers for tria cells :', fanbrs
  deallocate(fanbrs)

  call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,MED_FAMILY_NUMBER,MED_NODAL,coocha,geotra,nfanbrs,cret)
  if (cret .ne. 0) then
     print *,'Check family numbers quad4'
     call efexit(-1)
  endif
  allocate ( fanbrs(nquad4),STAT=cret )
  if (cret .ne. 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif
  if (nfanbrs .ne. 0) then
     call mmhfnr(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,fanbrs,cret)
     if (cret .ne. 0) then
        print *,'Read family numbers quad4'
        call efexit(-1)
     endif
  else
     do n=1,nquad4
        fanbrs(n) = 0
     enddo
  endif
  print *, 'Family numbers for quad cells :', fanbrs
  deallocate(fanbrs)

! close MED file
  call mficlo(fid,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR :  close file'
     call efexit(-1)
  endif

end program UsesCase_MEDmesh_11

