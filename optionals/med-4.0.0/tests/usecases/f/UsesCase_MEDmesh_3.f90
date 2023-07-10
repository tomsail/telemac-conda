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
!*  Use case 3 : read an unstructured mesh : generic approach
!*  - Computation step : NO
!*

program UsesCase_MEDmesh_3

  implicit none
  include 'med.hf90'

  integer cret
  integer*8 fid
  integer   nmesh, imesh, naxis, igeo, geotyp, nelt
  character(64)  :: mname, gtname
  character(200) :: desc
  character(16)  :: dtunit
  integer nstep, mdim, sdim, stype, mtype, atype
  integer coocha, geotra, nnodes, ngeo
  character(16), dimension(:), allocatable :: aname
  character(16), dimension (:), allocatable :: aunit
  real*8, dimension(:), allocatable :: ncoord

  integer, dimension(:), allocatable :: connectivity

  ! open file             **
  call mfiope(fid,'UsesCase_MEDmesh_1.med',MED_ACC_RDONLY, cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : open file'
     call efexit(-1)
  endif

  ! how many mesh in the file ? **
  call mmhnmh(fid,nmesh,cret)
  if (cret .ne. 0 ) then
     print *,'Read how many mesh'
     call efexit(-1)
  endif
  print *,'Number of mesh = ',nmesh

  do imesh=1,nmesh

     print *,'mesh iterator =',imesh

     ! read computation space dimension **
     call mmhnax(fid,imesh,naxis,cret)
     if (cret .ne. 0 ) then
        print *,'Read number of axis in the mesh'
        call efexit(-1)
     endif
     print *,'Number of axis in the mesh  = ',naxis

     allocate ( aname(naxis), aunit(naxis) ,STAT=cret )
     if (cret > 0) then
        print *,'Memory allocation'
        call efexit(-1)
     endif
     ! read mesh informations **
     call  mmhmii(fid, imesh, mname, sdim, mdim, mtype, desc, dtunit, stype, nstep, atype, aname, aunit, cret)
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

     ! read how many nodes in the mesh
     call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NO_GEOTYPE,MED_COORDINATE,MED_NO_CMODE,coocha,geotra,nnodes,cret)
     if (cret .ne. 0 ) then
        print *,'Read how many nodes in the mesh'
        call efexit(-1)
     endif
     print *,"number of nodes in the mesh =", nnodes

     ! read mesh nodes coordinates
     allocate ( ncoord(nnodes*2) ,STAT=cret )
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

     ! read number of geometrical types for cells 
     call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_GEO_ALL,MED_CONNECTIVITY,MED_NODAL,coocha,geotra,ngeo,cret)
     if (cret .ne. 0 ) then
        print *,'Read number of geometrical types for cells'
        call efexit(-1)
     endif
     print *,"number of geometrical types for cells =", ngeo

     do igeo=1,ngeo

        print *,'mesh iterator =',imesh

        ! get geometry type
        call mmheni(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,igeo,gtname,geotyp,cret)
        if (cret .ne. 0 ) then
           print *,'Read geometry type'
           call efexit(-1)
        endif
        print *,"Geometry type =", geotyp

        ! how many cells of type geotype ?
        call mmhnme(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,geotyp,MED_CONNECTIVITY,MED_NODAL,coocha,geotra,nelt,cret)
        if (cret .ne. 0 ) then
           print *,'Read number of cells in the geotype'
           call efexit(-1)
        endif
        print *,"number of cells in the geotype =", nelt

        ! read mesh nodes coordinates
        allocate ( connectivity(nelt*4) ,STAT=cret )
        if (cret > 0) then
           print *,'Memory allocation - connectivity'
           call efexit(-1)
        endif

        ! read cells connectivity in the mesh
        call mmhcyr(fid,mname,MED_NO_DT,MED_NO_IT,MED_CELL,geotyp,MED_NODAL,MED_FULL_INTERLACE,connectivity,cret)
        if (cret .ne. 0 ) then
           print *,'Connectivity'
           call efexit(-1)
        endif
        print *,"Connectivity =", connectivity
        deallocate(connectivity)

     enddo
  enddo

  ! close file **
  call mficlo(fid,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR :  close file'
     call efexit(-1)
  endif

end program UsesCase_MEDmesh_3

