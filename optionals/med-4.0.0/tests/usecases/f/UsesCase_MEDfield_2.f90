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
!*  Field use case 2 : read the field of use case 1
!*

program UsesCase_MEDfield_2

  implicit none
  include 'med.hf90'

  integer cret
  integer*8 fid

  character(64) :: mname
  ! field name
  character(64) :: finame = 'TEMPERATURE_FIELD'
  ! nvalues, local mesh, field type
  integer nstep, nvals, lcmesh, fitype
  ! component name
  character(16) :: cpname
  ! component unit
  character(16) :: cpunit
  character(16) :: dtunit

  ! vertices values      
  real*8, dimension(:), allocatable :: verval
  real*8, dimension(:), allocatable :: tria3v
  real*8, dimension(:), allocatable :: quad4v

  ! open MED file with READ ONLY access mode **
  call mfiope(fid,'UsesCase_MEDfield_1.med',MED_ACC_RDONLY,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : opening file'
     call efexit(-1)
  endif

  ! ... we know that the MED file has only one field with one component , 
  ! a real code working would check ... 

  ! if you know the field name, direct access to field informations
  call mfdfin(fid,finame,mname,lcmesh,fitype,cpname,cpunit,dtunit,nstep,cret)
    print *,cret
  if (cret .ne. 0 ) then
     print *,'ERROR : field info by name'
     call efexit(-1)
  endif
  print *, 'Mesh name :', mname
  print *, 'Local mesh :', lcmesh
  print *, 'Field type :', fitype
  print *, 'Component name :', cpname
  print *, 'Component unit :', cpunit
  print *, 'dtunit :', dtunit
  print *, 'nstep :', nstep

  ! ... we know that the field values are defined on vertices and MED_TRIA3
  ! and MED_QUAD4 cells, a real code working would check ...

  ! MED_NODE
  call mfdnva(fid,finame,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,nvals,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : read number of values ...'
     call efexit(-1)
  endif

  print *, 'Node number :', nvals

  allocate ( verval(nvals),STAT=cret )
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mfdrvr(fid,finame,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,verval,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : read fields values on vertices ...'
     call efexit(-1)
  endif

  print *, 'Fields values on vertices :', verval

  deallocate(verval)

  ! MED_TRIA3
  call mfdnva(fid,finame,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,nvals,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : read number of values ...'
     call efexit(-1)
  endif

  print *, 'Triangulars cells number :', nvals

  allocate ( tria3v(nvals),STAT=cret )
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mfdrvr(fid,finame,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,tria3v,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : read fields values for MED_TRIA3 cells ...'
     call efexit(-1)
  endif

  print *, 'Fiels values for MED_TRIA3 cells :', tria3v

  deallocate(tria3v)

  ! MED_QUAD4
  call mfdnva(fid,finame,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,nvals,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : read number of values ...'
     call efexit(-1)
  endif

  print *, 'Quadrangulars cells number :', nvals

  allocate ( quad4v(nvals),STAT=cret )
  if (cret > 0) then
     print *,'Memory allocation'
     call efexit(-1)
  endif

  call mfdrvr(fid,finame,MED_NO_DT,MED_NO_IT,MED_CELL,MED_QUAD4,MED_FULL_INTERLACE,MED_ALL_CONSTITUENT,quad4v,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : read fields values for MED_QUAD4 cells ...'
     call efexit(-1)
  endif

  print *, 'Fiels values for MED_QUAD4 cells :', quad4v

  deallocate(quad4v)

  ! close file **
  call mficlo(fid,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR :  close file'
     call efexit(-1)
  endif

end program UsesCase_MEDfield_2

