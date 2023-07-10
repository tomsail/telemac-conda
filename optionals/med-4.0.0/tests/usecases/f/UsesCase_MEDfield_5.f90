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
!*   Field use case 5 : read a field with following with computing steps
!*

program UsesCase_MEDfield_5

  implicit none
  include 'med.hf90'

  integer cret
  integer*8 fid

  character(64) :: mname
  ! field name
  character(64) :: finame  = 'TEMPERATURE_FIELD'
  ! nvalues, local mesh, field type
  integer nstep, nvals, lcmesh, fitype
  integer ncompo
  !geotype
  integer geotp
  integer, dimension(MED_N_CELL_FIXED_GEO) :: geotps
  ! mesh num dt, mesh num it
  integer mnumdt, mnumit
  integer csit, numit, numdt, it
  real*8 dt
  character(16) :: dtunit
  ! component name
  character(16) :: cpname
  ! component unit
  character(16) :: cpunit
  real*8, dimension(:), allocatable :: values

  geotps = MED_GET_CELL_GEOMETRY_TYPE

  ! open MED file
  call mfiope(fid,'UsesCase_MEDfield_4.med',MED_ACC_RDONLY, cret)
  if (cret .ne. 0 ) then
     print *,'ERROR : open file'
     call efexit(-1)
  endif

  ! ... we know that the MED file has only one field with one component ,
  ! a real code working would check ...
  !
  ! if you know the field name, direct access to field informations
  call mfdfin(fid,finame,mname,lcmesh,fitype,cpname,cpunit,dtunit,nstep,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR :  Field info by name ...'
     call efexit(-1)
  endif
  print *, 'Mesh name :', mname
  print *, 'Local mesh :', lcmesh
  print *, 'Field type :', fitype
  print *, 'Component name :', cpname
  print *, 'Component unit :', cpunit
  print *, 'Dtunit :', dtunit
  print *, 'Nstep :', nstep

  ! Read field values for each computing step
  do csit=1,nstep
     call mfdcmi(fid,finame,csit,numdt,numit,dt,mnumdt,mnumit,cret)
     if (cret .ne. 0 ) then
        print *,'ERROR :  Computing step info ...'
        call efexit(-1)
     endif
     print *, 'csit :', csit
     print *, 'numdt :', numdt
     print *, 'numit :', numit
     print *, 'dt :', dt
     print *, 'mnumdt :', mnumdt
     print *, 'mnumit :', mnumit

     ! ... In our case, we suppose that the field values are only defined on cells ...

     do it=1,(MED_N_CELL_FIXED_GEO)

        geotp = geotps(it)

        call mfdnva(fid,finame,numdt,numit,MED_CELL,geotp,nvals,cret)
        if (cret .ne. 0 ) then
           print *,'ERROR : Read number of values ...'
           call efexit(-1)
        endif
        print *, 'Number of values of type :', geotp, ' :', nvals

        if (nvals .gt. 0) then
           allocate(values(nvals),STAT=cret )
           if (cret > 0) then
              print *,'Memory allocation'
              call efexit(-1)
           endif

           call mfdrvr(fid,finame,numdt,numit,MED_CELL,geotp,&
                       MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,values,cret)
           if (cret .ne. 0 ) then
              print *,'ERROR : Read fields values for cells ...'
              call efexit(-1)
           endif
           print *, 'Fields values for cells :', values

           deallocate(values)

        endif
     enddo
  enddo

  ! close file
  call mficlo(fid,cret)
  if (cret .ne. 0 ) then
     print *,'ERROR :  close file'
     call efexit(-1)
  endif

end program UsesCase_MEDfield_5

