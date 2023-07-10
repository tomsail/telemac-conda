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
!*  Use case 8 : read a 2D unstructured mesh with nodes coordinates modifications
!*  (generic approach)
!*

program UsesCase_MEDmesh_8

  implicit none
  include 'med.hf90'

  integer cret
  integer*8 fid

  ! mesh number
  integer nmesh
  ! mesh name
  character(MED_NAME_SIZE)  :: mname = ""
  ! mesh description
  character(MED_COMMENT_SIZE) :: mdesc = ""
  ! mesh dimension, space dimension
  integer mdim, sdim
  ! mesh sorting type
  integer stype
  integer nstep
  ! mesh type, axis type
  integer mtype, atype
  ! axis name, axis unit
  character(MED_SNAME_SIZE), dimension(:), allocatable :: aname
  character(MED_SNAME_SIZE), dimension(:), allocatable :: aunit
  character(MED_SNAME_SIZE)  :: dtunit = ""
  ! coordinates
  real*8, dimension(:), allocatable :: coords
  integer ngeo
  integer nnodes
  ! connectivity
  integer , dimension(:), allocatable :: conity

  ! coordinate changement, geometry transformation
  integer coocha, geotra

  integer i, it, j

  ! profil size
  integer profsz
  ! profil name
   character(MED_NAME_SIZE) :: profna = ""

  integer numdt, numit
  real*8 dt

  ! geometry type
  integer geotyp
  integer, dimension(MED_N_CELL_FIXED_GEO) :: geotps 

  ! print *, "MED_N_CELL_FIXED_GEO :", MED_N_CELL_FIXED_GEO
  ! print *, "MED_GET_CELL_GEOMETRY_TYPE :", MED_GET_CELL_GEOMETRY_TYPE

  geotps = MED_GET_CELL_GEOMETRY_TYPE
  ! do it=1, MED_N_CELL_FIXED_GEO
  !   print *, it, " : ", MED_GET_CELL_GEOMETRY_TYPE(it)
  !   geotps(it) = MED_GET_CELL_GEOMETRY_TYPE(it)
  !   print *, "geotps(",it,") =",geotps(it)
  !end do

  ! open MED file with READ ONLY access mode
  call mfiope(fid, "UsesCase_MEDmesh_6.med", MED_ACC_RDONLY, cret)
  if (cret .ne. 0 ) then
     print *, "ERROR : open file"
     call efexit(-1)
  endif

  ! read how many mesh in the file 
  call mmhnmh(fid, nmesh, cret)
  if (cret .ne. 0 ) then
     print *, "ERROR : read how many mesh"
     call efexit(-1)
  endif

  print *, "nmesh :", nmesh

  do i=1, nmesh

     ! read computation space dimension
     call mmhnax(fid, i, sdim, cret)
     if (cret .ne. 0 ) then
        print *, "ERROR : read computation space dimension"
        call efexit(-1)
     endif

     ! memory allocation
     allocate ( aname(sdim), aunit(sdim) ,STAT=cret )
     if (cret > 0) then
        print *, "ERROR : memory allocation"
        call efexit(-1)
     endif

     ! read mesh informations
     call  mmhmii(fid, i, mname, sdim, mdim, mtype, mdesc, dtunit, stype, nstep, &
                  atype, aname, aunit, cret)
     if (cret .ne. 0 ) then
        print *, "ERROR : read mesh informations"
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
     print *,"coordinates axis name =", aname
     print *,"coordinates axis units =", aunit
     deallocate(aname, aunit)

     ! read how many nodes in the mesh **
     call mmhnme(fid, mname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_NO_GEOTYPE, &
                 MED_COORDINATE, MED_NO_CMODE, coocha, geotra, nnodes, cret)
     if (cret .ne. 0 ) then
        print *, "ERROR : read how many nodes in the mesh"
        call efexit(-1)
     endif
     print *, "number of nodes in the mesh =", nnodes

     ! read mesh nodes coordinates
     allocate (coords(nnodes*sdim),STAT=cret)
     if (cret > 0) then
        print *,"ERROR : memory allocation"
        call efexit(-1)
     endif

     call mmhcor(fid, mname, MED_NO_DT, MED_NO_IT, MED_FULL_INTERLACE, coords, cret)
     if (cret .ne. 0 ) then
        print *,"ERROR : nodes coordinates"
        call efexit(-1)
     endif
     print *,"Nodes coordinates =", coords
     deallocate(coords)

     ! read all MED geometry cell types
     do it=1, MED_N_CELL_FIXED_GEO

        geotyp = geotps(it)

        print *, "geotps(it) :", geotps(it)

        call mmhnme(fid, mname, MED_NO_DT, MED_NO_IT, MED_CELL, geotyp, &
                                 MED_CONNECTIVITY, MED_NODAL, coocha,   &
                                 geotra, ngeo, cret)
        if (cret .ne. 0 ) then
           print *,"ERROR : number of cells"
           call efexit(-1)
        endif
        print *,"Number of cells =", ngeo

        ! print *, "mod(ngeo, 100) : ", mod(geotyp,100)
 
        if (ngeo .ne. 0) then
           allocate (conity(ngeo*mod(geotyp,100)), STAT=cret)
           if (cret > 0) then
              print *,"ERROR : memory allocation"
              call efexit(-1)
           endif

           call mmhcyr(fid, mname, MED_NO_DT, MED_NO_IT, MED_CELL, &
                       geotyp, MED_NODAL, MED_FULL_INTERLACE,      &
                       conity, cret)
           if (cret > 0) then
              print *,"ERROR : cellconnectivity", conity
              call efexit(-1)
           endif
           deallocate(conity)

        endif !ngeo .ne. 0
     end do ! read all MED geometry cell types

  ! read nodes coordinates changements step by step
  do it=1, nstep-1

     call mmhcsi(fid, mname, it+1, numdt, numit, dt, cret)
     if (cret .ne. 0 ) then
        print *,"ERROR : computing step info"
        call efexit(-1)
     endif
     print *,"numdt =", numdt
     print *,"numit =", numit
     print *,"dt =", dt

     ! test for nodes coordinates change
     call mmhnep(fid, mname, numdt, numit, MED_NODE, MED_NO_GEOTYPE, &
                 MED_COORDINATE, MED_NO_CMODE, MED_GLOBAL_STMODE, &
                 profna, profsz, coocha, geotra, nnodes, cret)
     if (cret .ne. 0 ) then
        print *,"ERROR : nodes coordinates"
        call efexit(-1)
     endif
     print *, "profna =", profna
     print *, "coocha =", coocha
     print *, "geotra =", geotra

     ! if only coordinates have changed, then read the new coordinates
     ! to verify if there is a matrix transformation => UsesCase_MEDmesh12 
     if (coocha == 1 .and. geotra == 1) then

        allocate (coords(nnodes*2),STAT=cret)
        if (cret > 0) then
           print *,"ERROR : memory allocation"
           call efexit(-1)
        endif

        call mmhcpr(fid, mname, numdt, numit,MED_GLOBAL_STMODE,profna,  &
                    MED_FULL_INTERLACE,MED_ALL_CONSTITUENT, coords, cret)
        if (cret .ne. 0 ) then
           print *,"ERROR : nodes coordinates"
           call efexit(-1)
        endif
        print *,"Nodes coordinates =", coords
        deallocate(coords)

     end if ! coocha == 1

  end do ! it=1, nstep-1

end do ! i=0, nmesh-1

  ! close file
  call mficlo(fid,cret)
  if (cret .ne. 0 ) then
     print *,"ERROR :  close file"
     call efexit(-1)
  endif

end program UsesCase_MEDmesh_8


