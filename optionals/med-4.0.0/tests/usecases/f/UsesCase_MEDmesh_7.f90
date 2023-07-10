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
!*  Use case 7 : read a 2D unstructured mesh with nodes coordinates modifications
!*

program UsesCase_MEDmesh_7

  implicit none
  include 'med.hf90'

  integer cret
  integer*8 fid

  ! mesh name
  character(MED_NAME_SIZE)  :: mname = "2D unstructured mesh"
  ! mesh description
  character(MED_COMMENT_SIZE) :: mdesc
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
  character(MED_SNAME_SIZE)  :: dtunit =""
  ! coordinates
  real*8, dimension(:), allocatable :: coords
  integer nnodes
  integer, dimension(:), allocatable :: tricon
  integer ntria3
  integer, dimension(:), allocatable :: quacon
  integer nquad4

  ! coordinate changement, geometry transformation
  integer coocha, geotra

  integer it

  ! profil size
  integer profsz
  ! profil name
   character(MED_NAME_SIZE) :: profna = ""

  integer numdt, numit
  real*8 dt

  ! open MED file with READ ONLY access mode
  call mfiope(fid, "UsesCase_MEDmesh_6.med", MED_ACC_RDONLY, cret)
  if (cret .ne. 0 ) then
     print *, "ERROR : open file"
     call efexit(-1)
  endif

  ! ... we know that the MED file has only one mesh, 
  ! a real code working would check ... 

  ! read mesh informations
  allocate ( aname(2), aunit(2) ,STAT=cret )
  if (cret > 0) then
     print *, "ERROR : memory allocation"
     call efexit(-1)
  endif

  call  mmhmin(fid, mname, sdim, mdim, mtype, mdesc, dtunit, stype, nstep, atype, aname, aunit, cret)
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

  ! we know that we only have MED_TRIA3 and MED_QUAD4 in the mesh
  ! a real code working would check all MED geometry cell types

  ! read how many triangular cells in the mesh
  call mmhnme(fid, mname, MED_NO_DT, MED_NO_IT, MED_CELL, MED_TRIA3, MED_CONNECTIVITY, &
              MED_NODAL, coocha, geotra, ntria3, cret)
  if (cret .ne. 0 ) then
     print *, "ERROR : read how many nodes in the mesh"
     call efexit(-1)
  endif
  print *,"number of triangular cells in the mesh =", ntria3

  ! read how many quadrangular cells in the mesh
  call mmhnme(fid, mname, MED_NO_DT, MED_NO_IT, MED_CELL, MED_QUAD4, MED_CONNECTIVITY, &
              MED_NODAL, coocha, geotra, nquad4, cret)
  if (cret .ne. 0 ) then
     print *, "ERROR : read how many nodes in the mesh"
     call efexit(-1)
  endif
  print *,"number of quadrangular cells in the mesh =", nquad4

  ! read mesh nodes coordinates in the initial mesh
  allocate (coords(nnodes*2),STAT=cret)
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

  ! read cells connectivity in the mesh
  allocate ( tricon(ntria3 * 3) ,STAT=cret )
  if (cret > 0) then
     print *,"ERROR : memory allocation"
     call efexit(-1)
  endif

  call mmhcyr(fid, mname, MED_NO_DT, MED_NO_IT, MED_CELL, MED_TRIA3, &
              MED_NODAL,MED_FULL_INTERLACE,tricon,cret)
  if (cret .ne. 0 ) then
     print *,"ERROR : MED_TRIA3 connectivity"
     call efexit(-1)
  endif
  print *,"MED_TRIA3 connectivity =", tricon
  deallocate(tricon)

  allocate ( quacon(nquad4*4) ,STAT=cret )
  if (cret > 0) then
     print *,"ERROR : memory allocation"
     call efexit(-1)
  endif

  call mmhcyr(fid, mname, MED_NO_DT, MED_NO_IT, MED_CELL, MED_QUAD4, &
              MED_NODAL, MED_FULL_INTERLACE, quacon, cret)
  if (cret .ne. 0 ) then
     print *,"ERROR : MED_QUAD4 connectivity"
     call efexit(-1)
  endif
  print *,"MED_QUAD4 connectivity =", quacon
  deallocate(quacon)

  ! we know that the family number of nodes and elements is 0, a real working would check ...

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
     print *, "profna = ", profna
     print *, "coocha =", coocha

     ! if coordinates have changed, then read the new coordinates
     if (coocha == 1) then

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

     end if

  end do

  ! close file
  call mficlo(fid,cret)
  if (cret .ne. 0 ) then
     print *,"ERROR :  close file"
     call efexit(-1)
  endif

end program UsesCase_MEDmesh_7


