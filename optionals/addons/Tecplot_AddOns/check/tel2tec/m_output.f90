! Copyright (C) 2004-2005 JP Renaud - University of Bristol

!-------------------------------------------------------------------------------

module m_output

!-------------------------------------------------------------------------------

! This program is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by the
! Free Software Foundation; either version 2 of the License, or any later
! version.

! This program is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
! or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
! more details.

! You should have received a copy of the GNU General Public License along
! with this program, in the text file 'gpl.txt'; if not, write to:
! Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
! MA 02111-1307 USA
! or get it from http://www.gnu.org/licenses/gpl.html

!-------------------------------------------------------------------------------
! Important note: when the Tecplot binary library tecio.a exists on your
! system, this software will use this library to produce binary files. The
! tecio.a library belongs to Tecplot Inc and is in no way covered by this
! license.
!-------------------------------------------------------------------------------

  use m_output_util
  use m_output_functions

  ! Public subroutines :
  public :: output_init 
  public :: output_data
  public :: output_end 

  ! Start the list of private variables
  private

  ! Set of flags used by the binary outplut Tecplot routines
  integer            :: dbg    = 0  ! debugging comments (1=yes, 0=no) 
  integer, parameter :: eps    = 0  ! precision for output: 0=single
  integer, parameter :: eltype = 2  ! type of element: 3=triangle
  integer, parameter :: sto    = 1  ! type of data storage: 1=block

  ! Flag used to detect the first output as x, y, and the
  ! connectivity table are not printed at each time step
  logical :: first_print

  ! Total number of variables to print.
  integer, parameter :: NbVarOut = 99

  ! The location of the variables to print (cell centered or
  ! node centered)
  integer, dimension(NbVarOut),target :: VarLocation

  ! The list of variables to share
  integer, dimension(NbVarOut), target :: ShareVarFromZone

!-----------------------------------------------------------------------





contains





subroutine output_init(fileout,length,ascii,sera,nvar,text,title)

!-----------------------------------------------------------------------
! Author:  JP Renaud
!-----------------------------------------------------------------------
! Function: This subroutine initializes the process of writing output
!           to the tecplot file.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------     
! Global declarations
  integer, intent(in) :: length
  character(len=length),intent(in) :: fileout
  logical, intent(in) :: ascii,sera
  integer, intent(in) :: nvar
  character(len=32), intent(in) :: text(nvar)
  character(len=80), intent(in) :: title
!-----------------------------------------------------------------------     
! Local declarations
!-----------------------------------------------------------------------     
! Set first_print flag
  first_print = .true.

! Call the relevant subroutine
  if (ascii) then
      call output_init_ascii(nvar,text,title)
  else
      call output_init_binary(fileout,length,nvar,text,title,sera)
  endif

!-----------------------------------------------------------------------
  return
end subroutine output_init





subroutine output_data(ascii,sera,nelem,npoin,nvar,ikle,x,y,variables, &
                       at,lt)

!-----------------------------------------------------------------------
! Author:  JP Renaud
!-----------------------------------------------------------------------
! Function: This subroutine write one time step to the tecplot file
!           The connectivity table is also written for the first printout.
!-----------------------------------------------------------------------

  implicit none


!-----------------------------------------------------------------------     
! Global declarations
  logical, intent(in) :: ascii,sera
  integer, intent(in) :: nelem,npoin,nvar
  integer, intent(in) :: ikle(nelem,3)

  real, intent(in) :: at, x(npoin), y(npoin)
  integer, intent(in) :: lt ! timestep no
  real, intent(in) :: variables(nvar,npoin)
!-----------------------------------------------------------------------     
! Local declarations
!-----------------------------------------------------------------------     
! Call the relevant subroutine
  if (ascii) then
      call output_data_ascii(sera,nelem,npoin,nvar,x,y,variables,at,lt)
      if (first_print) call output_connect_ascii(nelem,ikle)
  else
      call output_data_binary(sera,nelem,npoin,nvar,x,y,variables,at,lt)
      if (first_print) call output_connect_binary(nelem,ikle)
  endif
!-----------------------------------------------------------------------     
  return
end subroutine output_data






subroutine output_end(ascii,verbose)

!-----------------------------------------------------------------------
! Author:  JP Renaud
!-----------------------------------------------------------------------
! Function: This subroutine closes the tecplot output file.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------     
! Global declarations
  logical, intent(in) :: ascii,verbose
!-----------------------------------------------------------------------     
! Local declarations
!-----------------------------------------------------------------------     
! Call the relevant subroutine

  if (verbose) write (*,'(a)',advance='no') ' Restitution of the file: '
  if (ascii) then
      call output_end_ascii()
  else
      call output_end_binary()
  endif
  if (verbose) write (*,*) 'OK'
!-----------------------------------------------------------------------     
  return
end subroutine output_end




subroutine output_init_ascii(nvar,text,title)

!-----------------------------------------------------------------------
! Author: JP Renaud
!-----------------------------------------------------------------------
! Function: Writes the header in the tecplot ascii output file.
!
!           X, Y, Z and IMAT are always included. The next variables
!           are defined by the user. Therefore their content is dynamic.
!
!           [ IMAT is the soil type and is included in the varsor block so
!           that the variable name can be changed via nomvar_estel3d. it
!           however always treated separatidly from other variables in
!           varsor ]
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Global declarations
  integer, intent(in)           :: nvar
  character(len=32), intent(in) :: text(nvar)
  character(len=80), intent(in) :: title

!-----------------------------------------------------------------------
! Local declarations
  integer :: ivar
!-----------------------------------------------------------------------
! Set first_print flag
  first_print = .true.

! Header
  write(20,*)'TITLE = "',TITLE(1:62),'"'
  write(20,*)'VARIABLES = "X",'
  write(20,*)'"Y",'

  ! Print other variables names
  do ivar = 1, nvar-1

              ! Write name followed by a comma as it is not the last
              write(20,fmt=10) TEXT(ivar)(1:16)
10            format(1x,'"',a16,'",')

  enddo

  ! Write last name (no comma) and exit the loop 

   write(20,fmt=11) TEXT(nvar)(1:16)
11 format(1x,'"',a16,'"')

!-----------------------------------------------------------------------
  return
end subroutine output_init_ascii





subroutine output_data_ascii(sera,nelem,npoin,nvar,x,y,variable, at,lt)

!-----------------------------------------------------------------------
! Author: JP Renaud
!-----------------------------------------------------------------------
! Function: Writes one zone in the tecplot ascii file
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Global declarations
  logical, intent(in) :: sera
  integer, intent(in) :: nelem,npoin,nvar
  real, intent(in)    :: at, x(npoin), y(npoin)
  integer, intent(in) :: lt
  real, intent(in)    :: variable(nvar,*)
!-----------------------------------------------------------------------
! Local declarations
  integer           :: ivar, idat, ndat
  character(len=11) :: ZoneName

!-----------------------------------------------------------------------

! Build the ZoneName

  write(unit=ZoneName,fmt=60) at
60 format('T= ',f7.2)

! Zone header  

  write(20,*)'ZONE T="',ZoneName,'"',   & ! Zone name
             ', DATAPACKING = BLOCK',   & ! Type of data storage
             ', ZONETYPE = FETRIANGLE'    ! Type of element


  write (20,*) ', N =',NPOIN ,                        &
               ', E =',NELEM ,                        &
               ', AUXDATA Common.Time=" '   ,r2char(AT),' " ',&
               ', AUXDATA Timestep=" '   ,i2char(LT),' " '

  ! Variables to share between zones (x, y, z and imat)
  ! Connectivity table is shared between zones

  if (.not. first_print) then
      write (20,*) ', VARSHARELIST = ( [1-2]=1 ) '
      write (20,*) ',CONNECTIVITYSHAREZONE = 1'
  endif

  ! Precision for the variables, all in double precision but needs to be
  ! adjusted to the actual number of variables.

  ! Print X and Y fort the first zone

  if (first_print) then
      write(20,*) x
      write(20,*) y
  endif

  ! Other variables

  ndat = nelem
  if (sera) ndat = npoin

  do ivar = 1, nvar
      write(20,*) (variable(ivar,idat),idat=1,ndat)
  enddo

!-----------------------------------------------------------------------
  return
end subroutine output_data_ascii





!-----------------------------------------------------------------------
subroutine output_connect_ascii(nelem,ikle)
!-----------------------------------------------------------------------
! Author: JP Renaud
!-----------------------------------------------------------------------
! Function: Writes the connectivity table in the tecplot ascii file
!           but only for the first zone
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Global declarations
  integer, intent(in) :: nelem
  integer, intent(in) :: ikle(nelem,3)
!-----------------------------------------------------------------------
! Local declarations
  integer :: ielem
!-----------------------------------------------------------------------

  do ielem = 1, nelem   
      write(20,*) ikle(ielem,1), &
                  ikle(ielem,2), &
                  ikle(ielem,3)
  end do

  ! Change first_print flag to false as the first zone has
  ! been fully printed now

  first_print = .false.

!-----------------------------------------------------------------------
  return
end subroutine output_connect_ascii





!-----------------------------------------------------------------------
subroutine output_end_ascii()
!-----------------------------------------------------------------------
! Author: JP Renaud
!-----------------------------------------------------------------------
! Function:  Close the output file in the case of ascii output
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Global declarations
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------
! Close the file.

  close(20)

!-----------------------------------------------------------------------
  return
end subroutine output_end_ascii




subroutine output_init_binary(fileout,flength,nvar,text,title,sera)
!-----------------------------------------------------------------------
! Authors:  JP Renaud
!           Heavily inspired by the workd of Minh-Phuong Lam and
!           Regina Nebauer in ESTEL-3D
!-----------------------------------------------------------------------
! Function: Initialises the binary file output
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------     
! Global declarations
  integer, intent(in) :: flength
  character(len=length), intent(in) :: fileout
  integer, intent(in)           :: nvar
  character(len=32), intent(in) :: text(nvar)
  character(len=80), intent(in) :: title
  logical, intent(in)           :: sera
!-----------------------------------------------------------------------     
! Local declarations
  integer           :: ok, ivar,ValueLocation,length
  character(len=200):: variables
!-----------------------------------------------------------------------
! Location of value in each cell (1=node-centered, 0=cell-centered)

  ValueLocation = 0
  if (sera) ValueLocation = 1

!-----------------------------------------------------------------------
! Build the variables to print

  ! Start with X and Y

  write(unit=variables, fmt=30) ! x and y

  VarLocation(1) = 1 ! X is node-centered
  VarLocation(2) = 1 ! Y is node-centered

30 format('X ,Y')

  ! Add names of optional variables 

  do ivar =1, nvar
    write(unit=variables((ivar-1)*18 + 8:),fmt=31) text(ivar)(1:16)
31  format(' ,',a16)

    VarLocation(ivar+2) = ValueLocation
  enddo

!-----------------------------------------------------------------------
! Variables to share
! Only X and Y are shared therefore we initialize at zero and 
! use 1 only for X and Y

  ShareVarFromZone(:) = 0  ! init to no share for all vars
  ShareVarFromZone(1) = 1  ! init to share for x coordinate
  ShareVarFromZone(2) = 1  ! init to share for y coordinate

! Write the file header in the tecplot file

  length = index(fileout,' ') - 1

  ok = TECINI100(title//char(0),             & ! Title
                 variables//char(0),         & ! Names of variables
                 fileout(1:flength)//char(0), & ! Name of binary file
                 '.'//char(0),               & ! directory for scratch file
                 dbg,                        & ! Debugging information
                 eps )                         ! Precision

   call check_call(ok)

!----------------------------------------------------------------------- 
  return
end subroutine output_init_binary




subroutine output_end_binary()
!-----------------------------------------------------------------------
! Authors: JP Renaud
!-----------------------------------------------------------------------
! Function: Close the binary tecplot flow file.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------     
! Global declarations
!-----------------------------------------------------------------------
! Local declarations
  integer           :: ok
!-----------------------------------------------------------------------

!  Close the current data file

  ok = TECEND()  
  call check_call(ok)

!-----------------------------------------------------------------------
  return
end subroutine output_end_binary




subroutine output_connect_binary(nelem,ikle)
!-----------------------------------------------------------------------
! Author: JP Renaud
!-----------------------------------------------------------------------
! Function: Write the connectivity table in the binary file.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------     
! Global declarations
  integer, intent(in) :: nelem
  integer, intent(in) :: ikle(nelem,3)
!-----------------------------------------------------------------------
! Local declarations
  integer                        :: ielem, ok
  integer, dimension(3,nelem)    :: ndata
!-----------------------------------------------------------------------     
! Build connectivity table
! Merely a copy of the array ikle(nelem,3) into the array ndata(3,nelem)

  do ielem=1,nelem
      ndata(1,ielem) = ikle(ielem,1)
      ndata(2,ielem) = ikle(ielem,2)
      ndata(3,ielem) = ikle(ielem,3)
  end do

! Write connectivity table in the tecplot file.

  ok = TECNOD100(ndata)
  call check_call(ok)

! Change first_print flag to false as the first zone has
! been fully printed now
  first_print = .false.
      
!-----------------------------------------------------------------------
  return
end subroutine output_connect_binary




subroutine output_data_binary(sera,nelem,npoin,nvar,x,y,variable,at,lt)
!-----------------------------------------------------------------------
! Authors:  JP Renaud
!           Heavily inspired by the workd of Minh-Phuong Lam and
!           Regina Nebauer in ESTEL-3D
!-----------------------------------------------------------------------
! Function: Write one zone in the binary file.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Global declarations
  logical, intent(in) :: sera
  integer, intent(in) :: nelem,npoin,nvar
  real, intent(in)    :: at, x(npoin), y(npoin)
  integer, intent(in) :: lt
  real, intent(in)    :: variable(nvar,*)
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  integer                        :: ok
  integer                        :: ivar
  integer                        :: idat

  integer,dimension(:), pointer  :: PtrToVarShar
  integer                        :: ShareConnect

  integer, dimension(1:nvar+2), target :: varShar

  integer :: ndat  ! number of data points to write

  character(len=11) :: ZoneName

  ! Temporary real array for writing integer data (soil number) to the 
  ! tecplot file (double precision data only)

  real :: thevar(nelem)

!-----------------------------------------------------------------------

! Create a null pointer

! Build the zone name, which is the time of the simulation
! (may be we should add the unit .....)

  write(unit=ZoneName,fmt=50) AT
50 format('T= ',f7.2)

! Initialisation of the variables for the teczne call.
! If it is the first zone we write, we need to write up all data.
! Nothing is shared actually, and we should write the connectivity
! table too.
! If we already wrote up the first zone with the node coordinates, the
! connectivity and the soil id, we will share this variables and the 
! connectivity table.
! The tables are initialized by output_init_tecplot
! NOTE : we should specify the size of the arrays, because they
! are maximized.

  if (first_print) then
      ! If this is our first printout (the first time step) then
      ! we do not share any variables, nor the connectivity table.
      ! NOTE : in the tecplot example, a null pointer is used in this
      ! case. we had multiple problems with this null pointer and use 
      ! now a table initialized to 0. it seems to work nice,
      ! at least for hp-ux where we encountered these problems ...
      varShar(:)   = 0
      PtrToVarShar => varShar(1:nvar+2)
      ShareConnect = 0      ! Connectivity shared.
  else
      ! If it is not the first printout, we share variables specified in
      ! the list ShareVarFromZone and we share the connectivity too.
      PtrToVarShar => ShareVarFromZone(1:nvar+2) 
      ShareConnect = 1 
  endif

! Create the zone : write up the zone header and the format of 
! the data which will come with tecdat.

  ok = TECZNE100(ZoneName//char(0), & ! Zone name 
            ELTYPE,                 & ! Type of element: 2=fetriangle
            NPOIN,                  & ! Number of nodes
            NELEM,                  & ! Number of elements
            0,0,0,0,                & ! N/A (ordered zones)  
            STO,                    & ! Type of data storage: 1=block
            0,0,                    & ! N/A (face connections)
            VarLocation(1:nvar),    & ! Location of value in cell 
            PtrToVarShar,           & ! Do not share variables
            ShareConnect)             ! Do not share connectivity 

!  check if it was ok for tecplot.
   call check_call(ok)

! Auxiliary data for the zone : the timestep and the time 
! TODO : the unit for the time !!!!!

  ok = TECZAUXSTR100('Timestep'//char(0),trim(i2char(lt))//char(0))
  call check_call(ok)
  ok = TECZAUXSTR100('Common.Time'//char(0),trim(r2char(at))//char(0))
  call check_call(ok)

! Mandatory output for first zone
! writing the coordinates of the nodes and the soil number  

  if (first_print) then

      ok = TECDAT100(npoin, x, eps) ! x (node centered)
      call check_call(ok)
      ok = TECDAT100(npoin, y, eps) ! y (node centered)
      call check_call(ok)

  endif

          if (sera) then  ! Node centered
              ndat = npoin
          else ! Cell centered
              ndat = nelem
          endif

! Here we write the output which is not shared.

  do ivar=1, nvar

       ! TODO: should really use a pointer !!!

          do idat=1,ndat
              thevar(idat) = variable(ivar,idat)
          enddo

          ! write the data with the right dimension of the array.
          ok = TECDAT100(ndat, thevar(1:ndat), eps)
          call check_call(ok)
          
  enddo

!-----------------------------------------------------------------------
  return
end subroutine output_data_binary





end module m_output
