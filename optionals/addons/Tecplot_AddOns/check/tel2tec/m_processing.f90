! Copyright (C) 2004-2005 JP Renaud - University of Bristol

!-------------------------------------------------------------------------------

module m_processing

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

!-------------------------------------------------------------------------------
! This module 
!-------------------------------------------------------------------------------

  use m_output
  use m_output_util
 
  private

  integer              :: npoin, nelem           ! Number of nodes and elements
  integer              :: nbvar                  ! Number of variables
  integer, allocatable :: ikle(:,:) , ikle2(:,:) ! Connectivity tables

                                           ! (The connectivity table will be
                                           !   stored in ikle(3,nelem) and
                                           !   ikle2(nelem,3)...)

  character(len=80) :: title ! Title in the serafin file

  real, allocatable :: x(:), y(:), variable(:,:) ! X and Y coordinates
                                                 ! Values of the variables

  logical :: novar ! if true indicates no variable in the input file
  logical :: sera  ! if true indicates a serafin file (not volfin)


  ! Public variables
  logical, public :: verbose ! Verbose output required
  logical, public :: ascii   ! ASCII output required

  ! Public subroutines
  public           convert_the_dam_file ! ??

  integer :: timestep




!-------------------------------------------------------------------------------
  contains
!-------------------------------------------------------------------------------





!-------------------------------------------------------------------------------
subroutine convert_the_dam_file(fileout,length)

  implicit none
!-------------------------------------------------------------------------------
! Arguments

  ! We need fileout here because in the case of binary output, the file is
  ! actually open by the Tecplot routine.
  
  integer :: length
  character(len=length),intent(in) :: fileout
!-------------------------------------------------------------------------------
! Local variables
  integer :: nbv1, nbv2
  logical :: fin
  integer :: ios

!-------------------------------------------------------------------------------
! Read/write title and number of variables

  rewind (10)
!  read (10) title
  read (10,iostat=ios) title
  if (ios.ne.0) call read_error()
! read (10) nbv1, nbv2
  read (10,iostat=ios) nbv1, nbv2
  if (ios.ne.0) call read_error()
      
  nbvar = nbv1 + nbv2
      
      ! Create a variable called DUMMY if no variable in the file (only mesh)
      if (nbvar.lt.1) then

        if (verbose) then        
                write(*,*) ' Hey! there is no variable in your file!?!   '
                write(*,*) ' I will try and make a dummy one called DUMMY'
        endif

        novar = .true.
        nbvar = 1

      endif
      
      if (verbose) then
          write (*,*) ' Extract of the title: ', title(1:32)
          write (*,fmt=10) nbvar
10        format('Number of variables: ',i2)
          if (nbvar.gt.99) then
              write (*,*) 'Oops, you can''t have more than 99 variables!'
              write (*,*) 'Sorry, I have to crash...'
              stop
          endif
      endif 
!-------------------------------------------------------------------------------
! Read/write file header + first zone

      call intro(fileout,length,nbv1,nbv2)

      timestep = 1

!-------------------------------------------------------------------------------
! Read/write variables at each time step

  fin = .false.

  do

      if (.not.fin) then

        ! File not finished, read variables
        call var(fin)
        timestep = timestep + 1

      else
              
        ! End of file, close the file and exit the loop
        call output_end(ascii,verbose)
        exit

      endif

  enddo


!-------------------------------------------------------------------------------
  return
end subroutine convert_the_dam_file





subroutine intro(fileout,length,nbv1,nbv2)

!-------------------------------------------------------------------------------
! Author: JP Renaud
!-------------------------------------------------------------------------------
! Function: Read the header and the first set of values (first time step)
!           in the input file and output them accordingly.
!-------------------------------------------------------------------------------

  implicit none

!-------------------------------------------------------------------------------
! Arguments
  integer, intent(in) :: length
  character(len=length),intent(in) :: fileout
  integer, intent(in) :: nbv1,nbv2

!-------------------------------------------------------------------------------
! Local variables
  integer :: i, j, ivar, junk(10), junk1, junk2, ndat, idat,ios
  character(len=32), allocatable :: text(:)
  real :: time
!-------------------------------------------------------------------------------
! Read title and list of variables

  if (verbose) write (*,*) 'List of variables:'

  ! Case where there are no actual variables in the Telemac file
  ! Create one with the name DUMMY and exit

  if (novar) then
     allocate( text(1) )
     text(1)='dummy'
     if (verbose) write (*,*) '   - ',text(1)
  else
     allocate (text(nbv1+nbv2))

     if (nbv1.gt.0) then
        do ivar = 1, nbv1
            read (10,iostat=ios) text(ivar)
            if (ios.ne.0) call read_error()
            if (verbose) write (*,*) '   - ',text(ivar)
         enddo
     endif

     if (nbv2.gt.0) then
        do ivar = 1, nbv2
           read (10) text(nbv1+ivar)
           if (verbose) write (*,*) '   - ',text(nbv1+ivar)
        enddo
     endif

  endif

!-------------------------------------------------------------------------------
! Read dummy integers, nelem, npoin

      read (10,iostat=ios) junk
      if (ios.ne.0) call read_error()
      read (10,iostat=ios) nelem, npoin, junk1, junk2
      if (ios.ne.0) call read_error()

      if (verbose) then
        write (*,*) 'Number of nodes: ', npoin
        write (*,*) 'Number of elements: ',nelem 
      endif

!-------------------------------------------------------------------------------
! Read X, Y and connectivity table

      if (verbose) write (*,'(a)',advance='no') ' Reading X, Y and connectivity table: '

      allocate ( ikle(3,nelem) , ikle2(nelem,3) ) 
      allocate ( x(npoin) )
      allocate ( y(npoin) )

      read (10,iostat=ios) ikle
      if (ios.ne.0) call read_error()
      read (10,iostat=ios)          !ipobo we actually ignore the ipobo line ...
      if (ios.ne.0) call read_error()
      read (10,iostat=ios) x
      if (ios.ne.0) call read_error()
      read (10,iostat=ios) y
      if (ios.ne.0) call read_error()

      if (verbose) write (*,*) 'OK'

      ! convert ikle in ikle2 for tecplot binary output
      do i=1,nelem
          do j=1,3
              ikle2(i,j) = ikle(j,i)
          enddo
      enddo
!-------------------------------------------------------------------------------
! Find file format (serafin/volfin)

      if (novar) sera = .true.
      if (.not. novar) call testformat(nelem,npoin,sera,verbose)

      call output_init(fileout,length,ascii,sera,nbvar,text,title) 

      deallocate(text)
!-------------------------------------------------------------------------------
! Read variable values

      ndat = nelem
      if (sera) ndat = npoin
      allocate( variable (nbvar,ndat))

     if (novar) then
        if (verbose) write (*,'(a)',advance='no') ' Creating variable values: '
             time = 0.d0
             do idat=1,ndat
                variable(1,idat) = 0.d0
             enddo
     else        
        if (verbose) write (*,'(a)',advance='no') ' Reading variable values: '
        read(10,iostat=ios) time
        if (ios.ne.0) call read_error()
        do ivar=1,nbvar
                read(10,iostat=ios) variable(ivar,:)
                if (ios.ne.0) call read_error()
        enddo
     endif

     if (verbose) write (*,*) 'OK'

     call output_data(ascii,sera,nelem,npoin,nbvar,ikle2,x,y,variable, &
                      time,timestep)

     deallocate (ikle)
!-------------------------------------------------------------------------------
  return
end subroutine intro





subroutine var (fin)

!-------------------------------------------------------------------------------
! Author: JP Renaud
!-------------------------------------------------------------------------------
! Function: Read one time record in the input file. Returns fin=.true.
!           if the end of file has been reached.
!-------------------------------------------------------------------------------
  implicit none
!-------------------------------------------------------------------------------
! Arguments
  logical, intent(inout) :: fin
!-------------------------------------------------------------------------------
! Local variables
  integer :: ivar, ios
  real    :: time
!-------------------------------------------------------------------------------

  if (verbose) write (*,'(a)',advance='no') ' Reading time step: '
  read (10,iostat=ios) time

  ! No read error: there should be values to follow
  if (ios.eq.0) then
      if (verbose) write (*,*) time
      do ivar=1, nbvar
          read (10,iostat=ios) variable(ivar,:)
          if (ios.ne.0) call read_error()
      enddo
           
      call output_data(ascii,sera,nelem,npoin,nbvar,ikle2,x,y,variable, &
                       time,timestep)

  ! Positive read error, this is really a read error, crash.
  else if (ios.gt.0) then

      call read_error()

  ! Negative read error, should be the end of file, set fin to .true and exit.
  ! TODO: is that true all the time? Check Fortran90 standard.
  else

      if (verbose) write (*,*) '... end of file reached!'
      fin = .true.

  endif

!-------------------------------------------------------------------------------
return
end subroutine var






subroutine testformat (nelem, npoin, sera, verbose)

!-------------------------------------------------------------------------------
! Author: JP Renaud
!-------------------------------------------------------------------------------
! Function: Determines the format of the input file: volfin or serafin
!
! As there are more elements than nodes, we try to read nelem
! values (volfin format). If this produces a reading error the file must be 
! in serafin format. Not very pretty but it seems to work.

! Note that we do check that reading npoin values does indeed work
! before carrying on. 
!-------------------------------------------------------------------------------

  implicit none
!-------------------------------------------------------------------------------
! Arguments

  logical, intent(out) :: sera
  logical, intent(in)  :: verbose
  integer, intent(in)  :: nelem,npoin

!-------------------------------------------------------------------------------
! Local variables

  real :: time
  real, dimension(nelem) :: volfin_array
  real, dimension(npoin) :: serafin_array
  
  integer :: ios

!-------------------------------------------------------------------------------

  if (verbose) write (unit=*,fmt='(a)',advance='no') &
                       ' Detection of the file format: '

   read (10,iostat=ios) time
   if (ios.ne.0) call read_error()

   ! Try to read nelem values
   read (10,iostat=ios) volfin_array 

   if (ios.eq.0) then ! Volfin format detected

       if (verbose) write(*,*) 'Volfin'
       sera = .false.
       backspace(10)
       backspace(10)
     
   else ! Not volfin, let's try serafin

       backspace(10)
       backspace(10)

       read(10,iostat=ios) time
       if (ios.ne.0) call read_error()

       read (10,iostat=ios) serafin_array

       if (ios.eq.0) then
           if (verbose) write(*,*) 'Serafin'
           sera = .true.
           backspace(10)
           backspace(10)
     
       else

           if (verbose) write (*,*) 'Oops...'
           write (*,*) 'I can''t figure out if the input file is'// &
                       ' in serafin or volfin format...'
           write (*,*) 'I''m very sorry but I have to stop.'
           stop

       endif
   endif

!-------------------------------------------------------------------------------
return
end subroutine testformat



end module m_processing
