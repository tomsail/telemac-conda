! Copyright (C) 2004-2005 JP Renaud - University of Bristol

!-------------------------------------------------------------------------------

program tel2tec

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
!tel2tec, a little program which converts serafin and volfin telemac files
! into F.E. tecplot format. Binary support is supported (and is the default)
! if your system includes the Tecplot library tecio.a. Read the makefile for
! more details.
!
! This is the main program and merely steers the application, i.e. read the
! command line options, check that the files are there to be used and that's
! about it...
!
! Note that this file is NOT clean Fortran90. It uses the iargc() and getarg()
! extensions to deal with command line arguments. It works fine with the Intel
! compiler ifort v8.x and v9.x. It works with the NAGf95 compiler too.
!-------------------------------------------------------------------------------

  use m_help
  use m_processing
  use m_output_functions

  implicit none

!-------------------------------------------------------------------------------

  integer :: i,j,length
  integer :: nargs

  character(len=1 ) :: check
  character(len=50) :: buffer
  character(len=50) :: filein
  character(len=50) :: fileout

  logical :: help, filein_done, fileout_done, does_exist
  logical :: overwrite, license
  integer :: ios

  ! Some compilers might require the next line

  !intrinsic index

  integer, external :: iargc

!-------------------------------------------------------------------------------
! Initializations

        filein_done  = .false.
        fileout_done = .false.
        verbose      = .false.
        help         = .false.
        overwrite    = .false.
        ascii        = .false. 

!-------------------------------------------------------------------------------
! Get command line arguments and check them

      nargs = iargc()

      ! Treat the no arguments problem
      if (nargs.eq.0) then
        write (*,*) 'Input error! You need to specify some command line arguments'
        write (*,*) 'Please run "tel2tec -h" for help'
        stop

      endif

! At that stage, there is at least one command line argument
! so we can loop on then no probs.

      do 10 i=1,nargs
      
       ! Get the argument and calculate its character length 
       ! Note that we use the first blank character in the string to calculate
       ! the length ...
       
       ! This mean that file names with spaces will break the code

        call getarg(i,buffer)
        length = index(buffer,' ') - 1        

       ! Get away with something that should really not happen ... 
        if (length.le.0) then
          write (*,*) 'An error has occured when reading the arguments'
          write (*,*) 'This should not happen, please contact JP Renaud'
          write (*,*) ' <j.p.renaud@bristol.ac.uk> to get that fixed ASAP.'
          stop

        endif

        ! Separate the command line options from the file names to use

        ! Command line options

        ! Note, this very simple loop will work both
        ! for options like "-v -o -a" and "-ova"

        ! If your filename start with a "-" you're doomed

        if (buffer(1:1).eq.'-') then

                if (length.eq.1) cycle ! Ignore single dashes (" - ") 

                do 20 j=2,length

                if (buffer(j:j) .eq. 'v') then
                      verbose = .true.

                elseif (buffer(j:j) .eq. 'h') then
                      help = .true.
                      
                elseif (buffer(j:j) .eq. 'o') then
                      overwrite = .true.
                      
                elseif (buffer(j:j) .eq. 'l') then
                      license = .true.

                elseif (buffer(j:j) .eq. 'a') then
                      ascii = .true.
                      
                else
                        ! So far, just ignore other options
                endif
               
20              continue

        ! File names
        else

                ! The idea here is that the name of the binary file comes before
                ! the name of the ASCII file. We use the two flags filein_done and
                ! fileout_done to decide in which character string we store the
                ! information

                ! Note that we cannot give more than two file names.

                if (.not. filein_done) then

                        filein=buffer(1:length)
                        filein_done = .true.
                        cycle

                else if (.not. fileout_done) then

                        fileout=buffer(1:length)
                        fileout_done = .true.
                        cycle
                else
                       write (*,*) 'Input error: there are too many file names!' 
                       write (*,*) 'You cannot give more than two (2) file names'
                       write (*,*) 'at the command line. Use "-" for options'
                       write (*,*) 'Please run "tel2tec -h" to find out more'
                       stop
                       
                endif

                        
        endif
        
10    continue

!-------------------------------------------------------------------------------
! Make sure we have a file name for both the input and output file

      if (.not. (filein_done.and.fileout_done) ) then

              ! Display help
              if (help) then
                  call display_help()
                  stop

              ! Display license
              elseif (license) then
                  call display_license()
                  stop

              ! Nothing specific asked so output error message    
              else
                  write (*,*) 'You need to specify two file names, one for the'
                  write (*,*) 'binary telemac file and one for the tecplot file'
                  write (*,*) 'Please run "tel2tec -h" to find out more'
                  stop
              endif
      endif
!-------------------------------------------------------------------------------
! Some text in the console window

      if (verbose) then

        write (*,*)
        write (*,*) 'tel2tec version 0.6, Copyright (C) 2004-2005 JP Renaud'
        write (*,*)
        write (*,*) 'tel2tec comes with ABSOLUTELY NO WARRANTY; for details'
        write (*,*) 'use option -c.  tel2tec is free software, and you are welcome'
        write (*,*) 'to redistribute it under certain conditions; run "tel2tec -l"' 
        write (*,*) 'for details.'
        write (*,*)
        write (*,*) 'Questions? Email JP Renaud <j.p.renaud@bristol.ac.uk>'
        write (*,*)

      endif

!-------------------------------------------------------------------------------
! Check file existence and open the files

      ! Input file
      
      length = index(filein,' ') - 1

      if (verbose) write (unit=*,fmt='(a)',advance='no')       & 
          ' Reading the input file ('//filein(1:length)//') : '

      open (10,file=filein(1:length),status='old',form='unformatted', iostat=ios)

      if (ios.ne.0) then
          if (verbose) write(*,*) 'Failed!'
              write (*,*) 'Problem opening the input file.'
              write (*,*) 'Make sure that it exists, is readable etc...'
              write (*,*) 'Sorry I have to stop.'
              stop
      else
          if (verbose) write(*,*) 'OK'
      endif

      ! Output file

      length = index(fileout,' ') - 1

      if (verbose) write (unit=*,fmt='(a)',advance='no') &
          ' Opening the output file ('//fileout(1:length)//') : '

      inquire(file=fileout(1:length),exist=does_exist)

      if (does_exist) then

            if (overwrite) then
                   if (ascii) & 
                       open(20,file=fileout(1:length),form='formatted', &
                               status='replace',iostat=ios)

            else
              if (verbose) write (*,*) 'Oops!'
              write (*,*)
              write (*,*) 'The output file that you specified already exists!'
              write (*,'(a)',advance='no') ' Are you sure you want to overwrite it ? (y/N) : '
              read (*,*) check
              if ( .not. ((check .eq. 'y') .or. (check .eq. 'Y') ) ) then
                   write (*,*) 'OK then, let''s stop here ...'
                   write (*,*) 'It was nice meeting you.'
                   stop 
              else
                 if (verbose) write (unit=*,fmt='(a)',advance='no') &
                     ' OK, let''s try... '
                 if (ascii) &
                   open (20,file=fileout(1:length),form='formatted', &
                            status='replace',iostat=ios)
              endif
            endif

      else
          if (ascii) &
            open (20,file=fileout(1:length),form='formatted', &
                     status='new',iostat=ios)
      endif

      if (ios.ne.0) then
          if (verbose) write(*,*) 'Failed!'
              write (*,*) 'Problem opening the ouput file'
              write (*,*) 'Sorry I have to stop.'
              stop
      else
          if (verbose) write(*,*) 'OK'
      endif

!-------------------------------------------------------------------------------
! Potential problem with binary output

  if ( (.not. ascii) .and. (.not.got_tecplot_bin) ) then

      write (*,*) '---'
      write (*,*) 'WARNING!'
      write (*,*) '  Your version of tel2tec has been compiled without binary support!'
      write (*,*) '  Reverting to ASCII output in '//fileout(1:length)
      write (*,*) ' '
      write (*,*) '  Use "tel2tec -a" to avoid this warning message.'
      write (*,*) '---'

  endif

!-------------------------------------------------------------------------------
! Start conversion of the file

  call convert_the_dam_file(fileout(1:length),length)

!-------------------------------------------------------------------------------
! End of conversion

      if (verbose) write (*,*) ' '
      if (verbose) write (*,*) 'Conversion now finished in ', fileout(1:length)

!-------------------------------------------------------------------------------

stop
end


