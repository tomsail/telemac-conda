! Copyright (C) 2004-2005 JP Renaud - University of Bristol

!-------------------------------------------------------------------------------

module m_output_util

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





  contains





subroutine check_call(ios)

!-----------------------------------------------------------------------
! Author: JP Renaud
!-----------------------------------------------------------------------
! Function: Checks the return variable of a tecplot call
!           and stops execution if an error occured.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Global declarations
!-----------------------------------------------------------------------
! Local declarations
  integer, intent(in) ::ios 
!-----------------------------------------------------------------------

  if(ios.ne.0) then
      write (*,*) 'Problem with a Tecplot binary routine'
      write (*,*) 'Try the ASCII option "-a" maybe?'
      write (*,*) 'I''m very sorry but I have to stop'
      stop
  end if

!-----------------------------------------------------------------------
  return
end subroutine check_call




subroutine read_error()

!-----------------------------------------------------------------------
! Author: JP Renaud
!-----------------------------------------------------------------------
! Function: Write a nice little message after read errors and stops.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Global declarations
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  write (*,*) 'Oops...'
  write (*,*) 'Error when reading the input file'
  write (*,*) 'I''m very sorry but I have to stop'
  write (*,*) 'Contact JP Renaud if you are 100% sure your file is right'
  stop

!-----------------------------------------------------------------------
  return
end subroutine read_error




character(len=12) function i2char(int_in) 
!-----------------------------------------------------------------------      
! Description:
!
! Converts an integer into a string. Maximum of 12 digits.
! The integer is formatted properly so that the returned string can
! then be trimmed ( trim(i2char() )
!-----------------------------------------------------------------------      
  implicit none 
!-----------------------------------------------------------------------      
! Arguments
  integer, intent(in) :: int_in ! The integer to convert
!-----------------------------------------------------------------------      
! Local variables
  character(len=12) :: string    ! Temporary string
  character(len=4)  :: theformat ! Format to use for the integer
  integer           :: n         ! Number of decimals in the integer
!-----------------------------------------------------------------------      
  ! We look for n such that 10^{n-1} < n < 10^{n}
  n = 1
  do while (int_in.ge.10**n)
      n = n + 1
  enddo

  ! Create a format for the integer
  select case (n)
      case (1)
        theformat='(i1)'
      case (2)
        theformat='(i2)'
      case (3)
        theformat='(i3)'
      case (4)
        theformat='(i4)'
      case (5)
        theformat='(i5)'
      case (6)
        theformat='(i6)'
      case (7)
        theformat='(i7)'
      case (8)
        theformat='(i8)'
      case (9)
        theformat='(i9)'
      case (10)
        theformat='(i10)'
      case (11)
        theformat='(i11)'
      case (12)
        theformat='(i12)'
      ! [...] Add some more if you care...
      case default
        theformat= '(i)'
  end select

  ! Write the integer in a string with the right format
  write(unit=string,fmt=trim(theformat)) int_in

  ! Trim the string and return
  i2char = trim(string)
!-----------------------------------------------------------------------      
end function i2char





character(len=20) function r2char(r_in) 
!-----------------------------------------------------------------------      
! Description:
!
! Converts a double precision into a string. Maximum of 11 digits.
! The real is formatted properly so that the returned string can
! then be trimmed ( trim(r2char() )
!-----------------------------------------------------------------------      
  implicit none 
!-----------------------------------------------------------------------      
! Arguments
  real, intent(in) :: r_in ! The real to convert
!-----------------------------------------------------------------------      
! Local declarations
  character(len=20) :: string
!-----------------------------------------------------------------------      
  write(string,'(ES20.12)') r_in
  r2char = string
!-----------------------------------------------------------------------      
end function r2char





end module m_output_util
