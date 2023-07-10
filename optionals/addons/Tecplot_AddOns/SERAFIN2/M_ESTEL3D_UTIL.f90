module m_estel3d_util
!-----------------------------------------------------------------------
! This file is part of ESTEL-3D
! Copyright (c) EDF 2006,2007
!-----------------------------------------------------------------------
! Function: This module provides utilities that are used by other
! subroutines in ESTEL-3D for writing messages (write_listing) to the
! screen and dealing with standard errors.
!
! This module should be the only module of ESTYEL-3D requiring the
! lng/lu common block.
!
! List of PUBLIC Subroutines :
! ----------------------------
!  error_end()        - write an error message and exit
!                       THIS IS THE ONLY PROCEDURE OF ESTEL3D
!                       CONTAINING THE STOP STATEMENT
!  check_allocation() - checks the result of a memory allocation
!  error_file_format()- format error while reading a file
!  error_file_end()   - unexpected end of a file
!  write_listing()    - writes a message to the listing
!  write_line()       - writes a line of '-' (or any other character)
!  write_blank_line() - writes a blank line
!  proc_begin()       - debug message - beginning of procedure
!  proc_end()         - debug message - end of procedure
!  i2char()           - converts an integer to a string
!  r2char()           - converts a real to a string
!
! Note about i2char and r2char:
!------------------------------
! The two utilities are very useful to convert a double precision or
! integer to a character string using some defaults formats, format_int
! and format_dp (defined further down).
! It is *very* important to know that as i2char and r2char use
! a write statement, they *cannot* be re-used inside a write statement
! as nested writes are illegal in Fortran95. You therefore often have
! to use a temporary variable. It needs to be 20 characters long for a
! double precision and 12 for an integer.
!
! Note about the use of write_listing :
!--------------------------------------
! write_listing is a procedure with a generic interface. Its arguments
! can be :
!
! . string only
! . string + another string
! . string + integer
! . string + real
! . string + integer + string
! . string + real    + string
!
! If you need another one, add the procedure name in the interface
! block and create the relevant subroutine.
!
! As we need to distinguish the languages, a string is a table
! with two entries : English and French.
! You can use the predefined strings message_1(2) and message_2(2)
! the first entry in the table is the English one, the second one the
! French message. E.g. :
!
! message_1(1) = 'The upper limit ('
! message_2(1) = ') was reached.'
! message_1(2) = 'La limite superieure ('
! message_2(2) = ') a ete atteintte.'
!
! call write_listing(message_1,count,message_2)
!
! where count is an integer number (or real ...) which will be placed
! between message_1 and message_2.
!
! ATTENTION : the error_end command does not include the write_listing
! command but you can give an error code to it. see the procedure
! description for details.
!-----------------------------------------------------------------------
  implicit none

  integer, private :: lng = 2
  integer, private :: lu = 6

  logical :: okoutput ! Used to identify when output is to be produced
                      ! both in the listing and in the output files


! The smallest and the biggest real numbers as defined by the precision
! of the machine. used for comparisions with zero or initialisations
! to something very big ...

  double precision            :: a_real
  double precision, parameter :: BIG_REAL = HUGE(a_real)
  double precision, parameter :: EPS_REAL = EPSILON(a_real)

! The global variables message_1 and message_2 correspond to the error
! message to write out in English or French respectively.
! They are used by write_listing.

  character(len=150), dimension(2) :: message_1
  character(len=150), dimension(2) :: message_2

! The following integer defines the depth of the procedure for the
! debug message. It is set to 0 at the beginning of the program
! (in HOMERE_ESTEL3D)

  integer  :: debug_depth = 0
  logical  :: debug = .TRUE.

! Default formats for integers and double precisions.
! Note that the integer form,at needs to be synchronised with the
! maximum number of digits for integers (format_int_max)
  integer, parameter          :: format_int_max = 12
  character(len=5), parameter :: format_int     = '(i12)'
  character(len=9), parameter :: format_dp      = '(es20.12)'

! Possible error codes for ESTEL-3D

  integer, parameter :: ERR_CONV      = 1 ! Error convergence
  integer, parameter :: ERR_MESH      = 2 ! mesh error
  integer, parameter :: ERR_MEM       = 3 ! out of memory
  integer, parameter :: ERR_FIL_FORM  = 4 ! bad file format
  integer, parameter :: ERR_FIL_EOF   = 5 ! end of file
  integer, parameter :: ERR_FIL_WRITE = 6 ! error while writing file
  integer, parameter :: ERR_PARAMETER = 7 ! user parameter error
  integer, parameter :: ERR_FORMAT    = 8 ! Internal format error

  integer, parameter :: MAX_ERR       = 8 ! number of error declared.

! Error messages corresponding to the above codes
  character(len=35), dimension(2,MAX_ERR) :: ERROR_MESS
  data ERROR_MESS /                       &
  'Error  : Convergence not reached   ' , &
  'Erreur : Convergence non atteinte  ' , & !ERR_CONV
  'Error  : Mesh                      ' , &
  'Erreur : Maillage                  ' , & !ERR_MESH
  'Error  : Out of memory             ' , &
  'Erreur : Plus de memoire           ' , & !ERR_MEM
  'Error  : File format               ' , &
  'Erreur : Format fichier errone     ' , & !ERR_FIL_FORM
  'Error  : Unexpected end of file    ' , &
  'Erreur : Fin de fichier inattendue ' , & !ERR_FIL_EOF
  'Error  : Error while writing file  ' , &
  'Erreur : Erreur a l''ecriture fich.' , & !ERR_FIL_WRITE
  'Error  : User Parameters           ' , &
  'Erreur : Parametres utilisateur    ' , & !ERR_PARAMETER
  'Error  : Internal format problem   ' , &
  'Erreur : Probleme de format interne'   / !ERR_FORMAT

! Interface block for the generic write_listing() procedure.
! More could be added if necessary

  interface write_listing
      module procedure write_str
      module procedure write_str_str
      module procedure write_str_int
      module procedure write_str_real
      module procedure write_str_int_str
      module procedure write_str_real_str
  end interface





 contains





subroutine error_end( code )
!-----------------------------------------------------------------------
! Description:
!
! This subroutine is called when a fatal error occured.
! The code argument can be used to indicate the status of the error.
!
! To give more detailed information about the error, use write_listing
! _before_ calling error_end.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in), optional :: code ! Optional code identifying the
                                        ! error (see top of module)
!-----------------------------------------------------------------------
! Local variables
  integer :: code_err ! Used when the optional argument "code" is
                      ! not given
!-----------------------------------------------------------------------

  ! If the "code" argument is not given, use MAX_ERR + 1 to
  ! generate an "undefined error" message

  if ( present(code) ) then
      code_err = code
  else
      code_err = MAX_ERR + 1
  endif

  call write_line('*')
  if ( code_err .le. max_err ) then
      call write_listing(ERROR_MESS(:,code))
  else
      message_1(1) = 'Undefined error'
      message_1(2) = 'Erreur non definie'
      call write_listing(message_1)
  end if
  call write_line('*')

   STOP
!  call PLANTE(0)
!-----------------------------------------------------------------------
  end subroutine error_end






subroutine check_allocation(err,str)
!-----------------------------------------------------------------------
! Description:
!
! This subroutine is called to check the status of a memory allocation.
! If an error occured, this subroutine exits estel3d, if the status is
! ok, it returns to the calling procedure.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer,          intent(in) :: err ! Status returned by allocate()
  character(len=*), intent(in) :: str ! String identifying the variable
                                      ! to allocate
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  if ( err .ne. 0 ) then

      message_1(1) = 'ERROR - ALLOCATION OF ' // str // 'FAILED'
      message_1(2) = 'ERREUR - ALLOCATION DE '// str // 'ECHOUEE'

      call write_listing(message_1)
      call error_end(ERR_MEM)

  end if
!-----------------------------------------------------------------------
end subroutine check_allocation


subroutine check_deallocation(err,str)
!-----------------------------------------------------------------------
! Description:
!
! This subroutine is called to check the status of a memory allocation.
! If an error occured, this subroutine exits estel3d, if the status is
! ok, it returns to the calling procedure.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer,          intent(in) :: err ! Status returned by allocate()
  character(len=*), intent(in) :: str ! String identifying the variable
                                      ! to allocate
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  if ( err .ne. 0 ) then

      message_1(1) = 'ERROR - DEALLOCATION OF ' // str // 'FAILED'
      message_1(2) = 'ERREUR - DEALLOCATION DE '// str // 'ECHOUEE'

      call write_listing(message_1)
      call error_end(ERR_MEM)

  end if
!-----------------------------------------------------------------------
end subroutine check_deallocation




subroutine error_file_format(lu)
!-----------------------------------------------------------------------
! Description:
!
! This subroutine is called when an unexpected character was found when
! reading in a file. The procedure will generate the error-message and
! then call error_end().
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: lu
!-----------------------------------------------------------------------
! Local declarations
  character(len=30) :: file_name
!-----------------------------------------------------------------------
! Get the name of the file from the unit.
  inquire(unit=lu,name=file_name)

  message_1(1) = 'Unexpected file format '
  message_1(2) = 'Format de fichier inattendu '

  call write_listing(message_1,file_name)
  call error_end(ERR_FIL_FORM)
!-----------------------------------------------------------------------
end subroutine error_file_format





subroutine error_file_end(lu)
!-----------------------------------------------------------------------
! Description:
!
! This subroutine is called when an unexected EOF occured. It generates
! the error message and then calls error_end().
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: lu
!-----------------------------------------------------------------------
! Local declarations
  character(len=30) :: file_name
!-----------------------------------------------------------------------
! Get the name of the file from the unit.
  INQUIRE(unit=lu,name=file_name)

  message_1(1) = 'Unexpected EOF while reading '
  message_1(2) = 'Fin du fichier inattendue '

  call write_listing(message_1,file_name)
  call error_end(ERR_FIL_EOF)
!-----------------------------------------------------------------------
end subroutine error_file_end





subroutine proc_begin(name)
!-----------------------------------------------------------------------
! Description:
!
! This subroutine is called at the beginning of a procedure if the DEBUG
! flag is set and write the name of the subroutine and a number of "-"
! depending of the DEBUG_DEPTH
!-----------------------------------------------------------------------
  implicit none

! integer :: lng,lu
!  common/info/lng,lu
!-----------------------------------------------------------------------
! Arguments
  character(len=*) , intent(in) :: name !name of the procedure called
!-----------------------------------------------------------------------
! Local declarations
  integer :: i
!-----------------------------------------------------------------------
! Increment the debugging depth
  debug_depth = debug_depth + 1

! Write the subroutine name
  if (lng.eq.1) then
          write(*,*)('-',i=1,debug_depth),'DEBUT - ' , name
  else
          write(*,*)('-',i=1,debug_depth),'BEGIN - ' , name
  endif
!-----------------------------------------------------------------------
end subroutine proc_begin





subroutine proc_end(name)
!-----------------------------------------------------------------------
! Description:
!
! Is called at the end of a procedure if the debug flag is set and
! writes the name of the procedure left preceded by a number of "-"
! depending on the debugging depth.
!-----------------------------------------------------------------------
  implicit none

! integer :: lng,lu
! common/info/lng,lu
!-----------------------------------------------------------------------
! Arguments
  character(len=*), intent(in) :: name
!-----------------------------------------------------------------------
! Local declarations
  integer :: i
!-----------------------------------------------------------------------
! Write the subroutine name
  if (lng.eq.1) then
          write(*,*)('-',i=1,debug_depth),'FIN   - ' , name
  else
          write(*,*)('-',i=1,debug_depth),'END   - ' , name
  endif
  

! Decrement the debugging depth
  debug_depth = debug_depth - 1
!-----------------------------------------------------------------------
end subroutine proc_end





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
  character(len=5)  :: theformat ! Format to use for the integer
  integer           :: n         ! Number of decimals in the integer
!-----------------------------------------------------------------------
! We look for n such that 10^{n-1} < int_in < 10^{n}
! This is done to make sure that we do not create a format "overflow"
  n = 1
  do while (int_in.ge.10**n)
      n = n + 1
  enddo

! Check on the "length" of the integer
  if (n .le. 9) then

!   Write the integer in a string with the right format
    write(unit=theformat,fmt='(''(i'',i1,'')'')') n
    write(unit=string,fmt=theformat) int_in

  else if ( (n .ge. 10) .and. (n .le. format_int_max) ) then

!   Write the integer in a string with the right format
    write(unit=theformat,fmt='(''i'',i2)') n
    write(unit=string,fmt=theformat) int_in

  else

!    It is not possible to output this integer witht he default format
    message_1(1) = 'Format error in i2char.'
    message_1(2) = 'Erreur de format dans i2char.'
    call write_listing(message_1)
    call error_end(ERR_FORMAT)

  endif

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
  double precision, intent(in) :: r_in ! The real to convert
!-----------------------------------------------------------------------
! Local declarations
  character(len=20) :: string
!-----------------------------------------------------------------------
  write(string,format_dp) r_in
  r2char = trim( string )
!-----------------------------------------------------------------------
end function r2char




!***********************************************************************
!             Generic procedure for error messages
!***********************************************************************

! Check the write_listing interface at the beginning of the file
! to see when this routines are called.

! For each of the write_str_* routines, the choice of language is done
! via the lng common variable and message_x(1) contains the English
! version and message_x(2) the French version. the "messages" are
! trimmed of trailing blanks before being printed.


subroutine write_str(str)
!-----------------------------------------------------------------------
! Description of the subroutine :
! Writes a string to the listing.
!-----------------------------------------------------------------------
  implicit none

! integer :: lng,lu
! common/info/lng,lu
!-----------------------------------------------------------------------
! Arguments
      character(len=*), dimension(2), intent(in) :: str
!-----------------------------------------------------------------------
! Print the message to the listing
  select case (lng)
     case (2)
       write(*,*) trim(str(1))
     case (1)
       write(*,*) trim(str(2))
  end select
!-----------------------------------------------------------------------
end subroutine write_str





subroutine write_str_str(str1,str2)
!-----------------------------------------------------------------------
! Description:
!       Writes a message to the listing in the form string string.
!       A blank is added between the two strings.
!-----------------------------------------------------------------------
  implicit none

! integer :: lng,lu
! common/info/lng,lu
!-----------------------------------------------------------------------
! Arguments
  character(len=*), dimension(2), intent(in) :: str1
  character(len=*),               intent(in) :: str2
!-----------------------------------------------------------------------
! Print the message to the listing
  select case (lng)
     case (2)
       write(*,*) trim(str1(1)) // ' ' // trim(str2)
     case (1)
       write(*,*) trim(str1(2)) // ' ' // trim(str2)
  end select
!-----------------------------------------------------------------------
end subroutine write_str_str





subroutine write_str_int(str, i)
!-----------------------------------------------------------------------
! Description:
!       Writes a message to the listing in the form string integer.
!       The integer is formatted according to its value. A blank
!       is also added between the string and the integer.
!-----------------------------------------------------------------------
  implicit none

! integer :: lng,lu
! common/info/lng,lu
!-----------------------------------------------------------------------
! Arguments
  character(len=*), dimension(2), intent(in) :: str
  integer,                        intent(in) :: i
!-----------------------------------------------------------------------
! Local variables
  character(len=12) :: string    ! Temporary string to write the integer
!-----------------------------------------------------------------------
! Convert the integer as a string
  string = i2char(i)

! Print the message to the listing
  select case (lng)
     case (2)
       write(*,*) trim(str(1)) // ' ' // trim(string)
     case (1)
       write(*,*) trim(str(2)) // ' ' // trim(string)
  end select
!-----------------------------------------------------------------------
end subroutine write_str_int





subroutine write_str_real(str, r)
!-----------------------------------------------------------------------
! Description:
!       Write a message in the form string real. The real r is first
!       converted to the format ES11.4. A blank is added between the 2.
!-----------------------------------------------------------------------
  implicit none

! integer :: lng,lu
! common/info/lng,lu
!-----------------------------------------------------------------------
! Arguments
  character(len=*), dimension(2), intent(in) :: str
  double precision,               intent(in) :: r
!-----------------------------------------------------------------------
! Local variables
  character(len=20) :: string
!-----------------------------------------------------------------------
! Convert the real to a string
 string = r2char(r)

! Print the message to the listing
  select case (lng)
     case (2)
       write(*,*) trim(str(1)) // ' ' // trim(string)
     case (1)
       write(*,*) trim(str(2)) // ' ' // trim(string)
  end select
!-----------------------------------------------------------------------
end subroutine write_str_real





subroutine write_str_int_str(str1, i, str2)
!-----------------------------------------------------------------------
! Description:
!
! Writes a message in the format "string1 integer string2".
! Blanks are added to separate the different elements.
!-----------------------------------------------------------------------
  implicit none

! integer :: lng,lu
! common/info/lng,lu
!-----------------------------------------------------------------------
  character(len=*), dimension(2), intent(in) :: str1, str2
  integer,                        intent(in) :: i
!-----------------------------------------------------------------------
! Local variables
  character(len=12) :: string    ! Temporary string to write the integer
!-----------------------------------------------------------------------
! Convert the integer to a string
  string = i2char(i)

! Print the message to the listing
  select case (lng)
     case (2)
       write(*,*) trim(str1(1)) // ' ' // trim(string)  &
                                 // ' ' // trim(str2(1))
     case (1)
       write(*,*) trim(str1(2)) // ' ' // trim(string)  &
                                 // ' ' // trim(str2(2))
  end select

!-----------------------------------------------------------------------
end subroutine write_str_int_str





subroutine write_str_real_str(str1, r, str2)
!-----------------------------------------------------------------------
!
! Description:
!
! Writes a message in the listing in the format "string real string".
! The real r is converted to ES11.4 format first, Blanks are added
! between the strings and the real.
!-----------------------------------------------------------------------
  implicit none

! integer :: lng,lu
! common/info/lng,lu

!-----------------------------------------------------------------------
! Arguments
  character(len=*), dimension(2), intent(in) :: str1,str2
  double precision,               intent(in) :: r
!-----------------------------------------------------------------------
! Local variables
  character(len=20) :: string
!-----------------------------------------------------------------------
! Convert the real to a string
  string = r2char(r)

! Print the message to the listing
  select case (lng)
     case (2)
       write(*,*) trim(str1(1)) // ' ' // trim(string) &
                                 // ' '// trim(str2(1))
     case (1)
       write(*,*) trim(str1(2)) // ' ' // trim(string) &
                                 // ' '// trim(str2(2))
  end select
!-----------------------------------------------------------------------
end subroutine write_str_real_str





subroutine write_line(thechar)
!-----------------------------------------------------------------------
! Description:
!      Write a line in the listing using 72 characters 'thechar'
!      Usually thechar will be a '-' but to emphasizes output, '=' or
!      even '*' could be used.
!-----------------------------------------------------------------------
    implicit none

!   integer :: lng,lu
!   common/info/lng,lu

!-----------------------------------------------------------------------
! Arguments
    character(len=1), intent(in) ::  thechar
!-----------------------------------------------------------------------
! Local variables
    character(len=12) :: theformat
!-----------------------------------------------------------------------
    ! Build a format statement
    write(unit=theformat,fmt=10) thechar
10  format('(1x,72(''',a1,'''))')

    ! Write the line in the listing
    write(unit=lu,fmt=theformat)
!-----------------------------------------------------------------------
end subroutine write_line





subroutine write_blank_line()
!-----------------------------------------------------------------------
! Description:
!      Write a blank line in the listing
!-----------------------------------------------------------------------
    implicit none

!   integer :: lng,lu
!   common/info/lng,lu
!-----------------------------------------------------------------------
    write(unit=lu,fmt=*)
!-----------------------------------------------------------------------
end subroutine write_blank_line



!-----------------------------------------------------------------------
end module m_estel3d_util
