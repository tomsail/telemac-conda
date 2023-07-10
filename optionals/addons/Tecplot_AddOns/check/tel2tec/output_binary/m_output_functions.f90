! Copyright (C) 2004-2005 JP Renaud - University of Bristol

!-------------------------------------------------------------------------------

module m_output_functions

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

!-----------------------------------------------------------------------
! Author: JP Renaud
!
! Description:
!
!       This module contains:
!
!           a) the logical "got_tecplot_bin" which indicates that
!           binary support for Tecplot _IS_ included.
!
!           b) the declaration of the Tecplot subroutines as external
!-----------------------------------------------------------------------

  ! Binary Tecplot support included
  logical, parameter, PUBLIC  :: got_tecplot_bin = .true.

  ! Declaration of the subroutines from the Tecplot library
  integer, external :: TECINI100
  integer, external :: TECZNE100 
  integer, external :: TECDAT100 
  integer, external :: TECZAUXSTR100
  integer, external :: TECFIL100
  integer, external :: TECNOD100
  integer, external :: TECEND

!-----------------------------------------------------------------------

end module m_output_functions
