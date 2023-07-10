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
!           binary support for Tecplot output is _NOT_ included
!
!           b) a set of "blank" Tecplot subroutines which have the same
!           interface as the actual subroutines in the Tecplot library
!           They allow the linking stage to be done with no modification
!           to the main output modules.
!-----------------------------------------------------------------------

  ! Binary Tecplot support _NOT_ included
  logical, parameter, PUBLIC  :: got_tecplot_bin = .false.





  contains





!-----------------------------------------------------------------------
integer function TECINI100 ( Title,      &
                             Variables,  &
                             FName,      &
                             ScratchDir, &
                             Debug,      &
                             VIsDouble )

  implicit none

  character(len=*) :: Title
  character(len=*) :: Variables
  character(len=*) :: FName
  character(len=*) :: ScratchDir
  integer          :: Debug
  integer          :: VIsDouble

  ! Return zero
  TECINI100 = 0

end function TECINI100
!-----------------------------------------------------------------------





!-----------------------------------------------------------------------
integer function TECZNE100( ZoneTitle,          &
                            ZoneType,           &
                            IMxOrNumPts,        &
                            JMxOrNumElements,   &
                            KMx,                &
                            ICellMax,           &
                            JCellMax,           &
                            KCellMax,           &
                            IsBlock,            &
                            NumFaceConnections, &
                            FaceNeighborMode,   &
                            ValueLocation,      &
                            SharevarFromZone,   &
                            ShareConnectivityFromZone )

  implicit none

  character(len=*)      :: ZoneTitle
  integer               :: ZoneType
  integer               :: IMxOrNumPts
  integer               :: JMxOrNumElements
  integer               :: KMx
  integer               :: ICellMax
  integer               :: JCellMax
  integer               :: KCellMax
  integer               :: IsBlock
  integer               :: NumFaceConnections
  integer               :: FaceNeighborMode
  integer, dimension(*) :: ValueLocation
  integer, dimension(*) :: ShareVarFromZone
  integer               :: ShareConnectivityFromZone

  ! Return zero
  TECZNE100 = 0

end function TECZNE100
!-----------------------------------------------------------------------





!-----------------------------------------------------------------------
integer function TECDAT100 (N, TheData, isDouble)

  implicit none

  integer :: N, isDouble
  real, dimension(*) :: TheData 

  ! Return zero
  TECDAT100 = 0

end function TECDAT100
!-----------------------------------------------------------------------





!-----------------------------------------------------------------------
integer function TECZAUXSTR100 (TheName, TheValue)

  implicit none

  character(len=*) :: TheName
  character(len=*) :: TheValue

  ! Return zero
  TECZAUXSTR100 = 0

end function TECZAUXSTR100
!-----------------------------------------------------------------------





!-----------------------------------------------------------------------
integer function TECFIL100 (F)

  implicit none

  integer :: F

  ! Return zero
  TECFIL100 = 0

end function TECFIL100
!-----------------------------------------------------------------------





!-----------------------------------------------------------------------
integer function TECNOD100 (NData)

  implicit none

  integer, dimension(*)  :: NData

  ! Return zero
  TECNOD100 = 0

end function TECNOD100
!-----------------------------------------------------------------------





!-----------------------------------------------------------------------
integer function TECEND ( )

  implicit none


  ! Return zero
  TECEND = 0

end function TECEND
!-----------------------------------------------------------------------





!-----------------------------------------------------------------------
end module m_output_functions
