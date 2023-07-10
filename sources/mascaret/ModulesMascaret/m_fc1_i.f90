!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

module M_FC1_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   function FC1( &
               X & ! Maillage
                 )

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Argument ..
   !--------------
   real(DOUBLE)                                  :: FC1
   real(DOUBLE)                  , intent(in   ) :: X

   !.. Communs ..
   !-------------
   real(DOUBLE) :: CGAUC,CDROI
   common /ICOEC1/ CGAUC,CDROI

   end function FC1

   end interface

end module M_FC1_I
