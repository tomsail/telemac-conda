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

MODULE M_PRECISION
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   implicit none
   integer, parameter :: DOUBLE = SELECTED_REAL_KIND(15,100)
   integer, parameter :: SIMPLE = SELECTED_REAL_KIND( 6, 37)

   CONTAINS

   subroutine afficher_double
      write(*,*) 'numero du sous type simple',SIMPLE
      write(*,*) 'numero du sous type double',DOUBLE
   end subroutine afficher_double

end module M_PRECISION
