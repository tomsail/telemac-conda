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

function FC1( &
              X ) ! Maillage

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!-----------------------------------------------------------------------

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

   !.. Variables locales ..
   !-----------------------

   real(DOUBLE) :: CDROI2
   real(DOUBLE) :: A6, A4, A3, A2, A0
   !character(132) :: !arbredappel_old  ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   CDROI2 = CDROI**2
   A6     = 1._DOUBLE
   A4     = -9._DOUBLE * CDROI2
   A3     = 16._DOUBLE * CGAUC * CDROI2
   A2     = -1._DOUBLE * ( 8._DOUBLE * ( CGAUC**2 ) * CDROI2 + CDROI2**2 )
   A0     = CDROI2**3
   FC1    = A6 * (X**6) + A4 * (X**4) + A3 * (X**3) + A2 * (X**2) + A0

   !------------------
   ! Fin du traitement
   !------------------

   return

end function FC1
