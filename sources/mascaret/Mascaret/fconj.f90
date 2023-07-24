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

function FCONJ( &
           TIRANT ) ! Tirant d'eau

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : FONCTION UTILISEE POUR LE CALCUL DE LA SOLUTION
!                     ANALYTIQUE DANS LE SOUS-PROGRAMME SEUIL
!
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
   real(DOUBLE)                                  :: FCONJ
   real(DOUBLE),                   intent(in)    :: TIRANT

   !.. Communs ..
   !-------------
   integer      :: ALPHA
   real(DOUBLE) :: A1,A2,A3
   common /COEFS/ ALPHA
   common /COEFH/ A1,A2,A3

   !.. Variables locales ..
   !-----------------------
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   FCONJ = A1 * TIRANT**( 2._DOUBLE * ALPHA + 1._DOUBLE ) +  A2 * TIRANT**ALPHA +  A3

   !------------------
   ! Fin du traitement
   !------------------

  return

end function FCONJ
