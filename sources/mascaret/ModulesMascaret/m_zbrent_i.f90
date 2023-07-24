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

module M_ZBRENT_I
! *********************************************************************
! PROGICIEL : MASCARET         J.-M. HERVOUET
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
   interface

   subroutine ZBRENT ( &
        FC1          , &
        EPS          , &
        X1           , &
        X2           , &
        ITMAX        , &
        Impression   , &
        Erreur         &
                  )

!***********************************************************************
!  FONCTION  :  SOLUTION D'UNE EQUATION DONT UN ZERO UNIQUE EST ENTRE
!               LES POINTS X1 ET X2.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! !      NOM       !MODE!                   ROLE
! !________________!____!______________________________________________
! !   FC1          ! -->! FONCTION DONT ON CHERCHE LE ZERO
! !                !    ! DOIT ETRE DEFINIE EN REAL(DOUBLE)
! !                !    ! PAR AILLEURS.
! !   EPS          ! -->! PRECISION CHERCHEE.
! !   X1,X2        ! -->! ENCADREMENT DE LA SOLUTION ENTREE
! !                !<-->! X2 = SOLUTION EN SORTIE.
! !   ITMAX        ! -->! NOMBRE MAXIMUM D'ITERATIONS.
! !________________!____!______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!  FONCTION APPELEE : FC1
!
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   external      :: FC1
   real(DOUBLE),                   intent(in)    :: EPS
   real(DOUBLE),                   intent(in)    :: X1
   real(DOUBLE),                   intent(inout) :: X2
   integer     ,                   intent(in)    :: ITMAX
   logical     ,                   intent(in)    :: Impression
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine ZBRENT

   end interface

end module M_ZBRENT_I
