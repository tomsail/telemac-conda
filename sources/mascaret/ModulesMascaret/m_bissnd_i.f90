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

module M_BISSND_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
   interface

   subroutine BISSND( &
        X           , &
        A           , &
        B           , &
        C           , &
        KM          , &
        NFU         , &
        ERREUR        &
                  )

!***********************************************************************
!     CODE MASCARET : RESOLUTION DE SYSTEMES TRIDIAGONAUX
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  X        ! TR ! M  ! ENTREE: SECOND MEMBRES                       !
! !           !    !    ! SORTIE: RESULTATS                            !
! !  A,B,C    ! TR ! M  ! MATRICE TRIDIAGONALE                         !
! !  KM       !  I ! D  ! DIMENSION DU SYSTEME                         !
! !  NFU      !  I ! D  ! NB FONCTIONS SECOND MEMBRE (=1 DANS MASCARET)!
! !___________!____!____!______________________________________________!
!***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C   ! Messages d'erreur
   use M_PARAMETRE_C ! EPS5, EPS10, EPS15
   use M_ERREUR_T    ! ERREUR
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   type (ERREUR_T)           , intent(inout) :: ERREUR
   real(DOUBLE), dimension(:), intent(inout) :: X
   real(DOUBLE), dimension(:), intent(inout) :: B
   real(DOUBLE), dimension(:), intent(in)    :: A
   real(DOUBLE), dimension(:), intent(in)    :: C
   integer     ,               intent(in)    :: KM
   integer     ,               intent(in)    :: NFU

   end subroutine BISSND

   end interface

end module M_BISSND_I
