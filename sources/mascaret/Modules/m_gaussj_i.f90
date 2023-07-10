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

module M_GAUSSJ_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE      S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  GAUSSJ( &
         ! Donnees modifiees
               A           , &
         ! Donnees non modifiees
               N           , &
               NP          , &
               B           , &
               M           , &
               MP          , &
               Erreur        &
                           )

! .....................................................................
!    SOUS-PROGRAMME APPELANT  :  CALCUL
!------------------------------------------------------------------------ 

   !=========================== Declarations =============================
   use M_PRECISION        ! Type DOUBLE
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Parameters .. 
   integer, parameter :: NMAX = 100

   !.. Arguments .. 
   integer                       , intent(in   ) :: N, NP
   integer                       , intent(in   ) :: M, MP
   real(DOUBLE), dimension(NP,NP), intent(inout) :: A
   real(DOUBLE), dimension(NP,MP), intent(inout) :: B
   type(ERREUR_T)                , intent(inout) :: Erreur

   end subroutine GAUSSJ

   end interface

end module M_GAUSSJ_I
