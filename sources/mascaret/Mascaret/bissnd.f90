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

subroutine BISSND( &
                   X , &
                   A , &
                   B , &
                   C , &
                  KM , &
                 NFU , &
              ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!
!                 RESOLUTION DE SYSTEMES TRIDIAGONAUX
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
   use M_MESSAGE_C         ! Messages d'erreur
   use M_PARAMETRE_C       ! EPS5, EPS10, EPS15
   use M_ERREUR_T          ! ERREUR
   use M_TRAITER_ERREUR_I  ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   type (ERREUR_T)             , intent(inout) :: ERREUR
   real(DOUBLE) , dimension(:) , intent(inout) :: X
   real(DOUBLE) , dimension(:) , intent(inout) :: B
   real(DOUBLE) , dimension(:) , intent(in)    :: A
   real(DOUBLE) , dimension(:) , intent(in)    :: C
   integer      ,                intent(in)    :: KM
   integer      ,                intent(in)    :: NFU

   !.. Variables locales ..
   !-----------------------
   integer        :: KM1,K
   logical        :: premier_tour
   real(DOUBLE)   :: DET
   !character(132) :: arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================
   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>BISSND'
   KM1 = KM - 1

   ! CONTROLE DE COHERENCE DES DONNEES
   ! Controle des erreurs
   if( KM1 < 1 ) then
      Erreur%Numero = 101
      Erreur%ft     = err_101
      Erreur%ft_c   = err_101c
      call TRAITER_ERREUR( Erreur , KM1 )
      return
   endif

   if( NFU /= 1 ) then
      Erreur%Numero = 102
      Erreur%ft     = err_102
      Erreur%ft_c   = err_102c
      call TRAITER_ERREUR( Erreur , NFU )
      return
   endif

   ! DESCENTE DU SYSTEME
   do K = 2 , KM1
      B(K) = B(K) - ( A(K) * C(K - 1) ) / B(K - 1)
      X(K) = X(K) - ( A(K) * X(K - 1) ) / B(K - 1)
   end do

   DET = A(KM) * C(KM1) - B(KM1) * B(KM)

   if( dabs( DET ) < EPS5 ) then
      Erreur%Numero = 103
      Erreur%ft     = err_103
      Erreur%ft_c   = err_103c
      call TRAITER_ERREUR( Erreur , DET )
      return
   endif

   if( dabs( X(KM) ) > EPS15 ) then
      X(KM) = ( A(KM) * X(KM1) - B(KM1) * X(KM) ) / DET
   end if

   K = KM

   ! REMONTEE DU SYSTEME
   premier_tour = .true.

   do while( K > 1 .or. premier_tour )
      premier_tour = .false.
      K            = K - 1
      X(K)       = ( X(K) - C(K) * X(K + 1) ) / B(K)
   end do

   do K = 1 , KM
      if( dabs( X(K) ) < EPS10 ) X(K) = 0._DOUBLE
   end do

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine BISSND
