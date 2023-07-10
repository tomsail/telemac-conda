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

module M_BORNE_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
   interface

   subroutine BORNE ( &
        SFIN        , &
        QFIN        , &
        INDIC       , &
        IBIEF       , &
        IFIN        , &
        IDEB        , &
        XFRON       , &
        X           , &
        SNODE       , &
        QNODE       , &
        Erreur        &
               )

!***********************************************************************
!   CODE MASCARET : CALCUL DES BORNES AMONT et AVAL POUR UN BIEF
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  IBIEF    !  I !    !                                              !
! !  IDEB     !  I !  A ! INDICE DE DEBUT D'UN BIEF                    !
! !  IFIN     !  I !  D ! INDICE DE FIN D'UN BIEF                      !
! !  XFRON    !  R !  D ! POSITION DU FRONT D'ONDE                     !
! !  X        ! TR !  M ! ABSCISSE DE CALCUL                           !
! !  NBARA    !  I !    !                                              !
! !  SFIN     !  R !  D ! SECTION MOUILLEE DE FIN DE BIEF              !
! !  QFIN     !  R !  D ! DEBIT DE FIN DE BIEF                         !
! !  SNODE    ! TR !  D ! SECTION MOUILLEE                             !
! !  INDIC    ! TI !    ! INDICATEUR DE BIEF                           !
! !  NBSING   !  I !    ! Nombre de singularites                       !
! !  NBSECT   !  I !    ! Nombre de sections                           !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C           ! Messages d'erreur
   use M_ERREUR_T            ! Type ERREUR_T
   use M_INDICE_I            ! Interface du sous-programme INDICE
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   integer     , dimension(:), intent(  out) :: INDIC
   integer     ,               intent(inout) :: IDEB,IFIN
   integer     ,               intent(in)    :: IBIEF
   real(DOUBLE),               intent(in)    :: XFRON
   real(DOUBLE),               intent(inout) :: SFIN,QFIN
   ! 1ere dimension IM
   real(DOUBLE), dimension(:), intent(in)    :: X
   ! 1ere dimension IM
   real(DOUBLE), dimension(:), intent(in)    :: SNODE,QNODE
   Type (ERREUR_T)           , intent(inout) :: ERREUR

   end subroutine BORNE

   end interface

end module M_BORNE_I
