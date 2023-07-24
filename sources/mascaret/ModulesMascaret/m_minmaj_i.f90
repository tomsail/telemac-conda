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

module M_MINMAJ_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine MINMAJ( &
            SMIN    , &
            QMIN    , &
            SMAJ    , &
            QMAJ    , &
            BETA    , &
           SNODE    , &
           QNODE    , &
           Noeud    , &
              JG    , &
              JD    , &
           SGEO1    , &
            SGEO    , &
          DEBGE1    , &
          DEBGE2    , &
          NMLARG    , &
          ERREUR    &
                    )

!***********************************************************************
!     CODE MASCARET : CALCUL DE LA REPARTITION MINEUR MAJEUR
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SMIN     ! TR !  R ! SECTION MOUILLEE MINEUR AU NOEUD             !
! !  QMIN     ! TR !  D ! DEBIT MINEUR                                 !
! !  SMAJ     ! TR !  D ! SECTION MOUILLEE MAJEUR                      !
! !  QMAJ     ! TR !  D ! DEBIT MOUILLE MAJEUR                         !
! !  BETA     ! TR !  D ! FONCTION DU MODELE DEBORD                    !
! !  SNODE    ! TR !  D ! SECTION MOUILLEE                             !
! !  QNODE    ! TR !  R ! DEBIT MOUILLE                                !
! !  SGEO1    ! TR !  A ! SURFACE MOUILLE LIT MINEUR                   !
! !  SGEO     ! TR !  A ! SURFACE MOUILLEE TOTALE                      !
! !  DEBGE1   ! TR !  A ! DEBITANCE MINEUR                             !
! !  DEBGE2   ! TR !  A ! DEBITANCE MAJEUR                             !
! !  SEPS     !  R !    ! PRECISION SUR LA SECTION MOUILLEE            !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!    SGEO,SGEO1 font partie d'une structure de donnees STRUCTURE_SECTION
!    DEBGE1 DEBGE2

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! EPS6
   use M_ERREUR_T    ! ERREUR
   use M_DICHO_I   ! Interface du sous-programme DICHO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(  out) :: SMIN,QMIN,SMAJ,QMAJ,BETA
   real(DOUBLE), dimension(:)    , intent(in)    :: SNODE,QNODE
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO1,SGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: DEBGE1,DEBGE2
   integer     ,                   intent(in)    :: NMLARG
   integer                       , intent (in)   :: NOEUD
   integer                       , intent( in)   :: JG,JD
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine MINMAJ

   end interface

end module M_MINMAJ_I
