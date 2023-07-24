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

SUBROUTINE VVPROP( TI1 , TI2 , TIS1 , TIS2 , LAMDI1 , LAMDI2 , BETA , UG , CG , ERREUR )
!
!***********************************************************************
! PROGICIEL : MASCARET        F. MAUREL          N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!   FONCTION : CALCUL DES VALEURS PROPRES ET DES VECTEURS PROPRES 
!                 DE LA LINEARISEE DE ROE -IMPLICITATION
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  LAMDA1   !  D !  R ! VALEUR PROPRE 1, UTILD-CTILD                 !
! !  LAMDA2   !  D !  R ! VALEUR PROPRE 2, UTILD+CTILD                 !
! !  UTILD    !  D !  D ! VITESE MOYENNE DE ROE                        !
! !  CTILD    !  D !  D ! CELERITE MOYENNE DE ROE                      !
! !  UG,UD    !  D !  D ! VITESSE DANS LA CELLULE DE GAUCHE (DROITE)   !
! !  CG,CD    !  D !  D ! CELERITE DANS LA CELLULE DE GAUCHE (DROITE)  !
! !  LAMDA1G  !  D !  A ! VALEUR PROPRE 1 CELLULE DE GAUCHE, UG-CG     !
! !  LAMDA2G  !  D !  A ! VALEUR PROPRE 2 CELLULE DE GAUCHE, UG+CG     !
! !  LAMDA1D  !  D !  A ! VALEUR PROPRE 1 CELLULE DE DROITE, UD-CD     !
! !  LAMDA2D  !  D !  A ! VALEUR PROPRE 2 CELLULE DE DROITE, UD+CD     !
! !  T1       ! TD !  R ! VECTEUR PROPRE ASSOCIE A LA V.P 1            !
! !  T2       ! TD !  R ! VECTEUR PROPRE ASSOCIE A LA V.P 2            !
! !  TS1      ! TD !  R ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  TS2      ! TD !  R ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  ISEC     !  E !  D ! INDICATEUR SI CELLULE GAUCHE SECHE           !
! !___________!____!____!______________________________________________!
!
!
   ! DECLARATION DES VARIABLES
   ! =========================
   !
   use M_PRECISION
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(  out) :: LAMDI1,LAMDI2
   ! 1ere dimension 2
   real(DOUBLE), dimension(:)    , intent(  out) :: TI1,TI2,TIS1,TIS2
   real(DOUBLE),                   intent(in)    :: BETA
   real(DOUBLE),                   intent(in)    :: UG,CG
   Type (ERREUR_T)               , intent(inout) :: ERREUR
   !
   !     VARIABLES LOCALES
   !
   real(DOUBLE)                                 ::UG1,CG1

   !
   ! 1 ) CALCUL DES VALEURS PROPRES dans la cellule I
   ! -----------------------------
   !
   LAMDI1 = UG - CG
   LAMDI2 = UG + CG
   LAMDI1 = BETA * UG - DSQRT( CG**2 - ( BETA * ( 1.D0 - BETA ) * UG**2 ) )
   LAMDI2 = BETA * UG + DSQRT( CG**2 - ( BETA * ( 1.D0 - BETA ) * UG**2 ) )

   !
   ! 2 ) CALCUL DES VECTEURS PROPRES dans la cellule I
   ! -------------------------------
   !
   UG1 = BETA * UG
   CG1 = DSQRT( CG**2 - BETA * ( 1.D0 - BETA )* UG**2 )
   !
   TI1(1)  = 1.D0
   TI1(2)  = UG1 - CG1
   TIS1(1) = ( UG1 + CG1 ) / ( 2.D0 * CG1 )
   TIS1(2) = -1.D0 / ( 2.D0 * CG1 )
   !
   TI2(1)  = 1.D0
   TI2(2)  = UG1 + CG1
   TIS2(1) = -( UG1 - CG1 ) / ( 2.D0 * CG1 )
   TIS2(2) = 1.D0 / ( 2.D0 * CG1 )

   RETURN

END SUBROUTINE VVPROP

