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

SUBROUTINE VVPROPI( LAMDA1 , LAMDA2 , T1 , T2 , TS1 , TS2 , & 
                    UTILD , CTILD , BETA , FRD , FRG , UD , UG , CG , CD , ISEC , CORRG , &
                    ERREUR )
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
   real(DOUBLE),                   intent(  out) :: LAMDA1,LAMDA2
   ! 1ere dimension 2
   real(DOUBLE), dimension(:)    , intent(  out) :: T1,T2,TS1,TS2
   real(DOUBLE),                   intent(in)    :: UTILD,CTILD
   real(DOUBLE),                   intent(in)    :: BETA
   real(DOUBLE),                   intent(in)    :: FRD,FRG,UD,UG,CD,CG
   integer     ,                   intent(in)    :: ISEC
   integer     ,                   intent(  out) :: CORRG
   Type (ERREUR_T)               , intent(inout) :: ERREUR
   real(DOUBLE)  LAMD1G,LAMD1D,LAMD2G,LAMD2D 
   real(DOUBLE)  UTILD1,CTILD1
   real(DOUBLE)  Z

   ! 1 ) CALCUL DES VALEURS PROPRES
   ! -----------------------------
   !
   Z = DSQRT( CTILD * CTILD - ( BETA * ( 1.D0 - BETA ) * UTILD * UTILD ) )
   LAMDA1 = BETA * UTILD - Z
   LAMDA2 = BETA * UTILD + Z

   !
   !  CORRECTION ENTROPIQUE (LEVEQUE)
   !  ---------------------
   !
   if( ( FRD > 1._DOUBLE ).AND.( FRG < 1._DOUBLE ).AND.( UTILD > 0._DOUBLE ).AND.( ISEC == 0 ) ) THEN
      LAMD1D = BETA * UD - CD
      LAMD2D = BETA * UD + CD
      LAMD1G = BETA * UG - CG
      LAMD2G = BETA * UG + CG

      LAMDA1 = LAMD1G * ( LAMDA1 - LAMD1D ) / ( LAMD1G - LAMD1D )
   endif

   CORRG = 0

   if( ( FRD < 1._DOUBLE ).AND.( FRG > 1._DOUBLE ).AND.( UTILD < 0._DOUBLE ).AND.( ISEC == 0 ) ) THEN
      LAMD1D = BETA * UD - CD
      LAMD2D = BETA * UD + CD
      LAMD1G = BETA * UG - CG
      LAMD2G = BETA * UG + CG

      LAMDA2 = LAMD2D * ( LAMDA2 - LAMD2G ) / ( LAMD2D - LAMD2G )
      CORRG = 1
   endif

   !
   ! 2 ) CALCUL DES VECTEURS PROPRES
   ! -------------------------------
   !
   UTILD1 = BETA * UTILD
   CTILD1 = Z
   T1(1)  = 1.D0
   T1(2)  = UTILD1 - CTILD1
   TS1(1) = ( UTILD1 + CTILD1 ) / ( 2.D0 * CTILD1 )
   TS1(2) = -1.D0 / ( 2.D0 * CTILD1 )

   T2(1)  = 1.D0
   T2(2)  = UTILD1 + CTILD1
   TS2(1) = -( UTILD1 - CTILD1 ) / ( 2.D0 * CTILD1 )
   TS2(2) = 1.D0 / ( 2.D0 * CTILD1 )

   RETURN
END SUBROUTINE VVPROPI
