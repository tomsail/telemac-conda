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

SUBROUTINE MATRIA( &
                   A , &
              LAMDI1 , &
              LAMDI2 , &
                 TI1 , &
                 TI2 , &
                TIS1 , &
                TIS2 , &
              ERREUR )
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!   FONCTION : CONSTRUCTION DES MATRICES POUR L'IMPLICITATION
!
!-----------------------------------------------------------------------
!                            ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  A        ! R  !  A ! MATRICE BLOC 2x2 DIAGONALE INFERIEURE        !
! !  B        ! R  !  A ! MATRICE BLOC 2x2 DIAGONALE INFERIEURE        !
! !  C        ! R  !  A ! MATRICE BLOC 2x2 DIAGONALE INFERIEURE        !
! !  T1       ! TD !  A ! VECTEUR PROPRE ASSOCIE A LA V.P 1            !
! !  T2       ! TD !  A ! VECTEUR PROPRE ASSOCIE A LA V.P 2            !
! !  TS1      ! TD !  A ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  TS2      ! TD !  A ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  DT       ! TD !  A ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  XI       ! TD !  A ! ABSCISSE DU POINT I                          !
! !  XIP1     !    !    ! ABSCISEE DU POINT I+1                        !
! !  XIM1     !    !    ! ABSCISSE DU POINT I-1                        r
! !___________!____!____!______________________________________________!
!
!     TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!  DECLARATION DES VARIABLES
!
   use M_PRECISION
   use M_ERREUR_T

   IMPLICIT NONE
   !
   !    Matrices (2x2)
   !
   Real(DOUBLE), dimension(:),   intent(inout) ::  A
   !
   !    Vecteurs
   !
   Real(DOUBLE), dimension(:),   intent(in   ) ::  TI1,TI2,TIS1,TIS2
   !
   Real(DOUBLE),                 intent(in   ) ::  LAMDI1,LAMDI2
   Type(ERREUR_T)            ,   intent(inout) ::  ERREUR
   !
   !      DECLARATION DES VARABLES LOCALES
   !
   Real(DOUBLE)   :: X1,Y1,X2,Y2,X3,Y3
   Real(DOUBLE)   :: X4,Y4
   !
   !      CONSTRUCTION DE LA JACOBIENNE
   !
   X1 = TI1(1)*TIS1(1)
   Y1 = TI2(1)*TIS2(1)
   X2 = TI1(1)*TIS1(2)
   Y2 = TI2(1)*TIS2(2)
   X3 = TI1(2)*TIS1(1)
   Y3 = TI2(2)*TIS2(1)
   X4 = TI1(2)*TIS1(2)
   Y4 = TI2(2)*TIS2(2)
   !
   A(1) = LAMDI1 * X1 + LAMDI2 * Y1
   A(2) = LAMDI1 * X2 + LAMDI2 * Y2
   A(3) = LAMDI1 * X3 + LAMDI2 * Y3
   A(4) = LAMDI1 * X4 + LAMDI2 * Y4
   !
   RETURN

END SUBROUTINE MATRIA
