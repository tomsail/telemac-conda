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

SUBROUTINE MATRI( &
                  D , &
                  I , &
             LAMDA1 , &
             LAMDA2 , &
                 T1 , &
                 T2 , &
                TS1 , &
                TS2 , &
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
! !  XIM1     !    !    ! ABSCISSE DU POINT I-1                        !
! !___________!____!____!______________________________________________!
!
!     TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!
!  DECLARATION DES VARIABLES
!
   use M_PRECISION
   use M_ERREUR_T
   !
   IMPLICIT NONE
   !
   !    Matrices (2x2)
   !
   Real(DOUBLE), dimension(:),   intent(inout) ::  D,I
   !
   !    Vecteurs
   !
   Real(DOUBLE), dimension(:),   intent(in   ) ::  T1,T2,TS1,TS2
   !
   Real(DOUBLE),                 intent(in   ) ::  LAMDA1,LAMDA2
   Type(ERREUR_T)            ,   intent(inout) ::  ERREUR
   !
   !      DECLARATION DES VARABLES LOCALES
   !
   Real(DOUBLE)   :: X1,Y1,X2,Y2,X3,Y3
   Real(DOUBLE)   :: X4,Y4,VAB1,VAB2

   !
   !      CONSTRUCTION DE LA JACOBIENNE
   !
   X1 = T1(1) * TS1(1)
   Y1 = T2(1) * TS2(1)
   X2 = T1(1) * TS1(2)
   Y2 = T2(1) * TS2(2)
   X3 = T1(2) * TS1(1)
   Y3 = T2(2) * TS2(1)
   X4 = T1(2) * TS1(2)
   Y4 = T2(2) * TS2(2)
   !
   !     CONSTRUCTION DE LA DIAGONALE
   !
   VAB1 = DABS( LAMDA1 )
   VAB2 = DABS( LAMDA2 )
   D(1) = VAB1 * X1 + VAB2 * Y1
   D(2) = VAB1 * X2 + VAB2 * Y2
   D(3) = VAB1 * X3 + VAB2 * Y3
   D(4) = VAB1 * X4 + VAB2 * Y4
   !
   I(1) = 1.D0
   I(2) = 0.D0
   I(3) = 0.D0
   I(4) = 1.D0
!
   RETURN 
!
END SUBROUTINE MATRI
