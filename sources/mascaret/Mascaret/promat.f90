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

SUBROUTINE PROMAT( A , B , C , N )
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!   FONCTION : PRODUIT DE 2 MATRICES CARREES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  A        ! R  !  A ! MATRICE PRODUIT DE B et C                    !
! !  B        ! R  !  A ! MATRICE                                      !
! !  C        ! R  !  A ! MATRICE                                      !
! !  N        ! TD !  A ! DIMENSION DES MATRICES                       !
! !___________!____!____!______________________________________________!
!
!     TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
   use M_PRECISION
!
   IMPLICIT NONE
!
   Real(DOUBLE)              ,   intent(inout)  :: A(4),B(4),C(4)
   Integer                   ,   intent(in   )  :: N
   !
   !     Varables locales
   !
   Integer                                      :: K1,K2,K3
   !
   !       VARIABLES LOCALES
   !
   Integer                                      :: I,J,K
   !
   DO 10 I = 1 , N
      DO 20 J = 1 , N
         K1 = ( I - 1 ) * 2 + J
         A(K1) = 0.D0
         DO 30 K = 1 , N
            K2 = ( I - 1 ) * 2 + K
            K3 = ( K - 1 ) * 2 + J
            A(K1) = A(K1) + B(K2) * C(K3)
         30 CONTINUE
      20 CONTINUE
   10 CONTINUE
!
   RETURN
END SUBROUTINE PROMAT
