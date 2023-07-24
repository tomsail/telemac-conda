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

module M_PROMVT_I

   interface

   SUBROUTINE PROMVT(A,B,C,N)
!***********************************************************************
!   CODE MASCARET : PRODUIT MATRICE VECTEUR
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
   Use M_PRECISION

   IMPLICIT NONE

   Real(Double)                   , Intent(inout) :: A(4),B(4),C(4)
   Integer                                        :: N

   end subroutine PROMVT

   end interface

end module M_PROMVT_I
