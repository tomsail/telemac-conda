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

module M_BISSN1_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
   interface

   SUBROUTINE BISSN1( X  ,  A  ,  B  ,  C  ,  KM  , NFU  ,ERREUR )

!***********************************************************************
!     CODE MASCARET : RESOLUTION DE SYSTEMES TRIDIAGONAUX
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !   X       ! TR ! M  ! ENTREE: SECOND MEMBRES                       !
! !           !    !    ! SORTIE: RESULTATS                            !
! ! A,B,C     ! TR ! M  ! MATRICE TRIDIAGONALE PAR BLOC                !
! !  KM       ! E  ! D  ! DIMENSION DU SYSTEME                         !
! !  NFU      ! E  ! D  ! NOMBRE DE FONCTIONS                          !
! !___________!____!____!______________________________________________!
!***********************************************************************

   !
   !   DECLARATION DES VARIABLES
   !
   use M_PRECISION
   use M_ERREUR_T
   use M_PROMAT_I
   use M_PROMVT_I
   use M_INVMAT_I

   Implicit none

   Real(DOUBLE), dimension(:,:)    ,intent(inout) :: A,B,C,X
   Integer                         ,intent(in   ) :: NFU,KM
   Type (ERREUR_T)                 ,intent(inout) :: ERREUR

   end subroutine

   end interface

end module M_BISSN1_I
