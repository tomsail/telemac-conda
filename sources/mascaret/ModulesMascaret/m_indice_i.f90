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

module M_INDICE_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine INDICE ( &
        I            , &
        ABSC         , &
        X            , &
        NBSECT       , &
       Erreur          &
                  )

!***********************************************************************
! FONCTION :        RECHERCHE PAR DICHOTOMIE L'INDICE DE CALCUL
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  I        !  I !  A ! INDICE                                       !
! !  ABSC     !  R !  D ! ABSCISSE                                     !
! !  X        ! TR !  D ! ABSCISSE DU MAILLAGE                         !
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
   use M_PARAMETRE_C         ! EPS2
   use M_ERREUR_T            ! ERREUR
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   integer     ,                   intent(  out) :: I
   real(DOUBLE),                   intent(in)    :: ABSC
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   integer     ,                   intent(in)    :: NBSECT
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine INDICE

   end interface

end module M_INDICE_I
