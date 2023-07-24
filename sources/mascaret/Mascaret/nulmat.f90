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

subroutine NULMAT( &
                   A , &
                  IM , &
                  JM , &
              Erreur &
                     )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : MISE A ZERO D'UNE MATRICE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  A        ! TR ! R  ! MATRICE MISE A ZERO                          !
! !  IM       !  I ! D  ! NOMBRE DE LIGNE DE LA MATRICE                !
! !  JM       !  I ! D  ! NOMBRE DE COLONNE DE LA MATRICE              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!-----------------------------------------------------------------------
!***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM, 2nde dimension JM
   real(DOUBLE), dimension(:,:)  , intent(  out) :: A
   integer     ,                   intent(in)    :: IM,JM
   Type (ERREUR_T)               , intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer        :: I,J
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>NULMAT'

   do I = 1, IM
      do J = 1, JM
         A(I,J) = 0._DOUBLE
      end do
   end do

  !------------------
  ! Fin du traitement
  !------------------

  !Erreur%arbredappel = !arbredappel_old

  return

end subroutine NULMAT
