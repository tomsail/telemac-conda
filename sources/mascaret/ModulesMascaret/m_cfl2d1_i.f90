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

module M_CFL2D1_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine CFL2D1 ( &
             DT2D    , &
             W       , &
             BXY     , &
             VNOIN   , &
             NELMIN  , &
             DT1D    , &
             HEPS    , &
             Erreur    &
                )

!***********************************************************************
!
!   CODE MASCARET : CALCUL DU PAS DE TEMPS DANS LE CONFLUENT
!   -------------    AVEC LES REGLES SUIVANTES :
!                    - NOMBRE DE COURANT < 0.5
!                    - NOMBRE DE SOUS ITERATIONS >= 5
!
!                    SOUS PROGRAMME APPELANT : FLUSRC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  DT2D     !  R !  R ! PAS DE TEMPS DANS LE MODELE 2D               !
! !  W        ! TR !  D ! VARIABLE D'ETAT DANS LE CONFLUENT            !
! !  BXY      ! TR !  D ! BARYCENTRE DES CELLULES 2D                   !
! !  VNOIN    ! TR !  D ! VECTEUR NORMAL AUX CELLULES INTERIEURS       !
! !  NELMIN   ! TI !  D ! TABLEAU DES CELLULES FONCTION DES SEGMENTS   !
! !  DT1D     !  R !  D ! PAS DE TEMPS DANS LE MODELE 1D               !
! !  HEPS     !  R !  D ! HAUTEUR D'EAU MINIMALE                       !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  DIST     !  R !  A ! DISTANCE CENTRES DE CELLULES-SEGMENT         !
! !  XNC      !  R !  A ! RAPPORT (U+C)/DIST                           !
! !___________!____!____!______________________________________________!
!
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
   use M_PARAMETRE_C ! GPES
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(  out) :: DT2D
   ! 1ere dimension 3
   real(DOUBLE), dimension(:,:)  , intent(in)    :: W
   ! 1ere dimension 12
   real(DOUBLE), dimension(:,:)  , intent(in)    :: BXY
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:)  , intent(in)    :: VNOIN
   ! 1ere dimension 12
   integer     , dimension(:,:)  , intent(in)    :: NELMIN
   real(DOUBLE),                   intent(in)    :: DT1D
   real(DOUBLE),                   intent(in)    :: HEPS
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine CFL2D1

   end interface

end module M_CFL2D1_I
