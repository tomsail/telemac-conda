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

module M_CFL2D_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine CFL2D ( &
              W     , &
              BXY   , &
              NELMIN, &
              DT    , &
              HEPS  , &
              ICONF , &
              ERREUR  &
                )

!***********************************************************************
!     CODE MASCARET : CALCUL DU PAS DE TEMPS POUR LE MODELE 2D
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  W        ! TR !  D ! VARIABLE D'ETAT A L'INSTANT N SUR 12 CELLULES!
! !  BXY      ! TR !  D ! BARYCENTRE DES 12 CELLULES                   !
! !  NELMIN   ! TI !  D ! DONNE LES NUMEROS GLOBAUX DES EXT. DES ARETES!
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  HEPS     !  R !  D ! PRECISION SUR LA HAUTEUR D'EAU               !
! !  ICONF    !  I !  D ! NUMERO DU CONFLUENT                          !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!    AKGEO et SGEO font partie d'une structure de donnees STRUCTURE_SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C      ! GPES
   use M_MESSAGE_C        ! Messages d'erreur
   use M_ERREUR_T         ! ERREUR
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension 3
   real(DOUBLE), dimension(:,:)  , intent(in)    :: W
   ! 1ere dimension 12           
   real(DOUBLE), dimension(:,:)  , intent(in)    :: BXY
   integer     , dimension(:,:)  , intent(in)    :: NELMIN
   real(DOUBLE),                   intent(in)    :: DT,HEPS
   integer     ,                   intent(in)    :: ICONF
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine CFL2D

   end interface

end module M_CFL2D_I
