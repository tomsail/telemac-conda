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

module M_FPERD_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   function FPERD ( &
        NOEUD     , &
        Y         , &
        DZD       , &
        FPGEOD    , &
        NMLARG    , &
        Erreur      &
             )

!***********************************************************************
!     CODE MASCARET : CALCUL DU PERIMETRE HYDRAULIQUE
!                     AUX INTERFACES DE CELLULE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !   NOEUD   !  I ! D  ! NUMERO DU POINT OU ON CALCULE LE PERIMETRE   !
! !   Y       !  R ! D  ! TIRANT D'EAU                                 !
! !   DZD     ! TR ! D  ! PAS DE PLANIMETRAGE AUX INTERFACES           !
! !   FPGEOP  ! TR ! D  ! PERIMETRE MOUILLE PLANIMETRE AUX INTERFACES  !
! !   NMLARG  !  I !    ! Nombre de pas des sections                   !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !   JG      !  I !  R ! BORNE GAUCHE DE L'INTERVALLE CONTENANT Y     !
! !   JD      !  I !  R ! BORNE DROITE DE L'INTERVALLE CONTENANT Y     !
! !   YG      !  R !  A ! TIRANT D'EAU POUR LA BORNE GAUCHE            !
! !   YD      !  R !  A ! TIRANT D'EAU POUR LA BORNE DROITE            !
! !   FPG     !  R !  A ! PERIMETRE MOUILLE POUR LA BORNE GAUCHE       !
! !   FPD     !  R !  A ! PERIMETRE MOUILLE POUR LA BORNE DROITE       !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!  FPGEOD fait partie d'une structure de donnees STRUCTURE_SECTIONS

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C           ! Messages d'erreur
   use M_ERREUR_T  ! ERREUR
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                  :: FPERD
   integer     ,                   intent(in)    :: NOEUD
   real(DOUBLE),                   intent(in)    :: Y
   ! 1ere dimension IM1
   real(DOUBLE), dimension(:)    , intent(in)    :: DZD
   real(DOUBLE), dimension(:,:)  , intent(in)    :: FPGEOD
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)               , intent(inout) :: Erreur

   end function FPERD

   end interface

end module M_FPERD_I
