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

module M_AKIVM1_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   function AKIVM1( &
                NOEUD , &
                AKDES , &
                AKGEO , &
                 SGEO , &
               NMLARG , &
               Erreur & ! Erreur
                  )

!***********************************************************************
!     CODE MASCARET : CALCUL DE L'INVARIANT DE RIEMMAN 
!                EN FONCTION DE LA SURFACE MOUILLEE SUR MAILLAGE INITIAL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  AKIVM1   !  R !  R ! SURFACE POUR L'INVARIANT DE RIEMMAN AKDES    !
! !  NOEUD    !  I !  D ! NOEUD CONSIDERE DU MAILLAGE                  !
! !  AKDES    !  R !  D ! INVARIANT DE RIEMMAN AU NOEUD                !
! !  AKGEO    ! TR !  D ! INVARIANT DE RIEMMAN PLANIMETRE              !
! !  SGEO     ! TR !  D ! SURFACE MOUILLE PLANIMETREE                  !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  JG       !  I !  A ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !  JD       !  I !  A ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !  SG       !  R !  A ! SURFACE MOUILLE POUR LA BORNE GAUCHE         !
! !  SD       !  R !  A ! SURFACE MOUILLE POUR LA BORNE DROITE         !
! !  AKG      !  R !  A ! INVARIANT DE RIEMMAN POUR LA BORNE GAUCHE    !
! !  AKD      !  R !  A ! INVARIANT DE RIEMMAN POUR LA BORNE DROITE    !
! !___________!____!____!______________________________________________!
!
!  SGEO et AKGEO font partie d'une structure de donnees STRUCTURE_SECTION
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
   use M_ERREUR_T  ! ERREUR
   use M_DICHO_I   ! Interface du sous-programme DICHO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                :: AKIVM1
   integer     ,                 intent(in)    :: NOEUD
   real(DOUBLE),                 intent(in)    :: AKDES
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:), intent(in)    :: AKGEO,SGEO
   integer     ,                 intent(in)    :: NMLARG
   Type (ERREUR_T)             , intent(inout) :: ERREUR

   end function AKIVM1

   end interface

end module M_AKIVM1_I
