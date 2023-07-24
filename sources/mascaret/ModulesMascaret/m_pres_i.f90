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

module M_PRES_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   function PRES ( &
          NOEUD  , &
          SURF   , &
          DZ     , &
          PRGEO  , &
          SGEO   , &
          NMLARG , &
          ERREUR   &
              )

!***********************************************************************
!     CODE MASCARET : CALCUL DU TERME DE PRESSION  EN FONCTION DE LA
!                     SURFACE MOUILLEE AUX INTERFACES DE CELLULES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  NOEUD    !  I !  D ! NOEUD CONSIDERE DU MAILLAGE                  !
! !  SURF     !  R !  D ! SURFACE MOUILLEE AU NOEUD                    !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  PRGEO    ! TR !  D ! PRESSION PLANIMETRE AUX INTERFACES           !
! !  SGEO     ! TR !  D ! SURFACE MOUILLE PLANIMETREE AUX INTERFACES   !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  JG       !  I !  R ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !  JD       !  I !  R ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !  SG       !  R !  A ! SURFACE MOUILLE POUR LA BORNE GAUCHE         !
! !  SD       !  R !  A ! SURFACE MOUILLE POUR LA BORNE DROITE         !
! !  PRG      !  R !  A ! PRESSION POUR LA BORNE GAUCHE                !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!   PRGEO et SGEO font partie d'une structure de donnees STRUCTURE _SECTIONS

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES
   use M_ERREUR_T    ! ERREUR
   use M_DICHO_I     ! Interface du sous-programme DICHO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                  :: PRES
   integer     ,                   intent(in)    :: NOEUD
   real(DOUBLE),                   intent(in)    :: SURF
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   real(DOUBLE), dimension(:,:)  , intent(in)    :: PRGEO,SGEO
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end function PRES

   end interface

end module M_PRES_I
