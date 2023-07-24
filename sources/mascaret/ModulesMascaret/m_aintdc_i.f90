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

module M_AINTDC_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   function AINTDC( &
                NOEUD , & ! NOEUD CONSIDERE DU MAILLAGE
                 SURF , & ! SURFACE MOUILLEE AU NOEUD
                 SGEO , & ! SURFACE MOUILLE PLANIMETREE
                AIGEO , & ! FONCTION INTEGRALE PLANIMETRE
               NMLARG , &
               ERREUR  & ! Erreur
                       )

!***********************************************************************
!     CODE MASCARET : CALCUL DE L'INTEGRALE
!                     SOMME DE 0 A S DE : 1./S * (DC/DX A S CONST) * DS
!                     AVEC C CELERITE DES ONDES ET S SURFACE MOUILLEE
!                     LA DERIVEE EST PRISE A S CONSTANT
!                     SUR LE MAILLAGE INITIAL
!
!***********************************************************************
!     CODE MASCARET : CALCUL DE L'INVARIANT DE RIEMMAN 
!                EN FONCTION DE LA SURFACE MOUILLEE SUR MAILLAGE INITIAL
!-----------------------------------------------------------------------
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  JG       !  I !  A ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !  JD       !  I !  A ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !  SG       !  R !  A ! SURFACE MOUILLE POUR LA BORNE GAUCHE         !
! !  SD       !  R !  A ! SURFACE MOUILLE POUR LA BORNE DROITE         !
! !  AIG      !  R !  A ! FONCTION INTEGRALE POUR LA BORNE GAUCHE      !
! !  AID      !  R !  A ! FONCTION INTEGRALE POUR LA BORNE DROITE      !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!
! SGEO et AIGEO font partie d'une structure de donnees : STRUCTURE_SECTIONS

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
   real(DOUBLE)                                :: AINTDC
   integer     ,                 intent(in)    :: NOEUD
   real(DOUBLE),                 intent(in)    :: SURF
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:), intent(in)    :: SGEO,AIGEO
   integer     ,                 intent(in)    :: NMLARG
   Type (ERREUR_T)             , intent(inout) :: ERREUR

   end function AINTDC

   end interface

end module M_AINTDC_I
