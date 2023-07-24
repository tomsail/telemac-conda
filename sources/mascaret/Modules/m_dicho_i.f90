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

module M_DICHO_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL      F. MAUREL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine DICHO              ( &
             PasBas              , & ! BORNE basse DE L'INTERV CONTENANT SurfaceMouillee 
             PasHaut             , & ! BORNE haute DE L'INTERV CONTENANT SurfaceMouillee
            SurfaceMouillee      , & ! surface mouillee 
             SurfaceMouilleePlani, & ! Surface mouillee planimetree
             Erreur                & ! Erreur
                                 )

!***********************************************************************
!
! FONCTION :        RECHERCHE PAR DICHOTOMIE DES BORNES DE L'INTERVALLE
!                   CONTENANT LA SURFACE MOUILLEE SurfaceMouillee SUR LE MAILLAGE
!                   INITIAL
!-----------------------------------------------------------------------
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  SG       !  R !  A ! SURFACE MOUILLE POUR LA BORNE GAUCHE         !
! !  NMAX     !  I !  A !                                              !
! !  NT       !  I !  A ! NOMBRE D'ITERATIONS DEJA EFFECTUEES          !
! !  JGP1     !  I !  A ! PasBas+1 (<PasHaut TANT QU ON ITERE)         !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************

   !  SurfaceMouilleePlani fait partie d'une structure de donnees  STRUCTURES_SECTION

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C        ! Messages d'erreurs
   use M_ERREUR_T         ! Type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   integer     ,                   intent(  out) :: PasBas
   integer     ,                   intent(  out) :: PasHaut
   real(DOUBLE),                   intent(in   ) :: SurfaceMouillee
   real(DOUBLE), dimension(:)    , intent(in   ) :: SurfaceMouilleePlani
   type(ERREUR_T),                 intent(inout) :: Erreur

   end subroutine DICHO

   end interface

end module M_DICHO_I
