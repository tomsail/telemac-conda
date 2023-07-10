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

module M_RIEMAN_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine RIEMAN ( &
             SVRAI   , &
             QVRAI   , &
             ZVRAI   , &
             YVRAI   , &
             UVRAI   , &
             SGAUC   , &
             SDROI   , &
             X       , &
             T       , &
             ITEMP   , &
             NBSECT  , &
         Impression  , & ! Flag d'impression
         UniteListing, & ! Unite logique fichier listing
           Erreur      &
                     )

!***********************************************************************
!  PROGICIEL : MASCARET
!
!  AVERTISSEMENT :   L UTILISATEUR DEVRA CHANGER DANS CE SOUS PROGRAMME
!  ***************   LES VARIABLES XBAR (ABSCISSE DU BARRAGE) ET ALARG
!                    (LARGEUR DU CANAL) SUIVANT LES VALEURS REELLES
!-----------------------------------------------------------------------
!***********************************************************************
!
!  FONCTION :
!  --------
!
!      CALCUL DE LA SOLUTION DU PROBLEME DE RIEMMAN
!
!-----------------------------------------------------------------------
!
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SVRAI    ! TR !  R ! SURFACE VRAIE LORS DE LA VALIDATION          !
! !  QVRAI    ! TR !  R ! DEBIT VRAI LORS DE LA VALIDATION             !
! !  ZVRAI    ! TR !  R ! SURFACE LIBRE VRAIE LORS DE LA VALIDATION    !
! !  YVRAI    ! TR !  R ! TIRANT D'EAU VRAI LORS DE LA VALIDATION      !
! !  UVRAI    ! TR !  R ! VITESSE VRAIE LORS DE LA VALIDATION          !
! !  SGAUC    !  R !  D ! SECTION MOUILLE A GAUCHE DU BIEF             !
! !  SDROI    !  R !  D ! SECTION MOUILLE A DROITE DU BIEF             !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  T        !  R !  D ! INSTANT DU CALCUL                            !
! !  ITEMP    !  I !    !                                              !
! !  NBSECT   !  I !    ! Nombre de sections                           !
! !  ALARG    ! FR !  A ! LARGEUR DU CANAL                             !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  XBAR     !  R !  A ! POSITION DU BARRAGE                          !
! !  EPS      !  R !  A ! PRECISION DANS LA RESOLUTION PAR ZBRENT      !
! !  MAXFEN   !  I !  A ! MAXIMUM D'ITERATION POUR LE SOLVEUR ZBRENT   !
! !  *G       !  R !  A ! VARIABLES LIEES A LA DEFINITION DU RESSAUT   !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!*****************************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS2, EPS6
   use M_ERREUR_T  ! ERREUR
   use M_FC1_I       ! Interface de la fonction    FC1
   use M_SOLEX_I     ! Interface du sous-programme SOLEX
   use M_ZBRENT_I    ! Interface du sous-programme ZBRENT

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE), dimension(:)    , intent(  out) :: SVRAI,QVRAI,ZVRAI,YVRAI,UVRAI
   real(DOUBLE),                   intent(in)    :: SGAUC
   real(DOUBLE),                   intent(inout) :: SDROI
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE),                   intent(in)    :: T
   integer     ,                   intent(in)    :: ITEMP
   integer     ,                   intent(in)    :: NBSECT
   logical                       , intent(in   ) :: Impression
   integer     ,                   intent(in   ) :: UniteListing
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Communs ..
   !-------------
   real(DOUBLE) :: CGAUC,CDROI
   common /ICOEC1/ CGAUC,CDROI

   end subroutine RIEMAN

   end interface

end module M_RIEMAN_I
