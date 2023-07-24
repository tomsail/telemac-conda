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

module M_CARAC3_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine CARAC3 ( &
             UREM    , &
             SREM    , &
             CREM    , &
             FREM    , &
             IPOINT  , &
             UNODE   , &
             SNODE   , &
             CNODE   , &
             AKNODE  , &
             TCAR    , &
             X       , &
             DT      , &
             NSECG   , &
             NSECD   , &
             COTR    , &
             FROT    , &
             SGEO    , &
             AIGEO   , &
             DYGEO   , &
             NBSECT  , &
             NMLARG  , &
         Impression  , & ! Flag d'impression
         UniteListing, & ! Unite logique fichier listing
           ERREUR      &
                 )

!***********************************************************************
!     CODE MASCARET : REMONTEE FAIBLE DES INVARIANTS DE RIEMANN POUR LES
!                     CONDITIONS AUX LIMITES
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  UREM     !  R ! R  ! VITESSE AU PIED DE LA CARACTERISTIQUE        !
! !  SREM     !  R ! R  ! SECTION AU PIED DE LA CARACTERISTIQUE        !
! !  CREM     !  R ! R  ! CELERITE AU PIED DE LA CARACTERISTIQUE       !
! !  FREM     !  R ! R  ! VALEUR REMONTEE PAR LA CARACTERISTIQUE       !
! !  IPOINT   !  I ! D  ! POINT A PARTIR DUQUEL ON TRACE LA            !
! !           !    !    ! CARACTERISTIQUE                              !
! !  UNODE    ! TR ! D  ! VITESSES                                     !
! !  SNODE    ! TR ! D  ! SECTION MOUILLEE                             !
! !  CNODE    ! TR ! D  ! CELERITE                                     !
! !  AKNODE   ! TR ! D  ! TERME K(S) DANS INVARIANT DE RIEMANN         !
! !  TCAR     !  I ! D  ! +1 : CARACTERISTIQUE CPLUS                   !
! !           !    !    ! -1 : CARACTERISTIQUE CMOINS                  !
! !  X        ! TR ! D  ! TABLEAU DES COORDONNEES                      !
! !  DT       !  R ! D  ! PAS DE TEMPS                                 !
! !  NSECG    !  I ! D  ! INDICE DE LA SECTION GAUCHE DU DOMAINE ETUDIE!
! !  NSECD    !  I ! D  ! INDICE DE LA SECTION DROITE DU DOMAINE ETUDIE!
! !  COTR     ! TR ! D  ! COTR TABLEAU DE DIMENSION NBSECT-1           !
! !           !    !    ! COTR(I) PENTE DU RADIER ENTRE I ET I+1       !
! !           !    !    !                                              !
! !  FROT     ! TR ! D  ! FROTTEMENT                                   !
! !  SGEO     ! TR ! D  !                                              !
! !  AIGEO    ! TR ! D  ! AIGEO  PLANIMETREE SUR LE MAILLAGE           !
! !  DYGEO    ! TR ! D  ! DYGEO  PLANIMETREE SUR LE MAILLAGE           !
! !  NBSECT   !  I ! D  ! Nombre de sections                           !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!-----------------------------------------------------------------------
!     SOUS PROGRAMME APPELE :  BISSND
!
!***********************************************************************
! SGEO, AIGEO et DYGEO font partie d'une structure de donnees

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS2, EPS5, EPS10
   use M_ERREUR_T    ! Erreur
   use M_AINTDC_I    ! Interface de la fonction    AINTDC
   use M_BISSND_I    ! Interface du sous-programme BISSND
   use M_DYDXSC_I    ! Interface de la fonction    DYDXSC

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(  out) :: UREM,SREM,CREM,FREM
   integer     ,                   intent(in)    :: IPOINT
   real(DOUBLE), dimension(:)    , intent(in)    :: UNODE,SNODE,CNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: AKNODE
   integer     ,                   intent(in)    :: TCAR
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE),                   intent(in)    :: DT
   integer     ,                   intent(in)    :: NSECG,NSECD
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR,FROT
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO,AIGEO,DYGEO
   integer     ,                   intent(in)    :: NBSECT
   integer     ,                   intent(in)    :: NMLARG
   logical                       , intent(in   ) :: Impression
   integer     ,                   intent(in   ) :: UniteListing
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine CARAC3 

   end interface

end module M_CARAC3_I
