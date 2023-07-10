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

module M_FLUROE_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine FLUROE ( &
             FLULOC  , &
             FLUSOD  , &
             FLUSOG  , &
             FLUFRG  , &
             FLUFRD  , &
             FLUSOC  , &
             SOTILD  , &
             SOPRIM  , &
             SOFROT  , &
             LAMDA1  , &
             LAMDA2  , &
             T1      , &
             T2      , &
             TS1     , &
             TS2     , &
             SG      , &
             SD      , &
             QG      , &
             QD      , &
             PRG     , &
             PRD     , &
             BETAG   , &
             BETAD   , &
             X       , &
             UTILD   , &
             I       , &
             INDIC   , &
             CORRG   , &
             Erreur    &
               )

!***********************************************************************
!   CODE MASCARET : CALCUL DES FLUX DE ROE POUR
!                         LA MASSE : FLULOC(I,1)
!                         LA QUANTITE DE MOUVEMEMENT : FLULOC(I,2)
!                         LES TREMES SOURCES : FLUSOD,FLUSOG,FLUSOC
!                         LE FROTTEMENT : FLUFRG,FLUFRD 
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  FLULOC   ! TR !  A ! FLUX LOCAL A TRAVERS UNE INTERFACE           !
! !  FLUSOD   ! TR !  A ! FLUX DE T. SOURCE PARTIE DROITE DE LA CELLULE!
! !  FLUSOG   ! TR !  A ! FLUX DE T. SOURCE PARTIE GAUCHE DE LA CELLULE!
! !  FLUFRG   ! TR !  A ! FLUX DE FROTTEMENT GAUCHE DE LA CELLULE      !
! !  FLUFRD   ! TR !  A ! FLUX DE FROTTEMENT DROITE DE LA CELLULE      !
! !  FLUSOC   ! TR !  A ! FLUX DE T. SOURCE PARTIE CENTRE DE LA CELLULE!
! !  SOTILD   ! TR !  A ! T. SOURCE  INTERFACE                         !
! !  SOPRIM   ! TR !  A ! T. SOURCE (CENTRE DE LA CELLULE)             !
! !  SOFROT   ! TR !  A ! FROTTEMENT INTERFACE                         !
! !  LAMDA1   !  R !  A ! VALEUR PROPRE 1, UTILD-CTILD                 !
! !  LAMDA2   !  R !  A ! VALEUR PROPRE 2, UTILD+CTILD                 !
! !  T1       ! TR !  A ! VECTEUR PROPRE ASSOCIE A LA V.P 1            !
! !  T2       ! TR !  A ! VECTEUR PROPRE ASSOCIE A LA V.P 2            !
! !  TS1      ! TR !  A ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  TS2      ! TR !  A ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  SG(D)    !  R !  A ! SECTION MOUILLEE CELLULE DE GAUCHE (DROITE)  !
! !  QG(D)    !  R !  A ! VITESSE CELLULE DE GAUCHE (DROITE)           !
! !  PRG(D)   !  R !  A ! PRESSION CELLULE DE GAUCHE (DROITE)          !
! !  BETAG(D) !  R !    !                                              !
! !  X        !  R !    !                                              !
! !  UTILD    !  R !  A ! VITESE MOYENNE DE ROE                        !
! !  I        !  I !    !                                              !
! !  INDIC    !  I !  A ! 0 SI LES 2 CEL. PLEINES, 1 SI CEL DROITE VIDE!
! !           !    !    ! 2 SI CEL. GAUCHE VIDE., 3 SI 2 CEL. VIDES    !
! !  CORRG    !  I !  A ! INDIC DE CORRECTION ENTROPIQUE AVEC VITESSE<0!
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  PSFLU    !  D !  A ! PROD. SCALAIRE LIE AU FLULOC (MASSE, QTE MVT)!
! !  PSFLU1   !  D !  A ! P. SCALAIRE LIE AU FLULOC (MASSE, QTE MVT)   !
! !  PSFLU2   !  D !  A ! P. SCALAIRE LIE AU FLULOC (MASSE, QTE MVT)   !
! !  PSSOG    !  D !  A ! P. SCALAIRE LIE T.SOURCES GAUCHE DE LA CELLUL!
! !  PSSOD    !  D !  A ! P. SCALAIRE LIE T.SOURCES DROITE DE LA CELLUL!
! !  PSFRG    !  D !  A ! P. SCALAIRE FROTTEMENT GAUCHE DE LA CELLULE  !
! !  PSFRD    !  D !  A ! P. SCALAIRE  FROTTEMENT DROITE DE LA CELLULE !
! !___________!____!____!______________________________________________!
!
!     TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
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

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM, 2nde dimension 2
   real(DOUBLE), dimension(:,:)  , intent(  out) :: FLULOC,FLUSOD,FLUSOG
   real(DOUBLE), dimension(:,:)  , intent(  out) :: FLUFRG,FLUFRD,FLUSOC
   ! 1ere dimension 2
   real(DOUBLE), dimension(:)    , intent(in)    :: SOTILD,SOPRIM,SOFROT
   real(DOUBLE),                   intent(in)    :: LAMDA1,LAMDA2
   ! 1ere dimension 2
   real(DOUBLE), dimension(:)    , intent(in)    :: T1,T2,TS1,TS2
   real(DOUBLE),                   intent(in)    :: SG,SD
   real(DOUBLE),                   intent(in)    :: QG,QD
   real(DOUBLE),                   intent(in)    :: PRG,PRD
   real(DOUBLE),                   intent(in)    :: BETAG,BETAD
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE),                   intent(in)    :: UTILD
   integer     ,                   intent(in)    :: I,INDIC,CORRG
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine FLUROE

   end interface

end module M_FLUROE_I
