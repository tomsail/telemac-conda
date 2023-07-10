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

module M_CALCON_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
   interface

   subroutine CALCON ( &
        W            , &
        NSEGP        , &
        NSEGR        , &
        AIRS         , &
        BXY          , &
        ZF           , &
        ST2D         , &
        NELMIN       , &
        NFRELM       , &
        VNOIN        , &
        VNOFR        , &
        DT           , &
        EPS          , &
       Erreur         &
                  )

!***********************************************************************
!   CODE MASCARET : RESOLUTION DE SAINT VENANT-2D PAR UNE METHODE VOLUME FINI
!                   (SOLVEUR DE ROE) POUR LE CALCUL DES CONFLUENTS
!                   PRISE EN COMPTE DU FROTTEMENT DE MANIERE IMPLICITE
!
!                  SOUS PROGRAMME APPELANT : CONFLU
!                  SOUS PROGRAMME APPELE   : FLUSRC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  W        ! TR !  M ! VARIABLE D'ETAT DANS LE CONFLUENT            !
! !  NSEGP    !  I !  M ! NOMBRE DE SEGMENT TOTAL                      !
! !  NSEGR    !  I !  M ! NOMBRE DE SEGMENT INTERIEURS                 !
! !  AIRS     ! TR !  D ! SURFACE DES CELLULES 2D                      !
! !  BXY      ! TR !  D ! BARYCENTRE DES CELLULES 2D                   !
! !  ZF       ! TR !  D ! COTE DU FOND DES CELLULES 2D                 !
! !  ST2D     !  R !  D ! STRICKLER UNIQUE DANS LE 2D                  !
! !  NELMIN   ! TI !  D ! TABLEAU DES CELLULES FONCTION DES SEGMENTS   ! 
! !  NFRELM   ! TI !  D ! CELLULE FONCTION DU SEGMENT FRONTIERE        !
! !  VNOIN    ! TR !  D ! VECTEUR NORMAL AUX CELLULES INTERIEURS       !
! !  VNOFR    ! TR !  D ! VECTEUR NORMAL AUX CELLULES FRONTIERES       !
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  EPS      !  R !  D ! HAUTEUR D'EAU MINIMALE                       !
! !___________!____!____!______________________________________________!  
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.  
! !  FLUX     ! TR !  A ! FLUX ASSEMBLES                               !
! !  FLULOC   ! TR !  A ! FLUX DE ROE                                  !
! !  FLUSCE   ! TR !  A ! FLUX LIES AUX TERMES SOURCES                 !
! !  T,TW,TR  ! TR !  A ! VECTEURS PROPRES                             !
! !  RLAMBX   !  R !  A ! VALEUR PROPRE                                !
! !  KFROT    !  R !  A ! COEFFICIENT POUR CALCUL DU FROTTMEENT        !
! !  DELTA    !  R !  A ! DISCRIMANT EQ 2EME DEGRE SUR FROTTMENT       !
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
   use M_PARAMETRE_C ! GPES, EPS8
   use M_ERREUR_T    ! ERREUR
   use M_FLUSRC_I    ! Interface du sous-programme FLUSRC

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   ! .. Arguments ..
   !----------------
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:), intent(inout) :: W
   integer     ,                 intent(inout) :: NSEGP,NSEGR
   ! 1ere dimension 12 (2nde dimension 3 pour BXY)
   real(DOUBLE), dimension(:)  , intent(in)    :: AIRS
   real(DOUBLE), dimension(:,:), intent(in)    :: BXY
   real(DOUBLE), dimension(:)  , intent(in)    :: ZF
   real(DOUBLE),                 intent(in)    :: ST2D
   ! 1ere dimension 12, 2nde dimension 2
   integer     , dimension(:,:), intent(in)    :: NELMIN
   ! 1ere dimension 18
   integer     , dimension(:)  , intent(in)    :: NFRELM
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:), intent(in)    :: VNOIN
   ! 1ere dimension 3, 2nde dimension 18
   real(DOUBLE), dimension(:,:), intent(in)    :: VNOFR
   real(DOUBLE),                 intent(in)    :: DT,EPS
   Type (ERREUR_T)             , intent(inout) :: ERREUR

   end subroutine CALCON

   end interface

end module M_CALCON_I
