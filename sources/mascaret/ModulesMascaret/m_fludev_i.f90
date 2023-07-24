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

module M_FLUDEV_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine FLUDEV(  &
             FLULOC  , &
             FLULOD  , &
             FLULOG  , &
             NSECD0  , &
             ITYP    , &
             IPOS    , &
             ZDEV    , &
             DEBIT   , &
     Epaisseur_Seuil , &
     Nb_Point        , &
     Loi_Debit       , &
     Loi_Cote        , &
             SD      , &
             ZD      , &
             PRD     , &
             SG      , &
             ZG      , &
             HG      , &
             PRG     , &
             QG      , &
             QD      , &
             DZ      , &
             ALGEO   , &
             COTR    , &
             DT      , &
             COEF    , &
             NMLARG  , &
             ERREUR    &
           )

!***********************************************************************
!   CODE MASCARET :  CALCUL DES FLUX DU AU DEVERSEMENT
!                    AU DESSUS D'UN BARRAGE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  FLULOC   ! TR !  A ! FLUX LOCAL  CALCULE PAR ROE                  !
! !  FLULOD   ! TR !  D ! FLUX SORTANT A DROITE DU BARRAGE             !
! !  FLULOG   ! TR !  D ! FLUX RENTRANT A GAUCHE DU BARRAGE            !
! !  NSECD0   ! TR !  D ! SECTION LIMITE DROITE                        !
! !  ITYP     !  I !  D ! TYPE DOUVRAGE                                !
! !  IPOS     !  I !  D ! POSITION DU BARRAGE                          !
! !  ZDEV     !  R !  D ! COTE DE DEVERSEMENT                          !
! !  SD       !  R !  M ! ETAT A DROITE DU BARRAGE                     !
! !  ZD       !  R !    !                                              !
! !  PRD      !  R !  M !         "                                    !
! !  SG       !  R !  M ! ETAT A GAUCHE DU BARRAGE                     !
! !  ZG       !  R !    !                                              !
! !  HG       !  R !  M !         "                                    !
! !  PRG      !  R !  M !         "                                    !
! !  QG       !  R !  M !         "                                    !
! !  QD       !  R !  M !                                              !
! !  DZ       ! TR !    !                                              !
! !  ALGEO    ! TR !  M ! LARGEUR         PLANIMETREE                  !
! !  COTR     ! TR !  M ! COTE DU FOND                                 !
! !  DT       ! TR !  M ! PAS DE TEMPS                                 !
! !  COEF     ! TR !  D ! COEFFICICIENT DE DEBIT                       !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!   ALGEO fait partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_INTERPOLATION_S  ! Interpolation
   use M_PARAMETRE_C ! EPS3
   use M_ERREUR_T    ! Type ERREUR_T
   use M_QSING_I     ! Interface de la fonction    QSING
   use M_ERODEV_I    ! Interface du sous-programme ERODEV

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM, 2nde dimension 2
   real(DOUBLE), dimension(:,:)  , intent(  out) :: FLULOC
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(  out) :: FLULOD,FLULOG
   integer     ,                   intent(inout) :: NSECD0
   integer     ,                   intent(inout) :: ITYP
   integer     ,                   intent(in)    :: IPOS
   integer     ,                   intent(in)    :: Epaisseur_Seuil
   real(DOUBLE),                   intent(inout) :: ZDEV
   real(DOUBLE),                   intent(inout) :: DEBIT
   real(DOUBLE),                   intent(in)    :: SD,ZD
   real(DOUBLE),                   intent(in)    :: PRD
   real(DOUBLE),                   intent(in)    :: SG,ZG,HG
   real(DOUBLE),                   intent(in)    :: PRG
   real(DOUBLE),                   intent(in)    :: QG,QD
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: ALGEO
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   ! Dimension Nb_Point
   real(DOUBLE),dimension(:)     , intent(in)    :: Loi_Debit,Loi_Cote
   real(DOUBLE),                   intent(in)    :: DT,COEF
   integer     ,                   intent(in)    :: NMLARG,Nb_Point
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine FLUDEV

   end interface

end module M_FLUDEV_I
