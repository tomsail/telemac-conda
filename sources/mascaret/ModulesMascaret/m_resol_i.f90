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

module M_RESOL_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine RESOL ( &
        SNODE       , &
        QNODE       , &
        UNODE       , &
        ZNODE       , &
        YNODE       , &
        FROUD       , &
        CNODE       , &
        FLUX        , &
        DebitFlux   , &
 JGNODE,JDNODE,IFIGE, &
        BETA        , &
        SDROIT      , &
        QDROIT      , &
        SGAUCH      , &
        QGAUCH      , &
       QIN   , X    , &
                    !    SectionPlan , &
     SGEO,ALGEO,SGEOD,PRGEOD,DEBGEO,DEBGED, &
       S1GEO        , & 
                     !
        COTR        , &
        DZD         , &
        DZ          , &
        DT          , &
        DTI         , &
        ICOMPT      , &
      HEPS  , SEPS  , &
        NSECG       , &
        NSECD       , &
        NBARAD      , &
        NBBAR       , &
        Singularite , &
        PCSing      , &
        SPREC       , &
        QPREC       , &
        NMLARG      , &
        NBSECT      , &
        FRTIMP      , &
        Impli_Trans , &
 PerteElargissementTrans , &
        Boussinesq  , &
        CQMV        , &
       ERREUR        &
                   )

!***********************************************************************
!   CODE MASCARET : CALCUL DES FLUX DE ROE 
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SNODE    ! TR !  M ! SURFACE MOUILLEE                             !
! !  QNODE    ! TR !  M ! DEBIT                                        !
! !  UNODE    ! TR !  M ! VITESSE                                      !
! !  ZNODE    ! TR !    !                                              !
! !  YNODE    ! TR !    !                                              !
! !  FROUD    ! TR !    !                                              !
! !  CNODE    ! TR !    !                                              !
! !  BETA     ! TR !    !                                              !
! !  SDROIT   !  R !  D ! SURFACE MOUILLE TN+1 CELLULE AMONT           !
! !  QDROIT   !  R !  D ! DEBIT TN+1 CELLULE AMONT                     !
! !  SGAUCH   !  R !  D ! SURFACE MOUILLE TN+1 CELLULE AVAL            !
! !  QGAUCH   !  R !  D ! DEBIT TN+1 CELLULE AVAL                      !
! !  QIN      ! TR !  D ! DEBIT D'APPORT                               !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  SGEO     ! TR !  D ! SURFACES PLANIMETREES                        !
! !  ALGEO    ! TR !  D ! LARGEUR PLANIMETREE                          !
! !  PRGEOD   ! TR !  D ! PRESSION PLANIMETREE (MAILLAGE DECALE)       !
! !  SGEOD    ! TR !  D ! SURFACES PLANIMETREES(MAILLAGE DECALE)       !
! !  DEBGEO   ! TR !    !                                              !
! !  DEBGED   ! TR !    !                                              !
! !  COTR     ! TR !  D ! COTE DU RADIER                               !
! !  DZD      ! TR !    !                                              !
! !  DZ       ! TR !    !                                              !
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  HEPS     !  R !  D ! VALEUR MINIMALE DU TIRANT D'EAU              !
! !  SEPS     !  R !  D ! VALEUR MINIMALE DE LA SURFACE                !
! !  NSECG    !  I !  D ! NUMERO DE LA SECTION GAUCHE DU BIEF A TRAITER!
! !  NSECD    !  I !  D ! NUMERO DE LA SECTION DROITE DU BIEF A TRAITER!
! !  NBARAD   !  I !    !                                              !
! !  NBBAR    !  I !    !                                              !
! !  IBAR     ! TI !    !                                              !
! !  ZDEV     ! TR !    !                                              !
! !  NMLARG   !  I !  D ! NOMBRE DE PAS DE PLANIMETRAGE                !
! !  NBSECT   !  I !  D ! NOMBRE DE POINTS DU MAILLAGE                 !
! !  FRTIMP   !  I !  D ! INDIQUE SI ON IMPLICITE LE FROTTEMENT        !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
! ALGEO, PRGEOD, SGEOD, DEBGEO, DEBGED STRUCTURES DE DONNEEES

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! ERREUR
   use M_SECTION_PLAN_T  ! Type SECTION_PLAN_T
   use M_SINGULARITE_T   ! Type SINGULARITE_T
   use M_CLIPP_I   ! Interface du sous-programme CLIPP
   use M_SOLVRO_I  ! Interface du sous-programme SOLVRO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(inout) :: SNODE,QNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: UNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: ZNODE,YNODE,FROUD
   real(DOUBLE), dimension(:)    , intent(in)    :: CNODE,DTI
   integer     , dimension(:)    , intent(in)    :: JGNODE,JDNODE,IFIGE
   real(DOUBLE), dimension(:,:)  , intent(inout) :: FLUX
   real(DOUBLE), dimension(:)    , intent(inout) :: DebitFlux
   real(DOUBLE), dimension(:)    , intent(inout) :: SPREC,QPREC
   real(DOUBLE), dimension(:)    , intent(in)    :: BETA
   real(DOUBLE),                   intent(in)    :: SDROIT,QDROIT,SGAUCH,QGAUCH
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: QIN
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   real(DOUBLE), dimension(:)    , intent(in)    :: PCSing
   ! 1ere dimension IM-1
   real(DOUBLE), dimension(:)    , intent(in)    :: DZD
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   real(DOUBLE),                   intent(in)    :: DT,HEPS,SEPS
   integer     ,                   intent(in)    :: NSECG,NSECD
   integer     ,                   intent(in)    :: NBARAD,NBBAR
   integer     ,                   intent(inout) :: ICOMPT
   type(Singularite_T), dimension(:),intent(inout) :: SINGULARITE
   integer     ,                   intent(in)    :: NMLARG
   integer     ,                   intent(in)    :: NBSECT
   logical     ,                   intent(in)    :: FRTIMP
   logical     ,                   intent(in)    :: Impli_Trans
   logical     ,                   intent(in)    :: PerteElargissementTrans
   logical     ,                   intent(in)    :: Boussinesq
   integer     ,                   intent(in)    :: CQMV
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  ,intent(in)     :: SGEO,S1GEO
   real(DOUBLE), dimension(:,:)  ,intent(in)    :: ALGEO
   ! 1ere dimension IM-1, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  ,intent(in)   :: PRGEOD,SGEOD
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  ,intent(in)   :: DEBGEO
   ! 1ere dimension IM-1
   real(DOUBLE), dimension(:,:)  ,intent(in)   :: DEBGED
   !  Type(SECTION_PLAN_T)          , intent(in   ) :: SectionPlan
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine RESOL

   end interface

end module M_RESOL_I
