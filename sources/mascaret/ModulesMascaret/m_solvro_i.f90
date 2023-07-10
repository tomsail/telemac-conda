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

module M_SOLVRO_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine SOLVRO ( &
              SNODE  , &
              QNODE  , &
              UNODE  , &
              ZNODE  , &
              YNODE  , &
              FROUD  , &
       CNODE  , FLUX , &
            DebitFlux, &
       JGNODE,JDNODE , &
        IFIGE,KTEMPS , &
              BETA   , &
              QIN    , &
              X      , &
              SGEO   , &
              ALGEO  , &
              SGEOD  , &
              PRGEOD , &
              DEBGEO , &
              DEBGED , &
              S1GEO  , &
              COTR   , &
              DZD    , &
              DZ     , &
              DT     , &
              HEPS   , &
              NSECG  , &
              NSECD  , &
              NBARAD , &
              NBBAR  , &
         SINGULARITE , &
             PCSing  , &
             SPREC   , &
             QPREC   , &
              NBSECT , &
              FRTIMP , &
         Impli_Trans , &
 PerteElargissementTrans , &
          Boussinesq , &
                CQMV , &
              NMLARG , &
              ERREUR   &
                    )

!***********************************************************************
!   CODE MASCARET : RESOLUTION DES EQUATIONS DE SAINT VENANT
!                      PAR UN SCHEMA DE ROE
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
! !  QIN      ! TR !  D ! DEBIT D'APPORT                               !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  SGEO     ! TR !  D ! SURFACES PLANIMETREES                        !
! !  ALGEO    ! TR !  D ! LARGEUR PLANIMETREE                          !
! !  SGEOD    ! TR !    !                                              !
! !  PRGEOD   ! TR !  D ! PRESSION PLANIMETREE (MAILLAGE DECALE)       !
! !  DEBGEO   ! TR !    !                                              !
! !  DEBGED   ! TR !    !                                              !
! !  COTR     ! TR !  D ! COTE DU RADIER                               !
! !  DZD      ! TR !    !                                              !
! !  DZ       ! TR !    !                                              !
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  HEPS     !  R !    !                                              !
! !  SESP     !  R !    !                                              !
! !  NSECG    !  I !    !                                              !
! !  NSECD    !  I !    !                                              !
! !  NBARAD   !  I !    !                                              !
! !  NBBAR    !  I !    !                                              !
! !  IBAR     ! TI !    !                                              !
! !  ZDEV     ! TR !    !                                              !
! !  NBSECT   !  I !  A ! NOMBRE DE MAILLES                            !
! !  FRTIMP   !  L !  A ! INDIQUE SI LE FROTTEMENT EST IMPLICITE       !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  FLUX     ! TR !  A ! FLUX ASSEMBLE DANS CHAQUE CELLULE            !
! !  ZG       !  R !  A ! COTE SURFACE LIBRE CELLULE DE GAUCHE         !
! !  ZD       !  R !  A ! COTE SURFACE LIBRE CELLULE DE DROITE         !
! !  HG(D)    !  R !  A ! TIRANT D'EAU CELLULE DE GAUCHE (DROITE)      !
! !  PRG(D)   !  R !  A ! PRESSION CELLULE DE GAUCHE (DROITE)          !
! !  QG(D     !  R !  A ! VITESSE CELLULE DE GAUCHE (DROITE)           !
! !  SG(D)    !  R !  A ! SECTION MOUILLEE CELLULE DE GAUCHE (DROITE)  !
! !  UG(D     !  R !  A ! VITESSE CELLULE DE GAUCHE (DROITE)           !
! !  CG(D)    !  R !  A ! CELERITE CELLULE DE GAUCHE (DROITE)          !
! !  FRG(D)   !  R !  A ! FROUDE CELLULE DE GAUCHE (DROITE)            !
! !  UTILD    !  R !  A ! VITESE MOYENNE DE ROE                        !
! !  CTILD    !  R !  A ! CELERITE MOYENNE DE ROE                      !
! !  SDIFF    !  R !  A ! DIFFERENCE ENTRE SECTION DROITE ET GAUCHE    !
! !  LAMDA1   !  R !  A ! VALEUR PROPRE 1, UTILD-CTILD                 !
! !  LAMDA2   !  R !  A ! VALEUR PROPRE 2, UTILD+CTILD                 !
! !  FLULOC   ! TR !  A ! FLUX LOCAL A TRAVERS UNE INTERFACE           !
! !  T1       ! TR !  A ! VECTEUR PROPRE ASSOCIE A LA V.P 1            !
! !  T2       ! TR !  A ! VECTEUR PROPRE ASSOCIE A LA V.P 2            !
! !  TS1      ! TR !  A ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  TS2      ! TR !  A ! VECTEUR DE L'INVERSE DE LA MATRICE (T1,T2)-1 !
! !  SOTILD   ! TR !  A ! T. SOURCE  INTERFACE                         !
! !  SOPRIM   !  R !  A ! T. SOURCE (CENTRE DE LA CELLULE)             !
! !  FLUSOG   ! TR !  A ! FLUX DE T. SOURCE PARTIE GAUCHE DE LA CELLULE!
! !  FLUSOD   ! TR !  A ! FLUX DE T. SOURCE PARTIE DROITE DE LA CELLULE!
! !  FLUSOC   ! TR !  A ! FLUX DE T. SOURCE PARTIE CENTRE DE LA CELLULE!
! !  SOFROT   ! TR !  A ! FROTTEMENT INTERFACE                         !
! !  FLUFRG   ! TR !  A ! FLUX DE FROTTEMENT GAUCHE DE LA CELLULE      !
! !  FLUFRD   ! TR !  A ! FLUX DE FROTTEMENT DROITE DE LA CELLULE      !
! !  TIRANT   !  R !  A ! TIRANT D'EAU                                 !
! !  COEF     !  R !  A ! COEF DANS L'EQ 2EME DEGRE (CAS IMPLICITE)    !
! !  DELTA    !  R !  A ! DISCRIMINANT EQ 2EME DEGRE (CAS IMPLICITE)   !
! !  IVIDE    ! TI !  A ! INDICATEUR DE CELLULE GAUCHE VIDE            !
! !  INDIC    !  I !  A ! 0 SI LES 2 CEL. PLEINES, 1 SI CEL DROITE VIDE!
! !           !    !    ! 2 SI CEL. GAUCHE VIDE., 3 SI 2 CEL. VIDES    !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
! SGEO ALGEO SGEOD PRGEOD DEBGEO DEBGED STRUCTURE DE DONNEES SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS3, EPS6, EPSN6
   use M_ERREUR_T    ! ERREUR
   use M_SINGULARITE_T ! singularites
   use M_BISSN1_I    ! Interface du sous-Programme BISSN
   use M_CALETA_I    ! Interface du sous-programme CALETA
   use M_DEBITA_I    ! Interface du sous-programme DEBITA
   use M_MATRI_I     ! Interface du sous-Programme Matri
   use M_MATRIA_I    ! Interface du sous-Programme Matri
   use M_FLUDEV_I    ! Interface du sous-programme FLUDEV
   use M_FLUROE_I    ! Interface du sous-programme FLUROE
   use M_PRESD_I     ! Interface de la fonction    PRESD
   use M_SOURCE_I    ! Interface du sous-programme SOURCE
   use M_VVPROPI_I   ! Interface du sous-programme VVPROPI
   use M_VVPROP_I    ! Interface du sous-programme VVPROP
   use M_BOUSSI_I    !
   use M_DICHOM_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(inout) :: SNODE,QNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: UNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: ZNODE,YNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: FROUD,CNODE,BETA
   real(DOUBLE), dimension(:,:)  , intent(inout) :: FLUX
   real(DOUBLE), dimension(:)    , intent(inout) :: DebitFlux
   real(DOUBLE), dimension(:)    , intent(inout) :: SPREC,QPREC
   real(DOUBLE), dimension(:)    , intent(in)    :: QIN
   real(DOUBLE), dimension(:)    , intent(in)    :: PCSing
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: ALGEO
   real(DOUBLE), dimension(:,:)  , intent(in)     :: S1GEO
   ! 1ere dimension IM-1, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEOD
   real(DOUBLE), dimension(:,:)  , intent(in)    :: PRGEOD
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: DEBGEO
   ! 1ere dimension IM-1, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: DEBGED
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   ! 1ere dimension IM-1
   real(DOUBLE), dimension(:)    , intent(in)    :: DZD
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   real(DOUBLE),                   intent(in)    :: DT
   real(DOUBLE),                   intent(in)    :: HEPS
   integer     ,dimension(:)      ,intent(in)    :: JGNODE,JDNODE,IFIGE
   integer     ,                   intent(in)    :: NSECG,NSECD,KTEMPS
   integer     ,                   intent(in)    :: NBARAD,NBBAR
    ! Type singularite,
   type(SINGULARITE_T), dimension(:) , intent(inout) :: SINGULARITE
   integer     ,                   intent(in)    :: NBSECT
   logical     ,                   intent(in)    :: FRTIMP
   logical     ,                   intent(in)    :: Impli_Trans
   logical     ,                   intent(in)    :: PerteElargissementTrans
   logical     ,                   intent(in)    :: Boussinesq
   integer     ,                   intent(in)    :: CQMV
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)                ,intent(inout) :: ERREUR

   end subroutine SOLVRO

   end interface

end module M_SOLVRO_I
