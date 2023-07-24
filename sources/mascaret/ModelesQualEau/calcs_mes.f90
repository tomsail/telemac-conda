!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

SUBROUTINE CALCS_MES( RNU , S , &
                      Nbsect , NBTRA  , Nbsing , &
                      Q , A , H , RH , ST , C , &
                      SA , T , TParph , DT )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION : MODELE MES ET SEDIMENTS
!  --------
!
! CE SOUS PROGRAMME CALCULE LES TERMES SOURCES IMPLICITES
! ET EXPLICITES, VOLUMIQUES ET SURFACIQUES,
! UTILISEES DANS L'EQUATION DE CONSERVATION DU TRACEUR
!
!       POUR UN PROBLEME DE TRANSPORT DE SEDIMENT PAR SUSPENSION
!       (!! SANS EVOLUTION DES FONDS !!)
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! ! NBTRA     ! E  ! M  ! NOMBRE DE TRACEURS                           !
! !  Q        ! TR ! D  ! DEBIT                                        !
! !  A        ! TR ! D  ! SECTION MOUILLEE                             !
! !  Z        ! TR ! D  ! HAUTEUR D EAU                                !
! !  RH       ! TR ! D  ! RAYON HYDRAULIQUE                            !
! !  ST       ! TR ! D  ! STRICKLER                                    !
! !  IM       ! E  ! M  ! NOMBRE DE SECTIONS DE CALCUL                 !
! !  C        ! TR ! D  ! CONCENTRATIONS                               !
! !  SVA      ! TR ! D  ! TERMES SOURCES VOLUMIQUE AJOUTES             !
! !  SSA      ! TR ! D  ! TERME SOURCE SURFACIQUE  AJOUTES             !
! !  T        !  R ! D  ! TEMPS                                        !
! !  DT       !  R ! D  ! PAS DE TEMPS                                 !
!  RESULTATS------------------------------------------------------------
! !  RNUV     ! TR ! D  ! TERMES SOURCES VOLUMIQUES IMPLICITES         !
! !  RNUS     ! TR ! D  ! TERME SOURCE SURFACIQUE IMPLICITES           !
! !  SV       ! TR ! D  ! TERMES SOURCES  EXPLICITES                   !
! !___________!____!____!______________________________________________!
!                               COMMON
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  NMSCAL   ! E  ! M  ! NOMBRE MAXIMUM DE SECTIONS DE CALCUL         !
! !  NMTRA    ! E  ! M  ! NOMBRE MAXIMUM DE TRACEURS                   !
! !___________!____!____!______________________________________________!
!                          VARIABLES INTERNES
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  J        ! TR ! D  ! PENTE DE LA LIGNE DE CHARGE                  !
! !  TAUB     ! TR ! D  ! TENSION DE CISAILLEMENT AU FOND              !
! !  ERO      ! R  !    ! TAUX D EROSION                               !
! !  TAUS     ! R  !    ! CONTRAINTE CRITIQUE DE REMISE EN SUSPENSION  !
! !  TAUR     ! R  !    ! CONTRAINTE CRITIQUE DE SEDIMENTATION         !
! !  VITCHU   ! R  !    ! VITESSE DE CHUTE DES MES                     !
! !  LAMBD    ! R  !    ! CONSTANTE DE DESINTEGRATION EXPONENTIELLE    !
! !  KD       ! R  !    ! COEFFICIENT DE DISTRIBUTION                  !
! !  KDESORP  ! R  !    ! CONSTANTE CINETIQUE DE DESORPTION            !
! !           !    !    !                                              !
! !  IF1      ! TR ! D  ! INDIC DE LECTURE DU FICHIER DES PARAMETRES   !
! !___________!____!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!***********************************************************************

   USE  M_PRECISION
   USE  M_CONSTANTES_TRACER_T
   USE  M_PARAMETRES_QUALITE_EAU_T

   IMPLICIT NONE

   REAL(DOUBLE)       , DIMENSION(:,:)  ,intent(inout) :: RNU , S , SA
   REAL(DOUBLE)       , DIMENSION(:)    ,intent(in   ) :: Q , A , H , ST , RH
   REAL(DOUBLE)       , DIMENSION(:,:)  ,intent(inout) :: C
   INTEGER                                             :: Nbsect , NBTRA , nbsing
   REAL(DOUBLE)                                        :: T , DT
   !
   !  DECLARATION DES PARAMETRES PHYSIQUES
   !
   type (PARAMETRES_QUALITE_EAU_T )                ::  TPARPH
   !
   !  VARIABLES LOCALES
   !
   REAL(DOUBLE), DIMENSION (Nbsect)        :: PJ , TAUB
   REAL(DOUBLE), DIMENSION (Nbsect)        :: SEDP , RS
   REAL(DOUBLE), DIMENSION (Nbsect,nbtra)  :: SS , SV
   REAL(DOUBLE) ERO , TAUR , TAUS , VITCHU , RO , GRAV
   INTEGER I , K
   INTEGER :: IF1 = 0
   !
   SAVE RO , GRAV , ERO , TAUR , TAUS , VITCHU , IF1
   !
   ! ----------------------------------------------------------------
   ! C1 : MATIERES EN SUSPENSION        MES  << kg/m3 >>
   ! C2 : SEDIMENTS DE FOND             SED  << kg/m2 >>
   !
   ! En commentaires dans la subroutine :
   ! C2 : SEDIMENTS DE FOND             SED  << kg/m >>
   ! ----------------------------------------------------------------
   ! -I- INITIALISATIONS
   !
   ! 1) AFFECTATIONS DES PARAMETRES PHYSIQUES
   !
   IF(IF1.EQ.0) THEN
      ERO    = TPARPH%ParQual_eau(1)
      TAUS   = TPARPH%ParQual_eau(2)
      TAUR   = TPARPH%ParQual_eau(3)
      VITCHU = TPARPH%ParQual_eau(4)
      !
      IF1  = 1
      RO   = 1000.D0
      GRAV = 9.81D0
   ENDIF
   !
   ! 2) CALCULS PRELIMINAIRES
   !
   DO I = 1 , Nbsect
      !
      !     Tension de cisaillement sur le fond
      !
      PJ(I)   = ( Q(I) / (ST(I) * A(I) * RH(I)**(2.D0/3.D0) ) )**2
      TAUB(I) = RO * GRAV * RH(I) * PJ(I)
      !
      !     Probabilite de depot (Flux de depot / conc en MES)
      !
      SEDP(I)= VITCHU * DMAX1 ( 1.D0 - TAUB(I) /TAUS , 0.D0 )
      !
      !     Flux d'erosion
      !
      RS(I) = DMIN1 ( ERO * DMAX1( TAUB(I) / TAUR - 1.D0 , 0.D0 ) , C(I,2) / DT )
      !
      ! Si les grandeurs relatives aux sediments sont exprimees en ../m
      !     RS(I) = MIN ( ERO * MAX(TAUB(I)/TAUR - 1.,0.) , C(I,2)/DT/(A(I)/H(I)) )
      !
   ENDDO
   !
   !----------------------------------------------------------------------
   ! -II- CALCUL DES TERMES SOURCES
   !
   DO I = 1 , Nbsect
      !
      ! TRACEUR 1 : [MES] Matieres en suspension
      !
      SV(I,1) = 0.D0
      SS(I,1) = RS(I) - SEDP(I) * C(I,1)
      !
      ! TRACEUR 2 : [SED] Sediments
      !
      SV(I,2) = - RS(I) + SEDP(I) * C(I,1)
      ! Si les grandeurs relatives aux sediments sont exprimees en ../m
      !      SV(I,2) = ( - RS(I) + SEDP(I) * C(I,1) ) * A(I)/H(I)
      SS(I,2) = 0.D0
   ENDDO
   !
   !----------------------------------------------------------------------
   ! -III- ASSEMBLAGE DES TERMES SOURCES
   !       (sources volumiques, surfaciques et ajoutees par l'utilisateur)
   !
   DO K = 1 , NBTRA
      DO I = 1 , nbsect
         S(I,K) = SV(I,K) + SS(I,K) / H(I) + SA(I,K)
         RNU(I,K) = 0.D0
      ENDDO
   ENDDO

   RETURN
END SUBROUTINE CALCS_MES
