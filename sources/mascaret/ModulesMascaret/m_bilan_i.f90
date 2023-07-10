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

module M_BILAN_I
! *********************************************************************
! PROGICIEL : MASCARET         F. MAUREL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine BILAN ( &
        H2OIB       , &
        H2OTB       , &
        H2OEB       , &
        H2OSB       , &
        H2OIBS      , &
        H2OTBS      , &
        CONSB       , &
        H2OIC       , &
        H2OTC       , &
        H2OEC       , &
        H2OSC       , &
        CONSC       , &
        H2OIG       , &
        H2OTG       , &
        H2OEG       , &
        H2OSG       , &
        H2OIGS      , &
        H2OTGS      , &
        CONSG       , &
        SNODE, VOLS , &
        QNODE       , &
        QIN , X     , &
        Connect     , &
        NbExtremite , &
        W , AIRS    , &
        Confluent   , &
        IDEB , IFIN , &
        INDIC,INDICO, &
        XBAR, ZBAR  , &
        DT          , &
        ITEMP       , &
        Phase_post_imp, NPMAX     , &
        T,NP,TMAX   , &
        NBBIEF, NBNOEU            , &
        Impression  , UniteListing, &
        CALCOS      , &
        STOCKAGE    , &
        Erreur        & ! Erreur
                  )

!***********************************************************************
!  FONCTION :
!  --------
!
!       CALCUL DES BILANS DE MASSE DE LIQUIDE, 
!       ET DONC DE LA CONSERVATIVITE DANS CHAQUE BIEF ET GLOBALEMENT
!
!-----------------------------------------------------------------------
!
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !H2OI(B,C,G)!T R !  R ! MASSE D'EAU INITIALE (BIEF,CONFLUENT,GLOBAL) !
! !H2OT(B,S,G)!T R !  R ! MASSE D'EAU TOTALE   (BIEF,CONFLUENT,GLOBAL) !
! !H2OE(B,C,G)!T R !  R ! MASSE D'EAU ENTREE   (BIEF,CONFLUENT,GLOBAL) !
! !H2OS(B,S,G)!T R !  R ! MASSE D'EAU SORTIE   (BIEF,CONFLUENT,GLOBAL) !
! !CONS(B,C,G)!T R !  R ! CONSERVATIVITE (BIEF,CONFLUENT,GLOBAL)       !
! !H2OI(B,G)S !T R !  R !                                              !
! !H2OT(B,G)S !T R !  R !                                              !
! !  SNODE    ! TR !  D ! SURFACE MOUILLEE                             !
! !  VOLS     ! TR !  D !                                              !
! !  QNODE    ! TR !  D ! DEBIT                                        !
! !  QIN      ! TR !  D ! DEBIT D'APPORT                               !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  W        ! TR !  D ! VARAIBLE D'EATAT DANS LE CONFLUENT           !
! !  AIRS     ! TR !  D ! SURFACE DES CELLULES DU MAILLAGE CONFLUENT   !
! !  FINBIE   ! TI !  D ! INDICATEUR DE DEBUT ET FIN DE BIEF           !
! !  ISEC     ! TI !  D ! NUMERO DE SECTION LIMITE A UN CONFLUENT      !
! ! IDEB,IFIN ! TI !  D ! LIMITES DE LA ZONE DE CALCUL PAR BIEF        !
! !  INDIC    ! TI !  D ! INDICATEUR DE CALCUL PAR BIEF                !
! !  INDICO   ! TI !  D ! INDICATEUR DE CALCUL PAR CONFLUENT           !
! !  XBAR     !  R !  D ! POSITION DU BARRAGE                          !
! !  ZBAR     !  R !  D ! COTE DE LA SURFACE LIBRE AU BARRAGE          !
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  ITEMP    !  I !  D ! NUMERO DU PAS DE TEMPS                       !
! !  NPMAX    !  I !  D ! NOMBRE DE PAS DE TEMPS MAXIMAL               !
! !  T        !  R !  D ! TEMPS                                        !
! !  TMAX     !  R !  D ! TEMPS MAXIMAL                                !
! !  NBBIEF   !  I !  D ! NOMBRE DE BIEFS                              !
! !  NBNOEU   !  I !  D ! NOMBRE DE CONFLUENCES                        !
! !  Impression     !  L !  D ! LOGIQUE D'IMPRESSION DU BILAN DE MASSE !
! !  CALCOS   !  L !  D ! LOGIQUE DE CALCUL ONDE DE SUBMERSION         !
! !  STOCKAGE !  L !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  H2OB     !  R !  A ! VOLUME DU BARRAGE QUI CASSE                  !
! !  H2OTD    !  R !  A ! VOLUME D'EAU PRESENT LE DOMAINE DE CALCUL    !
! !  H2OXXM   !  R !  A ! VALEUR DE H2OXX A L'INSTANT PRECEDENT        !
! !___________!____!____!______________________________________________!
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
   use M_CONNECT_T ! Type CONNECT
   use M_ERREUR_T  ! ERREUR
   use M_CONFLUENT_T ! Type Confluent
   use M_CONSTANTES_CALCUL_C ! phase du calcul

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE), dimension(:)    , intent(  out) :: H2OIB,H2OTB,H2OEB,H2OSB
   real(DOUBLE), dimension(:)    , intent(  out) :: H2OIBS,H2OTBS
   real(DOUBLE), dimension(:)    , intent(  out) :: CONSB
   real(DOUBLE), dimension(:)    , intent(  out) :: H2OIC,H2OTC,H2OEC,H2OSC
   real(DOUBLE), dimension(:)    , intent(  out) :: CONSC
   real(DOUBLE),                   intent(  out) :: H2OIG,H2OTG,H2OEG,H2OSG
   real(DOUBLE),                   intent(  out) :: H2OIGS,H2OTGS
   real(DOUBLE),                   intent(  out) :: CONSG
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: SNODE,VOLS,QNODE,QIN,X
   type(CONNECT_T),                intent(in)    :: Connect
   integer        ,                intent(in)    :: NbExtremite
   type (CONFLUENT_T),dimension(:),             intent(in)    :: Confluent
   ! 2nde dimension 12
   ! 1ere dimension  3 (pour W)
   real(DOUBLE), dimension(:,:,:), intent(in)    :: W
   real(DOUBLE), dimension(:,:)  , intent(in)    :: AIRS
   integer     , dimension(:)    , intent(in)    :: IDEB,IFIN,INDIC,INDICO
   real(DOUBLE),                   intent(in)    :: XBAR,ZBAR
   real(DOUBLE),                   intent(in)    :: DT
   integer     ,                   intent(in)    :: ITEMP,NPMAX
   real(DOUBLE),                   intent(in)    :: T,TMAX
   integer     ,                   intent(in)    :: NBBIEF,NBNOEU
   logical     ,                   intent(in)    :: Impression
   integer     ,                   intent(in)    :: UniteListing
   integer     ,                   intent(in)    :: Phase_post_imp
   logical     ,                   intent(in)    :: CALCOS,STOCKAGE
   integer     ,                   intent(in)    :: NP
   Type (ERREUR_T)               , intent(inout) :: Erreur

   end subroutine BILAN

   end interface

end module M_BILAN_I
