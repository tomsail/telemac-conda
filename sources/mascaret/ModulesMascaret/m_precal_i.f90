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

module M_PRECAL_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine PRECAL ( &
        SNODE        , &
        SMIN  , SMAJ , &
        YNODE , FROUD, &
        VOLS  , UNODE, &
        CNODE , ZNODE, &
        QNODE        , &
       JGNODE,JDNODE , &
        QMIN  , QMAJ , &
        BETA  , ZINIT, &
        XFRON,         &
        IBARP        , &
        confluent ,    &
        W     , AIRS , &
        QVRAI , SVRAI, &
        UVRAI , YVRAI, &
        ZVRAI        , &
        X            , &
        I1    , I2   , &
        NBBIEF,NBNOEU, &
        NBSECT       , &
        NMLARG       , &
        DZ           , &
        COTR  ,        &
        sectionPlan,   &
        REP   , SUITE, &
        SEPS,          &
        CALCOS,CALCVA, &
        STOCKAGE,AVAL, &
        Erreur         &
                  )
!***********************************************************************
! FONCTION : PREPARATION DU CALCUL,
!            INITIALISATION DES DIFFERENTES VARIABLES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SNODE    ! TR !  R ! SURFACE MOUILLEE                             !
! !  SMIN     ! TR !  D ! COEFFFICIENTS DE STRICKLER (AUX NOEUDS)      !
! !  SMAJ     ! TR !  D !                                              !
! !  YNODE    ! TR !  R ! TIRANT D'EAU                                 !
! !  FROUD    ! TR !  R ! NOMBRE DE FROUDE                             !
! !  VOLS     ! TR !    !                                              !
! !  UNODE    ! TR !  R ! VITESSE                                      !
! !  CNODE    ! TR !  R ! CELERITE DES ONDES                           !
! !  ZNODE    ! TR !  D ! COTE DE LA SURFACE LIBRE                     !
! !  QNODE    ! TR !  D ! DEBIT                                        !
! !  QMIN     ! TR !    !                                              !
! !  QMAJ     ! TR !    !                                              !
! !  BETA     ! TR !    !                                              !
! !  ZINIT    ! TR !  R ! COTE DE LA SURFACE LIBRE A L'INSTANT INITIAL !
! !  QMAX     ! TR !  R ! DEBIT MAXIMAL                                !
! !  TQMAX    ! TR !  R ! INSTANT OU LE DEBIT MAXIMAL EST OBSERVE      !
! !  ZMAX     ! TR !  R ! COTE DE LA SURFACE LIBRE MAXIMALE            !
! !  TZMAX    ! TR !  R ! INSTANT OU ZMAX EST OBSERVE                  !
! !  UTZMAX   ! TR !  R ! VITESSE LORSQU'ON ATTEINT LA HAUTEUR MAXIMALE!
! !  NRJMAX   ! TR !  R ! ENERGIE CINETIQUE MAXIMALE                   !
! !  TFRON    ! TR !  R ! TEMPS DE L'ARRIVEE DE L'ONDE                 !
! !  XFRON    ! TR !  R ! POSITION DU FRONT D'ONDE DANS CHAQUE BIEF    !
! !  XBARP    !  R !  D ! POSITION DU BARRAGE PRINCIPAL                !
! !  W        ! TR !  D ! VARIABLE D'ETAT DANS LE CONFLUENT            !
! !  AIRS     ! TR !  D ! SURFACE DES CELLULES 2D                      !
! !XCONF,YCONF! TR !  D ! COORDONEES DES EXTREMEITES DU CONFLUENT      !
! !  TETACO   ! TR !  D ! DIRECTION DE CHAQUE BIEF AU CONFLUENT        !
! !  ISEC     ! TI !  D ! NUMERO SECTION CALCUL 1D LIMITE A UN CONF    !
! !  ISECVO   ! TI !  D ! NUMERO DE SA VOISINE                         !
! !  FINBIE   ! TI !  D ! INDICATEUR DE DEBUT ET FIN DE BIEF           !
! !  QVRAI    ! TR !  R ! DEBIT VRAI LORS DE LA VALIDATION             !
! !  SVRAI    ! TR !  R ! SURFACE VRAIE LORS DE LA VALIDATION          !
! !  UVRAI    ! TR !  R ! VITESSE VRAIE LORS DE LA VALIDATION          !
! !  YVRAI    ! TR !  R ! TIRANT D'EAU VRAI LORS DE LA VALIDATION      !
! !  ZVRAI    ! TR !  R ! SURFACE LIBRE VRAIE LORS DE LA VALIDATION    !
! !  X        ! TR !  D ! ABSCISSE DES SECTION DE CALCUL               !
! !  I1,I2    ! TI !  D ! INDICE DES EXTREMITES DE CHAQUE BIEF         !
! !  NBBIEF   !  I !  A ! NOMBRE DE BIEF                               !
! !  NBNOEU   !  I !  A ! NOMBRE DE NOEUD                              !
! !  NBSECT   !  I !    !                                              !
! !  COTR     ! TR !  D ! COTE DU FOND                                 !
! !  SGEO     ! TR !    !                                              !
! !  SGEO1    ! TR !    !                                              !
! !  SSGEO    ! TR !    !                                              !
! !  ALGEO    ! TR !  D ! LARGEUR AU MIROIR PLANIMETREE                !
! !  DEBGE1   ! TR !    !                                              !
! !  DEBGE2   ! TR !    !                                              !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  TINIT    !  R !  D ! INSTANT INITIAL DU CALCUL                    !
! !  REP      !  L !    !                                              !
! !  SUITE    !  L !    !                                              !
! !  SEPS     !  R !  D ! VALEUR MINIMALE DE LA SECTION MOUILLEE       !
! !  CALCOS   !  L !  A ! CALCUL D'une OS                              !
! !  CALCVA   !  L !  A ! CALCUL D'UNE SOL. ANAL                       !
! !  STOCKAGE !  L !  A !                                              !
! !  AVAL     !  L !  A ! CALCUL AVAL d'UNE OS                         !
! !  NFREPL   !  I !    !                                              !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  NOEUD    !  I !  A ! NUMERO DU NOEUD COURANT                      !
! !  IBIEF    !  I !  A ! NUMERO DU BIEF COURANT                       !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!************************************************************************* 
!  SGEO, SGEO1, SSGEO,ALGEO,DEBGE1,DEBGE2 font partie d'une structure 
!             de donnees  STRUCTURE_SECTION

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C
   use M_SECTION_PLAN_T 
   use M_CONFLUENT_T 
   use M_ERREUR_T    ! Definition du type d'erreur
   use M_CELE_I      ! Interface de la fonction    CELE
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_INIGEO_I    ! Interface du sous-programme INIGEO
   use M_MINMAJ_I    ! Interface du sous-programme MINMAJ
   use M_DICHO_I
   use M_TRAITER_ERREUR_I ! Interface de traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   type (SECTION_PLAN_T)                ,intent(in) :: SECTIONPLAN
   type (confluent_T)     ,dimension(:), intent(in) :: CONFLUENT
   real(DOUBLE), dimension(:)    , intent(  out) :: SNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: SMIN,SMAJ
   real(DOUBLE), dimension(:)    , intent(  out) :: YNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: FROUD,VOLS,UNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: CNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: ZNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: QNODE
   integer     , dimension(:)    , intent(  out) :: JGNODE,JDNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: QMIN,QMAJ,BETA
   real(DOUBLE), dimension(:)    , intent(inout) :: ZINIT
   real(DOUBLE), dimension(:)    , intent(  out) :: XFRON
   integer      ,                   intent(in)   :: IBARP
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:,:), intent(  out) :: W
   ! 1ere dimension 12
   real(DOUBLE), dimension(:,:)  , intent(  out) :: AIRS
   real(DOUBLE), dimension(:)    , intent(  out) :: QVRAI,SVRAI,UVRAI,YVRAI,ZVRAI
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   integer     , dimension(:)    , intent(in)    :: I1,I2
   integer     ,                   intent(in)    :: NBBIEF,NBNOEU
   integer     ,                   intent(in)    :: NBSECT
   integer     ,                   intent(in)    :: NMLARG
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   real(DOUBLE) ,dimension(:)    , intent(in)    :: DZ
   real(DOUBLE)                  , intent(in)    :: SEPS
   logical     ,                   intent(in)    :: REP,SUITE
   logical     ,                   intent(in)    :: CALCOS,CALCVA,STOCKAGE,AVAL
   type (erreur_T)               , intent(inout) :: Erreur

   end subroutine PRECAL

   end interface

end module M_PRECAL_I
