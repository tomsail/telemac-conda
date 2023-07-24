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

module M_CALVAR_I
! *********************************************************************
! PROGICIEL : MASCARET         F. MAUREL    N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine CALVAR ( &
        QMIN  , QMAJ , &
        SMIN  , SMAJ , &
        BETA         , &
        XFRON        , &
        YNODE        , &
        FROUD        , &
        UNODE        , &
        CNODE        , &
        ZNODE        , &
        QNODE        , &
        SNODE        , &
        pas_bas      , &
        Pas_haut     , &      
        IFIGE        , &
        ZINIT        , &
        X            , &
        IDEB  , IFIN , &
        COTR         , &
        SectionPlan  , &
        DZ           , &
        T            , &
        NBBIEF       , &
        NMLARG       , &
        CALCOS       , &
        AVAL         , &
        Erreur         &
              )

!***********************************************************************
! FONCTION :
!            CALCUL DES DIFFERENTES VARIABLES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  QMIN     ! TR !  R !                                              !
! !  QMAJ     ! TR !  R !                                              !
! !  SMIN     ! TR !  R ! COEFFFICIENTS DE STRICKLER (AUX NOEUDS)      !
! !  SMAJ     ! TR !  R ! COEFFFICIENTS DE STRICKLER (AUX NOEUDS)      !
! !  BETA     ! TR !  R !                                              !
! !  XFRON    ! TR !  R ! POSITION DU FRONT D'ONDE DANS CHAQUE BIEF    !
! !  YNODE    ! TR !  R ! TIRANT D'EAU                                 !
! !  FROUD    ! TR !  R ! NOMBRE DE FROUDE                             !
! !  UNODE    ! TR !  R ! VITESSE                                      !
! !  UMIN     ! TR !  R !                                              !
! !  CNODE    ! TR !  R ! CELERITE DES ONDES                           !
! !  ZNODE    ! TR !  R ! COTE DE LA SURFACE LIBRE                     !
! !  QNODE    ! TR !  D ! DEBIT                                        !
! !  SNODE    ! TR !  D ! SURFACE MOUILLEE                             !
! !  ZINIT    ! TR !  D ! COTE DE LA SURFACE LIBRE A L'INSTANT INITIAL !
! !  X        ! TR !  D ! ABSCISSE DES SECTION DE CALCUL               !
! !  IDEB     ! TI !  D !                                              !
! !  IFIN     ! TI !  D !                                              !
! !  I1,I2    ! TI !  D ! INDICE DES EXTREMITES DE CHAQUE BIEF         !
! !  COTR     ! TR !  D ! COTE DU FOND                                 !
! !  SGEO     ! TR !  D !                                              !
! !  SGEO1    ! TR !  D !                                              !
! !  ALGEO    ! TR !  D ! LARGEUR AU MIROIR PLANIMETREE                !
! !  DEBGE1   ! TR !  D !                                              !
! !  DEBGE2   ! TR !  D !                                              !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  T        !  R !  D ! INSTANT  DU CALCUL                           !
! !  SEPS     ! TR !  D ! VALEUR MINIMALE DE LA SECTION MOUILLEE       !
! !  NBBIEF   !  I !  D ! Nombre de biefs                              !
! !  CALCOS   !    !  D !  LOGIQUE INDIQUANT SI ON CALCULE 1 OS        !
! !  AVAL     !    !  D !  LOGIQUE INDIQUANT SI ON CALCULE 1 OS        !
! !  NMLARG   !    !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  DELTAZ   !  R !  A !                                              !
! !  NRJ      !  R !  A ! ENERGIE CINETIQUE (=U*U/2G)                  !
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
!  SGEO, SGEO1, ALGEO, DEBGE1 et DEBGE2 font partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C
   use M_ERREUR_T       ! ERREUR
   use M_SECTION_PLAN_T ! Donnees hydrauliques aux sections
   use M_CELE_I      ! Interface de la fonction    CELE
   use M_CSURM1_I    ! Interface de la fonction    CSURM1
   use M_MINMAJ_I    ! Interface du sous-programme MINMAJ

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   Type (SECTION_PLAN_T)       ,intent(in)     :: SectionPlan
   real(DOUBLE), dimension(:)  , intent(  out) :: QMIN,QMAJ
   real(DOUBLE), dimension(:)  , intent(  out) :: SMIN,SMAJ
   real(DOUBLE), dimension(:)  , intent(  out) :: BETA
   real(DOUBLE), dimension(:)  , intent(inout) :: XFRON
   real(DOUBLE), dimension(:)  , intent(  out) :: YNODE
   real(DOUBLE), dimension(:)  , intent(  out) :: FROUD,UNODE
   real(DOUBLE), dimension(:)  , intent(  out) :: CNODE,ZNODE
   integer, dimension(:)  , intent(  out) :: pas_haut,pas_bas
   integer,dimension(:)   , intent(in   ) :: IFIGE
   real(DOUBLE), dimension(:)  , intent(in)    :: QNODE
   real(DOUBLE), dimension(:)  , intent(in)    :: SNODE
   real(DOUBLE), dimension(:)  , intent(in)    :: ZINIT
   real(DOUBLE), dimension(:)  , intent(in)    :: X
   integer     , dimension(:)  , intent(in)    :: IDEB,IFIN
   real(DOUBLE), dimension(:)  , intent(in)    :: COTR
   real(DOUBLE), dimension(:)  , intent(in)    :: DZ
   real(DOUBLE),                 intent(in)    :: T
   integer     ,                 intent(in)    :: NBBIEF
   logical     ,                 intent(in)    :: CALCOS
   logical     ,                 intent(in)    :: AVAL
   integer     ,                 intent(in)    :: NMLARG
   Type (ERREUR_T)             , intent(inout) :: ERREUR

   end subroutine CALVAR

   end interface

end module M_CALVAR_I
