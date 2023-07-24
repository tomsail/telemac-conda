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

module M_LIMITE_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL       F. LEPEINTRE
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine LIMITE ( &
             Y1      , &
             S1      , &
             Q1      , &
             NBSECT  , &
             ITYP    , &
             QFIX    , &
             YFIX    , &
             AMONT   , &
             DT      , &
             IS      , &
             NB      , &
             X       , &
             SNODE   , &
             CNODE   , &
             AKNODE  , &
             QNODE   , &
             UNODE   , &
             COTR    , &
             FRNODE  , &
             DZ      , &
             SGEO    , &
             ALGEO   , &
             DEBGEO  , &
             AKGEO   , &
             AIGEO   , &
             DYGEO   , &
             NMLARG  , &
         Impression  , & ! Flag d'impression
       UniteListing  , & ! Unite logique fichier listing
             Erreur    &
                 )

!***********************************************************************
!  FONCTION :
!  --------
!       CALCUL DES CONDITIONS AUX LIMITES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____._______________________________________________
! !    NOM    !TYPE!MODE!                   ROLE
! !___________!____!____!______________________________________________
! !  Y1       !  R !  R ! COTE A L AMONT                               !
! !  S1       !  R !  R ! SECTION A L AMONT                            !
! !  Q1       !  R !  R ! DEBIT A LAMONT                               !
! !  NBSECT   !  I !  D ! NOMBRE DE POINTS DU MAILLAGE                 !
! !  ITYP     !  I !    !                                              !
! !  QFIX     !  R !  D ! DEBIT IMPOSE                                 !
! !  YFIX     !  R !  D ! COTE IMPOSEE                                 !
! !  AMONT    !  L !    !                                              !
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  IS       !  I !    !                                              !
! !  NB       !  I !    !                                              !
! !  X        ! TR !  D ! ABSCISSE DES SECTIONS DE CALCUL              !
! !  SNODE    ! TR !  D ! SECTION MOUILLEE AU TEMPS N                  !
! !  CNODE    ! TR !  D ! CELERITE         AU TEMPS N                  !
! !  AKNODE   ! TR !  D ! K                AU TEMPS N                  !
! !  QNODE    ! TR !  D ! Debit            AU TEMPS N                  !
! !  UNODE    ! TR !  D ! VITESSE          AU TEMPS N                  !
! !  COTR     ! TR !    !                                              !
! !  FRNODE   ! TR !  D ! FROTTEMENT  AU TEMPS N                       !
! !  DZ       ! TR !    !                                              !
! !  SGEO     ! TR !  D ! SECTION PLANIMETREE                          !
! !  ALGEO    ! TR !  D ! LARGEUR PLANIMETREE                          !
! !  DEBGEO   ! TR !    !                                              !
! !  AKGEO    ! TR !  D !       K PLANIMETREE SUR LE MAILLAGE          !
! !  AIGEO    ! TR !  D !  AIGEO  PLANIMETREE SUR LE MAILLAGE          !
! !  DYGEO    ! TR !  D !  DYGEO  PLANIMETREE SUR LE MAILLAGE          !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! EPS6
   use M_ERREUR_T    ! ERREUR
   use M_AKIV_I      ! Interface de la fonction    AKIV
   use M_AKIVM1_I    ! Interface de la fonction    AKIVM1
   use M_CARAC_I     ! Interface de la fonction    CARAC
   use M_CARAC3_I    ! Interface du sous-programme CARAC3
   use M_CELE_I      ! Interface de la fonction    CELE
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_CSURM1_I    ! Interface de la fonction    CSURM1
   use M_FROTTE_I    ! Interface du sous-programme FROTTE

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(  out) :: Y1,S1,Q1
   integer     ,                   intent(in)    :: NBSECT,ITYP
   real(DOUBLE),                   intent(in)    :: QFIX,YFIX
   logical     ,                   intent(in)    :: AMONT
   real(DOUBLE),                   intent(in)    :: DT
   integer     ,                   intent(in)    :: IS,NB
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE), dimension(:)    , intent(inout) :: SNODE,CNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: AKNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: QNODE,UNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   real(DOUBLE), dimension(:)    , intent(  out) :: FRNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO, ALGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: DEBGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: AKGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: AIGEO,DYGEO
   integer     ,                   intent(in)    :: NMLARG
   logical                       , intent(in   ) :: Impression
   integer     ,                   intent(in   ) :: UniteListing
   type(ERREUR_T)                , intent(inout) :: Erreur

   end subroutine LIMITE

   end interface

end module M_LIMITE_I
