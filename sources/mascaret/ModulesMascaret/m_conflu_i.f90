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

module M_CONFLU_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine CONFLU ( &
            S1       , &
            Q1       , &
            W        , &
            AIRS     , &
           Confluent , &
             X1D     , &
             QNODE   , &
             SNODE   , &
             ZNODE   , &
             COTR    , &
             ST      , &
             DZ      , &
             SGEO    , &
             ICONF   , &
             ITEMP   , &
             DT      , &
             EPS     , &
             NMLARG  , &
             ERREUR    &
               )

!***********************************************************************
!
!   CODE MASCARET : CALCUL  DES CONDITIONS LIMITES 1D AUX EXTREMITES
!                           D'UN CONFLUENT PAR UN COUPLAGE ACVEC UN
!                           GEOMETRIE 2D SIMPLIFIEE
!
!                    SOUS PROGRAMME APPELANT : CALCUL
!                    SOUS PROGRAMME APPELLE  : INIGEO
!                                              CFL2D1
!                                              CALCON
!                                              CFL2D
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  S1,Q1    ! TR !  R ! ETAT LIMITE RECHERCHE                        !
! !  W        ! TR !  M ! VARIABLE D'ETAT DANS LE CONFLUENT            !
! !  AIRS     ! TR !  D ! SURFACE DES CELLULES 2D                      !
! !XCONF,YCONF! TR !  D ! COORDONEES DES EXTREMEITES DU CONFLUENT      !
! !  TETACO   ! TR !  D ! DIRECTION DE CHAQUE BIEF AU CONFLUENT        !
! !  X1D      ! TR !  D ! COORDONEES MAILLAGE 1D                       !
! !  QNODE    ! TR !  D ! DEBIT (1D)                                   !
! !  SNODE    ! TR !  D ! SURFACE MOUILLEE (1D)                        !
! !  ZNODE    ! TR !  D ! COTE DE LA SURFACE LIBRE (1D)                !
! !  COTR     ! TR !  D ! COTE DU FOND (1D)                            !
! !  ST       ! TR !  D ! COEFF. DE STRICKLER (1D)                     !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  SGEO     ! TR !  D ! SURFACE MOUILLEE PLANIMETREE                 !
! !  ISEC     ! TI !  D ! NUMERO SECTION CALCUL 1D LIMITE A UN CONF    !
! !  ISECVO   ! TI !  D ! NUMERO DE SA VOISINE                         !
! !  FINBIE   ! TI !  D ! INDICATEUR DE DEBUT ET FIN DE BIEF           !
! !  ICONF    !  I !  D ! NUMERO DU CONFLUENT                          !
! !  ITEMP    !  I !  D !                                              !
! !  DT       !  R !  D ! PAS DE TEMPS (1D)                            !
! !  EPS      !  R !  D ! HAUTEUR D'EAU MINIMALE                       !
! !  NMLARG   !    !  D !                                              !
! !___________!____!____!______________________________________________!  
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.  
! !  DT2D     !  R !  A ! PAS DE TEMPS DANS LE MODELE 2D               !
! !  T2D      !  R !  A ! TEMPS ECOULE DANS LE MODELE 2D               !
! !  BXY      ! TR !  A ! BARYCENTRE DES CELLULES 2D                   !
! !  NELMIN   ! TI !  A ! TABLEAU DES CELLULES FONCTION DES SEGMENTS   !
! !  VNOIN    ! TR !  A ! VECTEUR NORMAL AUX CELLULES INTERIEURS       !
! !  AIRSM1   ! TR !  A ! SURFACE DES CELLULES 2D AU PDT PRECEDENT     !
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
   use M_ERREUR_T    ! ERREUR
   use M_CONFLUENT_T ! confluent
   use M_CONSTANTES_CALCUL_C ! Phase du calcul
   use M_CALCON_I  ! Interface du sous-programme CALCON
   use M_CFL2D_I   ! Interface du sous-programme CFL2D
   use M_CFL2D1_I  ! Interface du sous-programme CFL2D1
   use M_CSUR_I    ! Interface de la fonction    CSUR
   use M_INIGEO_I  ! Interface du sous-programme INIGEO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(  out) :: S1,Q1
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:)  , intent(inout) :: W
   ! 1ere dimension 12
   real(DOUBLE), dimension(:)    , intent(inout) :: AIRS
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: X1D,QNODE,SNODE,ZNODE,COTR
   real(DOUBLE), dimension(:)    , intent(in)    :: ST,DZ
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   ! 1ere dimension 3
   integer     ,                   intent(in)    :: ICONF,ITEMP
   real(DOUBLE),                   intent(in)    :: DT,EPS
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)                ,intent(inout) :: ERREUR
   Type (CONFLUENT_T), dimension(:) ,intent(in   ) :: Confluent

   end subroutine CONFLU

   end interface

end module M_CONFLU_I
