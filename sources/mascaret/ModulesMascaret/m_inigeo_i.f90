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

module M_INIGEO_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine INIGEO ( &
             AIRS    , &
             BXY     , &
             ZF      , &
             NELMIN  , &
             NFRELM  , &
             NSEGP   , &
             NSEGR   , &
             VNOIN   , &
             VNOFR   , &
             XCONF   , &
             YCONF   , &
             TETACO  , &
             SNODE   , &
             ZNODE   , &
             COTR    , &
             X1D     , &
             DZ      , &
             SGEO    , &
             ISEC    , &
             ISECVO  , &
             ICONF   , &
             NMLARG  , &
             ERREUR    &
             )

!***********************************************************************
!
!   CODE MASCARET : CREATION DU MAILLAGE 2D DANS LE CONFLUENT
!                   (6 TRIANGLES + 6 RECTANGLES)
!                   EN TENANT COMPTE DE L'ETAT DE LA RIVIERE
!
!                   SOUS PROGRAMME APPELANT : PRECAL
!                                             CONFLU
!                   SOUS PROGRAMME APPELE   :
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  AIRS     ! TR !  R ! SURFACE DES CELLULES 2D                      !
! !  BXY      ! TR !  R ! BARYCENTRE DES CELLULES 2D                   !
! !  ZF       ! TR !  R ! COTE DU FOND DES CELLULES 2D                 !
! !  NELMIN   ! TI !  R ! TABLEAU DES CELLULES FONCTION DES SEGMENTS   !
! !  NFRELM   ! TI !  R ! CELLULE FONCTION DU SEGMENT FRONTIERE        !
! !  NSEGP    !  I !  R ! NOMBRE DE SEGMENT TOTAL                      !
! !  NSEGR    !  I !  R ! NOMBRE DE SEGMENT INTERIEURS                 !
! !  VNOIN    ! TR !  R ! VECTEUR NORMAL AUX CELLULES INTERIEURS       !
! !  VNOFR    ! TR !  R ! VECTEUR NORMAL AUX CELLULES FRONTIERES       !
! !XCONF,YCONF! TR !  D ! COORDONEES DES EXTREMITES DU CONFLUENT       !
! !  TETACO   ! TR !  D ! DIRECTION DE CHAQUE BIEF AU CONFLUENT        !
! !  SNODE    ! TR !  D ! SURFACE MOUILLEE (1D)                        !
! !  ZNODE    ! TR !  D ! COTE DE LA SURFACE LIBRE (1D)                !
! !  COTR     ! TR !  D ! COTE DU FOND (1D)                            !
! !  X1D      ! TR !  D ! COORDONEES MAILLAGE 1D                       !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  SGEO     ! TR !  D ! SURFACE MOUILLEE PLANIMETREE                 !
! !  ISEC     ! TI !  D ! NUMERO SECTION CALCUL 1D LIMITE A UN CONF    !
! !  ISECVO   ! TI !  D ! NUMERO DE SA VOISINE                         !
! !  ICONF    !  I !  D ! NUMERO DU CONFLUENT                          !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  TIRANT   !  R !  A ! TIRANT D'EAU                                 !
! !  SURF     !  R !  A ! SURFACE MOUILLEE                             !
! !  X,Y      !  R !  A ! COORDONEES DES SOMMETS DU MAILLAGE           !
! !  L        !  R !  A ! LARGEUR DE LA RIVIERE AU CONFLUENT           !
! !  DX       !  R !  A ! LONGEUR DES CELLULES RECTANGLES              !
! !BX,BY,SURFT!  R !  A ! VARIABLES DE CALCUL                          !
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
   use M_MESSAGE_C           ! Messages d'erreur
   use M_PARAMETRE_C         ! PI
   use M_ERREUR_T            ! ERREUR
   use M_CSUR_I              ! Interface de la fonction CSUR
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension 12
   real(DOUBLE), dimension(:)    , intent(  out) :: AIRS
   ! 1ere dimension 12, 2nde dimension 2
   real(DOUBLE), dimension(:,:)  , intent(  out) :: BXY
   ! 1ere dimension 12
   real(DOUBLE), dimension(:)    , intent(  out) :: ZF
   ! 1ere dimension 12, 2nde dimension 2
   integer     , dimension(:,:)  , intent(  out) :: NELMIN
   ! 1ere dimension 18
   integer     , dimension(:)    , intent(  out) :: NFRELM
   integer     ,                   intent(  out) :: NSEGP,NSEGR
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:)  , intent(  out) :: VNOIN
   ! 1ere dimension 3, 2nde dimension 18
   real(DOUBLE), dimension(:,:)  , intent(  out) :: VNOFR
   ! 1ere dimension 3
   real(DOUBLE), dimension(:)    , intent(in)    :: XCONF,YCONF,TETACO
   real(DOUBLE), dimension(:)    , intent(in)    :: SNODE,ZNODE,COTR,X1D,DZ
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   ! 1ere dimension 3
   integer     , dimension(:)    , intent(in)    :: ISEC,ISECVO
   integer     ,                   intent(in)    :: ICONF
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)              ,intent(inout)  :: Erreur

   end subroutine INIGEO

   end interface

end module M_INIGEO_I
