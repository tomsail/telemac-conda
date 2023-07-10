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

subroutine INIGEO ( &
                 AIRS , &
                  BXY , &
                   ZF , &
               NELMIN , &
               NFRELM , &
                NSEGP , &
                NSEGR , &
                VNOIN , &
                VNOFR , &
                XCONF , &
                YCONF , &
               TETACO , &
                SNODE , &
                ZNODE , &
                 COTR , &
                  X1D , &
                   DZ , &
                 SGEO , &
                 ISEC , &
               ISECVO , &
                ICONF , &
               NMLARG , &
               ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!   FONCTION : CREATION DU MAILLAGE 2D DANS LE CONFLUENT
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

   !.. Variables locales ..
   !-----------------------
   integer        :: IAFFLU,ISEGR,IEL,I1,I2
   real(DOUBLE)   :: TIRANT,SURF
   real(DOUBLE)   :: X(19),Y(19)
   real(DOUBLE)   :: L(3),DX(3)
   real(DOUBLE)   :: BXT1,BXT2,BXT3,BXT4,BYT1,BYT2,BYT3,BYT4
   real(DOUBLE)   :: SURFT1,SURFT2,SURFT3,SURFT4
   real(DOUBLE)   :: X1,Y1,X2,Y2,XN,YN,RNORM
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>INIGEO'

   do IAFFLU = 1 , 3

      TIRANT = ZNODE(ISECVO(IAFFLU)) - COTR(ISECVO(IAFFLU))
      SURF   = SNODE(ISECVO(IAFFLU))
      if( TIRANT < 1.0_DOUBLE ) then
         TIRANT = 1.0_DOUBLE
         SURF   = CSUR( ISECVO(IAFFLU) , TIRANT , DZ , SGEO , NMLARG , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif
      endif
      L(IAFFLU)  = SURF / TIRANT
      DX(IAFFLU) = dabs( X1D(ISEC(IAFFLU) )- X1D(ISECVO(IAFFLU)) )

   end do

   !  1 - CALCUL DES COORDONEES DU MAILLAGE 2D
   !  ----------------------------------------
   X(1) = XCONF(1) - L(1) * dcos( TETACO(1) + PI / 2._DOUBLE ) / 2._DOUBLE
   Y(1) = YCONF(1) - L(1) * dsin( TETACO(1) + PI / 2._DOUBLE ) / 2._DOUBLE

   X(2) = XCONF(1) + L(1) * dcos( TETACO(1) + PI / 2._DOUBLE ) / 2._DOUBLE
   Y(2) = YCONF(1) + L(1) * dsin( TETACO(1) + PI / 2._DOUBLE ) / 2._DOUBLE

   X(3) = XCONF(2) - L(2) * dcos( TETACO(2) + PI / 2._DOUBLE ) / 2._DOUBLE
   Y(3) = YCONF(2) - L(2) * dsin( TETACO(2) + PI / 2._DOUBLE ) / 2._DOUBLE

   X(4) = XCONF(2) + L(2) * dcos( TETACO(2) + PI / 2._DOUBLE ) / 2._DOUBLE
   Y(4) = YCONF(2) + L(2) * dsin( TETACO(2) + PI / 2._DOUBLE ) / 2._DOUBLE

   X(5) = XCONF(3) - L(3) * dcos( TETACO(3) + PI / 2._DOUBLE ) / 2._DOUBLE
   Y(5) = YCONF(3) - L(3) * dsin( TETACO(3) + PI / 2._DOUBLE ) / 2._DOUBLE

   X(6) = XCONF(3) + L(3) * dcos( TETACO(3) + PI / 2._DOUBLE ) / 2._DOUBLE
   Y(6) = YCONF(3) + L(3) * dsin( TETACO(3) + PI / 2._DOUBLE ) / 2._DOUBLE

   X(7) = X(1) + DX(1) * dcos( TETACO(1) )
   Y(7) = Y(1) + DX(1) * dsin( TETACO(1) )

   X(8) = X(2) + DX(1) * dcos( TETACO(1) )
   Y(8) = Y(2) + DX(1) * dsin( TETACO(1) )

   X(9) = X(3) + DX(2) * dcos( TETACO(2) )
   Y(9) = Y(3) + DX(2) * dsin( TETACO(2) )

   X(10) = X(4) + DX(2) * dcos( TETACO(2) )
   Y(10) = Y(4) + DX(2) * dsin( TETACO(2) )

   X(11) = X(5) + DX(3) * cos( TETACO(3) )
   Y(11) = Y(5) + DX(3) * sin( TETACO(3) )

   X(12) = X(6) + DX(3) * dcos( TETACO(3) )
   Y(12) = Y(6) + DX(3) * dsin( TETACO(3) )

   X(13) = X(1) + 2._DOUBLE * DX(1) * dcos( TETACO(1) )
   Y(13) = Y(1) + 2._DOUBLE * DX(1) * dsin( TETACO(1) )

   X(14) = X(2) + 2._DOUBLE * DX(1) * dcos( TETACO(1) )
   Y(14) = Y(2) + 2._DOUBLE * DX(1) * dsin( TETACO(1) )

   X(15) = X(3) + 2._DOUBLE * DX(2) * cos( TETACO(2) )
   Y(15) = Y(3) + 2._DOUBLE * DX(2) * sin( TETACO(2) )

   X(16) = X(4) + 2._DOUBLE * DX(2) * dcos( TETACO(2) )
   Y(16) = Y(4) + 2._DOUBLE * DX(2) * dsin( TETACO(2) )

   X(17) = X(5) + 2._DOUBLE * DX(3) * dcos( TETACO(3) )
   Y(17) = Y(5) + 2._DOUBLE * DX(3) * dsin( TETACO(3) )

   X(18) = X(6) + 2._DOUBLE * DX(3) * dcos( TETACO(3) )
   Y(18) = Y(6) + 2._DOUBLE * DX(3) * dsin( TETACO(3) )

   !     CALCUL DU CENTRE DE GRAVITE DE L'HEXAGONE
   SURFT1 = 0.5_DOUBLE * dabs( ( X(2) - X(1) ) * ( Y(3) - Y(1) ) - ( Y(2) - Y(1) ) * ( X(3) - X(1) ) )
   SURFT2 = 0.5_DOUBLE * dabs( ( X(3) - X(1) ) * ( Y(4) - Y(1) ) - ( Y(3) - Y(1) ) * ( X(4) - X(1) ) )
   SURFT3 = 0.5_DOUBLE * dabs( ( X(4) - X(1) ) * ( Y(6) - Y(1) ) - ( Y(4) - Y(1) ) * ( X(6) - X(1) ) )
   SURFT4 = 0.5_DOUBLE * dabs( ( X(4) - X(6) ) * ( Y(5) - Y(6) ) - ( Y(4) - Y(6) ) * ( X(5) - X(6) ) )

   BXT1 = ( X(1) + X(2) + X(3) ) / 3._DOUBLE
   BYT1 = ( Y(1) + Y(2) + Y(3) ) / 3._DOUBLE
   BXT2 = ( X(1) + X(3) + X(4) ) / 3._DOUBLE
   BYT2 = ( Y(1) + Y(3) + Y(4) ) / 3._DOUBLE
   BXT3 = ( X(1) + X(4) + X(6) ) / 3._DOUBLE
   BYT3 = ( Y(1) + Y(4) + Y(6) ) / 3._DOUBLE
   BXT4 = ( X(6) + X(4) + X(5) ) / 3._DOUBLE
   BYT4 = ( Y(6) + Y(4) + Y(5) ) / 3._DOUBLE

   X(19) = ( SURFT1 * BXT1 + SURFT2 * BXT2 + SURFT3 * BXT3 + SURFT4 * BXT4 ) &
          / ( SURFT1 + SURFT2 + SURFT3 + SURFT4 )
   Y(19) = ( SURFT1 * BYT1 + SURFT2 * BYT2 + SURFT3 * BYT3 + SURFT4 * BYT4 ) &
          / ( SURFT1 + SURFT2 + SURFT3 + SURFT4 )

   ! 2 - CALCUL DE LA SURFACE DE CHAQUE CELLULE
   ! ------------------------------------------
   do IAFFLU = 1 , 3
      AIRS(IAFFLU)   = L(IAFFLU) * DX(IAFFLU)
      AIRS(IAFFLU+3) = AIRS(IAFFLU)
   end do

   do IEL = 7 , 12
      I1 = IEL - 6
      if( IEL < 12 ) then
         I2 = IEL - 5
      else
         I2 = 1
      endif
      AIRS(IEL) = 0.5_DOUBLE * ( ( X(I1) - X(19) ) * ( Y(I2) - Y(19) ) - ( Y(I1) - Y(19) ) * ( X(I2) - X(19) ) )
      if( AIRS(IEL) < 0._DOUBLE ) then
         Erreur%Numero = 107
         Erreur%ft   = err_107
         Erreur%ft_c = err_107c
         call TRAITER_ERREUR  (Erreur, ICONF)
         return
      endif
   end do

   ! 3 - CALCUL DU CENTRE DE GRAVITE DE CHAQUE CELLULE
   ! -------------------------------------------------
   BXY(1,1) = ( X(1) + X(2) + X(7) + X(8) ) / 4._DOUBLE
   BXY(1,2) = ( Y(1) + Y(2) + Y(7) + Y(8) ) / 4._DOUBLE

   BXY(2,1) = ( X(3) + X(4) + X(9) + X(10) ) / 4._DOUBLE
   BXY(2,2) = ( Y(3) + Y(4) + Y(9)+ Y (10) ) / 4._DOUBLE

   BXY(3,1) = ( X(5) + X(6) + X(11) + X(12) ) / 4._DOUBLE
   BXY(3,2) = ( Y(5) + Y(6) + Y(11) + Y(12) ) / 4._DOUBLE

   BXY(4,1) = ( X(7) + X(8) + X(13) + X(14) ) / 4._DOUBLE
   BXY(4,2) = ( Y(7) + Y(8) + Y(13) + Y(14) ) / 4._DOUBLE

   BXY(5,1) = ( X(9) + X(10) + X(15) + X(16) ) / 4._DOUBLE
   BXY(5,2) = ( Y(9) + Y(10) + Y(15) + Y(16) ) / 4._DOUBLE

   BXY(6,1) = ( X(11) + X(12) + X(17) + X(18) ) / 4._DOUBLE
   BXY(6,2) = ( Y(11) + Y(12) + Y(17) + Y(18) ) / 4._DOUBLE

   BXY(7,1) = ( X(1) + X(2) + X(19) ) /3._DOUBLE
   BXY(7,2) = ( Y(1) + Y(2) + Y(19) ) / 3._DOUBLE

   BXY(8,1) = ( X(2) + X(3) + X(19) ) / 3._DOUBLE
   BXY(8,2) = ( Y(2) + Y(3) + Y(19) ) / 3._DOUBLE

   BXY(9,1) = ( X(3) + X(4) + X(19) ) / 3._DOUBLE
   BXY(9,2) = ( Y(3) + Y(4) + Y(19) ) / 3._DOUBLE

   BXY(10,1) = ( X(4) + X(5) + X(19) ) / 3._DOUBLE
   BXY(10,2) = ( Y(4) + Y(5) + Y(19) ) / 3._DOUBLE

   BXY(11,1) = ( X(5) + X(6) + X(19) ) / 3._DOUBLE
   BXY(11,2) = ( Y(5) + Y(6) + Y(19) ) / 3._DOUBLE

   BXY(12,1) = ( X(6) + X(1) + X(19) ) / 3._DOUBLE
   BXY(12,2) = ( Y(6) + Y(1) + Y(19) ) / 3._DOUBLE

   ! 4 - CALCUL DE LA COTE DU FOND DANS CHAQUE CELLULE
   ! -------------------------------------------------
   do IAFFLU = 1 , 3
      ZF(IAFFLU)   = COTR(ISEC(IAFFLU))
      ZF(IAFFLU+3) = COTR(ISECVO(IAFFLU))
   end do

   ZF(7) = ( 7._DOUBLE * ZF(1) + 1._DOUBLE * ZF(2) + 1._DOUBLE * ZF(3) ) / 9._DOUBLE
   ZF(9 )= ( 1._DOUBLE * ZF(1) + 7._DOUBLE * ZF(2) + 1._DOUBLE * ZF(3) ) / 9._DOUBLE
   ZF(11)= ( 1._DOUBLE * ZF(1) + 1._DOUBLE * ZF(2) + 7._DOUBLE * ZF(3) ) / 9._DOUBLE

   ZF(8)  = ( 4._DOUBLE * ZF(1) + 4._DOUBLE * ZF(2) + 1._DOUBLE * ZF(3) ) / 9._DOUBLE
   ZF(10) = ( 1._DOUBLE * ZF(1) + 4._DOUBLE * ZF(2) + 4._DOUBLE * ZF(3) ) / 9._DOUBLE
   ZF(12) = ( 4._DOUBLE * ZF(1) + 1._DOUBLE * ZF(2) + 4._DOUBLE * ZF(3) ) / 9._DOUBLE

   ! 5 - CONSTRUCTION DES VOISINS DES ARETES
   ! ---------------------------------------
   NELMIN(1 ,1) = 7
   NELMIN(1 ,2) = 1
   NELMIN(2 ,1) = 9
   NELMIN(2 ,2) = 2
   NELMIN(3 ,1) = 11
   NELMIN(3 ,2) = 3
   NELMIN(4 ,1) = 1
   NELMIN(4 ,2) = 4
   NELMIN(5 ,1) = 2
   NELMIN(5 ,2) = 5
   NELMIN(6 ,1) = 3
   NELMIN(6 ,2) = 6
   NELMIN(7 ,1) = 8
   NELMIN(7 ,2) = 7
   NELMIN(8 ,1) = 9
   NELMIN(8 ,2) = 8
   NELMIN(9 ,1) = 10
   NELMIN(9 ,2) = 9
   NELMIN(10,1) = 11
   NELMIN(10,2) = 10
   NELMIN(11,1) = 12
   NELMIN(11,2) = 11
   NELMIN(12,1) = 07
   NELMIN(12,2) = 12

   ! 6 - CONSTRUCTION DES NORMALES INTERNES
   ! --------------------------------------

   !  6.1 ARETES DANS LES CELLULES CARREES
   X1    = X(1)
   Y1    = Y(1)
   X2    = X(2)
   Y2    = Y(2)
   XN    = Y2 - Y1
   YN    = X1 - X2
   RNORM = dsqrt ( XN * XN + YN * YN )
   XN    = XN / RNORM
   YN    = YN / RNORM

   VNOIN (1,1) = XN
   VNOIN (2,1) = YN
   VNOIN (3,1) = RNORM

   X1          = X(3)
   Y1          = Y(3)
   X2          = X(4)
   Y2          = Y(4)
   XN          = Y2 - Y1
   YN          = X1 - X2
   RNORM       = dsqrt ( XN * XN + YN * YN )
   XN          = XN / RNORM
   YN          = YN / RNORM
   VNOIN (1,2) = XN
   VNOIN (2,2) = YN
   VNOIN (3,2) = RNORM

   X1          = X (5)
   Y1          = Y (5)
   X2          = X(6)
   Y2          = Y(6)
   XN          = Y2 - Y1
   YN          = X1 - X2
   RNORM       = dsqrt ( XN * XN + YN * YN )
   XN          = XN / RNORM
   YN          = YN / RNORM
   VNOIN (1,3) = XN
   VNOIN (2,3) = YN
   VNOIN (3,3) = RNORM

   do IAFFLU = 1 , 3
      VNOIN(1,IAFFLU+3) = VNOIN(1,IAFFLU)
      VNOIN(2,IAFFLU+3) = VNOIN(2,IAFFLU)
      VNOIN(3,IAFFLU+3) = VNOIN(3,IAFFLU)
   end do

   ! 6.2  ARETES DANS L'HEXAGONE
   X1          = X(19)
   Y1          = Y(19)
   X2          = X(2)
   Y2          = Y(2)
   XN          = Y2 - Y1
   YN          = X1 - X2
   RNORM       = dsqrt ( XN * XN + YN * YN )
   XN          = XN / RNORM
   YN          = YN / RNORM
   VNOIN (1,7) = XN
   VNOIN (2,7) = YN
   VNOIN (3,7) = RNORM

   X2          = X(3)
   Y2          = Y(3)
   XN          = Y2 - Y1
   YN          = X1 - X2
   RNORM       = dsqrt ( XN * XN + YN * YN )
   XN          = XN / RNORM
   YN          = YN / RNORM
   VNOIN (1,8) = XN
   VNOIN (2,8) = YN
   VNOIN (3,8) = RNORM

   X2          = X(4)
   Y2          = Y(4)
   XN          = Y2 - Y1
   YN          = X1 - X2
   RNORM       = dsqrt ( XN * XN + YN * YN )
   XN          = XN / RNORM
   YN          = YN / RNORM
   VNOIN (1,9) = XN
   VNOIN (2,9) = YN
   VNOIN (3,9) = RNORM

   X2           = X(5)
   Y2           = Y(5)
   XN           = Y2 - Y1
   YN           = X1 - X2
   RNORM        = dsqrt ( XN * XN + YN * YN )
   XN           = XN / RNORM
   YN           = YN / RNORM
   VNOIN (1,10) = XN
   VNOIN (2,10) = YN
   VNOIN (3,10) = RNORM

   X2           = X(6)
   Y2           = Y(6)
   XN           = Y2 - Y1
   YN           = X1 - X2
   RNORM        = dsqrt ( XN * XN + YN * YN )
   XN           = XN / RNORM
   YN           = YN / RNORM
   VNOIN (1,11) = XN
   VNOIN (2,11) = YN
   VNOIN (3,11) = RNORM

   X2           = X(1)
   Y2           = Y(1)
   XN           = Y2 - Y1
   YN           = X1 - X2
   RNORM        = dsqrt ( XN * XN + YN * YN )
   XN           = XN / RNORM
   YN           = YN / RNORM
   VNOIN (1,12) = XN
   VNOIN (2,12) = YN
   VNOIN (3,12) = RNORM

   ! 7 - CONSTRUCTION DES ARETES FRONTIERES
   ! --------------------------------------

   ! 7.1  SEGMENT RACCORD 2D-1D
   NSEGR = 3
   do ISEGR = 1 , NSEGR
      NFRELM (ISEGR) = ISEGR + 3
      I1             = 13 + 2 * ( ISEGR - 1 )
      I2             = 14 + 2 * ( ISEGR - 1 )
      X1             = X (I1)
      Y1             = Y (I1)
      X2             = X(I2)
      Y2             = Y(I2)
      XN             = Y2 - Y1
      YN             = X1 - X2
      RNORM          = dsqrt ( XN * XN + YN * YN )
      XN             = XN / RNORM
      YN             = YN / RNORM
      VNOFR(1,ISEGR) = XN
      VNOFR(2,ISEGR) = YN
      VNOFR(3,ISEGR) = RNORM
   end do

   ! 7.2 NORMALES A LA PAROI
   NSEGP = NSEGR + 15

   I1    = 1
   I2    = 7
   X1    = X(I1)
   Y1    = Y(I1)
   X2    = X(I2)
   Y2    = Y(I2)
   XN    = Y2 - Y1
   YN    = X1 - X2
   RNORM = dsqrt ( XN * XN + YN * YN )
   XN    = XN / RNORM
   YN    = YN / RNORM

   NFRELM(4)    = 1
   VNOFR (1,4 ) = XN
   VNOFR (2,4 ) = YN
   VNOFR (3,4 ) = RNORM

   NFRELM(10)   = 4
   VNOFR (1,10) = XN
   VNOFR (2,10) = YN
   VNOFR (3,10) = RNORM

   I1    = 8
   I2    = 2
   X1    = X(I1)
   Y1    = Y(I1)
   X2    = X(I2)
   Y2    = Y(I2)
   XN    = Y2 - Y1
   YN    = X1 - X2
   RNORM = dsqrt ( XN * XN + YN * YN )
   XN    = XN / RNORM
   YN    = YN / RNORM

   NFRELM(5)  =1
   VNOFR(1,5) = XN
   VNOFR(2,5) = YN
   VNOFR(3,5) = RNORM

   NFRELM(11)  = 4
   VNOFR(1,11) = XN
   VNOFR(2,11) = YN
   VNOFR(3,11) = RNORM

   I1 = 3
   I2 = 9
   X1 = X(I1)
   Y1 = Y(I1)
   X2 = X(I2)
   Y2 = Y(I2)
   XN = Y2 - Y1
   YN = X1 - X2
   RNORM = dsqrt ( XN * XN + YN * YN )
   XN = XN / RNORM
   YN = YN / RNORM

   NFRELM(6)  = 2
   VNOFR(1,6) = XN
   VNOFR(2,6) = YN
   VNOFR(3,6) = RNORM

   NFRELM(12)  = 5
   VNOFR(1,12) = XN
   VNOFR(2,12) = YN
   VNOFR(3,12) = RNORM

   I1    = 10
   I2    = 4
   X1    = X(I1)
   Y1    = Y(I1)
   X2    = X(I2)
   Y2    = Y(I2)
   XN    = Y2 - Y1
   YN    = X1 - X2
   RNORM = dsqrt ( XN * XN + YN * YN )
   XN    = XN / RNORM
   YN    = YN / RNORM

   NFRELM(7)  = 2
   VNOFR(1,7) = XN
   VNOFR(2,7) = YN
   VNOFR(3,7) = RNORM

   NFRELM(13)  = 5
   VNOFR(1,13) = XN
   VNOFR(2,13) = YN
   VNOFR(3,13) = RNORM

   I1    = 5
   I2    = 11
   X1    = X(I1)
   Y1    = Y(I1)
   X2    = X(I2)
   Y2    = Y(I2)
   XN    = Y2 - Y1
   YN    = X1 - X2
   RNORM = dsqrt ( XN * XN + YN * YN )
   XN    = XN / RNORM
   YN    = YN / RNORM

   NFRELM(8)  = 3
   VNOFR(1,8) = XN
   VNOFR(2,8) = YN
   VNOFR(3,8) = RNORM

   NFRELM(14)  = 6
   VNOFR(1,14) = XN
   VNOFR(2,14) = YN
   VNOFR(3,14) = RNORM

   I1 = 12
   I2 = 6
   X1 = X(I1)
   Y1 = Y(I1)
   X2 = X(I2)
   Y2 = Y(I2)
   XN = Y2 - Y1
   YN = X1 - X2
   RNORM = dsqrt( XN * XN + YN * YN )
   XN = XN / RNORM
   YN = YN / RNORM

   NFRELM(9)  = 3
   VNOFR(1,9) = XN
   VNOFR(2,9) = YN
   VNOFR(3,9) = RNORM

   NFRELM(15)  = 6
   VNOFR(1,15) = XN
   VNOFR(2,15) = YN
   VNOFR(3,15) = RNORM

   ! 6.2.2  NORMALE PAROI CELLULES ECHANGE
   I1    = 4
   I2    = 5
   X1    = X(I1)
   Y1    = Y(I1)
   X2    = X(I2)
   Y2    = Y(I2)
   XN    = Y2 - Y1
   YN    = X1 - X2
   RNORM = dsqrt ( XN * XN + YN * YN )
   XN    = XN / RNORM
   YN    = YN / RNORM

   NFRELM(16)  = 10
   VNOFR(1,16) = XN
   VNOFR(2,16) = YN
   VNOFR(3,16) = RNORM

   I1    = 6
   I2    = 1
   X1    = X(I1)
   Y1    = Y(I1)
   X2    = X(I2)
   Y2    = Y(I2)
   XN    = Y2 - Y1
   YN    = X1 - X2
   RNORM = dsqrt ( XN * XN + YN * YN )
   XN    = XN / RNORM
   YN    = YN / RNORM

   NFRELM(17)   = 12
   VNOFR (1,17) = XN
   VNOFR (2,17) = YN
   VNOFR (3,17) = RNORM

   I1    = 2
   I2    = 3
   X1    = X(I1)
   Y1    = Y(I1)
   X2    = X(I2)
   Y2    = Y(I2)
   XN    = Y2 - Y1
   YN    = X1 - X2
   RNORM = dsqrt ( XN * XN + YN * YN )
   XN    = XN / RNORM
   YN    = YN / RNORM

   NFRELM(18)  = 8
   VNOFR(1,18) = XN
   VNOFR(2,18) = YN
   VNOFR(3,18) = RNORM

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine INIGEO
