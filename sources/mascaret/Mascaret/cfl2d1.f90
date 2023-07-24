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

subroutine CFL2D1( &
                DT2D , &
                   W , &
                 BXY , &
               VNOIN , &
              NELMIN , &
                DT1D , &
                HEPS , &
              Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!   FONCTION : CALCUL DU PAS DE TEMPS DANS LE CONFLUENT
!   -------------    AVEC LES REGLES SUIVANTES :
!                    - NOMBRE DE COURANT < 0.5
!                    - NOMBRE DE SOUS ITERATIONS >= 5
!
!                    SOUS PROGRAMME APPELANT : FLUSRC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  DT2D     !  R !  R ! PAS DE TEMPS DANS LE MODELE 2D               !
! !  W        ! TR !  D ! VARIABLE D'ETAT DANS LE CONFLUENT            !
! !  BXY      ! TR !  D ! BARYCENTRE DES CELLULES 2D                   !
! !  VNOIN    ! TR !  D ! VECTEUR NORMAL AUX CELLULES INTERIEURS       !
! !  NELMIN   ! TI !  D ! TABLEAU DES CELLULES FONCTION DES SEGMENTS   !
! !  DT1D     !  R !  D ! PAS DE TEMPS DANS LE MODELE 1D               !
! !  HEPS     !  R !  D ! HAUTEUR D'EAU MINIMALE                       !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  DIST     !  R !  A ! DISTANCE CENTRES DE CELLULES-SEGMENT         !
! !  XNC      !  R !  A ! RAPPORT (U+C)/DIST                           !
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
   use M_PARAMETRE_C ! GPES
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(  out) :: DT2D
   ! 1ere dimension 3
   real(DOUBLE), dimension(:,:)  , intent(in)    :: W
   ! 1ere dimension 12
   real(DOUBLE), dimension(:,:)  , intent(in)    :: BXY
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:)  , intent(in)    :: VNOIN
   ! 1ere dimension 12
   integer     , dimension(:,:)  , intent(in)    :: NELMIN
   real(DOUBLE),                   intent(in)    :: DT1D
   real(DOUBLE),                   intent(in)    :: HEPS
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE)   :: VIT1,VIT2,XNC1,XNC2,DIST
   real(DOUBLE)   :: XNC, HALF_OVER_XNC
   integer        :: NSEGIN
   integer        :: IEL1,IEL2,ISEGIN
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CFL2D1'

   NSEGIN = 12
   XNC    = 0._DOUBLE

   ! CALCUL DE LA DISTANCE MINIMALE
   do ISEGIN = 1 , NSEGIN

      IEL1 = NELMIN(ISEGIN,1)
      IEL2 = NELMIN(ISEGIN,2)

      DIST = dabs ( ( BXY(IEL1,1) - BXY(IEL2,1) ) * VNOIN(1,ISEGIN) &
          +( BXY(IEL1,2) - BXY(IEL2,2) ) * VNOIN(2,ISEGIN) ) / 2._DOUBLE

      if( W(1,IEL1) >= HEPS ) then
         VIT1 = dsqrt( ( W(2,IEL1) / W(1,IEL1) )**2 + &
               ( W(3,IEL1) / W(1,IEL1) )**2 ) + &
               dsqrt( GPES * W(1,IEL1) )
      else
         VIT1 = 0._DOUBLE
      endif

      XNC1 = VIT1 / DIST

      if( W(1,IEL2) >= HEPS ) then
         VIT2 = dsqrt( ( W(2,IEL2) / W(1,IEL2) )**2 + &
             ( W(3,IEL2) / W(1,IEL2) )**2 ) + &
             dsqrt( GPES * W(1,IEL2) )
      else
         VIT2 = 0._DOUBLE
      endif

      XNC2 = VIT2 / DIST
      XNC  = dmax1( XNC , dmax1( XNC1 , XNC2 ) )

   end do

   !MS2019 : verif Yoann
   IF(XNC .GT. 0._DOUBLE)THEN
     HALF_OVER_XNC = 0.5_DOUBLE / XNC
   ELSE
     HALF_OVER_XNC = 2 * (DT1D / 9.9_DOUBLE)
   ENDIF

   DT2D = dmin1( HALF_OVER_XNC , DT1D / 9.9_DOUBLE )

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CFL2D1
