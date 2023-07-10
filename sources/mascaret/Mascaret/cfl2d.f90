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

subroutine CFL2D( &
                  W , &
                BXY , &
             NELMIN , &
                 DT , &
               HEPS , &
              ICONF , &
             ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL DU PAS DE TEMPS POUR LE MODELE 2D
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  W        ! TR !  D ! VARIABLE D'ETAT A L'INSTANT N SUR 12 CELLULES!
! !  BXY      ! TR !  D ! BARYCENTRE DES 12 CELLULES                   !
! !  NELMIN   ! TI !  D ! DONNE LES NUMEROS GLOBAUX DES EXT. DES ARETES!
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  HEPS     !  R !  D ! PRECISION SUR LA HAUTEUR D'EAU               !
! !  ICONF    !  I !  D ! NUMERO DU CONFLUENT                          !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!    AKGEO et SGEO font partie d'une structure de donnees STRUCTURE_SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C      ! GPES
   use M_MESSAGE_C        ! Messages d'erreur
   use M_ERREUR_T         ! ERREUR
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension 3
   real(DOUBLE), dimension(:,:)  , intent(in)    :: W
   ! 1ere dimension 12
   real(DOUBLE), dimension(:,:)  , intent(in)    :: BXY
   integer     , dimension(:,:)  , intent(in)    :: NELMIN
   real(DOUBLE),                   intent(in)    :: DT,HEPS
   integer     ,                   intent(in)    :: ICONF
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE) :: VIT1,VIT2,XNC1,XNC2,DIST
   integer      :: NSEGIN
   integer      :: IEL1,IEL2,ISEGIN
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CFL2D'

   NSEGIN = 12

   ! CALCUL DE LA DISTANCE MINIMALE
   do ISEGIN = 1 , NSEGIN

      IEL1 = NELMIN(ISEGIN,1)
      IEL2 = NELMIN(ISEGIN,2)

      DIST = dsqrt( ( BXY(IEL1,1) - BXY(IEL2,1) )**2 + &
            ( BXY(IEL1,2) - BXY(IEL2,2) )**2 )

      if( W(1,IEL1) >= HEPS ) then
         VIT1 = dsqrt( ( W(2,IEL1) / W(1,IEL1) )**2 + &
               ( W(3,IEL1) / W(1,IEL1) )**2 ) + &
               dsqrt( GPES * W(1,IEL1) )
      else
         VIT1 = 0._DOUBLE
      endif

      XNC1 = VIT1 * DT / DIST

      if( W(1,IEL2) >= HEPS ) then
         VIT2 = dsqrt( ( W(2,IEL2) / W(1,IEL2) )**2 + &
               ( W(3,IEL2) / W(1,IEL2) )**2 ) + &
                dsqrt( GPES * W(1,IEL2) )
      else
         VIT2 = 0._DOUBLE
      endif

      XNC2 = VIT2 * DT / DIST

      if( ( XNC1 >= 0.9_DOUBLE ).or.( XNC2 >= 0.9_DOUBLE ) ) then
         Erreur%Numero = 105
         Erreur%ft   = err_105
         Erreur%ft_c = err_105c
         call TRAITER_ERREUR  (Erreur, ICONF)
         return
       endif

   end do

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CFL2D
