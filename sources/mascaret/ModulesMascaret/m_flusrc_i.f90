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

module M_FLUSRC_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine FLUSRC ( &
             IEL1    , &
             IEL2    , &
             ISEGIN  , &
             VNOIN   , &
             W       , &
             FLUSCE  , &
             BXY     , &
             ZF      , &
             EPS     , &
             Erreur    &
              )

!***********************************************************************
!  FONCTION  : . CALCUL DES FLUX DUS AUX TERMES SOURCES
!                 - DE TYPE  DECENTRE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! ! . IEL1         !<-- ! ELEMENT VOISIN                               !
! ! . IEL2         !<-- ! ELEMENT VOISIN                               !
! ! . ISEGIN       !<-- ! ARETE INTERNE                                !
! ! . VNOIN        ! -->! NORMALE DU SEGMENT INTERNE                   !
! !                !    ! (2 PREMIERES COMPOSANTES) ET                 !
! !                !    ! LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)    !
! ! . W            ! -->! VARIABLES CONSERVATIVES DU PB A L'INSTANT N  !
! ! . FLUSCE       !<-- ! TABLEAU DES TERMES SOURCES                   !
! ! . BXY          ! -->! LES COORDONNEES X,Y DU BARYCENTRE DE         !
! !   ZF           !    !                                              !
! ! . EPS          ! -->! PRECISION SUR LES HAUTEURS D'EAU             !
! !________________!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!        -- (TABLEAU DE TRAVAIL)
!-----------------------------------------------------------------------
!     - SOUS PROGRAMME(S) APPELANT : CALCON
!     - PORTABILITE: 
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
   integer     ,                   intent(in)    :: IEL1,IEL2,ISEGIN
   ! 1ere dimension NFON, 2nde dimension NSEGIN
   real(DOUBLE), dimension(:,:)  , intent(in)    :: VNOIN
   ! 1ere dimension NFON, 2nde dimension NELEM
   real(DOUBLE), dimension(:,:)  , intent(inout) :: W
   ! 1ere dimension 3, 2nde dimension 100
   real(DOUBLE), dimension(:,:)  , intent(  out) :: FLUSCE
   ! 1ere dimension NELEM, 2nde dimension 2
   real(DOUBLE), dimension(:,:)  , intent(in)    :: BXY
   ! 1ere dimension NELEM
   real(DOUBLE), dimension(:)    , intent(in)    :: ZF
   real(DOUBLE),                   intent(in)    :: EPS
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine FLUSRC

   end interface

end module M_FLUSRC_I
