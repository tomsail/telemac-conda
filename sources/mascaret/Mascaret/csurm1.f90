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

function CSURM1( &
        SurfaceMouillee , & ! Surface mouillee
                     DZ , & ! Pas de planimetrage de la section
   SurfaceMouilleePlani , & ! Surface mouille planimetree
                 Erreur ) ! Erreur

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL DU TIRANT D'EAU A PARTIR DE LA SURFACE
!                     MOUILLEE SUR MAILLAGE INITIAL
!                     INVERSE DE CSUR
!
!-----------------------------------------------------------------------
!  SurfaceMouilleePlani fait partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! Erreur
   use M_DICHO_I   ! Interface du sous-programme DICHO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                  :: CSURM1
   real(DOUBLE),                   intent(in)    :: SurfaceMouillee
   real(DOUBLE)                  , intent(in)    :: DZ
   real(DOUBLE), dimension(:)    , intent(in)    :: SurfaceMouilleePlani
   ! Integer                       , intent(inout) :: J_haut, j_bas
   Type(ERREUR_T)                , intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer        :: pas_bas       ! PAS BAS  DE L'INTERVALLE CONTENANT SurfaceMouillee
   integer        :: pas_haut      ! PAS HAUT DE L'INTERVALLE CONTENANT SurfaceMouillee
   real(DOUBLE)   :: surf_basse    ! SURFACE MOUILLE POUR PAS BAS
   real(DOUBLE)   :: surf_haute    ! SURFACE MOUILLE POUR PAS HAUT
   real(DOUBLE)   :: hauteur_basse ! HAUTEUR D'EAU PAS BAS
   real(DOUBLE)   :: hauteur_haute ! HAUTEUR D'EAU PAS HAUT
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0

   ! RECHERCHE DE L'INTERVALLE CONTENANT SurfaceMouillee PAR DICHOTOMIE
   call DICHO (pas_bas             , &
               pas_haut            , &
               SurfaceMouillee     , &
               SurfaceMouilleePlani, &
               Erreur                &
                                   )

   ! CALCUL DU TIRANT D'EAU PAR INTERPOLATION
   surf_basse    = SurfaceMouilleePlani(pas_bas)
   surf_haute    = SurfaceMouilleePlani(pas_haut)

   hauteur_basse = real( pas_bas - 1 , DOUBLE ) * DZ
   hauteur_haute = real( pas_haut - 1 , DOUBLE ) * DZ

   CSURM1 = ( hauteur_haute * ( SurfaceMouillee - surf_basse ) +   &
             hauteur_basse * ( surf_haute - SurfaceMouillee ) ) / &
           ( surf_haute - surf_basse )

   !------------------
   ! Fin du traitement
   !------------------

   !  !Erreur%arbredappel = !arbredappel_old

   return

end function CSURM1
