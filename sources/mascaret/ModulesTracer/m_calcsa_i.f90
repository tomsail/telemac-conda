!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_CALCSA_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N. GOUTAL - M. LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine CALCSA( &
       SAD            , &  ! Terme Source ajoutee  (kg/m3/s)
       Source_Tracer  , &  ! Structure de donnees Traceur
       Qinjec         , &  ! Injection de debit (dans le calcul hydraulique)
       H              , &  ! Hauteur d'eau
       X              , &  ! Maillage longitudinal
       A              , &  ! Section mouillee totale
       Nbsect         , &  ! Nombre de sections de calcul
       Nbtra          , &  ! Nombre de traceurs
       Erreur           )

   !***********************************************************************
   !  FONCTION :
   !  --------
   !   Calcul des termes Sources ajoutees (surfaciques et volumiques)
   !   a l'instant t
   !
   !  SOUS-PROGRAMMES APPELANT :	TRACER
   !  ------------------------
   !
   !**********************************************************************

   use  M_PRECISION
   use  M_SOURCE_TRACER_T
   use  M_APPORT_T
   use  M_CONSTANTES_TRACER_T
   use  M_PARAMETRES_QUALITE_EAU_T 
   use  M_XINDIC_S
   use  M_ERREUR_T

   !==============================================================
   !           Declarations
   !==============================================================
   implicit none

   TYPE (SOURCE_TRACER_T), dimension(:), intent(inout)  :: Source_tracer 
   real (DOUBLE) , dimension(:,:)      , intent(  out) :: SAD ! sources ajoutees a l'instant t
   real (DOUBLE) , dimension(:)        , intent(in   ) :: Qinjec
   real (DOUBLE) , dimension (:)       , intent(in   ) :: X, A , H
   integer                             , intent(in   ) :: Nbsect, Nbtra
   type(ERREUR_T)         , intent(inout) :: Erreur

   end subroutine CALCSA

   end interface

end module M_CALCSA_I
