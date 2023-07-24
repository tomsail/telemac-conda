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

Module M_Diffu_tracer_I
!***********************************************************************
! PROGICIEL : TRACER         Ch. BERTIER - M. LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   Interface

   Subroutine Diffu_tracer( &
                            Conc , & ! Concentration a t+Dt
                            T2 , & ! Grandeur convectee (Sm.C) a t+Dt
                            Flux , & ! Flux lineaire de MES (kg/s/m)
                            Sm , & ! Surface mouillee a t+DT
                            Vit , & ! Vitesse a t+dt
                            Absc , & ! Abscisse des sections de calcul
                            Dt , & ! Pas de temps
                            Rk , & ! Coefficient de diffusion
                            conv ,  & ! Convection
                            scheco , & ! Schema pour la convection
                            QINJEC , & ! Debits d'apport
                            NbProf , & ! Nb de sections de calcul
                            type_cl_amont ,  & ! Type de conditions aux
                            type_cl_aval ,  & ! limites aval / amont
                            Erreur )

   !*************************************************************************
   !=========================================================================
   !
   !  Fonction : Calcul de la diffusion de la concentration
   !  --------
   !
   !  Sous-programme appelant : DansLo - Resequ
   !  -----------------------
   !
   !  Sous-programme appele : Bissnd_tracer
   !  ---------------------
   !
   !=========================================================================

   use M_PRECISION         ! Definition de la precision DOUBLE ou SIMPLE
   use M_PARAMETRE_C       ! Definition des constante tq EPS*, W0, ...
   use M_ERREUR_T          ! Type ERREUR_T
   use M_MESSAGE_C         ! Messages d'erreur
   use M_TRAITER_ERREUR_I  ! Traitement de l'errreur
   use M_BISSND_TRACER_I

   !=========================================================================
   ! DECLARATIONS
   !=========================================================================
   !.. Implicit Declarations .. 
   implicit none
   ! Variables d'entree
   real(DOUBLE), intent(in) :: Dt
   real(DOUBLE), dimension(:), intent(in   ) :: T2
   real(DOUBLE), dimension(:), intent(in   ) :: Flux
   real(DOUBLE), dimension(:), intent(in   ) :: Sm
   real(DOUBLE), dimension(:), intent(in   ) :: Vit
   real(DOUBLE), dimension(:), intent(in   ) :: Absc
   real(DOUBLE), dimension(:), intent(in   ) :: Rk
   integer                   , intent(in   ) :: scheco, NbProf
   logical                   , intent(in   ) :: CONV
   real(DOUBLE), dimension(:), intent(in   ) :: QINJEC
   integer     , intent(in)                  :: type_cl_amont
   integer     , intent(in)                  :: type_cl_aval
   ! Variables de sortie
   real(DOUBLE), dimension(:), intent(  out) :: Conc
   type(ERREUR_T), intent(inout) :: Erreur

   End Subroutine Diffu_tracer

   End Interface

End Module M_Diffu_tracer_I
