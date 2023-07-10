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

module M_POST_IMP_TRACER_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine POST_IMP_TRACER( &
                           X , &
            CTraceur, Nbtrac , & ! Concentrations en traceur
              MASSE , FLUMAS , & ! Masse de traceur
             FLUENT , FLUSOR , & ! Flux de traceur
                      FLUSRC , &
                   NbCourant , & ! Nombre de courant max
                     Connect , & ! Dimension spatiale
        FichierListingTracer , &
       ImpressionConcListing , & ! Logique pour les impressions
       ImpressionBilanTracer , &
                       TEMPS , &
             PhaseSimulation , &
                      Erreur )

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T
   use M_MESSAGE_C
   use M_TRAITER_ERREUR_I
   use M_FICHIER_T
   use M_CONNECT_T
   use M_CONSTANTES_CALCUL_C

   !.. Declarations Explicites .. 
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   type(ERREUR_T) ,                intent(inout):: Erreur
   type(FICHIER_T),                intent(in   ):: FichierListingTracer
   logical                        ,intent(in   ):: ImpressionConcListing,ImpressionBilanTracer
   type(CONNECT_T),                intent(in   ):: Connect
   real(DOUBLE)   , dimension(:,:),intent(in   ):: Ctraceur
   real(DOUBLE)   , dimension(:)  ,intent(in   ):: X
   real(DOUBLE)   , dimension(:,:),intent(inout):: MASSE,FLUMAS,FLUENT,FLUSOR,FLUSRC
   real(DOUBLE)   , dimension(:)  ,intent(in   ):: NbCourant
   integer,                        intent(in   ):: PhaseSimulation
   real(DOUBLE),                   intent(in   ):: TEMPS
   integer                                      :: Nbtrac

   end subroutine POST_IMP_TRACER

   end interface

end module M_POST_IMP_TRACER_I
