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

module M_LEC_SOURCE_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine LEC_SOURCE ( &
           Source_tracer , & ! Tableau des sources Tracer
                  Apport , & ! Tableau des apports hydrauliques
                 Connect , & ! Table de connectivite du reseau
                       X , & ! Maillage
               LoiTracer , & ! Lois tracer
                  Profil , & ! Profils geometriques
                  nbtrac , & ! nb de traceurs
            UniteListing , & ! Unite logique fichier listing
                 unitNum , & ! Unite logique du fichier .xcas
                  Erreur   & ! Erreur
                        )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_SOURCE_TRACER_T     ! Type SOURCE_TRACER_T
   use M_APPORT_T            ! Type APPORT_T
   use M_CONNECT_T           ! Type CONNECT_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_LOI_TRACER_T        ! Type LOI_TRACER_T
   use M_PROFIL_T            ! Type PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_MESSAGE_TRACER_C    ! Messages d'erreur Tracer
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   type(SOURCE_TRACER_T), dimension(:), pointer       :: Source_tracer
   type(APPORT_T)       , dimension(:), intent(in   ) :: Apport
   type(CONNECT_T)                    , intent(in   ) :: Connect
   real(DOUBLE)         , dimension(:), intent(in   ) :: X
   type(LOI_TRACER_T)   , dimension(:), intent(in   ) :: LoiTracer
   type(PROFIL_T)       , dimension(:), intent(in   ) :: Profil
   integer                            , intent(in   ) :: nbtrac
   integer                            , intent(in   ) :: UniteListing
   integer, intent(in)                                :: unitNum
   type(ERREUR_T)                     , intent(inout) :: Erreur

   end subroutine LEC_SOURCE

   end interface

end module M_LEC_SOURCE_I
