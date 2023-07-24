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

module M_LEC_LOI_TRACER_INTERFACE_I
!***********************************************************************
! PROGICIEL : TRACER         M. LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_LOI_TRACER_INTERFACE( &
                  LoiTracer , & ! Tableau des lois tracer
                     nbtrac , & ! Nombre de traceurs
         FichiersLoisTracer , & ! Fichier des lois tracer
                 impression , & ! Flag d'impression des lois
               UniteListing , & ! Unite logique fichier listing
               TempsMaximum , & ! Temps maximum du calcul
                    unitNum , & ! Unite logique fichier .xcas
                     Erreur   & ! Erreur
                           )

!*************************************************************************
!  Fonction : Lecture des lois temporelles d'evolution de concentration
!  --------   (pour CL et apports)
!
!  Sous-programme appelant : Pretrait_Traceur
!  -----------------------
!
!  Sous-programme appele : LecFicLoi_Tracer
!  ---------------------
!*************************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T              ! Type ERREUR_T
   use M_FICHIER_T             ! UniteListing
   use M_LOI_TRACER_T          ! Types LOI_TRACER_T
   use M_MESSAGE_C             ! Messages d'erreur
   use M_MESSAGE_TRACER_C
   use M_CONSTANTES_CALCUL_C   ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I      ! Traitement de l'errreur
   use M_LEC_FIC_LOI_TRACER_I  ! Interface de sous-programme
   use M_XCAS_S

   implicit none

   ! Arguments
   type(LOI_TRACER_T), dimension(:), pointer         :: LoiTracer
   type(FICHIER_T),    dimension(:), pointer         :: FichiersLoisTracer
   logical                      , intent(in   )      :: impression
   integer                      , intent(in   )      :: Nbtrac
   integer                      , intent(in   )      :: UniteListing
   real(DOUBLE)                 , intent(in   )      :: TempsMaximum
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur

   end subroutine LEC_LOI_TRACER_INTERFACE

   end interface

end module M_LEC_LOI_TRACER_INTERFACE_I
