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

module M_LEC_APPORT_PLUIE_I
!***********************************************************************
! PROGICIEL : MASCARET       C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_APPORT_PLUIE( &
                  ApportPluie , &
                     NbCasier , &
                          Loi , &
                     unitNum  , & ! unite logique du fichier .xcas
                 UniteListing , &
                       Erreur)

   ! ******************************************************************
   ! LECTURE DE LA VARIABLE APPORT_PLUIE
   ! ******************************************************************
   !
   !   FICHIERS ENTREE/SORTIE :  --
   !   ----------------------
   !   SOUS PROGRAMMES APPELANTS : - PRETRAIT_CASIER
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :    --
   !   -------------------------
   !

   !========================== Declarations ==============================
   use M_APPORT_PLUIE_T
   use M_ERREUR_T
   use M_LOI_T
   use M_TRAITER_ERREUR_CASIER_I
   use M_TRAITER_ERREUR_I         ! Traitement de l'errreur
   use M_MESSAGE_CASIER_C
   use M_PRECISION
   use M_XCAS_S

   implicit none

   !.. Arguments ..
   type(APPORT_PLUIE_T), dimension(:), pointer       :: ApportPluie
   type(ERREUR_T)                    , intent(inout) :: Erreur
   type(LOI_T)         , dimension(:), intent(in   ) :: Loi
   integer                           , intent(in   ) :: NbCasier, UniteListing
   integer, intent(in)                               :: unitNum

   !.. Variables locales ..
   character(132) :: arbredappel_old
   integer :: nombre_apport, iapport, nbloi
   integer :: retour          ! code de retour des fonctions intrinseques
   character(len=256)  :: pathNode
   character(len=8192) :: line

   end subroutine LEC_APPORT_PLUIE

   end interface

end module M_LEC_APPORT_PLUIE_I
