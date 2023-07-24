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

module M_LEC_LIAISON_I
!***********************************************************************
! PROGICIEL : MASCARET       C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine LEC_LIAISON( &
                     Liaison , &  ! tableau des liaisons
                     Connect , &  ! matrice de connection casier - casier
                           X , &  ! Tableau des sections de calcul
                      Profil , &
                 ProfDebBief , &
                 ProfFinBief , &
                    unitNum  , & ! unite logique du fichier .xcas
                      Erreur )    ! erreur

   ! ******************************************************************
   ! LECTURE DE LA VARIABLE LIAISON
   ! ******************************************************************
   !
   !   FICHIERS ENTREE/SORTIE :  --
   !   ----------------------
   !   SOUS PROGRAMMES APPELANTS : - PRETRAIT_CASIER
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :    --
   !   -------------------------

   !========================== Declarations ==============================
   use M_PRECISION                ! type double
   use M_LIAISON_T                ! type liaison
   use M_PROFIL_T
   use M_ERREUR_T                 ! type erreur
   use M_MESSAGE_CASIER_C         ! messages d erreur propres a CASIER
   use M_CONSTANTES_CASIER_C      ! constantes de calcul propres a CASIER
   use M_TRAITER_ERREUR_CASIER_I  ! traitement des erreurs
   use M_TRAITER_ERREUR_I         ! Traitement de l'errreur
   use M_XINDIC_S
   use M_XCAS_S

   implicit none

   !.. Arguments ..
   type(LIAISON_T), dimension(:), pointer :: Liaison
   type(ERREUR_T)               , intent(inout) :: Erreur
   integer , dimension(:,:)   ,   intent(inout) :: Connect
   integer, intent(in)                          :: unitNum
   real(DOUBLE), dimension(:),    intent(in   ) :: X
   type(PROFIL_T), dimension(:) , pointer       :: Profil
   integer       , dimension(:) , pointer       :: ProfDebBief
   integer       , dimension(:) , pointer       :: ProfFinBief
   !.. Variables locales ..
   integer :: iliaison, nombre_liaison
   integer :: retour          ! code de retour des fonctions intrinseques
   !character(132) :: !arbredappel_old

   end subroutine LEC_LIAISON

   end interface

end module M_LEC_LIAISON_I
