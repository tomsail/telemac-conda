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

module M_BILVOL_I
!***********************************************************************
! PROGICIEL : MASCARET        C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine BILVOL( Casier , Liaison , dt , Erreur )
   ! *****************************************************************
   !CALCUL DU BILAN ENTREE - SORTIE DANS UN CASIER
   !******************************************************************
   !
   !   FICHIERS ENTREE/SORTIE :  --
   !   ----------------------
   !   SOUS PROGRAMMES APPELANTS : - CALCCASIER
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :    --
   !   -------------------------

   !========================== Declarations ==============================
   use M_PRECISION   ! type DOUBLE
   use M_CONSTANTES_CALCUL_C  ! Constantes parametres de calcul (TETA)
   use M_CASIER_T    ! type CASIER
   use M_LIAISON_T   ! type LIAISON
   use M_ERREUR_T    ! type ERREUR
   use M_TRAITER_ERREUR_CASIER_I  ! traitement des erreurs
   use M_MESSAGE_CASIER_C         ! messages d erreur propres a CASIER

   implicit none

   !.. Arguments..
   type(CASIER_T), intent(inout) :: Casier
   type(LIAISON_T),dimension(:), intent(in   ) :: Liaison
   type(ERREUR_T), intent(inout) :: Erreur
   real(DOUBLE)      , intent(in   ) :: dt

   !.. Variables locales..
   real(DOUBLE) :: debit
   integer :: iliaison
   !character(132) :: !arbredappel_old ! Arbre d'appel precedent l'entree du sous programme

   end subroutine BILVOL

   end interface

end module M_BILVOL_I
