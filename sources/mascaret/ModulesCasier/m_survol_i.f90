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

module M_SURVOL_I
!***********************************************************************
! PROGICIEL : MASCARET       A. LEBOSSE     C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine SURVOL( Casier , NumeroCasier , Erreur )
   ! *****************************************************************
   !CALCUL DE LA SURFACE INONDEE ET DU VOLUME STOCKE DANS UN CASIER
   !******************************************************************
   !
   !   FICHIERS ENTREE/SORTIE :  --
   !   ----------------------
   !   SOUS PROGRAMMES APPELANTS : - CALCCASIER, LECT1D
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :    --
   !   -------------------------

   !========================== Declarations ==============================
   use M_PRECISION                ! type DOUBLE
   use M_CASIER_T                 ! type CASIER
   use M_ERREUR_T                 ! type ERREUR
   use M_TRAITER_ERREUR_CASIER_I  ! traitement des erreurs
   use M_MESSAGE_CASIER_C         ! messages d erreur propres a CASIER

   implicit none

   !.. Arguments..
   type(CASIER_T), intent(inout) :: Casier
   type(ERREUR_T), intent(inout) :: Erreur
   integer       , intent(in   ) :: NumeroCasier
   !.. Variables locales..
   real(DOUBLE) :: hauteur, & ! tirant d eau
                   dz,      & ! difference entre le tirant d eau et le tirant
                              ! d eau approche par planimetrage
                   surface1,& ! surface du casier approchee par planimetrage (valeur inf)
                   surface2,& ! surface du casier approchee par planimetrage (valeur sup)
                   volume1    ! volume du casier approche par planimetrage (valeur inf)
   integer      :: nb_pas_planim ! numero du pas inferieur
   !character(132) :: !arbredappel_old ! Arbre d'appel precedent l'entree du sous programme

   end subroutine SURVOL

   end interface

end module M_SURVOL_I
