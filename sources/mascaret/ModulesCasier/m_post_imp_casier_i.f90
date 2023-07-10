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

module M_POST_IMP_CASIER_I
!***********************************************************************
! PROGICIEL : MASCARET       C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine POST_IMP_CASIER( &
                          Casier , &
            FichierListingCasier , &
                         Liaison , &
           FichierListingLiaison , &
                           TEMPS , &
                 PhaseSimulation , &
                          Erreur )

   !***********************************************************************
   !  FONCTION :   IMPRESSION DES VALEURS DES VARIABLES CASIER ET LIAISON
   !  --------     SUR LISTING
   !
   !  SOUS PROGRAMMES APPELANT(S) : SUPERVISEUR
   !  ---------------------------
   !  SOUS PROGRAMMES APPELE(S) :   Neant
   !  -------------------------
   !***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION
   use M_CASIER_T
   use M_LIAISON_T
   use M_ERREUR_T
   use M_FICHIER_T
   use M_CONSTANTES_CALCUL_C
   use M_MESSAGE_CASIER_C
   use M_TRAITER_ERREUR_CASIER_I

   implicit none

   !.. Arguments ..
   type(CASIER_T), dimension(:), pointer       :: Casier
   type(LIAISON_T), dimension(:),pointer       :: Liaison
   type(ERREUR_T),               intent(inout) :: Erreur
   type(FICHIER_T),              intent(in   ) :: FichierListingCasier
   type(FICHIER_T),              intent(in   ) :: FichierListingLiaison
   integer,                      intent(in   ) :: PhaseSimulation
   real(DOUBLE),                 intent(in   ) :: TEMPS

   !.. Variables locales
   character(132) :: arbredappel_old
   integer :: ull, ulc, retour
   integer :: icasier, nb_casier, iliaison, nb_liaison

   end subroutine POST_IMP_CASIER

   end interface

end module M_POST_IMP_CASIER_I
