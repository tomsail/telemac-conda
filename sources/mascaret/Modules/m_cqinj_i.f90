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

module M_CQINJ_I
!***********************************************************************
! PROGICIEL : MASCARET        C. RISSOAN          N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  CQINJ           ( &
                   Qinjec      , & ! Vecteur Debit d'apport
                   X           , & ! Abscisse des sections de calcul
                   Z           , & ! Cotes
                   Apport      , & ! Apports
                   Deversoir   , & ! Deversoirs
                   Qdeverse    , &
                   Erreur        & ! Erreur
                           )
!
! **********************************************************************
!   FONCTION : CALCUL DU TABLEAU QINJEC DES APPORTS (DEBITS + DEVRESOIRS)
!   --------
!
! ----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :     - UL_LST : Sortie listing
!   ----------------------      
!   SOUS PROGRAMMES APPELANTS :  - REZO, SARAP
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    - INTERPOLATION_S
!   -------------------------
!=========================================================================

   !=========================== Declarations ================================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_FICHIER_T       ! Numero du canal UL_LST
   use M_DEVERSOIR_T     ! Definition du type DEVERSOIR_T
   use M_APPORT_T        ! Definition du type APPORT_T
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_INTERPOLATION_S  ! Interpolation
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Arguments .. 
   real(DOUBLE)     , dimension(:), intent(  out) :: Qinjec
   real(DOUBLE)     , dimension(:), intent(in   ) :: X
   real(DOUBLE)     , dimension(:), intent(in   ) :: Z
   type(APPORT_T)   , dimension(:), intent(in   ) :: Apport
   type(DEVERSOIR_T), dimension(:), intent(in   ) :: Deversoir
   real(DOUBLE)     , dimension(:), intent(inout) :: Qdeverse
   type(ERREUR_T)                 , intent(inout) :: Erreur

   end subroutine CQINJ

   end interface

end module M_CQINJ_I
