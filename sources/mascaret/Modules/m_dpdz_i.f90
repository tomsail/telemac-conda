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

module M_DPDZ_I
!***********************************************************************
! PROGICIEL : MASCARET
!                        A. LEBOSSE    P. CHERUBINI    S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  DPDZ           ( & ! Resultats
               DPDZ1, DPDZ2   , & ! Gradient de P1 et P2 / Z
               NumSection     , & ! Gradient de P2 / Z
               Z              , & ! Cote de la surface libre
               ZREF           , & ! Cote de reference
               IDT, XDT       , & ! Reperage des sections / profils
               Profil         , & ! Profils geometrique
               ProfilPlan     , & ! Profils planimetrees
               Connect        , & ! Table de connectivite
               X              , & ! Maillage
               Impression     , & ! Flag d'impression
               UniteListing   , & ! Unite logique Fichier listing
               Erreur           & ! Erreur
                              )

!***********************************************************************
!
!  FONCTION :
!  --------
!           CALCUL DU GRADIENT DP / DZ DANS LA SECTION DE CALCUL NumSection
!           ( UTILE POUR LE SOUS PROGRAMME COEFFS )
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :   UniteListing  : Sortie listing
!   ----------------------
!   SOUS PROGRAMME APPELANT :  REZO
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!------------------------------------------------------------------------
   !========================== Declarations ================================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_PROFIL_T        ! Definition du type PROFIL_T
   use M_PROFIL_PLAN_T   ! Definition du type PROFIL_PLAN_T
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_CONNECT_T       ! Definition du type CONNECT_T
   use M_NUM_BIEF_S       ! Calcul du numero de bief d'une section
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Arguments .. 
   real(DOUBLE)                 , intent(  out) :: DPDZ1, DPDZ2
   type (PROFIL_T), dimension(:), intent(in   ) :: Profil
   type (PROFIL_PLAN_T)         , intent(in   ) :: ProfilPlan
   real(DOUBLE)   , dimension(:), intent(in   ) :: XDT
   integer        , dimension(:), intent(in   ) :: IDT
   real(DOUBLE)                 , intent(in   ) :: Z
   real(DOUBLE)                 , intent(in   ) :: ZREF
   integer                      , intent(in   ) :: NumSection
   type(CONNECT_T)              , intent(in   ) :: Connect
   real(DOUBLE), dimension(:)   , intent(in   ) :: X
   logical                      , intent(in   ) :: Impression
   integer                      , intent(in   ) :: UniteListing
   type(ERREUR_T)               , intent(inout) :: Erreur

   end subroutine DPDZ

   end interface

end module M_DPDZ_I
