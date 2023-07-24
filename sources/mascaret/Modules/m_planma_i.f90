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

module M_PLANMA_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine PLANMA( &
                      ! Resultats
            SectionPlan , & ! Section planimetrees
                      ! Donnees
                 Profil , & ! Caracteritiques des profils
             ProfilPlan , & ! Profils planimetrees
                      ! Modele
                  NbPas , & ! Nombre de pas de planimetrage
                      X , & ! Abscisse des sections de calcul
                     DZ , & ! Caracteristiques des sections
                     XD , & ! Abscisse des interfaces
                    DZD , & ! Pas de planimetrage d'une interface
                    XDT , & ! Position relative de la section/Profil
                    IDT , & ! Profil de donnees amont de la section
                Connect , & ! Connectivite du reseau
                    CF1 , & ! Coefficient de frottement mineur
                    CF2 , & ! Coefficient de frottement majeur
                      ! Parametre
   PresenceZoneStockage , & ! Presence de zone de stockage
          LoiFrottement , & ! Loi de frottement utilisee
          OptionCourlis , & ! Activation Courlis
                 varsed , & ! Courlis : profil evolution
           TempsInitial , & ! Courlis
                 Erreur )

   !***********************************************************************
   ! FONCTION :
   ! --------
   ! CODE MASCARET : CALCUL DES TABLEAUX PLANIMETRES COMPLETS POUR MASCARET
   !
   !
   ! -----------------------------------------------------------------------
   !
   !   FICHIERS  ENTREE/SORTIE :	--
   !   -------------------------
   !
   !   SOUS PROGRAMME APPELANT :    PLANIM
   !   -------------------------
   !   SOUS-PROGRAMMES APPELES :
   !   -------------------------
   !       - CALAIG : CALCUL DE L'INTEGRALE DE 0 A S DE -1/S*DC/DX (A S CONSTANT)
   !       - CALDEB : CALCUL DE LA DEBITANCE
   !       - CALDYG : CALCUL DE DY/DX A S CONSTANT
   !***********************************************************************

   !============================= Declarations ============================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C    ! GPES, SEPS
   use M_MESSAGE_C      ! Messages d'erreur
   use M_PROFIL_PLAN_T  ! Profils planimetres
   use M_PROFIL_T       ! Caracteristiques des profils
   use M_SECTION_PLAN_T ! Sections planimetres
   use M_SECTION_T      ! Caracteristiques des Sections
   use M_CONNECT_T      ! Tables de connectivite du reseau
   use M_ERREUR_T       ! Type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_CALAIG_I       ! Interface du sous-programme CALAIG
   use M_CALDEB_I       ! Interface du sous-programme CALDEB
   use M_CALDYG_I       ! Interface du sous-programme CALDYG

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! Resultat
   type(SECTION_PLAN_T)          , intent(  out) :: SectionPlan
   ! Donnees
   type(PROFIL_T) , dimension(:) , intent(in   ) :: Profil
   type(PROFIL_PLAN_T)           , intent(in   ) :: ProfilPlan
   ! Modele
   integer                       , intent(in   ) :: NbPas
   real(DOUBLE), dimension(:)    , intent(in   ) :: X
   real (DOUBLE)  , dimension(:) , pointer       :: DZ
   real(DOUBLE), dimension(:)    , pointer       :: XD
   real(DOUBLE), dimension(:)    , pointer       :: DZD
   real(DOUBLE), dimension(:)    , intent(in   ) :: XDT
   integer     , dimension(:)    , intent(in   ) :: IDT
   type(CONNECT_T)               , intent(in   ) :: Connect
   real(DOUBLE), dimension(:)    , intent(in   ) :: CF1
   real(DOUBLE), dimension(:)    , intent(in   ) :: CF2
   ! Parametre
   logical     ,                   intent(  out) :: PresenceZoneStockage
   integer     ,                   intent(in   ) :: LoiFrottement
   type(ERREUR_T),                 intent(inout) :: Erreur
   ! Courlis
   real(DOUBLE)    , dimension(:), pointer       :: varsed
   real(DOUBLE)                  , intent(in   ) :: TempsInitial
   logical                       , intent(in   ) :: OptionCourlis



   end subroutine PLANMA

   end interface

end module M_PLANMA_I
