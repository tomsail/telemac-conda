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

module M_PLANIM_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine PLANIM               ( &
! RESULTATS
               ProfilPlan          , & ! Profils planimetres
! DONNEES
               Profil              , & ! Caracteristiques des profils
               F1                  , &  ! Fonction impulsion
! Parametres
               DebProgressifLM     , & ! Debordement progressif lit majeur
               DebProgressifZS     , & ! Debordement progressif zones de stockaage
               ImpressionPlani     , & ! Impression du planimetrage
               UniteListing        , & ! Unite logique listing
               FrottParoiVerticale , & ! Conservation du frottement sur les parois verticales
               OptionCourlis       , & ! Activation de Courlis
               varsed              , & ! Courlis : profil evolution
               TempsInitial        , & ! Courlis
! Code de retour
               Erreur                & ! Erreur
                                   )

   ! **********************************************************************
   !
   !  FONCTION :
   !  --------
   !
   !               PLANIMETRAGE DES PROFILS
   !
   !-----------------------------------------------------------------------
   !
   !                         VARIABLES LOCALES
   ! _____________________________________________________________________
   ! ! DB1,DB2  ! R  !<-- ! LARGEUR AU MIROIR  ) PLANIMETRAGE
   ! ! DP1,DP2  ! R  !<-- ! PERIMETRE MOUILLE  )  1= MINEUR   2= MAJEUR
   ! ! DS1,DS2  ! R  !<-- ! SECTION MOUILEE    )
   ! ! DS2G     ! R  !<-- ! SECTION MOUILEE LIT MAJEUR GAUCHE
   ! ! DBS      ! R  !<-- ! LARGEUR AU MIROIR  ) PLANIMETRAGE
   ! ! DSS      ! R  !<-- ! SECTION MOUILEE    ) ZONE DE STOCKAGE
   ! ! DEB1,DEB2! R  ! -- ! DEBITANCE DANS LE LIT MINEUR ET LE LIT MAJEUR
   ! ! DZREF    ! R  ! -->! COTE LA PLUS BASSE DU PROFIL
   ! ! DXP,DYP  ! R  ! -->! COORDONNEES DES POINTS DEFINISSANT LE PROFIL
   ! !__________!____!____!______________________________________________
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-----------------------------------------------------------------------
   !
   !   FICHIERS  ENTREE/SORTIE : UniteListing : IMPRESSION DES RESULTATS GLOBAUX
   !   -------------------------
   !
   !   SOUS PROGRAMME APPELANT :    SUPERVISEUR
   !   -------------------------
   !   SOUS-PROGRAMMES APPELES :
   !   -------------------------
   !       - PAPY  : PLANIMETRAGE DES PROFILS ENTRES PAR POINTS
   !       - PLADEB: CALCUL DES SECTIONS DES LITS MAJEURS GAUCHE ET DROIT
   !                 OU DES ZONES DE STOCKAGE GAUCHE ET DROITE
   !                 SOUS LA COTE DE DEBORDEMENT DES PROFILS ENTRES PAR PTS
   !       - PLANMA: PLANIMETRAGE AUX SECTIONS POUR MASCARET
   !***********************************************************************

   !============================= Declarations ===========================
   use M_MY_CPT_PLANIM          ! Courlis planim counting
   use M_SHARE_VAR, ONLY: Temps ! Courlis
   use M_CSUR_I                  ! Courlis
   use M_PRECISION           ! Type DOUBLE
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_CONSTANTES_CALCUL_C ! NOYAU_MASCARET
   use M_PROFIL_T         ! Definition du type PROFIL_T
   use M_PROFIL_PLAN_T    ! Definition du type PROFIL_PLAN_T
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_PAPY_I           ! Interface de sous-programme
   use M_PLADEB_I         ! Interface de sous-programme
   use M_PLANMA_I         ! Interface de sous-programme

   !.. Implicit Declarations ..
   implicit none

   !.. Formal Arguments ..
   type (PROFIL_PLAN_T)               , intent(  out) :: ProfilPlan
   type (PROFIL_T)     , dimension(:) , intent(inout) :: Profil
   Real (DOUBLE)       , dimension(:,:), pointer  :: F1
   logical                            , intent(in   ) :: DebProgressifLM
   logical                            , intent(in   ) :: DebProgressifZS
   logical                            , intent(in   ) :: FrottParoiVerticale
   logical                            , intent(in   ) :: ImpressionPlani
   integer                            , intent(in   ) :: UniteListing
   type(ERREUR_T)                     , intent(inout) :: Erreur

   ! Courlis
   real(DOUBLE)                       , intent(in   ) :: TempsInitial
   real(DOUBLE)        , dimension(:), pointer        :: varsed
   logical                            , intent(in   ) :: OptionCourlis

   end subroutine PLANIM

   end interface

end module M_PLANIM_I
