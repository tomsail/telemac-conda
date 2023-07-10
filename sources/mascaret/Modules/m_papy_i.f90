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

module M_PAPY_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             E. BENSLAMA
!                             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  PAPY            ( &
                                 ! Resultats
                     DB1, DB2, DBS , & ! Variables largeur
                          DP1, DP2 , & ! au miroir, perimetre mouille
                          DS1, DS2 , & ! section mouillee, planimetres
                         DSS, DS2G , & ! pour la cote cote
                                 ! Donnees
                           NumProf , & ! Numero du profil
                              Cote , & ! Cote de calcul des divers parametres
                         LimiteMin , & ! Limites du lit mineur du profil
                         LimiteMaj , & ! Limites du lit majeur du profil
                          DXP, DYP , & ! Coordonnees geometriques des points du profil
                  S2SousCoteDebord , & ! Surfaces mouillees majeur sous cote de debordement
                  SSSousCoteDebord , & ! Surfaces mouillees stockage sous cote de debordement
                   DebProgressifLM , & ! Debordement progressif en lit majeur
                   DebProgressifZS , & ! Debordement progressif en zone de stockage
               FrottParoiVerticale , & ! Conservation du frottement sur les parois verticales
                   ImpressionPlani , & ! Impression du planimetrage
                      UniteListing , & ! Unite logique listing
                            Erreur & ! Erreur
                                   )

   !***********************************************************************
   !
   !  FONCTION :
   !
   !           CALCUL DES LARGEURS ,SECTIONS ET PERIMETRES MOUILLES
   !                  (LIT MINEUR , LIT MAJEUR ET ZONE DE STOCKAGE)
   !                  DE CHAQUE PROFIL
   !
   !                  PROFILS DONNES PAR POINTS
   !                  =========================
   !
   !-----------------------------------------------------------------------
   !
   !                         VARIABLES LOCALES
   ! .___________.________________________________________________________
   ! !   MINEUR  ! L  ! -- ! DEFINITION DU LIT MINEUR
   ! !   MAJEUR  ! L  ! -- ! DEFINITION DU LIT MAJEUR
   ! !   LG,LD   ! I  ! -->! LIMITES D'ECOULEMENT
   ! !   XG,XD   ! R  ! -->! LIMITES D'ECOULEMENT
   ! !___________!____!____!______________________________________________
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-----------------------------------------------------------------------
   !
   !   FICHIERS  ENTREE/SORTIE :  UniteListing  : IMPRESSION LISTING
   !   -------------------------
   !
   !   SOUS PROGRAMME APPELANT :  PLANIM
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :  ---
   !   -------------------------
   !***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION           ! Type DOUBLE
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   !.. Implicit Declarations ..
   implicit none

   integer, parameter :: NB_MAX_CHENAUX = 100

   !.. Formal Arguments ..
   real(DOUBLE), intent(  out) :: DB1,DB2,DBS
   real(DOUBLE), intent(  out) :: DP1,DP2
   real(DOUBLE), intent(  out) :: DS1,DS2,DS2G,DSS
   logical     , intent(in   ) :: DebProgressifLM
   logical     , intent(in   ) :: DebProgressifZS
   logical     , intent(in   ) :: FrottParoiVerticale
   logical     , intent(in   ) :: ImpressionPlani
   integer     , intent(in   ) :: UniteListing
   integer     , intent(in   ) :: NumProf
   type(ERREUR_T), intent(inout) :: Erreur
   real(DOUBLE), dimension(2), intent(in   ) :: S2SousCoteDebord
   real(DOUBLE), dimension(2), intent(in   ) :: SSSousCoteDebord
   real(DOUBLE)              , intent(inout) :: Cote
   integer     , dimension(2), intent(in   ) :: LimiteMin
   integer     , dimension(2), intent(in   ) :: LimiteMaj
   real(DOUBLE), dimension(:), intent(in   ) :: DXP
   real(DOUBLE), dimension(:), intent(in   ) :: DYP

   end subroutine PAPY

   end interface

end module M_PAPY_I
