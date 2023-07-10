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

module M_CALC_MAILLAGE_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine CALC_MAILLAGE ( &
     X                   , & ! Tableau des abscisses
     TypeMaillage        , & ! Type de calcul du maillage
     FichierMaillage     , & ! Fichier du maillage
     FichierSauveMaillage, & ! Fichier de sauvegarde du maillage
     Profil              , & ! Profils geometriques
     ProfDebBief         , & ! Premier profil d'un bief
     ProfFinBief         , & ! Dernier profil d'un bief
     AbscRelExtDebBief   , & ! Abscisse rel de l'extremite debut du bief
     AbscRelExtFinBief   , & ! Abscisse rel de l'extremite debut du bief
     impression_geo      , & ! Flag d'impression de la geometrie
     UniteListing        , & ! Unite logique fichier listing
     unitNum             , & ! Unite logique .xcas
     Erreur                & ! Erreur
                         )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_ERREUR_T            ! Type ERREUR_T
   use M_FICHIER_T            ! Type FICHIER_T
   use M_MAILLE_T            ! Types MAILLE_E_T et MAILLE_R_T
   use M_PROFIL_T            ! Type PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_LEC_MAILLAGE_I      ! Lecture du maillage
   use M_MAILLER_I           ! Interface de sous-programme
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XCAS_S

   implicit none

   type SECTION_REL_T
      sequence
      integer      :: Branche     ! Numero de branche
      real(DOUBLE) :: AbscisseRel ! Abscisse relative
   end type SECTION_REL_T

   ! Arguments
   real(DOUBLE)      , dimension(:), pointer       :: X
   integer                         , intent(  out) :: TypeMaillage
   type(FICHIER_T)                 , intent(inout) :: FichierMaillage
   type(FICHIER_T)                 , intent(inout) :: FichierSauveMaillage
   type(PROFIL_T)    , dimension(:), intent(in   ) :: Profil
   integer           , dimension(:), intent(in   ) :: ProfDebBief
   integer           , dimension(:), intent(in   ) :: ProfFinBief
   logical                         , intent(in   ) :: impression_geo
   integer                         , intent(in   ) :: UniteListing
   real(DOUBLE)      , dimension(:), intent(in   ) :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:), intent(in   ) :: AbscRelExtFinBief
   integer, intent(in)                             :: unitNum
   type(ERREUR_T)                  , intent(inout) :: Erreur

   end subroutine CALC_MAILLAGE

   end interface

end module M_CALC_MAILLAGE_I
