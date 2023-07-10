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

module M_LEC_RESEAU_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_RESEAU( &
                     NbBief , & ! Nombre de biefs
          AbscRelExtDebBief , & ! Abscisse rel de l'extremite debut du bief
          AbscRelExtFinBief , & ! Abscisse rel de l'extremite debut du bief
          AbscAbsExtDebBief , & ! Abscisse abs de l'extremite debut du bief
          AbscAbsExtFinBief , & ! Abscisse abs de l'extremite debut du bief
                    NbNoeud , & ! Nombre de noeuds
                 NbExtNoeud , & ! Nombre d'extremite relie a chaque noeud
                 ExtDebBief , & ! Numero de l'extremite debut de chaque bief
                 ExtFinBief , & ! Numero de l'extremite fin de chaque bief
                   ExtNoeud , & ! Numero d'extremite lie a un noeud
                 NbExtLibre , & ! Nombre d'extremites libres
                NumExtLibre , & ! Numero d'extremite libre
                  Extremite , & ! Extremites libres
                  LoiHydrau , & ! Lois hydrauliques
           ImpressionReseau , & ! Flag d'impression du reseau
                      UlLst , & ! Unite logique fichier listing
                     Profil , & ! Profils geometriques
                ProfDebBief , & ! Premiers profils des biefs
                ProfFinBief , & ! Derniers profils des biefs
                      Noyau , & ! Noyau de calcul
                   unitNum  , & ! Unite logique .xcas
                     Erreur & ! Erreur
                         )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_EXTREMITE_T         ! Type EXTREMITE_T
   use M_LOI_T               ! Types LOI_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_PARAMETRE_C        ! parametres de calcul
   use M_TRAITER_ERREUR_I    ! Traitement de l'erreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XCAS_S

   implicit none

   ! Arguments
   integer                           , intent(  out) :: NbBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscRelExtFinBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscAbsExtDebBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscAbsExtFinBief
   integer                           , intent(  out) :: NbNoeud
   integer           , dimension(:)  , pointer       :: NbExtNoeud
   integer           , dimension(:)  , pointer       :: ExtDebBief
   integer           , dimension(:)  , pointer       :: ExtFinBief
   integer           , dimension(:,:), pointer       :: ExtNoeud
   integer                           , intent(  out) :: NbExtLibre
   integer           , dimension(:)  , pointer       :: NumExtLibre
   type(EXTREMITE_T) , dimension(:)  , pointer       :: Extremite
   type(LOI_T)       , dimension(:)  , intent(in   ) :: LoiHydrau
   logical                           , intent(in   ) :: ImpressionReseau
   integer                           , intent(in   ) :: UlLst
   type(PROFIL_T)    , dimension(:)  , intent(in   ) :: Profil
   integer           , dimension(:)  , intent(in   ) :: ProfDebBief
   integer           , dimension(:)  , intent(in   ) :: ProfFinBief
   integer                           , intent(in   ) :: Noyau
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur

   end subroutine LEC_RESEAU

   end interface

end module M_LEC_RESEAU_I
