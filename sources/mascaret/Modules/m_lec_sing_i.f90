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

module M_LEC_SING_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_SING( &
             Singularite         , & ! Pertes de charges singulieres
             LoiHydrau        , & ! Loi hydraulique
             Connect          , & ! Connectivite du reseau
             X                , & ! Maillage
             Profil           , & ! Profils geometriques
             ProfDebBief      , & ! Premiers profils des biefs
             ProfFinBief      , & ! Derniers profils des biefs
             AbscRelExtDebBief, & ! Abscisse rel de l'extremite debut du bief
             AbscRelExtFinBief, & ! Abscisse rel de l'extremite debut du bief
             Noyau            , & ! Noyau de calcul utilise
             UlLst            , & ! Unite logique fichier listing
             unitNum          , & ! Unite logique .xcas
             Erreur             & ! Erreur
                      )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
   use M_ERREUR_T            ! Type ERREUR_T
   use M_LOI_T               ! Type LOI_T
   use M_PROFIL_T            ! Type PROFIL_T
   use M_SINGULARITE_T       ! Type SINGULARITE_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   type(SINGULARITE_T), dimension(:), pointer       :: Singularite
   type(LOI_T)        , dimension(:), pointer       :: LoiHydrau
   type(CONNECT_T)                  , intent(in   ) :: Connect
   real(DOUBLE)       , dimension(:), intent(in   ) :: X
   type(PROFIL_T)    , dimension(:)  , intent(in   ) :: Profil
   integer           , dimension(:)  , intent(in   ) :: ProfDebBief
   integer           , dimension(:)  , intent(in   ) :: ProfFinBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtFinBief
   integer                           , intent(in   ) :: Noyau
   integer                           , intent(in   ) :: UlLst
   integer, intent(in)                               :: unitNum

   ! Traitement des erreurs
   type(ERREUR_T), intent(inout) :: Erreur

   end subroutine LEC_SING

   end interface

end module M_LEC_SING_I
