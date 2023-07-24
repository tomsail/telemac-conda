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

module M_ABS_ABS_S
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   contains

   function ABS_ABS_S( &
                    NumBief , &
           AbscisseRelative , &
                     Profil , &
                ProfDebBief , &
                ProfFinBief , &
                     Erreur &
                         )

      !==================== Declarations ===========================
      use M_PRECISION
      use M_ERREUR_T            ! Type ERREUR_T
      use M_MESSAGE_C           ! Messages d'erreur
      use M_PROFIL_T            ! Type PROFIL_T
      use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

      implicit none

      ! Arguments
      real(DOUBLE)                                :: ABS_ABS_S
      integer                     , intent(in   ) :: NumBief
      real(DOUBLE)                , intent(in   ) :: AbscisseRelative
      type(PROFIL_T), dimension(:), intent(in   ) :: Profil
      integer       , dimension(:), intent(in   ) :: ProfDebBief
      integer       , dimension(:), intent(in   ) :: ProfFinBief
      type(ERREUR_T)              , intent(inout) :: Erreur

      ! Variables locales
      integer        :: prof_deb         ! Premier profil du bief
      real(DOUBLE)   :: distance         ! Distance depuis le debut du bief
      !character(132) :: !arbredappel_old

      !========================= Instructions ===========================

      ! INITIALISATION
      ! --------------
      Erreur%Numero = 0
      !arbredappel_old = trim(!Erreur%arbredappel)
      !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>ABS_ABS_S'

      prof_deb  = ProfDebBief(NumBief)
      distance  = AbscisseRelative - Profil(prof_deb)%AbsRel
      ABS_ABS_S = Profil(prof_deb)%AbsAbs + distance

      ! Fin des traitements

      !Erreur%arbredappel = !arbredappel_old

      return

   end function ABS_ABS_S

end module M_ABS_ABS_S
