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

module M_LEC_PLANIM_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_PLANIM( &
                     Profil , & ! Profils geometriques
                      UlLst , & ! Unite logique fichier listing
                   unitNum  , & ! Unite logique .xcas
                     Erreur & ! Erreur
                         )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   type(PROFIL_T)    , dimension(:)  , intent(inout) :: Profil
   integer                           , intent(in   ) :: UlLst
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur

   end subroutine LEC_PLANIM

  end interface

end module M_LEC_PLANIM_I
