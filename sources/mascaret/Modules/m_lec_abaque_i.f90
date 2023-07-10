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

module M_LEC_ABAQUE_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_ABAQUE( &
                     Abaque , & ! Barrage
              FichierAbaque , & ! Fichier abaque
                     Erreur   & ! Erreur
                        )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_MESSAGE_C            ! messages d'erreur
   use M_ERREUR_T             ! Type ERREUR_T
   use M_FICHIER_T            ! Type FICHIER_T
   use M_TRAITER_ERREUR_I     ! traitement des erreurs

   implicit none

   ! Arguments
   real(DOUBLE)    , dimension(6,6,5) , intent(inout) :: Abaque
   type(FICHIER_T)                    , intent(inout) :: FichierAbaque
   type(ERREUR_T)                     , intent(inout) :: Erreur

   end subroutine LEC_ABAQUE

   end interface

end module M_LEC_ABAQUE_I
