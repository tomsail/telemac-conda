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

module M_CSURM1_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   function CSURM1          ( &
        SurfaceMouillee     , & ! Surface mouillee
        DZ                  , & ! Pas de planimetrage
        SurfaceMouilleePlani, & ! Surface mouille planimetree
        Erreur                & ! Erreur
                            )

!***********************************************************************
!     CODE MASCARET : CALCUL DU TIRANT D'EAU A PARTIR DE LA SURFACE
!                     MOUILLEE SUR MAILLAGE INITIAL
!                     INVERSE DE CSUR
!
!-----------------------------------------------------------------------
!  SurfaceMouilleePlani fait partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! Erreur
   use M_DICHO_I   ! Interface du sous-programme DICHO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                  :: CSURM1
   real(DOUBLE),                   intent(in)    :: SurfaceMouillee
   real(DOUBLE)                  , intent(in)    :: DZ
   real(DOUBLE), dimension(:)    , intent(in)    :: SurfaceMouilleePlani
   Type (ERREUR_T)               , intent(inout) :: Erreur

   end function CSURM1

   end interface

end module M_CSURM1_I
