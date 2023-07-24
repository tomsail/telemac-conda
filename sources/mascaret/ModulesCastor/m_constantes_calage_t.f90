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

module M_CONSTANTES_CALAGE_T
!***********************************************************************
! PROGICIEL : MASCARET       N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION   ! type DOUBLE

   TYPE CONSTANTES_CALAGE_T
      sequence
      Integer                                 :: NITER    ! Nombre d'iterations maximales
      Real (DOUBLE)                           :: PRECIS   ! Precision de la convergence
      Integer                                 :: IESTIM   ! Indicateur si lit mineur ou lit majeur
   END TYPE CONSTANTES_CALAGE_T

end module M_CONSTANTES_CALAGE_T
