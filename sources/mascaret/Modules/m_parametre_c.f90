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

module M_PARAMETRE_C
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION

   ! PRECISIONS
   !------------
   real(DOUBLE), parameter :: EPS1  = 1.e-1_DOUBLE
   real(DOUBLE), parameter :: EPS2  = 1.e-2_DOUBLE
   real(DOUBLE), parameter :: EPS3  = 1.e-3_DOUBLE
   real(DOUBLE), parameter :: EPS4  = 1.e-4_DOUBLE
   real(DOUBLE), parameter :: EPS5  = 1.e-5_DOUBLE
   real(DOUBLE), parameter :: EPS6  = 1.e-6_DOUBLE
   real(DOUBLE), parameter :: EPS8  = 1.e-8_DOUBLE
   real(DOUBLE), parameter :: EPS10 = 1.e-10_DOUBLE
   real(DOUBLE), parameter :: EPS15 = 1.e-15_DOUBLE
   real(DOUBLE), parameter :: EPS52 = 5.e-2_DOUBLE
   real(DOUBLE), parameter :: EPS53 = 5.e-3_DOUBLE
   real(DOUBLE), parameter :: EPS54 = 5.e-4_DOUBLE
   real(DOUBLE), parameter :: EPSN6 = -1.e-6_DOUBLE
   real(DOUBLE), parameter :: INFINI= 1.e+9_DOUBLE
   real(DOUBLE), parameter :: SEPS =  EPS5           ! Epsilon pour les sections
                                                     ! dans Mascaret

   ! FRACTIONS
   !-----------
   real(DOUBLE), parameter :: W0  = 0._DOUBLE
   real(DOUBLE), parameter :: W1  = 1._DOUBLE
   real(DOUBLE), parameter :: W2  = 2._DOUBLE
   real(DOUBLE), parameter :: W09 = 0.9_DOUBLE
   real(DOUBLE), parameter :: W12 = 0.5_DOUBLE
   real(DOUBLE), parameter :: W13 = 0.33333333333333_DOUBLE
   real(DOUBLE), parameter :: W16 = 0.16666666666667_DOUBLE
   real(DOUBLE), parameter :: W23 = 0.66666666666667_DOUBLE
   real(DOUBLE), parameter :: W53 = 1.66666666666667_DOUBLE
   real(DOUBLE), parameter :: W32 = 1.5_DOUBLE

   ! CONSTANTES PHYSIQUES
   !----------------------
   real(DOUBLE), parameter :: GPES = 9.81_DOUBLE
   real(DOUBLE), parameter :: PI = 3.141592653589_DOUBLE

   ! Ecart minimal des cotes pour le temps d arrivee de l onde
   real(DOUBLE), parameter :: EPS_ONDE = 0.05_DOUBLE

end module M_PARAMETRE_C
