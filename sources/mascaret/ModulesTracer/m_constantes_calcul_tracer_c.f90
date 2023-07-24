!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_CONSTANTES_CALCUL_TRACER_C
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION

   ! Description des modeles de QE
   integer     , parameter :: AUCUN_MODELE    = 1
   integer     , parameter :: MODELE_O2       = 2
   integer     , parameter :: MODELE_BIOMASS  = 3
   integer     , parameter :: MODELE_EUTRO    = 4
   integer     , parameter :: MODELE_MICROPOL = 5
   integer     , parameter :: MODELE_THERMIC  = 6
   integer     , parameter :: NB_MODELE       = 6
   character(8), dimension(NB_MODELE), parameter :: NOM_MODELE_QE = &
       (/           &
       "AUCUN   " , &
       "O2      " , &
       "BIOMASS " , &
       "EUTRO   " , &
       "MICROPOL" , &
       "THERMIC "   &
        /)

end module M_CONSTANTES_CALCUL_TRACER_C
