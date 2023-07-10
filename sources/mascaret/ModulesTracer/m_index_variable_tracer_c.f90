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

module M_INDEX_VARIABLE_TRACER_C
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   !***********************************************************************
   ! Module qui definit les constantes servant a reperer les variables
   !***********************************************************************
   ! Liste des indices sur les tableaux de variables
   !=================================================
   ! 1. Variables principales
   !--------------------------
   integer , parameter :: VARTR_X     = 1   ! maillage
   ! `X' doit etre la 1ere variable (indicee par 1)
   integer , parameter :: VARTR_ZREF  = 2   ! cote du fond
   integer , parameter :: VARTR_Q     = 3   ! debit total (Q1+Q2)
   integer , parameter :: VARTR_A     = 4   ! section mouillee (A1+A2)
   ! concentration traceurs
   integer, dimension(10) :: VARTR_CONC
   data VARTR_CONC / 5,6,7,8,9,10,11,12,13,14 /
   ! Constante representant le nombre de variables principales
   integer , parameter :: NB_VARTR_PRINCIPAL = 14
   ! Constante representant le nombre total de variables
   integer , parameter :: NB_TOT_VARTR = 14

end module M_INDEX_VARIABLE_TRACER_C
