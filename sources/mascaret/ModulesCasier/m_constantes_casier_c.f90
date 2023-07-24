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

module M_CONSTANTES_CASIER_C
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSEE    C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION
   use M_PARAMETRE_C

   !***********************************************************************
   !
   ! Constantes reperant le type de liaison
   !
   !***********************************************************************
   integer, parameter :: LIAISON_TYPE_SEUIL          = 1
   integer, parameter :: LIAISON_TYPE_CHENAL         = 2
   integer, parameter :: LIAISON_TYPE_SIPHON         = 3
   integer, parameter :: LIAISON_TYPE_ORIFICE        = 4
   integer, parameter :: LIAISON_TYPE_CANAL          = 5
   integer, parameter :: LIAISON_TYPE_NB_MAX         = 5
   integer, parameter :: LIAISON_TYPE_RIVIERE_CASIER = 1
   integer, parameter :: LIAISON_TYPE_CASIER_CASIER  = 2

   !***********************************************************************
   !
   ! Constantes de planimetrage
   !
   !***********************************************************************
   integer, parameter :: SURFACE_CONSTANTE = 1
   integer, parameter :: SURFACE_DEPEND_COTE = 2
   integer, parameter :: AUTOMATIQUE = 2
   integer, parameter :: MANUEL = 1

end module M_CONSTANTES_CASIER_C
