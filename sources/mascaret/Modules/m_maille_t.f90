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

module M_MAILLE_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
use M_PRECISION

TYPE MAILLE_R_T
  sequence
  integer            :: Branche       ! Branche
  real(DOUBLE)       :: AbscisseDeb   ! Abscisse debut
  real(DOUBLE)       :: AbscisseFin   ! Abscisse fin
  integer            :: NbSection     ! Nombre de sections entre

END TYPE MAILLE_R_T

TYPE MAILLE_E_T
  sequence
  integer            :: ProfilDeb     ! Numero du profil debit
  integer            :: ProfilFin     ! Numero du profil fin
  real(DOUBLE)       :: Pas           ! Distance desiree entre sections

END TYPE MAILLE_E_T

end module M_MAILLE_T
