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

module M_DONNEES_CRUES_CALAGE_T
!***********************************************************************
! PROGICIEL : MASCARET       A. LEBOSSE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   !=========================== Declarations ==============================
   use M_PRECISION   ! type DOUBLE
   !
   TYPE DONNEES_CRUES_CALAGE_T
   !
      sequence
      Integer                                 :: NB_Crue ! Nombre de crues de calage
      Integer       ,dimension(:)   ,pointer  :: NMES    => null() ! Nombre de mesures par crue
      Integer       ,dimension(:)   ,pointer  :: NbApports => null()  ! Nombre de debits d'apport
      Real (DOUBLE) ,dimension(:)   ,pointer  :: DEBIT => null()    ! Debit crues de calage 
      Real (DOUBLE) ,dimension(:)   ,pointer  :: ZAVAL => null()    ! Debit crues de calage
      Real (DOUBLE) ,dimension(:,:) ,pointer  :: Abscisse => null()  ! Abscise des debits d'apoorts
      Real (DOUBLE) ,Dimension(:,:) ,pointer  :: Apport => null()   ! Debits apport crues de calage
      Real (DOUBLE) ,Dimension(:,:) ,pointer  :: Apport_X => null()   ! Debits apport crues de calage
      Real (DOUBLE) ,dimension(:,:) ,pointer  :: XMESU => null()    ! Numero de section fin
      Real (DOUBLE) ,dimension(:,:) ,pointer  :: ZMESU => null()    !
      Real (DOUBLE) ,dimension(:,:) ,pointer  :: POND => null()     !

   END TYPE DONNEES_CRUES_CALAGE_T

end module M_DONNEES_CRUES_CALAGE_T
