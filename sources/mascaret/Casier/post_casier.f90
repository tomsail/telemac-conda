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

subroutine POST_CASIER ( &
                         Casier  , &
                         Liaison , &
                         TEMPS )

!***********************************************************************
!
! PROGICIEL : MASCARET
!
! VERSION : V8P4R0                  EDF-CEREMA
!
!  FONCTION :  CALCUL DES VARIABLES MAX
!  --------    
!
!  SOUS PROGRAMMES APPELANT(S) : SUPERVISEUR
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S)   : Neant
!  -------------------------
!
!***********************************************************************


   !============================= Declarations ===========================
   use M_CASIER_T
   use M_LIAISON_T
   use M_PRECISION

   implicit none

   !.. Arguments ..
   type(CASIER_T)  , dimension(:), pointer :: Casier
   type(LIAISON_T) , dimension(:), pointer :: Liaison
   real(DOUBLE) ,            intent(in   ) :: TEMPS

   !.. Variables locales ..
   integer :: nb_casier , icasier , nb_liaison , iliaison

   ! Calcul des variables max casier
   ! -------------------------------
   nb_casier = size( Casier )
   do icasier = 1 , nb_casier
      if( Casier(icasier)%Cote > Casier(icasier)%CoteMax ) then
         Casier(icasier)%CoteMax  = Casier(icasier)%Cote
         Casier(icasier)%TempsMax = TEMPS
      end if
   end do

   ! Calcul des variables max liaison
   ! --------------------------------
   nb_liaison = size( Liaison )
   do iliaison = 1 , nb_liaison

      if( Liaison(iliaison)%DebitEchange > Liaison(iliaison)%DebitMax ) then
         Liaison(iliaison)%DebitMax      = Liaison(iliaison)%DebitEchange
         Liaison(iliaison)%TempsDebitMax = TEMPS
      end if

      if( Liaison(iliaison)%VitesseEchange > Liaison(iliaison)%VitesseMax ) then
         Liaison(iliaison)%VitesseMax      = Liaison(iliaison)%VitesseEchange
         Liaison(iliaison)%TempsVitesseMax = TEMPS
      end if
   end do

   return

end subroutine POST_CASIER
 