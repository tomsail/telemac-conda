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

module M_DATE_S
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   contains

   subroutine DATE_S( ChaineDate )
   ! Ecriture de la date et de l'heure sur le canal specifie

   !=================== Declarations =======================
   implicit none
   ! Arguments
   Character(LEN=33)           , intent(  out) :: ChaineDate  
   ! Variables locales
   integer          , dimension(8)             :: date_time
   character(LEN=10), dimension(3)             :: temp
   character(9)     , dimension(12), parameter :: MOIS = &
    (/               &
       "JANVIER  " , &
       "FEVRIER  " , &
       "MARS     " , &
       "AVRIL    " , &
       "MAI      " , &
       "JUIN     " , &
       "JUILLET  " , &
       "AOUT     " , &
       "SEPTEMBRE" , &
       "OCTOBRE  " , &
       "NOVEMBRE " , &
       "DECEMBRE "   &    
    /)

    date_time(:) = 0
   !=================== Instructions =======================
   call DATE_AND_TIME(temp(1),temp(2),temp(3),date_time)
   write(ChaineDate,1000) date_time(3), trim(MOIS(date_time(2))), &
                         date_time(1), date_time(5), date_time(6)

   return

   1000 FORMAT('DATE : ',i2,' ',A,' ',i4,', ',i2,' H ',i2)

   end subroutine DATE_S

end module M_DATE_S
