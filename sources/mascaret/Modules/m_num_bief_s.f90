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

module M_NUM_BIEF_S
!***********************************************************************
! PROGICIEL : MASCARET       N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   contains

   integer function NUM_BIEF_S( &
                          Connect , & ! Connectivite
                          Section , & ! Numero de section
                           Erreur &   ! Erreur
                                 )

   !======================== Declarations =========================
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I    ! Traitement de l'erreur
   use M_MESSAGE_C           ! Liste des messages d'erreur

   implicit none

   type(CONNECT_T)            , intent (in   ) :: Connect
   integer                    , intent (in   ) :: Section
   type(ERREUR_T)             , intent (inout) :: Erreur
   integer :: ibief
   integer :: debut_bief
   integer :: fin_bief
   integer :: numero_bief
   !character(132) :: !arbredappel_old

   !======================== Instructions =========================
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>NUM_BIEF'

   numero_bief = 0

   do ibief = 1 , size( Connect%OrigineBief )
      debut_bief = Connect%OrigineBief( ibief )
      fin_bief   = Connect%FinBief( ibief )
      if( section >= debut_bief .and. section <= fin_bief ) then
         numero_bief = ibief
         exit
      endif
   end do

   if( numero_bief == 0 )then
      Erreur%Numero = 84
      Erreur%ft = err_84
      Erreur%ft_c = err_84c
      call TRAITER_ERREUR (Erreur, Section)
      NUM_BIEF_S = -1
      return
   endif

   NUM_BIEF_S = numero_bief

   !Erreur%arbredappel = !arbredappel_old

   return

   end function NUM_BIEF_S

end module M_NUM_BIEF_S
