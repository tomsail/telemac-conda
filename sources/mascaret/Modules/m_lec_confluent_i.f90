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

module M_LEC_CONFLUENT_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_CONFLUENT ( &
                      Confluent , & ! Tableau des lois confluents 2D
                        Connect , & ! Table de connectivite du reseau
                   UniteListing , & ! Unite logique fichier listing
                          Noyau , & ! Noyau de calcul
                       unitNum  , & ! Unite logique .xcas
                         Erreur & ! Erreur
                                )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_CONFLUENT_T         ! Type CONFLUENT_T
   use M_CONNECT_T           ! Type CONNECT_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_PARAMETRE_C        ! Parametres
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   type(CONFLUENT_T) , dimension(:)  , pointer       :: Confluent
   type(CONNECT_T  )                 , intent(in   ) :: Connect
   integer                           , intent(in   ) :: UniteListing
   integer                           , intent(in   ) :: Noyau
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur

   end subroutine LEC_CONFLUENT

   end interface

end module M_LEC_CONFLUENT_I
