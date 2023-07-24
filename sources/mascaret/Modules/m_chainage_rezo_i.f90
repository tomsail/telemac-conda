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

module M_CHAINAGE_REZO_I
!***********************************************************************
! PROGICIEL : MASCARET     F. ZAOUI
!
! VERSION : V8P4R0             EDF-CEREMA
!***********************************************************************
interface

subroutine CHAINAGE_REZO( Connect , Extremite , Matrice, OptionCasier, Liaison , Casier , Erreur )


  use M_CONNECT_T           ! Definition du type CONNECT_T
  use M_EXTREMITE_T         ! Definition du type EXTREMITE_T
  use M_REZOMAT_T           ! Definition du type REZOMAT_T
  use M_ERREUR_T            ! Definition du type ERREUR_T
  use M_MESSAGE_C           ! Messages d'erreur pre-definis
  use M_TRAITER_ERREUR_I    ! Traitement des erreurs
  use M_LIAISON_T           ! Definition du type LIAISON_T
  use M_CASIER_T            ! Definition du type CASIER_T
  use M_CONSTANTES_CASIER_C

  implicit none             ! Pas de type implicite par defaut

  type(ERREUR_T) , intent(out)  :: Erreur     ! Gestion des erreurs
  type(CONNECT_T) , intent(in)  :: Connect    ! Table descriptive du reseau de biefs
  type(EXTREMITE_T)  , dimension(:), intent(inout) :: Extremite
  type(REZOMAT_T) , intent(out) :: Matrice    ! Description de la matrice du probleme
  logical                           , intent(in   ) :: OptionCasier
  type(LIAISON_T)  , dimension(:)  , pointer, intent(in   ) :: Liaison    ! Liaisons
  type(CASIER_T)   , dimension(:)  , pointer, intent(in   ) :: Casier     ! Casiers

end subroutine CHAINAGE_REZO

end interface

end module M_CHAINAGE_REZO_I
