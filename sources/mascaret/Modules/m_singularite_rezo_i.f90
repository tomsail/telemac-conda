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

module M_SINGULARITE_REZO_I
!***********************************************************************
! PROGICIEL : MASCARET     F. ZAOUI
!
! VERSION : V8P4R0             EDF-CEREMA
!***********************************************************************
interface

subroutine SINGULARITE_REZO( Matrice , Singularite , X , Erreur )


  use M_REZOMAT_T           ! Definition du type REZOMAT_T
  use M_ERREUR_T            ! Definition du type ERREUR_T
  use M_MESSAGE_C           ! Messages d'erreur pre-definis
  use M_TRAITER_ERREUR_I    ! Traitement des erreurs
  use M_SINGULARITE_T       ! Definition du type SINGULARITE_T

  implicit none             ! Pas de type implicite par defaut

  type(ERREUR_T)  , intent(out)                  :: Erreur       ! Gestion des erreurs
  type(REZOMAT_T) , intent(inout)                :: Matrice      ! Description de la matrice du probleme
  type (SINGULARITE_T), dimension(:), intent(in) :: Singularite  ! Tableau des singularite
  real(DOUBLE)    , dimension(:) , intent(in   ) :: X            ! Pour connaitre le nombre de section

end subroutine SINGULARITE_REZO

end interface

end module M_SINGULARITE_REZO_I
