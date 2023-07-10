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

MODULE M_APIMASCARET_STATIC
!***********************************************************************
! PROGICIEL : MASCARET        J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
  use M_MASCARET_T
  implicit none

   ! Constantes
   integer,  PARAMETER :: NB_MAX_MASCARET           = 1000
   integer,  PARAMETER :: NB_VAR_MASCARET           =  548
   integer,  PARAMETER :: NB_MAX_ETAT_MASCARET_SAUV = 1000

   real(8), dimension(6,6,5), save :: ABAQUE

   ! Variable commune a tous les instance de MASCARET
   integer,             dimension(:),  pointer,  save :: mascaretCree   => null()
   type(MASCARET_T),    dimension(:),  pointer,  save :: ptrTabMascaret => null()
   character(len=256),  dimension(:),  pointer,  save :: ptrMsgsErreurs => null()
   logical,             dimension(:),  pointer,  save :: geometrieModifiee => null()
   character(len=256),  save :: MsgErreur = ''
   character(len=40),   dimension(NB_VAR_MASCARET),  save :: tabNomVar
   character(len=110),  dimension(NB_VAR_MASCARET),  save :: tabDescriptionVar
   logical,             save :: tabNomVarInitialise = .FALSE.

   type(ETAT_MASCARET_T), dimension(:), pointer,  save :: ptrTabEtatMascaretSauve => null()
   integer,               dimension(:), pointer,  save :: etatMascaretSauve   => null()

  contains

    function TEST_INIT_AND_ID(Identifiant, NomSubAppelante)
      implicit none

      integer                          :: TEST_INIT_AND_ID    ! different de 0 si erreur
      integer, intent(in )             :: Identifiant         ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
      character(*), intent(in )        :: NomSubAppelante     ! Nom de la subroutine appelante

      TEST_INIT_AND_ID = 0

      if (Identifiant <= 0) then
        MsgErreur = NomSubAppelante//' - Id number must be positive'
        TEST_INIT_AND_ID = 2
        RETURN
      end if
      if (Identifiant > NB_MAX_MASCARET) then
        MsgErreur = NomSubAppelante//' - Id number too high'
        TEST_INIT_AND_ID = 2
        RETURN
      end if
      if (.not. ASSOCIATED(mascaretCree)) then
        MsgErreur = NomSubAppelante//' - No new Mascaret model'
        TEST_INIT_AND_ID = 2
        RETURN
      end if
      if (mascaretCree(Identifiant) == 0) then
        ptrMsgsErreurs(Identifiant) = NomSubAppelante//' - Mascaret instance is not created'
        TEST_INIT_AND_ID = 2
        RETURN
      end if

      ptrMsgsErreurs(Identifiant) = ''

      RETURN
    end function TEST_INIT_AND_ID

end module M_APIMASCARET_STATIC
