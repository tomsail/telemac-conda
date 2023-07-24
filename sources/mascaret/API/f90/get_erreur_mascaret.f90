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

! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
   ! .................................................................................................................................
   ! Retourne le message d'une erreur
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
  subroutine GET_ERREUR_MASCARET(Erreur, Identifiant, Message)
    use M_APIMASCARET_STATIC
    implicit none
    integer, intent(out)                         :: Erreur                ! different de 0 si erreur
    integer, intent(in )                         :: Identifiant
    character(LEN=256), intent(out)              :: Message ! Message d'erreur

    Erreur = 0
    if (Identifiant == 0) then
      Message = MsgErreur
      RETURN
    end if
    if (Identifiant < 0) then
      Message = 'GET_ERREUR_MASCARET - Identifiant negatif : pas autorise'
      Erreur = 2
      RETURN
    end if
    if (Identifiant > NB_MAX_MASCARET) then
      Message = 'GET_ERREUR_MASCARET - Identifiant trop grand : pas autorise'
      Erreur = 2
      RETURN
    end if
    if (.not. ASSOCIATED(mascaretCree)) then
      MsgErreur = 'GET_ERREUR_MASCARET - Aucun Mascaret de cree'
      Erreur = 2
      RETURN
    end if
    if (.not. ASSOCIATED(ptrMsgsErreurs)) then
      Message = 'GET_ERREUR_MASCARET - Aucun message cree'
      Erreur = 2
      RETURN
    end if

    Message = ptrMsgsErreurs(Identifiant)
    RETURN

  end subroutine GET_ERREUR_MASCARET
