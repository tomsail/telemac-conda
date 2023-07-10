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
   !.................................................................................................................................
   ! Libere les ressources associees a une instance de Mascaret (Modele et Etat)
   ! RQ : Depend de l'instance du modele ou de l'etat
   ! .................................................................................................................................
  subroutine DELETE_MASCARET(Erreur, Identifiant)
    use M_APIMASCARET_STATIC
    implicit none
    integer, intent(out) :: Erreur
    integer, intent(in ) :: Identifiant

    ! Variables locales
    character(LEN=256) MessageErreur

    Erreur = TEST_INIT_AND_ID(Identifiant, 'DELETE_MASCARET')
    if (Erreur > 0 ) then
      RETURN
    end if


     Erreur = DESALLOUE_MASCARET(ptrTabMascaret(Identifiant), MessageErreur)

     if (Erreur > 0) then
       ptrMsgsErreurs(Identifiant) = MessageErreur
       MsgErreur = 'DELETE_MASCARET - Impossible de DESALLOUE MASCARET'
       RETURN
     endif

     mascaretCree(Identifiant) = 0
     MsgErreur = ''
     
     call FREE_ALL_SAVE_ETAT_MASCARET(Erreur, Identifiant)

  end subroutine DELETE_MASCARET
