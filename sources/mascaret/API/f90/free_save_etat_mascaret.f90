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
  ! DESALLOCATION d'un etat sauvegarde
  !.................................................................................................................................
  subroutine FREE_SAVE_ETAT_MASCARET(Erreur, IdentifiantEtat)
      use M_APIMASCARET_STATIC
      implicit none
      integer, intent(out) :: Erreur                ! different de 0 si erreur
      integer, intent(in ) :: IdentifiantEtat       ! Identifiant de l'etat Mascaret sauvegarde � supprimer

      if (etatMascaretSauve(IdentifiantEtat) /= 0) then
        Erreur = DESALLOUE_ETAT_MASCARET(ptrTabEtatMascaretSauve(IdentifiantEtat), MsgErreur)
        if (Erreur /= 0) then
             ptrMsgsErreurs(etatMascaretSauve(IdentifiantEtat)) = 'FREE_SAVE_ETAT_MASCARET - impossible de desallouer l''etat'
             MsgErreur = 'FREE_SAVE_ETAT_MASCARET - impossible de desallouer l''etat'
             RETURN
        end if
        etatMascaretSauve(IdentifiantEtat) = 0 ! libere l'emplacement
      else
          MsgErreur = 'FREE_SAVE_ETAT_MASCARET - valeur de l''identifiant incorrect'
          Erreur = 1
      end if

  end subroutine FREE_SAVE_ETAT_MASCARET
