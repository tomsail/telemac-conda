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
  !------------------------------------------------------------------------------------------------------------------------
  ! DESALLOCATION de tous les etats sauvegardes concernant un identifiant Mascaret
  !------------------------------------------------------------------------------------------------------------------------
  subroutine FREE_ALL_SAVE_ETAT_MASCARET(Erreur, Identifiant)
    use M_APIMASCARET_STATIC
    implicit none
    integer, intent(out) :: Erreur                ! different de 0 si erreur
    integer, intent(in ) :: Identifiant           ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"

    ! variable locale
    integer ::  i

    Erreur = 0
    if (.not. ASSOCIATED(etatMascaretSauve)) then
        RETURN
    end if
    do i=1, NB_MAX_ETAT_MASCARET_SAUV
        if (etatMascaretSauve(i) == Identifiant) then ! etat sauvegarde concernant l'identifiant Mascaret

            Erreur = DESALLOUE_ETAT_MASCARET(ptrTabEtatMascaretSauve(i), MsgErreur)
            if (Erreur /= 0) then
               ptrMsgsErreurs(Identifiant) = 'FREE_ALL_SAVE_ETAT_MASCARET - impossible de desallouer l''etat'
               MsgErreur = 'FREE_ALL_SAVE_ETAT_MASCARET - impossible de desallouer l''etat'
               RETURN
            end if
            etatMascaretSauve(i) = 0 ! libere l'emplacement
        end if
    end do

  end subroutine FREE_ALL_SAVE_ETAT_MASCARET
