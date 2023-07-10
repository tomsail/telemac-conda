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
  ! Initialise l'etat courant d'une instance de Mascaret a partir d'un etat prealablement sauvegarde par SAVE_ETAT_MASCARET
  ! Supprime la sauvegarde de l'State.
  !.................................................................................................................................
  subroutine SET_ETAT_MASCARET(Erreur, Identifiant, IdentifiantEtat, AndDelete)
    use M_APIMASCARET_STATIC
    implicit none
    integer, intent(out) :: Erreur                ! different de 0 si erreur
    integer, intent(in ) :: Identifiant           ! Identifiant de l'instance de Mascaret dont l'etat est modifie
    integer, intent(in ) :: IdentifiantEtat       ! Identifiant de l'etat Mascaret sauvegarde
    integer, intent(in ) :: AndDelete             ! Efface l'etat sauvegarde si different de zero

    interface
      function DUPLICATE_ETAT(Source, Dest)
        use M_ETAT_MASCARET_T
        implicit none
        integer                                 :: DUPLICATE_ETAT ! etat dupliquer et alloue
        type(ETAT_MASCARET_T),    intent(in)    :: Source         ! Instance du type derive que l'on souhaite dupliquer
        type(ETAT_MASCARET_T),    intent(inout) :: Dest           ! etat dupliquer et alloue
      end function DUPLICATE_ETAT
    end interface

    if (Identifiant <= 0) then
      MsgErreur = 'SET_ETAT_MASCARET - Identifiant negatif ou nul : pas autorise'
      Erreur = 2
      RETURN
    end if

    if (Identifiant > NB_MAX_MASCARET) then
      MsgErreur = 'SET_ETAT_MASCARET - Identifiant trop grand : pas autorise'
      Erreur = 2
      RETURN
    end if
    if (.not. ASSOCIATED(mascaretCree)) then
      MsgErreur = 'SET_ETAT_MASCARET - Aucun Mascaret de creer'
      Erreur = 2
      RETURN
    end if

    if (mascaretCree(Identifiant) == 0) then
      ptrMsgsErreurs(Identifiant) = 'SET_ETAT_MASCARET - Mauvais identifiant : Mascaret jamais cree'
      MsgErreur = 'SET_ETAT_MASCARET - Mauvais identifiant : Mascaret jamais cree'
      Erreur = 2
      RETURN
    end if

    if (etatMascaretSauve(IdentifiantEtat) == 0) then
      ptrMsgsErreurs(Identifiant) = 'SET_ETAT_MASCARET - Mauvais identifiant Etat : Etat non sauvegarde'
      MsgErreur = 'SET_ETAT_MASCARET - Mauvais identifiant Etat : Etat non sauvegarde'
      Erreur = 2
      RETURN
    end if

    !TODO: ask why it was commented
    !if (etatMascaretSauve(IdentifiantEtat) /= Identifiant) then
    !  ptrMsgsErreurs(Identifiant) = 'SET_ETAT_MASCARET - Mauvais identifiant Etat : Etat sauve pas de la bonne instance'
    !  MsgErreur = 'SET_ETAT_MASCARET - Mauvais identifiant Etat : Etat sauve pas de la bonne instance'
    !  Erreur = 2
    !  RETURN
    !end if

    Erreur = DESALLOUE_ETAT_MASCARET(ptrTabMascaret(Identifiant)%EtatMascaret, MsgErreur)
    if (Erreur /= 0) then
       ptrMsgsErreurs(Identifiant) = 'SET_ETAT_MASCARET - impossible de desallouer l''etat'
       MsgErreur = 'SET_ETAT_MASCARET - impossible de desallouer l''etat'
       RETURN
    end if

    Erreur = DUPLICATE_ETAT(ptrTabEtatMascaretSauve(IdentifiantEtat), ptrTabMascaret(Identifiant)%EtatMascaret)
    if (Erreur /=0) then
       MsgErreur = 'SAVE_ETAT_MASCARET - Impossible de dupliquer l etat courant'
       ptrMsgsErreurs(Identifiant) = 'SAVE_ETAT_MASCARET - Impossible de dupliquer l etat courant'
       RETURN
    endif
    
    if(AndDelete /= 0) then
        Erreur = DESALLOUE_ETAT_MASCARET(ptrTabEtatMascaretSauve(IdentifiantEtat), MsgErreur)
        if (Erreur /= 0) then
           ptrMsgsErreurs(Identifiant) = 'SET_ETAT_MASCARET - impossible de desallouer l''etat'
           MsgErreur = 'SET_ETAT_MASCARET - impossible de desallouer l''etat'
           RETURN
        end if
        etatMascaretSauve(IdentifiantEtat) = 0; ! libere l'emplacement
    endif

    
  end subroutine SET_ETAT_MASCARET
  