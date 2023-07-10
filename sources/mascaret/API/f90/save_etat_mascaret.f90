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
  ! Sauvegarde en memoire de l'etat courant d'une instance de Mascaret pour un usage ulterieur
  !.................................................................................................................................
  subroutine SAVE_ETAT_MASCARET(Erreur, Identifiant, IdentifiantEtat)
    use M_APIMASCARET_STATIC
    implicit none
    integer, intent(out) :: Erreur                ! different de 0 si erreur
    integer, intent(in ) :: Identifiant           ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
    integer, intent(out) :: IdentifiantEtat       ! Identifiant de l'etat Mascaret sauvegarde

    interface
      function DUPLICATE_ETAT(Source, Dest)
        use M_ETAT_MASCARET_T
        implicit none
        integer                                 :: DUPLICATE_ETAT ! etat dupliquer et alloue
        type(ETAT_MASCARET_T),    intent(in)    :: Source         ! Instance du type derive que l'on souhaite dupliquer
        type(ETAT_MASCARET_T),    intent(inout) :: Dest           ! etat dupliquer et alloue
      end function DUPLICATE_ETAT
    end interface
    ! Variables locales
    integer :: ErrEtatMascaretSauve =0, errTabEtatMascaret=0, i=0

    Erreur = TEST_INIT_AND_ID(Identifiant, 'SAVE_ETAT_MASCARET')
    if (Erreur > 0 ) then
       RETURN
    end if

    if (.not. ASSOCIATED(etatMascaretSauve)) then
        ALLOCATE(etatMascaretSauve(NB_MAX_ETAT_MASCARET_SAUV), STAT=ErrEtatMascaretSauve)
        if(ErrEtatMascaretSauve /= 0) then
            Erreur = 1
            ptrMsgsErreurs(Identifiant) = 'SAVE_ETAT_MASCARET - Impossible d allouer le tableau EtatMascaretSauve'
            RETURN
        else
            do i=1, NB_MAX_ETAT_MASCARET_SAUV
                etatMascaretSauve(i) = 0
            end do
        end if

        if(.not.associated(ptrTabEtatMascaretSauve)) &
             ALLOCATE(ptrTabEtatMascaretSauve(NB_MAX_ETAT_MASCARET_SAUV), STAT=errTabEtatMascaret)
        if(errTabEtatMascaret /= 0) then
            Erreur = 1
            ptrMsgsErreurs(Identifiant) = 'SAVE_ETAT_MASCARET - Impossible d allouer le tableau ptrTabEtatMascaretSauve'
            RETURN
        endif
    end if

    if(ErrEtatMascaretSauve /= 0) then
      Erreur = 1
      ptrMsgsErreurs(Identifiant) = 'SAVE_ETAT_MASCARET - Impossible d allouer le tableau EtatMascaretSauve'
      RETURN
    end if
    if(errTabEtatMascaret /= 0) then
      Erreur = 1
      ptrMsgsErreurs(Identifiant) = 'SAVE_ETAT_MASCARET - Impossible d allouer le tableau TabEtatMascaretSauve'
      RETURN
    end if

    do i=1, NB_MAX_ETAT_MASCARET_SAUV
        if (etatMascaretSauve(i) == 0) then ! 1er element de libre
            etatMascaretSauve(i) = Identifiant ! indication du numero d'instance de Mascaret d'ou provient l'etat sauve

            ! Affectation dans le tableau d'etat a sauvegarder de l'etat courant
            Erreur = DUPLICATE_ETAT(ptrTabMascaret(Identifiant)%EtatMascaret, ptrTabEtatMascaretSauve(i))
            if (Erreur /=0) then
               MsgErreur = 'SAVE_ETAT_MASCARET - Impossible de dupliquer l etat courant'
               ptrMsgsErreurs(Identifiant) = 'SAVE_ETAT_MASCARET - Impossible de dupliquer l etat courant'
               RETURN
            endif

            IdentifiantEtat = i
            MsgErreur = ''
            ptrMsgsErreurs(Identifiant) = ''
            RETURN
        end if
    end do

  end subroutine SAVE_ETAT_MASCARET