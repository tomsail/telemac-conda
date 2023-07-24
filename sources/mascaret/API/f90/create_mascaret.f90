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
  ! Initialise les ressources associees a une instance de Mascaret (Modele et Etat)
  ! Retourne l'identifiant de l'instance
  ! .................................................................................................................................
  subroutine CREATE_MASCARET(Erreur, Identifiant)
    use M_APIMASCARET_STATIC
    implicit none
    integer, intent(out) :: Erreur
    integer, intent(out) :: Identifiant
    integer :: errTabMascaret =0, errMascaretCree=0, errMsgsErreurs=0, errGeoModifiee=0, err=0
    integer :: i

    Identifiant = 0
    Erreur = 0

    if (.not.associated(mascaretCree)) then
        ALLOCATE(mascaretCree(NB_MAX_MASCARET), STAT=errMascaretCree)
        if(errMascaretCree /= 0) then
            Erreur = 1
            MsgErreur = 'CREATE_MASCARET - Impossible d allouer le tableau MascaretCree'
            RETURN
        else
                mascaretCree(1:NB_MAX_MASCARET) = 0
        end if
    endif

    if(.not.associated(geometrieModifiee)) then
        ALLOCATE(geometrieModifiee(NB_MAX_MASCARET), STAT=errGeoModifiee)
        if(errGeoModifiee /= 0) then
            DEALLOCATE(mascaretCree, STAT=err)
            nullify(mascaretCree)
            Erreur = 1
            MsgErreur = 'CREATE_MASCARET - Impossible d allouer le tableau geometrieModifiee'
            RETURN
        else
                geometrieModifiee(1:NB_MAX_MASCARET) = .FALSE.
        end if
    endif

    if(.not.associated(ptrTabMascaret)) then
        ALLOCATE(ptrTabMascaret(NB_MAX_MASCARET), STAT=errTabMascaret)
        if(errTabMascaret /= 0) then
            DEALLOCATE(mascaretCree, STAT=err)
            nullify(mascaretCree)
            DEALLOCATE(geometrieModifiee, STAT=err)
            nullify(geometrieModifiee)
            Erreur = 1
            MsgErreur = 'CREATE_MASCARET - Impossible d allouer le tableau d''instance de Mascaret'
            RETURN
        end if
    endif

    if(.not.associated(ptrMsgsErreurs)) then
        ALLOCATE(ptrMsgsErreurs(NB_MAX_MASCARET), STAT=errMsgsErreurs)
        if(errMsgsErreurs /= 0) then
            DEALLOCATE(mascaretCree, STAT=err)
            nullify(mascaretCree)
            DEALLOCATE(geometrieModifiee, STAT=err)
            nullify(geometrieModifiee)
            DEALLOCATE(ptrTabMascaret, STAT=err)
            nullify(ptrTabMascaret)
            Erreur = 1
            MsgErreur = 'CREATE_MASCARET - Impossible d allouer le tableau des messages d''erreurs'
            RETURN
        end if
    end if


    do i=1, NB_MAX_MASCARET
        if (mascaretCree(i) == 0) then ! 1er element de libre
            mascaretCree(i) = 1 ! le 1 indique que l'identifiant 1 est occupe
            Identifiant = i
            MsgErreur = ''
            ptrMsgsErreurs(Identifiant) = ''
            geometrieModifiee(Identifiant) = .FALSE.
            RETURN
        end if
    end do

    Identifiant = 0
    Erreur = 2 ! Impossible de creer un nouveau mascaret, plus de place disponible
    MsgErreur = 'CREATE_MASCARET - Impossible de creer un nouveau Mascaret'

  end subroutine CREATE_MASCARET
