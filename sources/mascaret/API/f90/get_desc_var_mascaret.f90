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
   ! Recupere la liste des variables de Mascaret accompagne d'une description
   ! RQ : Ne depend pas de l'instance du modele ou de l'etat
   !.................................................................................................................................
  subroutine GET_DESC_VAR_MASCARET(Erreur, Identifiant, TabNom, TabDesc, Taille)
    use M_APIMASCARET_STATIC
    use M_MASCARET_T

    implicit none

    integer, intent(out)                         :: Erreur             ! different de 0 si erreur
    integer , intent(in)                         :: Identifiant        ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
    character(len= 40), dimension(*), intent(out):: TabNom             ! Tableau des noms de variable du modele ou de l'etat
    character(len=110), dimension(*), intent(out):: TabDesc            ! Tableau des descriptions de variable du modele ou de l'etat
    integer, intent(in)                          :: Taille             ! Taille des tableaux des noms et des descriptions de variable

    ! Variable locale
    integer                                      :: i

    character(len=4)                             :: istring

    Erreur = 0

    if (.not. tabNomVarInitialise) then

        if (Taille /= NB_VAR_MASCARET) then
            Erreur = 2
            MsgErreur = 'GET_DESC_VAR_MASCARET - Taille doit etre egale a NB_VAR_MASCARET'
            ptrMsgsErreurs(Identifiant) = 'GET_DESC_VAR_MASCARET - Taille doit etre egale a NB_VAR_MASCARET'
            return
        end if
        i = 1
        call GET_TAB_VAR_MASC(i, tabNomVar, tabDescriptionVar)
        if (NB_VAR_MASCARET/=i) then
            Erreur = 2
            MsgErreur = 'GET_DESC_VAR_MASCARET - NB_VAR_MASCARET n''est pas correct'
            write(istring,'(i4)') i
            ptrMsgsErreurs(Identifiant) = 'GET_DESC_VAR_MASCARET - NB_VAR_MASCARET n''est pas '//istring
            return
        endif
        tabNomVarInitialise = .true.

    end if

    do i=1, NB_VAR_MASCARET
        TabNom(i) = tabNomVar(i)
        TabDesc(i) = tabDescriptionVar(i)
!AP        write(0,*) i,'  ',trim(TabNom(i)),'  ',trim(tabDescriptionVar(i))
    enddo

  end subroutine GET_DESC_VAR_MASCARET


   !.................................................................................................................................
   ! Retourne de nombre de variable dans Mascaret
   ! C'est a dire la taille des tableaux TabNom et TabDesc
   ! .................................................................................................................................
  subroutine GET_NB_VAR_MASCARET(NbVarMascaret)
    use M_APIMASCARET_STATIC

    implicit none
    integer, intent(out) :: NbVarMascaret ! different de 0 si erreur

    NbVarMascaret = NB_VAR_MASCARET

  end subroutine GET_NB_VAR_MASCARET

   !.................................................................................................................................
   ! Retourne la description d'une variable a partir de son nom
   ! .................................................................................................................................
  subroutine GET_DESCRIPTION_VAR(NomVar, Description)
    use M_APIMASCARET_STATIC

    implicit none
    character(len=40) , intent(in)  :: NomVar      ! different de 0 si erreur
    character(len=110), intent(out) :: Description ! different de 0 si erreur

    integer i
    integer erreur             ! different de 0 si erreur
    character(len= 40), dimension(NB_VAR_MASCARET) :: TabNom             ! Tableau des noms de variable du modele ou de l'etat
    character(len=110), dimension(NB_VAR_MASCARET) :: TabDesc            ! Tableau des descriptions de variable du modele ou de l'etat

    if (.not.tabNomVarInitialise) then
      call GET_DESC_VAR_MASCARET(erreur, 1, TabNom, TabDesc, NB_VAR_MASCARET)
    endif
    Description = ""
    i = 1
    do
      if (i > NB_VAR_MASCARET) then
          Description = ""
          exit
      endif
      if (TRIM(tabNomVar(i)) == TRIM(NomVar)) then
         Description = tabDescriptionVar(i)
         exit
      endif
      i = i+1
    enddo

  end subroutine GET_DESCRIPTION_VAR

