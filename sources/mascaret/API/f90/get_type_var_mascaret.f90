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

  subroutine GET_TYPE_VAR_MASCARET(Erreur, Identifiant, NomVar, TypeVar, Categorie, Modif, dimVar)
    use M_APIMASCARET_STATIC
    use M_MASCARET_T
    implicit none

     integer          , intent(out)   :: Erreur                   ! different de 0 si erreur
     integer          , intent(in)    :: Identifiant              ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
     character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
     character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
     character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
     integer          , intent(out)   :: Modif                    ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
     integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)

    ! Variables locales
    character(LEN=256) MessageErreur
    character(LEN=40)  NomVarTrim
    logical            Modifiable

    Erreur = 0
    TypeVar   = "          "
    Categorie = "          "
    Modif = 0
    dimVar = -1

    NomVarTrim = TRIM(NomVar)

    Erreur = GET_TYPE_VAR_MASC(NomVarTrim, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)

    MsgErreur = MessageErreur
    if (Identifiant > 0) then
       ptrMsgsErreurs(Identifiant) = MessageErreur
    endif
    if (Modifiable) then
       Modif = 1
    else
       Modif = 0
    endif


  end subroutine GET_TYPE_VAR_MASCARET
