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

module M_MASCARET_T
!***********************************************************************
! PROGICIEL : MASCARET        J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
use M_MODELE_MASCARET_T         ! Type MODELE_MASCARET_T
use M_ETAT_MASCARET_T           ! Type ETAT_MASCARET_T

  !=========================== Declarations ==============================

  TYPE MASCARET_T
     sequence
     type(MODELE_MASCARET_T)          :: ModeleMascaret
     type(ETAT_MASCARET_T)            :: EtatMascaret
  END TYPE MASCARET_T
contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_MASC(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

      ! --- MODELE_MASCARET_T ---
      call GET_TAB_VAR_MODELE_MASCARET(i, tabNomVar, tabDescriptionVar)
     ! --- MODELE_ETAT_MASCARET_T ---
      call GET_TAB_VAR_ETAT_MASCARET(i, tabNomVar, tabDescriptionVar)
      i=i-1

      return

    end subroutine GET_TAB_VAR_MASC

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_MASC(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_MASC        ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_MASC     = 0
      TypeVar               = ""
      Categorie             = ""
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( INDEX(NomVar,'Model.') > 0) then
          GET_TYPE_VAR_MASC = GET_TYPE_VAR_MODELE_MASCARET(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else if ( INDEX(NomVar,'State.') > 0) then
          GET_TYPE_VAR_MASC = GET_TYPE_VAR_ETAT_MASCARET(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else
        GET_TYPE_VAR_MASC     = 1
        TypeVar               = "?"
        Categorie             = ""
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_MASC - Unknown variable name"
      end if


    end function GET_TYPE_VAR_MASC

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
! .................................................................................................................................

   function GET_TAILLE_VAR_MASC(Instance, NomVar, index1, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_MASC           ! different de 0 si erreur
      type(MASCARET_T),       intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in) :: index1                         ! valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_MASC = 0
      taille1             = 0
      taille2             = 0
      taille3             = 0
      MessageErreur       = ""


      if (INDEX(NomVar,'Model.') > 0) then
             GET_TAILLE_VAR_MASC = GET_TAILLE_VAR_MODELE_MASCARET(Instance%ModeleMascaret,&
                                               NomVar, index1, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'State.') > 0) then
             GET_TAILLE_VAR_MASC = GET_TAILLE_VAR_ETAT_MASCARET(Instance%EtatMascaret,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else
         GET_TAILLE_VAR_MASC = 1
         taille1                 = -1
         taille2                 = -1
         taille3                 = -1
         MessageErreur           = "GET_TAILLE_VAR_MASC - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_MASC

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
! .................................................................................................................................

   function GET_DOUBLE_MASC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_MASC            ! different de 0 si erreur
      type(MASCARET_T),       intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_MASC = 0
      valeur          = -9999999.9999
      MessageErreur   = ""


      if (INDEX(NomVar,'Model.') > 0) then
          GET_DOUBLE_MASC = GET_DOUBLE_MODELE_MASCARET(instance%ModeleMascaret, NomVar, &
                                    index1, index2, index3, valeur, MessageErreur)
      else if ( INDEX(NomVar,'State.') > 0) then
          GET_DOUBLE_MASC = GET_DOUBLE_ETAT_MASCARET(instance%EtatMascaret, NomVar, &
                                    index1, index2, index3, valeur, MessageErreur)
      else
         GET_DOUBLE_MASC = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_MASC - Unknown variable name"
      end if
   end function GET_DOUBLE_MASC


   function GET_INT_MASC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_MASC           ! different de 0 si erreur
      type(MASCARET_T),       intent(in) :: Instance               ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                 ! Nom de la variable du modele
      integer,                intent(in) :: index1                 ! valeur du 1er indice
      integer,                intent(in) :: index2                 ! valeur du 2e  indice
      integer,                intent(in) :: index3                 ! valeur du 3e  indice
      integer,                intent(out):: valeur                 ! valeur de l'entier de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur          ! Message d'erreur

      GET_INT_MASC = 0
      valeur           = -9999
      MessageErreur    = ""

      if (INDEX(NomVar,'Model.') > 0) then
          GET_INT_MASC = GET_INT_MODELE_MASCARET(instance%ModeleMascaret, NomVar, &
                                    index1, index2, index3, valeur, MessageErreur)
       else if ( INDEX(NomVar,'State.') > 0) then
          GET_INT_MASC = GET_INT_ETAT_MASCARET(instance%EtatMascaret, NomVar, &
                                    index1, index2, index3, valeur, MessageErreur)
      else
         GET_INT_MASC = 1
         valeur           = -9999
         MessageErreur    = "GET_INT_MASC - Unknown variable name"
      end if
   end function GET_INT_MASC

   function GET_BOOL_MASC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_BOOL_MASC          ! different de 0 si erreur
      type(MASCARET_T),       intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(out):: valeur                     ! valeur du boolean de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_BOOL_MASC = 0
      valeur            = .FALSE.
      MessageErreur     = ""

      if (INDEX(NomVar,'Model.') > 0) then
          GET_BOOL_MASC = GET_BOOL_MODELE_MASCARET(instance%ModeleMascaret, NomVar, &
                                    index1, index2, index3, valeur, MessageErreur)
      else
         GET_BOOL_MASC = 1
         valeur           = .FALSE.
         MessageErreur    = "GET_BOOL_MASC - Unknown variable name"
      end if
   end function GET_BOOL_MASC

   function GET_STRING_MASC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_MASC        ! different de 0 si erreur
      type(MASCARET_T),       intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur de la chaine de caractere de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_MASC = 0
      valeur              = ""
      MessageErreur       = ""

      if (INDEX(NomVar,'Model.') > 0) then
          GET_STRING_MASC = GET_STRING_MODELE_MASCARET(instance%ModeleMascaret, NomVar, &
                                    index1, index2, index3, valeur, MessageErreur)
      else
         GET_STRING_MASC = 1
         valeur           = ""
         MessageErreur    = "GET_STRING_MASC - Unknown variable name"
      end if
   end function GET_STRING_MASC

! .................................................................................................................................
! Mutateur permettant de modifier les tailles des valeurs des differents champs du type
! .................................................................................................................................

   function SET_TAILLE_VAR_MASC(Instance, NomVar, index1, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: SET_TAILLE_VAR_MASC           ! different de 0 si erreur
      type(MASCARET_T),       intent(inout) :: Instance                   ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in) :: index1                         ! valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
      integer,                intent(in):: taille1                        ! valeur max du 1er indice
      integer,                intent(in):: taille2                        ! valeur max du 2e  indice
      integer,                intent(in):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      SET_TAILLE_VAR_MASC = 0
      MessageErreur       = ""


      if (INDEX(NomVar,'Model.') > 0) then
             SET_TAILLE_VAR_MASC = SET_TAILLE_VAR_MODELE_MASCARET(Instance%ModeleMascaret,&
                                               NomVar, index1, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'State.') > 0) then
             SET_TAILLE_VAR_MASC = SET_TAILLE_VAR_ETAT_MASCARET(Instance%EtatMascaret,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)

      else
         SET_TAILLE_VAR_MASC = 1
         MessageErreur           = "SET_TAILLE_VAR_MASC - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_MASC

! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
! .................................................................................................................................

   function SET_DOUBLE_MASC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_MASC        ! different de 0 si erreur
      type(MASCARET_T),       intent(inout) :: Instance                ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_MASC = 0
      MessageErreur          = ""

      if (INDEX(NomVar,'Model.') > 0) then
              SET_DOUBLE_MASC = SET_DOUBLE_MODELE_MASCARET(Instance%ModeleMascaret, NomVar, &
                                                       index1, index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.') > 0) then
              SET_DOUBLE_MASC = SET_DOUBLE_ETAT_MASCARET(Instance%EtatMascaret, NomVar, &
                                                       index1, index2, index3, valeur, MessageErreur)
      else
         SET_DOUBLE_MASC = 1
         MessageErreur         = "SET_DOUBLE_MASC - Unknown variable name"
      end if
   end function SET_DOUBLE_MASC


  function SET_INT_MASC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_MASC           ! different de 0 si erreur
      type(MASCARET_T),       intent(inout) :: Instance                ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur de l'entier de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_MASC  = 0
      MessageErreur = ""

      if (INDEX(NomVar,'Model.') > 0) then
          SET_INT_MASC = SET_INT_MODELE_MASCARET(Instance%ModeleMascaret, NomVar, &
                                                       index1, index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.') > 0) then
          SET_INT_MASC = SET_INT_ETAT_MASCARET(Instance%EtatMascaret, NomVar, &
                                                       index1, index2, index3, valeur, MessageErreur)
      else
         SET_INT_MASC = 1
         MessageErreur         = "SET_INT_MASC - Unknown variable name"
      end if
   end function SET_INT_MASC

   function SET_BOOL_MASC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_BOOL_MASC          ! different de 0 si erreur
      type(MASCARET_T),       intent(inout) :: Instance                ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(in) :: valeur                     ! valeur du boolean de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_BOOL_MASC = 0
      MessageErreur     = ""

      if (INDEX(NomVar,'Model.') > 0) then
          SET_BOOL_MASC = SET_BOOL_MODELE_MASCARET(instance%ModeleMascaret, NomVar, &
                                    index1, index2, index3, valeur, MessageErreur)
      else
          SET_BOOL_MASC = 1
          MessageErreur    = "SET_BOOL_MASC - Unknown variable name"
      end if
   end function SET_BOOL_MASC

   function SET_STRING_MASC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_MASC            ! different de 0 si erreur
      type(MASCARET_T),       intent(inout) :: Instance                ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du boolean de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_MASC = 0
      MessageErreur     = ""

      if (INDEX(NomVar,'Model.') > 0) then
          SET_STRING_MASC = SET_STRING_MODELE_MASCARET(instance%ModeleMascaret, NomVar, &
                                    index1, index2, index3, valeur, MessageErreur)
      else
         SET_STRING_MASC = 1
         MessageErreur    = "SET_STRING_MASC - Unknown variable name"
      end if
   end function SET_STRING_MASC

! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues de ModeleMascaret et de EtatMascaret
! .................................................................................................................................

   function DESALLOUE_MASCARET(Instance, MessageErreur)
      implicit none
      integer                        :: DESALLOUE_MASCARET  ! different de 0 si erreur
      type(MASCARET_T),intent(inout) :: Instance            ! Instance du type MASCARET_T dont on souhaite desalloue
      character(LEN=256), intent(out):: MessageErreur

      DESALLOUE_MASCARET = DESALLOUE_MODELE_MASCARET(Instance%ModeleMascaret, MessageErreur)
      if (DESALLOUE_MASCARET /= 0) then
          return
      endif
      DESALLOUE_MASCARET = DESALLOUE_ETAT_MASCARET(Instance%EtatMascaret, MessageErreur)


   end function DESALLOUE_MASCARET

end module M_MASCARET_T
