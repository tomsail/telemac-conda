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

module M_ZONE_SECHE_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
use M_PRECISION

Type ZONE_SECHE_T
  sequence
  integer      :: SectionDeb   ! Numero de section debut
  integer      :: SectionFin   ! Numero de section fin
end type ZONE_SECHE_T

contains
   ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_ZONE_SECHE(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.DryArea.FirstNode"
        tabDescriptionVar(i) ="Number of the first node"
        i=i+1
        tabNomVar(i)         ="Model.DryArea.LastNode"
        tabDescriptionVar(i) ="Number of the last node"
        i=i+1
     return

    end subroutine GET_TAB_VAR_ZONE_SECHE

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_ZONE_SECHE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_ZONE_SECHE    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_ZONE_SECHE = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""



       if ( index(NomVar, 'Model.DryArea.FirstNode') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.DryArea.LastNode') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else
        GET_TYPE_VAR_ZONE_SECHE = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_ZONE_SECHE - Unknown variable name"
      end if


    end function GET_TYPE_VAR_ZONE_SECHE

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_ZONE_SECHE(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_ZONE_SECHE      ! different de 0 si erreur
      type(ZONE_SECHE_T),     intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_ZONE_SECHE = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.DryArea.FirstNode') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.DryArea.LastNode') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_ZONE_SECHE = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_ZONE_SECHE - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_ZONE_SECHE

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_ZONE_SECHE(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_ZONE_SECHE      ! different de 0 si erreur
      type(ZONE_SECHE_T),     intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur


      SET_TAILLE_VAR_ZONE_SECHE = 0
      MessageErreur          = ""

   end function SET_TAILLE_VAR_ZONE_SECHE

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_INT_ZONE_SECHE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_ZONE_SECHE         ! different de 0 si erreur
      type(ZONE_SECHE_T),     intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_ZONE_SECHE = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.DryArea.FirstNode') > 0) then
         valeur = Instance%SectionDeb
      else if ( index(NomVar, 'Model.DryArea.LastNode') > 0) then
         valeur = Instance%SectionFin
      else
         GET_INT_ZONE_SECHE = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_ZONE_SECHE - Unknown variable name"
      end if
   end function GET_INT_ZONE_SECHE



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_INT_ZONE_SECHE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_ZONE_SECHE         ! different de 0 si erreur
      type(ZONE_SECHE_T),     intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_ZONE_SECHE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.DryArea.FirstNode') > 0) then
         Instance%SectionDeb = valeur
      else if ( index(NomVar, 'Model.DryArea.LastNode') > 0) then
         Instance%SectionFin = valeur
      else
         SET_INT_ZONE_SECHE = 1
         MessageErreur         = "SET_INT_ZONE_SECHE - Unknown variable name"
      end if
   end function SET_INT_ZONE_SECHE



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_ZONE_SECHE(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_ZONE_SECHE       ! different de 0 si erreur
      type(ZONE_SECHE_T),     intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      DESALLOUE_ZONE_SECHE = 0
      MessageErreur          = ""

   end function DESALLOUE_ZONE_SECHE

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_ZONE_SECHE(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_ZONE_SECHE       ! different de 0 si erreur
      type(ZONE_SECHE_T),     intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_ZONE_SECHE = 0
      MessageErreur          = ""

   end function NULLIFIER_ZONE_SECHE

end module M_ZONE_SECHE_T
