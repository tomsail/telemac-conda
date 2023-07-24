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

module M_LIAISONCC_T
!***********************************************************************
! PROGICIEL : MASCARET       C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION
   use M_CONSTANTES_CASIER_C

   TYPE LIAISONCC_T
      sequence
      integer      :: CasierOrigine
      integer      :: CasierFin
      real(DOUBLE) :: DQDZamont         ! derivee du debit de la liaison par rapport
                                        ! a la cote dans le casier amont
      real(DOUBLE) :: DQDZaval          ! derivee du debit de la liaison par rapport
                                        ! a la cote dans le casier aval
   END TYPE LIAISONCC_T

   contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_LIAISONCC(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.Link.StoS.S_i"
        tabDescriptionVar(i) ="Number of the storage area 'i' (discharge flowing from 'i' towards 'j')"
        i=i+1
        tabNomVar(i)         ="Model.Link.StoS.S_j"
        tabDescriptionVar(i) ="Number of the storage area 'j' (discharge flowing from 'i' towards 'j')"
        i=i+1
        tabNomVar(i)         ="Model.Link.StoS.DQDZup"
        tabDescriptionVar(i) ="Derivative of the flow discharge with respect to the water level in the upstream storage area"
        i=i+1
        tabNomVar(i)         ="Model.Link.StoS.DQDZdown"
        tabDescriptionVar(i) ="Derivative of the flow discharge with respect to the water level in the downstream storage area"
        i=i+1
      return
    end subroutine GET_TAB_VAR_LIAISONCC

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_LIAISONCC(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_LIAISONCC    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation point)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_LIAISONCC = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""


      if ( index(NomVar, 'Model.Link.StoS.S_i') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
      else if ( index(NomVar, 'Model.Link.StoS.S_j') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
      else if ( index(NomVar, 'Model.Link.StoS.DQDZup') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
      else if ( index(NomVar, 'Model.Link.StoS.DQDZdown') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
      else
        GET_TYPE_VAR_LIAISONCC = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_LIAISONCC - Unknown variable name"
      end if
    end function GET_TYPE_VAR_LIAISONCC

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_LIAISONCC(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_LIAISONCC       ! different de 0 si erreur
      type(LIAISONCC_T),      intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_LIAISONCC = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoS.S_i') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.StoS.S_j') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.StoS.DQDZup') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.StoS.DQDZdown') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_LIAISONCC = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_LIAISONCC - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_LIAISONCC

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_LIAISONCC(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_LIAISONCC       ! different de 0 si erreur
      type(LIAISONCC_T),      intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur


      SET_TAILLE_VAR_LIAISONCC = 0
      MessageErreur          = ""

   end function SET_TAILLE_VAR_LIAISONCC

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_LIAISONCC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_LIAISONCC       ! different de 0 si erreur
      type(LIAISONCC_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_LIAISONCC = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoS.DQDZup') > 0) then
         valeur = Instance%DQDZamont
      else if ( index(NomVar, 'Model.Link.StoS.DQDZdown') > 0) then
         valeur = Instance%DQDZaval
      else
         GET_DOUBLE_LIAISONCC = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_LIAISONCC - Unknown variable name"
      end if
   end function GET_DOUBLE_LIAISONCC


   function GET_INT_LIAISONCC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_LIAISONCC          ! different de 0 si erreur
      type(LIAISONCC_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_LIAISONCC = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoS.S_i') > 0) then
         valeur = Instance%CasierOrigine
      else if ( index(NomVar, 'Model.Link.StoS.S_j') > 0) then
         valeur = Instance%CasierFin
      else
         GET_INT_LIAISONCC = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_LIAISONCC - Unknown variable name"
      end if
   end function GET_INT_LIAISONCC



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_LIAISONCC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_LIAISONCC       ! different de 0 si erreur
      type(LIAISONCC_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_LIAISONCC = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoS.DQDZup') > 0) then
         Instance%DQDZamont = valeur
      else if ( index(NomVar, 'Model.Link.StoS.DQDZdown') > 0) then
         Instance%DQDZaval = valeur
      else
         SET_DOUBLE_LIAISONCC = 1
         MessageErreur         = "SET_DOUBLE_LIAISONCC - Unknown variable name"
      end if
   end function SET_DOUBLE_LIAISONCC


   function SET_INT_LIAISONCC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_LIAISONCC          ! different de 0 si erreur
      type(LIAISONCC_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_LIAISONCC = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoS.S_i') > 0) then
         Instance%CasierOrigine = valeur
      else if ( index(NomVar, 'Model.Link.StoS.S_j') > 0) then
         Instance%CasierFin = valeur
      else
         SET_INT_LIAISONCC = 1
         MessageErreur         = "SET_INT_LIAISONCC - Unknown variable name"
      end if
   end function SET_INT_LIAISONCC



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_LIAISONCC(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_LIAISONCC        ! different de 0 si erreur
      type(LIAISONCC_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      DESALLOUE_LIAISONCC = 0
      MessageErreur          = ""

   end function DESALLOUE_LIAISONCC

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_LIAISONCC(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_LIAISONCC        ! different de 0 si erreur
      type(LIAISONCC_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_LIAISONCC = 0
      MessageErreur          = ""

   end function NULLIFIER_LIAISONCC

end module M_LIAISONCC_T
