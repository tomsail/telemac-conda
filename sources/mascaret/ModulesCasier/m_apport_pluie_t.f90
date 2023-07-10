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

module M_APPORT_PLUIE_T
!***********************************************************************
! PROGICIEL : MASCARET       C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION

   TYPE APPORT_PLUIE_T
      sequence
      integer       :: Numero          ! Numero du casier recepteur
      integer       :: NumeroLoi       ! Numero de la loi associe a l'apport
      real(DOUBLE)  :: Debit           ! Valeur de l'apport (m3/s)
   END TYPE APPORT_PLUIE_T

   contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_APPORT_PLUIE(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.ExternalInflow.Number"
        tabDescriptionVar(i) ="Number of the storage area with an external inflow"
        i=i+1
        tabNomVar(i)         ="Model.ExternalInflow.GraphNumber"
        tabDescriptionVar(i) ="Number of the graph for an external inflow"
        i=i+1
        tabNomVar(i)         ="Model.ExternalInflow.Discharge"
        tabDescriptionVar(i) ="Value of the external inflow (m3/s)"
        i=i+1

      return

    end subroutine GET_TAB_VAR_APPORT_PLUIE

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_APPORT_PLUIE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_APPORT_PLUIE    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation point)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_APPORT_PLUIE = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""


       if ( index(NomVar, 'Model.ExternalInflow.Number') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ExternalInflow.GraphNumber') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ExternalInflow.Discharge') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
      else
        GET_TYPE_VAR_APPORT_PLUIE = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_APPORT_PLUIE - Unknown variable name"
      end if


    end function GET_TYPE_VAR_APPORT_PLUIE

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_APPORT_PLUIE(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_APPORT_PLUIE    ! different de 0 si erreur
      type(APPORT_PLUIE_T),   intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_APPORT_PLUIE = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.ExternalInflow.Number') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ExternalInflow.GraphNumber') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ExternalInflow.Discharge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_APPORT_PLUIE = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_APPORT_PLUIE - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_APPORT_PLUIE

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_APPORT_PLUIE(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_APPORT_PLUIE    ! different de 0 si erreur
      type(APPORT_PLUIE_T),   intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur


      SET_TAILLE_VAR_APPORT_PLUIE = 0
      MessageErreur          = ""

   end function SET_TAILLE_VAR_APPORT_PLUIE

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_APPORT_PLUIE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_APPORT_PLUIE    ! different de 0 si erreur
      type(APPORT_PLUIE_T),   intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_APPORT_PLUIE = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.ExternalInflow.Discharge') > 0) then
         valeur = Instance%Debit
      else
         GET_DOUBLE_APPORT_PLUIE = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_APPORT_PLUIE - Unknown variable name"
      end if
   end function GET_DOUBLE_APPORT_PLUIE


   function GET_INT_APPORT_PLUIE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_APPORT_PLUIE       ! different de 0 si erreur
      type(APPORT_PLUIE_T),   intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_APPORT_PLUIE = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.ExternalInflow.Number') > 0) then
         valeur = Instance%Numero
      else if ( index(NomVar, 'Model.ExternalInflow.GraphNumber') > 0) then
         valeur = Instance%NumeroLoi
      else
         GET_INT_APPORT_PLUIE = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_APPORT_PLUIE - Unknown variable name"
      end if
   end function GET_INT_APPORT_PLUIE



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_APPORT_PLUIE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_APPORT_PLUIE    ! different de 0 si erreur
      type(APPORT_PLUIE_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_APPORT_PLUIE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.ExternalInflow.Discharge') > 0) then
         Instance%Debit = valeur
      else
         SET_DOUBLE_APPORT_PLUIE = 1
         MessageErreur         = "SET_DOUBLE_APPORT_PLUIE - Unknown variable name"
      end if
   end function SET_DOUBLE_APPORT_PLUIE


   function SET_INT_APPORT_PLUIE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_APPORT_PLUIE       ! different de 0 si erreur
      type(APPORT_PLUIE_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_APPORT_PLUIE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.ExternalInflow.Number') > 0) then
         Instance%Numero = valeur
      else if ( index(NomVar, 'Model.ExternalInflow.GraphNumber') > 0) then
         Instance%NumeroLoi = valeur
      else
         SET_INT_APPORT_PLUIE = 1
         MessageErreur         = "SET_INT_APPORT_PLUIE - Unknown variable name"
      end if
   end function SET_INT_APPORT_PLUIE



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_APPORT_PLUIE(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_APPORT_PLUIE     ! different de 0 si erreur
      type(APPORT_PLUIE_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      DESALLOUE_APPORT_PLUIE = 0
      MessageErreur          = ""

   end function DESALLOUE_APPORT_PLUIE

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_APPORT_PLUIE(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_APPORT_PLUIE     ! different de 0 si erreur
      type(APPORT_PLUIE_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_APPORT_PLUIE = 0
      MessageErreur          = ""

   end function NULLIFIER_APPORT_PLUIE

end module M_APPORT_PLUIE_T
