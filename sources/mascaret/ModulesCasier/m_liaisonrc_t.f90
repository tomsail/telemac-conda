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

module M_LIAISONRC_T
!***********************************************************************
! PROGICIEL : MASCARET       C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION
   use M_CONSTANTES_CASIER_C

   TYPE LIAISONRC_T
      sequence
      integer      :: NumCasier       ! Numero du casier associe
      real(DOUBLE) :: Abscisse        ! Abscisse sur le bief
      integer      :: Section         ! Numero de la section de calcul associee
      integer      :: NumBief         ! Numero du bief associe
      real(DOUBLE) :: DQDZcasier      ! derivee du debit de la liaison par rapport
                                      ! a la cote dans le casier
      real(DOUBLE) :: DQDZriviere     ! derivee du debit de la liaison par rapport
                                      ! a la cote dans la riviere
   END TYPE LIAISONRC_T

   contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_LIAISONRC(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.Link.StoR.NumS"
        tabDescriptionVar(i) ="Corresponding storage area number (link Storage Area --- River)"
        i=i+1
        tabNomVar(i)         ="Model.Link.StoR.Abscissa"
        tabDescriptionVar(i) ="Abscissa of the link (link Storage Area --- River)"
        i=i+1
        tabNomVar(i)         ="Model.Link.StoR.Node"
        tabDescriptionVar(i) ="Corresponding node of the link (link Storage Area --- River)"
        i=i+1
        tabNomVar(i)         ="Model.Link.StoR.ReachNum"
        tabDescriptionVar(i) ="Number of the corresponding reach (link Storage Area --- River)"
        i=i+1
        tabNomVar(i)         ="Model.Link.StoR.DQDZsto"
        tabDescriptionVar(i) ="Derivative of the discharge with respect to the storage area level (link Storage Area --- River)"
        i=i+1
        tabNomVar(i)         ="Model.Link.StoR.DQDZriv"
        tabDescriptionVar(i) ="Derivative of the discharge with respect to the river level (link Storage Area --- River)"
        i=i+1
      return

    end subroutine GET_TAB_VAR_LIAISONRC

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_LIAISONRC(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_LIAISONRC    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation point)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "DOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_LIAISONRC = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""


      if ( index(NomVar, 'Model.Link.StoR.NumS') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.StoR.Abscissa') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.StoR.Node') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.StoR.ReachNum') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.StoR.DQDZsto') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Link.StoR.DQDZriv') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
      else
        GET_TYPE_VAR_LIAISONRC = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_LIAISONRC - Unknown variable name"
      end if


    end function GET_TYPE_VAR_LIAISONRC

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_LIAISONRC(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_LIAISONRC       ! different de 0 si erreur
      type(LIAISONRC_T),      intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_LIAISONRC = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoR.NumS') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.StoR.Abscissa') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.StoR.Node') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.StoR.ReachNum') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if (index(NomVar, 'Model.Link.StoR.DQDZsto') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Link.StoR.DQDZriv') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_LIAISONRC = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_LIAISONRC - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_LIAISONRC

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_LIAISONRC(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_LIAISONRC       ! different de 0 si erreur
      type(LIAISONRC_T),      intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur


      SET_TAILLE_VAR_LIAISONRC = 0
      MessageErreur          = ""

   end function SET_TAILLE_VAR_LIAISONRC

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_LIAISONRC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_LIAISONRC       ! different de 0 si erreur
      type(LIAISONRC_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_LIAISONRC = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoR.Abscissa') > 0) then
         valeur = Instance%Abscisse
      else if ( index(NomVar, 'Model.Link.StoR.DQDZsto') > 0) then
         valeur = Instance%DQDZcasier
      else if ( index(NomVar, 'Model.Link.StoR.DQDZriv') > 0) then
         valeur = Instance%DQDZriviere
      else
         GET_DOUBLE_LIAISONRC = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_LIAISONRC - Unknown variable name"
      end if
   end function GET_DOUBLE_LIAISONRC


   function GET_INT_LIAISONRC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_LIAISONRC          ! different de 0 si erreur
      type(LIAISONRC_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_LIAISONRC = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoR.NumS') > 0) then
         valeur = Instance%NumCasier
      else if ( index(NomVar, 'Model.Link.StoR.Node') > 0) then
         valeur = Instance%Section
      else if ( index(NomVar, 'Model.Link.StoR.ReachNum') > 0) then
         valeur = Instance%NumBief
      else
         GET_INT_LIAISONRC = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_LIAISONRC - Unknown variable name"
      end if
   end function GET_INT_LIAISONRC



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_LIAISONRC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_LIAISONRC       ! different de 0 si erreur
      type(LIAISONRC_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_LIAISONRC = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoR.Abscissa') > 0) then
         Instance%Abscisse = valeur
      else if ( index(NomVar, 'Model.Link.StoR.DQDZsto') > 0) then
         Instance%DQDZcasier = valeur
      else if ( index(NomVar, 'Model.Link.StoR.DQDZriv') > 0) then
         Instance%DQDZriviere = valeur
      else
         SET_DOUBLE_LIAISONRC = 1
         MessageErreur         = "SET_DOUBLE_LIAISONRC - Unknown variable name"
      end if
   end function SET_DOUBLE_LIAISONRC


   function SET_INT_LIAISONRC(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_LIAISONRC          ! different de 0 si erreur
      type(LIAISONRC_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_LIAISONRC = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Link.StoR.NumS') > 0) then
         Instance%NumCasier = valeur
      else if ( index(NomVar, 'Model.Link.StoR.Node') > 0) then
         Instance%Section = valeur
      else if ( index(NomVar, 'Model.Link.StoR.ReachNum') > 0) then
         Instance%NumBief = valeur
      else
         SET_INT_LIAISONRC = 1
         MessageErreur         = "SET_INT_LIAISONRC - Unknown variable name"
      end if
   end function SET_INT_LIAISONRC



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_LIAISONRC(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_LIAISONRC        ! different de 0 si erreur
      type(LIAISONRC_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      DESALLOUE_LIAISONRC = 0
      MessageErreur          = ""

   end function DESALLOUE_LIAISONRC

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_LIAISONRC(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_LIAISONRC        ! different de 0 si erreur
      type(LIAISONRC_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_LIAISONRC = 0
      MessageErreur          = ""

   end function NULLIFIER_LIAISONRC

end module M_LIAISONRC_T
