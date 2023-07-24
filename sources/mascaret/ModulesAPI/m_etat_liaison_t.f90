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

module M_ETAT_LIAISON_T
!***********************************************************************
! PROGICIEL : MASCARET        J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

!=========================== Declarations ==============================

use M_PRECISION           ! type DOUBLE
use M_CONSTANTES_CASIER_C ! constantes de calcul propres a CASIER

TYPE ETAT_LIAISON_T

  sequence

  real(DOUBLE)      :: DebitEchange    ! Debit de la liaison au debut du pas de temps
  real(DOUBLE)      :: DebitPrecedent
  real(DOUBLE)      :: DebitMax        ! Debit max au cours du calcul
  real(DOUBLE)      :: TempsDebitMax   ! Temps correspondant a Debit Max

  real(DOUBLE)      :: VitesseEchange  ! Vitesse de la liaison au debut du pas de temps
  real(DOUBLE)      :: VitesseMax      ! Vitesse max au cours du calcul
  real(DOUBLE)      :: TempsVitesseMax ! Temps correspondant a Vitesse Max

  real(DOUBLE) :: DQDZamont         ! derivee du debit de la liaison par rapport
                                    ! a la cote dans le casier amont
  real(DOUBLE) :: DQDZaval          ! derivee du debit de la liaison par rapport
								                    ! a la cote dans le casier aval
  real(DOUBLE) :: DQDZcasier        ! derivee du debit de la liaison par rapport
                                    ! a la cote dans le casier
  real(DOUBLE) :: DQDZriviere       ! derivee du debit de la liaison par rapport
                                    ! a la cote dans la riviere

END TYPE ETAT_LIAISON_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_ETAT_LIAISON(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

          tabNomVar(i)         ="State.Link.Discharge"
          tabDescriptionVar(i) ="Discharge (m3/s)"
          i=i+1
          tabNomVar(i)         ="State.Link.PrevDischarge"
          tabDescriptionVar(i) ="Previous discharge (m3/s)"
          i=i+1
          tabNomVar(i)         ="State.Link.MaxDischarge"
          tabDescriptionVar(i) ="Maximal discharge value (m3/s)"
          i=i+1
          tabNomVar(i)         ="State.Link.MaxDischTime"
          tabDescriptionVar(i) ="Time (s) corresponding to the maximal discharge value"
          i=i+1
          tabNomVar(i)         ="State.Link.ExchangeV"
          tabDescriptionVar(i) ="Exchange velocity"
          i=i+1
          tabNomVar(i)         ="State.Link.MaxV"
          tabDescriptionVar(i) ="Maximal velocity value"
          i=i+1
          tabNomVar(i)         ="State.Link.MaxVTime"
          tabDescriptionVar(i) ="Time (s) corresponding to the maximal velocity value"
          i=i+1
          tabNomVar(i)         ="State.Link.DQDZus"
          tabDescriptionVar(i) ="Derivative of the discharge with respect to the level in the upstream storage area"
          i=i+1
          tabNomVar(i)         ="State.Link.DQDZds"
          tabDescriptionVar(i) ="Derivative of the discharge with respect to the level in the downstream storage area"
          i=i+1
          tabNomVar(i)         ="State.Link.DQDZsto"
          tabDescriptionVar(i) ="Derivative of the discharge with respect to the storage area level"
          i=i+1
          tabNomVar(i)         ="State.Link.DQDZriv"
          tabDescriptionVar(i) ="Derivative of the discharge with respect to the river level"
          i=i+1

          return

    end subroutine GET_TAB_VAR_ETAT_LIAISON

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_ETAT_LIAISON(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_ETAT_LIAISON    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_ETAT_LIAISON = 0
      TypeVar               = ""
      Categorie             = "STATE"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'State.Link.Discharge') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.PrevDischarge') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.MaxDischarge') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.MaxDischTime') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.ExchangeV') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.MaxVTime') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.MaxV') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.DQDZus') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.DQDZds') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.DQDZsto') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.Link.DQDZriv') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
     else
        GET_TYPE_VAR_ETAT_LIAISON = 1
        TypeVar = "?"
        Categorie             = "STATE"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_ETAT_LIAISON - Unknown variable name"
      end if


    end function GET_TYPE_VAR_ETAT_LIAISON

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_ETAT_LIAISON(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_ETAT_LIAISON    ! different de 0 si erreur
      type(ETAT_LIAISON_T),   intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_ETAT_LIAISON = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.Link.Discharge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.PrevDischarge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.MaxDischarge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.MaxDischTime') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.ExchangeV') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.MaxVTime') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.MaxV') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.DQDZus') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.DQDZds') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.DQDZsto') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Link.DQDZriv') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_ETAT_LIAISON = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_ETAT_LIAISON - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_ETAT_LIAISON

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_ETAT_LIAISON(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_ETAT_LIAISON    ! different de 0 si erreur
      type(ETAT_LIAISON_T),   intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur


      SET_TAILLE_VAR_ETAT_LIAISON = 0
      MessageErreur          = ""

   end function SET_TAILLE_VAR_ETAT_LIAISON

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_ETAT_LIAISON(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_ETAT_LIAISON    ! different de 0 si erreur
      type(ETAT_LIAISON_T),   intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_ETAT_LIAISON = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'State.Link.Discharge') > 0) then
         valeur = Instance%DebitEchange
      else if ( index(NomVar, 'State.Link.PrevDischarge') > 0) then
         valeur = Instance%DebitPrecedent
      else if ( index(NomVar, 'State.Link.MaxDischarge') > 0) then
         valeur = Instance%DebitMax
      else if ( index(NomVar, 'State.Link.MaxDischTime') > 0) then
         valeur = Instance%TempsDebitMax
      else if ( index(NomVar, 'State.Link.ExchangeV') > 0) then
         valeur = Instance%VitesseEchange
      else if ( index(NomVar, 'State.Link.MaxVTime') > 0) then
         valeur = Instance%TempsVitesseMax
      else if ( index(NomVar, 'State.Link.MaxV') > 0) then
         valeur = Instance%VitesseMax
      else if ( index(NomVar, 'State.Link.DQDZus') > 0) then
         valeur = Instance%DQDZamont
      else if ( index(NomVar, 'State.Link.DQDZds') > 0) then
         valeur = Instance%DQDZaval
      else if ( index(NomVar, 'State.Link.DQDZsto') > 0) then
         valeur = Instance%DQDZcasier
      else if ( index(NomVar, 'State.Link.DQDZriv') > 0) then
         valeur = Instance%DQDZriviere
      else
         GET_DOUBLE_ETAT_LIAISON = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_ETAT_LIAISON - Unknown variable name"
      end if
   end function GET_DOUBLE_ETAT_LIAISON



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_ETAT_LIAISON(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_ETAT_LIAISON    ! different de 0 si erreur
      type(ETAT_LIAISON_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_ETAT_LIAISON = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.Link.Discharge') > 0) then
         Instance%DebitEchange = valeur
      else if ( index(NomVar, 'State.Link.PrevDischarge') > 0) then
         Instance%DebitPrecedent = valeur
      else if ( index(NomVar, 'State.Link.MaxDischarge') > 0) then
         Instance%DebitMax = valeur
      else if ( index(NomVar, 'State.Link.MaxDischTime') > 0) then
         Instance%TempsDebitMax = valeur
      else if ( index(NomVar, 'State.Link.ExchangeV') > 0) then
         Instance%VitesseEchange = valeur
      else if ( index(NomVar, 'State.Link.MaxVTime') > 0) then
         Instance%TempsVitesseMax = valeur
      else if ( index(NomVar, 'State.Link.MaxV') > 0) then
         Instance%VitesseMax = valeur
      else if ( index(NomVar, 'State.Link.DQDZus') > 0) then
         Instance%DQDZamont = valeur
      else if ( index(NomVar, 'State.Link.DQDZds') > 0) then
         Instance%DQDZaval = valeur
      else if ( index(NomVar, 'State.Link.DQDZsto') > 0) then
         Instance%DQDZcasier = valeur
      else if ( index(NomVar, 'State.Link.DQDZriv') > 0) then
         Instance%DQDZriviere = valeur
      else
         SET_DOUBLE_ETAT_LIAISON = 1
         MessageErreur         = "SET_DOUBLE_ETAT_LIAISON - Unknown variable name"
      end if
   end function SET_DOUBLE_ETAT_LIAISON



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_ETAT_LIAISON(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_ETAT_LIAISON     ! different de 0 si erreur
      type(ETAT_LIAISON_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      DESALLOUE_ETAT_LIAISON = 0
      MessageErreur          = ""

   end function DESALLOUE_ETAT_LIAISON

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_ETAT_LIAISON(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_ETAT_LIAISON     ! different de 0 si erreur
      type(ETAT_LIAISON_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_ETAT_LIAISON = 0
      MessageErreur          = ""

   end function NULLIFIER_ETAT_LIAISON

end module M_ETAT_LIAISON_T
