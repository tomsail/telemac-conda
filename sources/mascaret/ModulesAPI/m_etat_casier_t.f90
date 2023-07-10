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
module M_ETAT_CASIER_T

!=========================== Declarations ==============================

use M_PRECISION



TYPE ETAT_CASIER_T

    sequence


    real(DOUBLE) :: Cote            ! Cote de la surface libre :
                                    !	en entree ---> au debut du pas de temps traite
                                    ! en sortie ---> a la fin du pas de temps traite
    real(DOUBLE) :: Surface         ! Surface du casier
                                    !	en entree ---> au debut du pas de temps traite
                                    ! en sortie ---> a la fin du pas de temps traite
    real(DOUBLE) :: Volume          ! Volume du Casier
                                    !	en entree ---> au debut du pas de temps traite
                                    ! en sortie ---> a la fin du pas de temps traite
    real(DOUBLE) :: VolumeIni       ! volume initial
    real(DOUBLE) :: Bilan
    real(DOUBLE) :: BilanErreur

    real(DOUBLE) :: DzCas           ! Variation de cote dans le casier au cours
                                    ! du pas de temps traite
    real(DOUBLE) :: CoteMax         ! Cote max observee au cours du calcul
    real(DOUBLE) :: TempsMax        ! Temps correspondant a CoteMax

END TYPE ETAT_CASIER_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_ETAT_CASIER(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="State.StoArea.Level"
        tabDescriptionVar(i) ="Water level (m)"
        i=i+1
        tabNomVar(i)         ="State.StoArea.Surface"
        tabDescriptionVar(i) ="Surface (m2)"
        i=i+1
        tabNomVar(i)         ="State.StoArea.Volume"
        tabDescriptionVar(i) ="Volume (m3)"
        i=i+1
        tabNomVar(i)         ="State.StoArea.InitVolume"
        tabDescriptionVar(i) ="Initial volume (m3)"
        i=i+1
        tabNomVar(i)         ="State.StoArea.VolStatement"
        tabDescriptionVar(i) ="Volume statement"
        i=i+1
        tabNomVar(i)         ="State.StoArea.ErrVolStatement"
        tabDescriptionVar(i) ="Error volume statement"
        i=i+1
        tabNomVar(i)         ="State.StoArea.DzSto"
        tabDescriptionVar(i) ="Level variation"
        i=i+1
        tabNomVar(i)         ="State.StoArea.MaxLevel"
        tabDescriptionVar(i) ="Maximal water level (m)"
        i=i+1
        tabNomVar(i)         ="State.StoArea.MaxTime"
        tabDescriptionVar(i) ="Time corresponding to the maximal level"
        i=i+1

        return

    end subroutine GET_TAB_VAR_ETAT_CASIER

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_ETAT_CASIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_ETAT_CASIER ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_ETAT_CASIER = 0
      TypeVar               = ""
      Categorie             = "STATE"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'State.StoArea.Level') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.StoArea.Surface') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.StoArea.Volume') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.StoArea.InitVolume') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.StoArea.VolStatement') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.StoArea.ErrVolStatement') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.StoArea.DzSto') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.StoArea.MaxLevel') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'State.StoArea.MaxTime') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
     else
        GET_TYPE_VAR_ETAT_CASIER = 1
        TypeVar = "?"
        Categorie             = "STATE"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_ETAT_CASIER - Unknown variable name"
      end if


    end function GET_TYPE_VAR_ETAT_CASIER

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_ETAT_CASIER(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_ETAT_CASIER     ! different de 0 si erreur
      type(ETAT_CASIER_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_ETAT_CASIER = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.StoArea.Level') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.StoArea.Surface') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.StoArea.Volume') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.StoArea.InitVolume') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.StoArea.VolStatement') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.StoArea.ErrVolStatement') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.StoArea.DzSto') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.StoArea.MaxLevel') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.StoArea.MaxTime') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_ETAT_CASIER = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_ETAT_CASIER - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_ETAT_CASIER

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_ETAT_CASIER(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_ETAT_CASIER     ! different de 0 si erreur
      type(ETAT_CASIER_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur


      SET_TAILLE_VAR_ETAT_CASIER = 0
      MessageErreur          = ""

   end function SET_TAILLE_VAR_ETAT_CASIER

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_ETAT_CASIER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_ETAT_CASIER     ! different de 0 si erreur
      type(ETAT_CASIER_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_ETAT_CASIER = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'State.StoArea.Level') > 0) then
         valeur = Instance%Cote
      else if ( index(NomVar, 'State.StoArea.Surface') > 0) then
         valeur = Instance%Surface
      else if ( index(NomVar, 'State.StoArea.Volume') > 0) then
         valeur = Instance%Volume
      else if ( index(NomVar, 'State.StoArea.InitVolume') > 0) then
         valeur = Instance%VolumeIni
      else if ( index(NomVar, 'State.StoArea.VolStatement') > 0) then
         valeur = Instance%Bilan
      else if ( index(NomVar, 'State.StoArea.ErrVolStatement') > 0) then
         valeur = Instance%BilanErreur
      else if ( index(NomVar, 'State.StoArea.DzSto') > 0) then
         valeur = Instance%DzCas
      else if ( index(NomVar, 'State.StoArea.MaxLevel') > 0) then
         valeur = Instance%CoteMax
      else if ( index(NomVar, 'State.StoArea.MaxTime') > 0) then
         valeur = Instance%TempsMax
      else
         GET_DOUBLE_ETAT_CASIER = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_ETAT_CASIER - Unknown variable name"
      end if
   end function GET_DOUBLE_ETAT_CASIER



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_ETAT_CASIER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_ETAT_CASIER     ! different de 0 si erreur
      type(ETAT_CASIER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_ETAT_CASIER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.StoArea.Level') > 0) then
         Instance%Cote = valeur
      else if ( index(NomVar, 'State.StoArea.Surface') > 0) then
         Instance%Surface = valeur
      else if ( index(NomVar, 'State.StoArea.Volume') > 0) then
         Instance%Volume = valeur
      else if ( index(NomVar, 'State.StoArea.InitVolume') > 0) then
         Instance%VolumeIni = valeur
      else if ( index(NomVar, 'State.StoArea.VolStatement') > 0) then
         Instance%Bilan = valeur
      else if ( index(NomVar, 'State.StoArea.ErrVolStatement') > 0) then
         Instance%BilanErreur = valeur
      else if ( index(NomVar, 'State.StoArea.DzSto') > 0) then
         Instance%DzCas = valeur
      else if ( index(NomVar, 'State.StoArea.MaxLevel') > 0) then
         Instance%CoteMax = valeur
      else if ( index(NomVar, 'State.StoArea.MaxTime') > 0) then
         Instance%TempsMax = valeur
      else
         SET_DOUBLE_ETAT_CASIER = 1
         MessageErreur         = "SET_DOUBLE_ETAT_CASIER - Unknown variable name"
      end if
   end function SET_DOUBLE_ETAT_CASIER



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_ETAT_CASIER(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_ETAT_CASIER      ! different de 0 si erreur
      type(ETAT_CASIER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      DESALLOUE_ETAT_CASIER = 0
      MessageErreur          = ""

   end function DESALLOUE_ETAT_CASIER

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_ETAT_CASIER(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_ETAT_CASIER      ! different de 0 si erreur
      type(ETAT_CASIER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_ETAT_CASIER = 0
      MessageErreur          = ""

   end function NULLIFIER_ETAT_CASIER

end module M_ETAT_CASIER_T
