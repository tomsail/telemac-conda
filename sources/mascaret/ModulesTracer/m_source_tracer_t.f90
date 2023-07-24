!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_SOURCE_TRACER_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================

   use M_PRECISION

   integer     , parameter :: SOURCE_TRACER_TYPE_VOLUMIQUE  = 1
   integer     , parameter :: SOURCE_TRACER_TYPE_SURFACIQUE = 2
   integer     , parameter :: SOURCE_TRACER_TYPE_FLUX_TEMP  = 3
   integer     , parameter :: SOURCE_TRACER_TYPE_NB_MAX     = 3

   TYPE SOURCE_TRACER_T
      sequence
      character(30) :: Nom             ! Nom de la source
      integer       :: Type            ! Type de la source
      integer       :: NumBranche      ! Numero de la section debut de la source
      real(DOUBLE)  :: AbscisseRel     ! Abscisse relative de la source
      real(DOUBLE)  :: Longueur        ! Longueur de la source
      integer       :: SectionAm       ! Numero de la section debut
      integer       :: SectionAv       ! Numero de la section fin
      integer       :: NumeroLoi     ! Numero de la loi associee
      real (DOUBLE),dimension(:),pointer :: Apport_source  => null() ! Apport par la source pour chaque traceur (conc, flux volum ou surf selon le cas)
      logical       :: SuperpositionApport ! Si superposition a un apport hydrau
      integer       :: NumeroApport        ! Numero de l'apport hydrau associe
      real(DOUBLE)  :: debit_source    ! quantite de source
      integer       :: Numero_traceur  ! Numero du traceur correspondant
   END TYPE SOURCE_TRACER_T

contains

   ! Retourne les noms des champs du type ainsi qu'une description
   subroutine GET_TAB_VAR_SOURCE_TRACER(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="Model.Tracer.Sources.Name"
        tabDescriptionVar(i) ="Name of the source for the constituant"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.Type"
        tabDescriptionVar(i) ="Type of the source for the constituant"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.ReachNumber"
        tabDescriptionVar(i) ="Number of the reach for the source"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.RelAbscissa"
        tabDescriptionVar(i) ="Relative abscissa for the source position (m)"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.Length"
        tabDescriptionVar(i) ="Length of the source (m)"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.UpStreamNode"
        tabDescriptionVar(i) ="Number of the upstream node for the source"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.DownStreamNode"
        tabDescriptionVar(i) ="Number of the downstream node for the source"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.GraphNumber"
        tabDescriptionVar(i) ="Number of the graph for the source of constituant"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.Source"
        tabDescriptionVar(i) ="Source of the constituant"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.withInflow"
        tabDescriptionVar(i) ="Logical for the superposition with a hydraulic inflow"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Sources.InflowName"
        tabDescriptionVar(i) ="Name of the hydraulic inflow"
        i=i+1

        return

     end subroutine GET_TAB_VAR_SOURCE_TRACER

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_SOURCE_TRACER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_SOURCE_TRACER ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_TRACER sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_SOURCE_TRACER = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'Model.Tracer.Sources.Name') > 0) then
         TypeVar = 'STRING'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.Type') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.ReachNumber') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.RelAbscissa') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.Length') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.UpStreamNode') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.DownStreamNode') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.GraphNumber') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.Source') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Sources.withInflow') > 0) then
         TypeVar = 'BOOL'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.InflowName') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else
         GET_TYPE_VAR_SOURCE_TRACER = 1
         TypeVar = "?"
         Categorie             = "MODEL"
         Modifiable            = .false.
         dimVar                = -1
         MessageErreur         = "GET_TYPE_VAR_SOURCE_TRACER - Unknown variable name"
      end if

    end function GET_TYPE_VAR_SOURCE_TRACER

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_SOURCE_TRACER(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_SOURCE_TRACER     ! different de 0 si erreur
      type(SOURCE_TRACER_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du source
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_SOURCE_TRACER = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Sources.Name') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.Type') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.ReachNumber') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.RelAbscissa') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.Length') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.UpStreamNode') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.DownStreamNode') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.GraphNumber') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.Source') > 0) then
         taille1 = size(Instance%Apport_source)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.withInflow') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Sources.InflowName') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_SOURCE_TRACER = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_SOURCE_TRACER - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_SOURCE_TRACER

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_SOURCE_TRACER(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_SOURCE_TRACER     ! different de 0 si erreur
      type(SOURCE_TRACER_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, err

      SET_TAILLE_VAR_SOURCE_TRACER = 0
      MessageErreur          = ""
      !------------------------------------------------------------------------------
      ! Fin des appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type primitif
      !------------------------------------------------------------------------------
      if ( index(NomVar, 'Model.Tracer.Sources.Source') > 0) then
        if (ASSOCIATED(Instance%Apport_source)) then
           t1 = size(Instance%Apport_source)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Apport_source, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SOURCE_TRACER = err
                 MessageErreur = 'SET_TAILLE_VAR_SOURCE_TRACER : Unable to deallocate MODEL_SOURCE_TRACER_T.Source'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Apport_source) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Apport_source(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SOURCE_TRACER = err
              MessageErreur = 'SET_TAILLE_VAR_SOURCE_TRACER : Unable to allocate MODEL_SOURCE_TRACER_T.Source'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------
      else
         SET_TAILLE_VAR_SOURCE_TRACER = 1
         MessageErreur         = "SET_TAILLE_VAR_SOURCE_TRACER - Unknown variable name"
      end if

    end function SET_TAILLE_VAR_SOURCE_TRACER

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_SOURCE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_SOURCE_TRACER     ! different de 0 si erreur
      type(SOURCE_TRACER_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_SOURCE_TRACER = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Sources.RelAbscissa') > 0) then
         valeur = Instance%AbscisseRel
      else if ( index(NomVar, 'Model.Tracer.Sources.Length') > 0) then
         valeur = Instance%Longueur
      else if ( index(NomVar, 'Model.Tracer.Sources.Source') > 0) then
         valeur = Instance%Apport_source(index1)
      else
         GET_DOUBLE_SOURCE_TRACER = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_SOURCE_TRACER - Unknown variable name"
      end if
   end function GET_DOUBLE_SOURCE_TRACER

   function GET_INT_SOURCE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_SOURCE_TRACER            ! different de 0 si erreur
      type(SOURCE_TRACER_T),        intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_SOURCE_TRACER = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Sources.Type') > 0) then
         valeur = Instance%Type
      else if ( index(NomVar, 'Model.Tracer.Sources.ReachNumber') > 0) then
         valeur = Instance%NumBranche
      else if ( index(NomVar, 'Model.Tracer.Sources.UpStreamNode') > 0) then
         valeur = Instance%SectionAm
      else if ( index(NomVar, 'Model.Tracer.Sources.DownStreamNode') > 0) then
         valeur = Instance%SectionAv
      else if ( index(NomVar, 'Model.Tracer.Sources.GraphNumber') > 0) then
         valeur = Instance%NumeroLoi
      else if ( index(NomVar, 'Model.Tracer.Sources.InflowName') > 0) then
         valeur = Instance%NumeroApport
      else
         GET_INT_SOURCE_TRACER = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_SOURCE_TRACER - Unknown variable name"
      end if
   end function GET_INT_SOURCE_TRACER

   function GET_BOOL_SOURCE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_BOOL_SOURCE_TRACER       ! different de 0 si erreur
      type(SOURCE_TRACER_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(out):: valeur                     ! valeur du logical de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_BOOL_SOURCE_TRACER = 0
      valeur                = .FALSE.
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Sources.withInflow') > 0) then
         valeur = Instance%SuperpositionApport
      else
         GET_BOOL_SOURCE_TRACER = 1
         valeur                = .FALSE.
         MessageErreur         = "GET_BOOL_SOURCE_TRACER - Unknown variable name"
      end if
   end function GET_BOOL_SOURCE_TRACER

   function GET_STRING_SOURCE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_SOURCE_TRACER ! different de 0 si erreur
      type(SOURCE_TRACER_T),intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_SOURCE_TRACER = 0
      valeur                = ""
      MessageErreur          = ""

      if (INDEX(NomVar,'Model.Tracer.Sources.Name') > 0) then
         valeur = Instance%Nom
      else
         GET_STRING_SOURCE_TRACER = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_SOURCE_TRACER - Unknown variable name"
      end if
   end function GET_STRING_SOURCE_TRACER

! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_SOURCE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_SOURCE_TRACER     ! different de 0 si erreur
      type(SOURCE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_SOURCE_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Sources.RelAbscissa') > 0) then
         Instance%AbscisseRel = valeur
      else if ( index(NomVar, 'Model.Tracer.Sources.Length') > 0) then
         Instance%Longueur = valeur
      else if ( index(NomVar, 'Model.Tracer.Sources.Source') > 0) then
         Instance%Apport_source(index1) = valeur
      else
         SET_DOUBLE_SOURCE_TRACER = 1
         MessageErreur         = "SET_DOUBLE_SOURCE_TRACER - Unknown variable name"
      end if
   end function SET_DOUBLE_SOURCE_TRACER

   function SET_INT_SOURCE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_SOURCE_TRACER            ! different de 0 si erreur
      type(SOURCE_TRACER_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_SOURCE_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Sources.Type') > 0) then
         Instance%Type = valeur
      else if ( index(NomVar, 'Model.Tracer.Sources.ReachNumber') > 0) then
         Instance%NumBranche = valeur
      else if ( index(NomVar, 'Model.Tracer.Sources.UpStreamNode') > 0) then
         Instance%SectionAm = valeur
      else if ( index(NomVar, 'Model.Tracer.Sources.DownStreamNode') > 0) then
         Instance%SectionAv = valeur
      else if ( index(NomVar, 'Model.Tracer.Sources.GraphNumber') > 0) then
         Instance%NumeroLoi = valeur
      else if ( index(NomVar, 'Model.Tracer.Sources.InflowName') > 0) then
         Instance%NumeroApport = valeur
      else
         SET_INT_SOURCE_TRACER = 1
         MessageErreur         = "SET_INT_SOURCE_TRACER - Unknown variable name"
      end if
   end function SET_INT_SOURCE_TRACER

   function SET_BOOL_SOURCE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_BOOL_SOURCE_TRACER       ! different de 0 si erreur
      type(SOURCE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(in) :: valeur                     ! valeur du logical de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_BOOL_SOURCE_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Sources.withInflow') > 0) then
         Instance%SuperpositionApport = valeur
      else
         SET_BOOL_SOURCE_TRACER = 1
         MessageErreur         = "SET_BOOL_SOURCE_TRACER - Unknown variable name"
      end if
   end function SET_BOOL_SOURCE_TRACER

   function SET_STRING_SOURCE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_SOURCE_TRACER ! different de 0 si erreur
      type(SOURCE_TRACER_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_SOURCE_TRACER = 0
      MessageErreur          = ""

      if (INDEX(NomVar,'Model.Tracer.Sources.Name') > 0) then
         Instance%Nom = TRIM(valeur)
        else
         SET_STRING_SOURCE_TRACER = 1
         MessageErreur         = "SET_STRING_SOURCE_TRACER - Unknown variable name"
      end if
   end function SET_STRING_SOURCE_TRACER

! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_SOURCE_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_SOURCE_TRACER      ! different de 0 si erreur
      type(SOURCE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_SOURCE_TRACER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%Apport_source)) then
          taille = SIZE(Instance%Apport_source)
          if (taille > 0) then
              DEALLOCATE(Instance%Apport_source, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SOURCE_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_SOURCE_TRACER_T.Source'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Apport_source)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_SOURCE_TRACER

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_SOURCE_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_SOURCE_TRACER      ! different de 0 si erreur
      type(SOURCE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_SOURCE_TRACER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%Apport_source)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_SOURCE_TRACER

end module M_SOURCE_TRACER_T
