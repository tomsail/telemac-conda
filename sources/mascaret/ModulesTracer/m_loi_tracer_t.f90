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

module M_LOI_TRACER_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   use M_PRECISION

   implicit none

   integer, parameter :: LOI_UNITE_SECONDE = 1 !  Seconde "S"
   integer, parameter :: LOI_UNITE_MINUTE  = 2 !  Minute  "M"
   integer, parameter :: LOI_UNITE_HEURE   = 3 !  Heure   "H"
   integer, parameter :: LOI_UNITE_JOUR    = 4 !  Jour    "J"
   integer, parameter :: LOI_UNITE_NB_MAX  = 15

   TYPE LOI_TRACER_T
      sequence
      character(30)                         :: Nom        ! Nom de la loi
      real(DOUBLE), dimension(:), pointer   :: Temps => null()       ! Temps
      real(DOUBLE), dimension(:,:), pointer :: Conc => null()        ! Concentration en traceur
     ! (ou eventuellement flux volumique ou surfacique de traceur pour les sources)
   END TYPE LOI_TRACER_T

contains

   ! Retourne les noms des champs du type ainsi qu'une description
   subroutine GET_TAB_VAR_LOI_TRACER(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="Model.Tracer.Graph.Name"
        tabDescriptionVar(i) ="Name of the graph"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Graph.Time"
        tabDescriptionVar(i) ="Time values of the graph"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Graph.Concentration"
        tabDescriptionVar(i) ="Concentration values of the graph"
        i=i+1

        return

     end subroutine GET_TAB_VAR_LOI_TRACER

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_LOI_TRACER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_LOI_TRACER ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_TRACER sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_LOI_TRACER = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'Model.Tracer.Graph.Name') > 0) then
         TypeVar = 'STRING'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Graph.Time') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Graph.Concentration') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 2
      else
         GET_TYPE_VAR_LOI_TRACER = 1
         TypeVar = "?"
         Categorie             = "MODEL"
         Modifiable            = .false.
         dimVar                = -1
         MessageErreur         = "GET_TYPE_VAR_LOI_TRACER - Unknown variable name"
      end if

    end function GET_TYPE_VAR_LOI_TRACER

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_LOI_TRACER(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_LOI_TRACER     ! different de 0 si erreur
      type(LOI_TRACER_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du source
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_LOI_TRACER = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Graph.Name') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Graph.Time') > 0) then
         taille1 = size(Instance%Temps)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Graph.Concentration') > 0) then
         taille1 = size(Instance%Conc, 1)
         taille2 = size(Instance%Conc, 2)
         taille3 = 0
      else
         GET_TAILLE_VAR_LOI_TRACER = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_LOI_TRACER - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_LOI_TRACER

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_LOI_TRACER(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_LOI_TRACER     ! different de 0 si erreur
      type(LOI_TRACER_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, err

      SET_TAILLE_VAR_LOI_TRACER = 0
      MessageErreur          = ""
      !------------------------------------------------------------------------------
      ! Fin des appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type primitif
      !------------------------------------------------------------------------------
      if ( index(NomVar, 'Model.Tracer.Graph.Time') > 0) then
        if (ASSOCIATED(Instance%Temps)) then
           t1 = size(Instance%Temps)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Temps, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_LOI_TRACER = err
                 MessageErreur = 'SET_TAILLE_VAR_LOI_TRACER : Unable to deallocate MODEL_LOI_TRACER_T.Time'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Temps) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Temps(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_LOI_TRACER = err
              MessageErreur = 'SET_TAILLE_VAR_LOI_TRACER : Unable to allocate MODEL_LOI_TRACER_T.Time'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Tracer.Graph.Concentration') > 0) then
        if (ASSOCIATED(Instance%Conc)) then
           t1 = size(Instance%Conc, 1)
           t2 = size(Instance%Conc, 2)
           if ( (t1 /= NewT1).or.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%Conc, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_LOI_TRACER = err
                 MessageErreur = 'SET_TAILLE_VAR_LOI_TRACER : Unable to deallocate MODEL_LOI_TRACER_T.Concentration'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Conc) .OR. (t1 /= NewT1).or.(t2/=NewT2)) then
           allocate(Instance%Conc(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_LOI_TRACER = err
              MessageErreur = 'SET_TAILLE_VAR_LOI_TRACER : Unable to allocate MODEL_LOI_TRACER_T.Concentration'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------
      else
         SET_TAILLE_VAR_LOI_TRACER = 1
         MessageErreur         = "SET_TAILLE_VAR_LOI_TRACER - Unknown variable name"
      end if

    end function SET_TAILLE_VAR_LOI_TRACER

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_LOI_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_LOI_TRACER     ! different de 0 si erreur
      type(LOI_TRACER_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_LOI_TRACER = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Graph.Time') > 0) then
         valeur = Instance%Temps(index1)
      else if ( index(NomVar, 'Model.Tracer.Graph.Concentration') > 0) then
         valeur = Instance%Conc(index1, index2)
      else
         GET_DOUBLE_LOI_TRACER = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_LOI_TRACER - Unknown variable name"
      end if
   end function GET_DOUBLE_LOI_TRACER


   function GET_STRING_LOI_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_LOI_TRACER ! different de 0 si erreur
      type(LOI_TRACER_T),intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_LOI_TRACER = 0
      valeur                = ""
      MessageErreur          = ""

      if (INDEX(NomVar,'Model.Tracer.Graph.Name') > 0) then
         valeur = Instance%Nom
      else
         GET_STRING_LOI_TRACER = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_LOI_TRACER - Unknown variable name"
      end if
   end function GET_STRING_LOI_TRACER

! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_LOI_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_LOI_TRACER     ! different de 0 si erreur
      type(LOI_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_LOI_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Graph.Time') > 0) then
         Instance%Temps(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.Graph.Concentration') > 0) then
         Instance%Conc(index1, index2) = valeur
      else
         SET_DOUBLE_LOI_TRACER = 1
         MessageErreur         = "SET_DOUBLE_LOI_TRACER - Unknown variable name"
      end if
   end function SET_DOUBLE_LOI_TRACER

   function SET_STRING_LOI_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_LOI_TRACER ! different de 0 si erreur
      type(LOI_TRACER_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_LOI_TRACER = 0
      MessageErreur          = ""

      if (INDEX(NomVar,'Model.Tracer.Graph.Name') > 0) then
         Instance%Nom = TRIM(valeur)
        else
         SET_STRING_LOI_TRACER = 1
         MessageErreur         = "SET_STRING_LOI_TRACER - Unknown variable name"
      end if
   end function SET_STRING_LOI_TRACER

! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_LOI_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_LOI_TRACER      ! different de 0 si erreur
      type(LOI_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_LOI_TRACER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%Temps)) then
          taille = SIZE(Instance%Temps)
          if (taille > 0) then
              DEALLOCATE(Instance%Temps, STAT=err)
              if (err /= 0) then
                  DESALLOUE_LOI_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_LOI_TRACER_T.Time'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Temps)
      if (ASSOCIATED(Instance%Conc)) then
          taille = size(Instance%Conc, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%Conc, STAT=err)
              if (err /= 0) then
                  DESALLOUE_LOI_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_LOI_TRACER_T.Concentration'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Conc)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_LOI_TRACER

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_LOI_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_LOI_TRACER      ! different de 0 si erreur
      type(LOI_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_LOI_TRACER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%Temps)
      NULLIFY(Instance%Conc)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_LOI_TRACER

end module M_LOI_TRACER_T
