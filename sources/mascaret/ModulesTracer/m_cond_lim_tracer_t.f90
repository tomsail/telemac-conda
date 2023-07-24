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

module M_COND_LIM_TRACER_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION

   TYPE COND_LIM_TRACER_T
      sequence
      integer       :: Type          ! Type de la CL (1:Neumann ; 2:Dirichlet)
      integer       :: NumeroLoi     ! Numero de la loi associee
      real (DOUBLE),dimension(:),pointer :: Conc_lim  => null() ! Apport par la CL pour chaque traceur (conc, flux volum ou surf selon le cas)
   END TYPE COND_LIM_TRACER_T

contains

   ! Retourne les noms des champs du type ainsi qu'une description
   subroutine GET_TAB_VAR_COND_LIM_TRACER(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="Model.Tracer.BoundaryCond.Type"
        tabDescriptionVar(i) ="Type of BC (1:Neumann ; 2:Dirichlet)"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.BoundaryCond.Number"
        tabDescriptionVar(i) ="Number of the graph associated with the BC"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.BoundaryCond.Concentration"
        tabDescriptionVar(i) ="Concentration values for the BC"
        i=i+1

        return

     end subroutine GET_TAB_VAR_COND_LIM_TRACER

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_COND_LIM_TRACER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_COND_LIM_TRACER ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_TRACER sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_COND_LIM_TRACER = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'Model.Tracer.BoundaryCond.Type') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.BoundaryCond.Number') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.BoundaryCond.Concentration') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else
         GET_TYPE_VAR_COND_LIM_TRACER = 1
         TypeVar = "?"
         Categorie             = "MODEL"
         Modifiable            = .false.
         dimVar                = -1
         MessageErreur         = "GET_TYPE_VAR_COND_LIM_TRACER - Unknown variable name"
      end if

    end function GET_TYPE_VAR_COND_LIM_TRACER

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_COND_LIM_TRACER(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_COND_LIM_TRACER     ! different de 0 si erreur
      type(COND_LIM_TRACER_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du source
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur
      GET_TAILLE_VAR_COND_LIM_TRACER = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.BoundaryCond.Type') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.BoundaryCond.Number') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.BoundaryCond.Concentration') > 0) then
         taille1 = size(Instance%Conc_lim)
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_COND_LIM_TRACER = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_COND_LIM_TRACER - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_COND_LIM_TRACER

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_COND_LIM_TRACER(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_COND_LIM_TRACER     ! different de 0 si erreur
      type(COND_LIM_TRACER_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, err
      SET_TAILLE_VAR_COND_LIM_TRACER = 0
      MessageErreur          = ""
      !------------------------------------------------------------------------------
      ! Fin des appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type primitif
      !------------------------------------------------------------------------------
      if ( index(NomVar, 'Model.Tracer.BoundaryCond.Concentration') > 0) then
        if (ASSOCIATED(Instance%Conc_lim)) then
           t1 = size(Instance%Conc_lim)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Conc_lim, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_COND_LIM_TRACER = err
                 MessageErreur = 'SET_TAILLE_VAR_COND_LIM_TRACER : Unable to deallocate MODEL_COND_LIM_TRACER_T.Concentration'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Conc_lim) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Conc_lim(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_COND_LIM_TRACER = err
              MessageErreur = 'SET_TAILLE_VAR_COND_LIM_TRACER : Unable to allocate MODEL_COND_LIM_TRACER_T.Concentration'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------
      else
         SET_TAILLE_VAR_COND_LIM_TRACER = 1
         MessageErreur         = "SET_TAILLE_VAR_COND_LIM_TRACER - Unknown variable name"
      end if

    end function SET_TAILLE_VAR_COND_LIM_TRACER
! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_COND_LIM_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_COND_LIM_TRACER     ! different de 0 si erreur
      type(COND_LIM_TRACER_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_COND_LIM_TRACER = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.BoundaryCond.Concentration') > 0) then
         valeur = Instance%Conc_lim(index1)
      else
         GET_DOUBLE_COND_LIM_TRACER = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_COND_LIM_TRACER - Unknown variable name"
      end if
   end function GET_DOUBLE_COND_LIM_TRACER

   function GET_INT_COND_LIM_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_COND_LIM_TRACER            ! different de 0 si erreur
      type(COND_LIM_TRACER_T),        intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_COND_LIM_TRACER = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.BoundaryCond.Type') > 0) then
         valeur = Instance%Type
      else if ( index(NomVar, 'Model.Tracer.BoundaryCond.Number') > 0) then
         valeur = Instance%NumeroLoi
      else
         GET_INT_COND_LIM_TRACER = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_COND_LIM_TRACER - Unknown variable name"
      end if
   end function GET_INT_COND_LIM_TRACER
! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_COND_LIM_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_COND_LIM_TRACER     ! different de 0 si erreur
      type(COND_LIM_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_COND_LIM_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.BoundaryCond.Concentration') > 0) then
         Instance%Conc_lim(index1) = valeur
      else
         SET_DOUBLE_COND_LIM_TRACER = 1
         MessageErreur         = "SET_DOUBLE_COND_LIM_TRACER - Unknown variable name"
      end if
   end function SET_DOUBLE_COND_LIM_TRACER

   function SET_INT_COND_LIM_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_COND_LIM_TRACER            ! different de 0 si erreur
      type(COND_LIM_TRACER_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_COND_LIM_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.BoundaryCond.Type') > 0) then
         Instance%Type = valeur
      else if ( index(NomVar, 'Model.Tracer.BoundaryCond.Number') > 0) then
         Instance%NumeroLoi = valeur
      else
         SET_INT_COND_LIM_TRACER = 1
         MessageErreur         = "SET_INT_COND_LIM_TRACER - Unknown variable name"
      end if
   end function SET_INT_COND_LIM_TRACER

! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_COND_LIM_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_COND_LIM_TRACER      ! different de 0 si erreur
      type(COND_LIM_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_COND_LIM_TRACER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%Conc_lim)) then
          taille = SIZE(Instance%Conc_lim)
          if (taille > 0) then
              DEALLOCATE(Instance%Conc_lim, STAT=err)
              if (err /= 0) then
                  DESALLOUE_COND_LIM_TRACER = err
                  MessageErreur = 'Unable to deallocate MODEL_COND_LIM_TRACER_T.Concentration'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Conc_lim)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_COND_LIM_TRACER

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_COND_LIM_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_COND_LIM_TRACER      ! different de 0 si erreur
      type(COND_LIM_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_COND_LIM_TRACER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%Conc_lim)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_COND_LIM_TRACER

end module M_COND_LIM_TRACER_T
