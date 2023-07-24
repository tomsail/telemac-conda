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

Module M_NODE_TRACER_T
!***********************************************************************
! PROGICIEL : MASCARET-TRACER     F. ZAOUI
!
! VERSION : V8P4R0                   EDF-CEREMA
!***********************************************************************

   type NODE_TRACER_T

      sequence

      integer     , dimension(:)     ,    pointer :: NB_CHILD => null()    ! POUR CHAQUE NOEUD, LE NOMBRE DE NOEUDS AVAL
      integer     , dimension(:)     ,    pointer :: NB_PARENT => null()   ! POUR CHAQUE NOEUD, LE NOMBRE DE NOEUDS AMONT
      integer     , dimension(:,:)   ,    pointer :: CHILD => null()   ! POUR CHAQUE NOEUD, LA LISTE DES NOEUDS AVAL
      integer     , dimension(:,:)   ,    pointer :: PARENT => null()   ! POUR CHAQUE NOEUD, LA LISTE DES NOEUDS AMONT

   end type NODE_TRACER_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_NODE_TRACER(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="Model.Tracer.NodeTrac.NB_CHILD"
        tabDescriptionVar(i) ="The number of childs for each node of the tree (hydraulic system)"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.NodeTrac.NB_PARENT"
        tabDescriptionVar(i) ="The number of parents for each node of the tree (hydraulic system)"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.NodeTrac.CHILD"
        tabDescriptionVar(i) ="List of childs for each node of the tree (hydraulic system)"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.NodeTrac.PARENT"
        tabDescriptionVar(i) ="List of parents for each node of the tree (hydraulic system)"
        i=i+1

        return

    end subroutine GET_TAB_VAR_NODE_TRACER

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_NODE_TRACER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_NODE_TRACER ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_NODE_TRACER = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_CHILD') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_PARENT') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.CHILD') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 2
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.PARENT') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 2
      else
         GET_TYPE_VAR_NODE_TRACER = 1
         TypeVar = "?"
         Categorie             = "MODEL"
         Modifiable            = .false.
         dimVar                = -1
         MessageErreur         = "GET_TYPE_VAR_NODE_TRACER - Unknown variable name"
      end if

    end function GET_TYPE_VAR_NODE_TRACER


! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_NODE_TRACER(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_NODE_TRACER     ! different de 0 si erreur
      type(NODE_TRACER_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_NODE_TRACER = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_CHILD') > 0) then
         taille1 = size(Instance%NB_CHILD, 1)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_PARENT') > 0) then
         taille1 = size(Instance%NB_PARENT)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.CHILD') > 0) then
         taille1 = size(Instance%CHILD, 1)
         taille2 = size(Instance%CHILD, 2)
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.PARENT') > 0) then
         taille1 = size(Instance%PARENT, 1)
         taille2 = size(Instance%PARENT, 2)
         taille3 = 0
     else
         GET_TAILLE_VAR_NODE_TRACER = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_NODE_TRACER - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_NODE_TRACER

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_NODE_TRACER(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_NODE_TRACER     ! different de 0 si erreur
      type(NODE_TRACER_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_NODE_TRACER = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_CHILD') > 0) then
         if (associated(Instance%NB_CHILD)) then
            t1 = size(Instance%NB_CHILD)
            if (t1 /= NewT1) then
               deallocate(Instance%NB_CHILD, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_NODE_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_NODE_TRACER : Unable to deallocate NODE_TRACER_T.NB_CHILD'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%NB_CHILD) .or. (t1 /= NewT1)) then
            allocate(Instance%NB_CHILD(NewT1), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_NODE_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_NODE_TRACER : Unable to allocate NODE_TRACER_T.NB_CHILD'
               return
            endif
         endif
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_PARENT') > 0) then
         if (associated(Instance%NB_PARENT)) then
            t1 = size(Instance%NB_PARENT)
            if (t1 /= NewT1) then
               deallocate(Instance%NB_PARENT, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_NODE_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_NODE_TRACER : Unable to deallocate NODE_TRACER_T.NB_PARENT'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%NB_PARENT) .or. (t1 /= NewT1)) then
            allocate(Instance%NB_PARENT(NewT1), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_NODE_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_NODE_TRACER : Unable to allocate NODE_TRACER_T.NB_PARENT'
               return
            endif
         endif
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.CHILD') > 0) then
         if (associated(Instance%CHILD)) then
            t1 = size(Instance%CHILD, 1)
            t2 = size(Instance%CHILD, 2)
            if ( (t1 /= NewT1).or.(t2 /= NewT2) ) then
               deallocate(Instance%CHILD, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_NODE_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_NODE_TRACER : Unable to deallocate NODE_TRACER_T.CHILD'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%CHILD) .or. (t1 /= NewT1).or.(t2/=NewT2)) then
            allocate(Instance%CHILD(NewT1, NewT2), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_NODE_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_NODE_TRACER : Unable to allocate NODE_TRACER_T.CHILD'
               return
            endif
         endif
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.PARENT') > 0) then
         if (associated(Instance%PARENT)) then
            t1 = size(Instance%PARENT, 1)
            t2 = size(Instance%PARENT, 2)
            if ( (t1 /= NewT1).or.(t2 /= NewT2) ) then
               deallocate(Instance%PARENT, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_NODE_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_NODE_TRACER : Unable to deallocate NODE_TRACER_T.PARENT'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%PARENT) .or. (t1 /= NewT1).or.(t2/=NewT2)) then
            allocate(Instance%PARENT(NewT1, NewT2), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_NODE_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_NODE_TRACER : Unable to allocate NODE_TRACER_T.PARENT'
               return
            endif
         endif
         !--------------------------------------------------------------------
         ! Fin de la modification de la taille des pointers de types primitifs
         !--------------------------------------------------------------------
      else
         SET_TAILLE_VAR_NODE_TRACER = 1
         MessageErreur         = "SET_TAILLE_VAR_NODE_TRACER - Unknown variable name"
      end if

   end function SET_TAILLE_VAR_NODE_TRACER

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_INT_NODE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_NODE_TRACER            ! different de 0 si erreur
      type(NODE_TRACER_T),        intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_NODE_TRACER = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_CHILD') > 0) then
         valeur = Instance%NB_CHILD(index1)
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_PARENT') > 0) then
         valeur = Instance%NB_PARENT(index1)
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.CHILD') > 0) then
         valeur = Instance%CHILD(index1,index2)
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.PARENT') > 0) then
         valeur = Instance%PARENT(index1,index2)
      else
         GET_INT_NODE_TRACER = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_NODE_TRACER - Unknown variable name"
      end if
   end function GET_INT_NODE_TRACER

! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_INT_NODE_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_NODE_TRACER            ! different de 0 si erreur
      type(NODE_TRACER_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_NODE_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_CHILD') > 0) then
         Instance%NB_CHILD(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.NB_PARENT') > 0) then
         Instance%NB_PARENT(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.CHILD') > 0) then
         Instance%CHILD(index1,index2) = valeur
      else if ( index(NomVar, 'Model.Tracer.NodeTrac.PARENT') > 0) then
         Instance%PARENT(index1,index2) = valeur
      else
         SET_INT_NODE_TRACER = 1
         MessageErreur         = "SET_INT_NODE_TRACER - Unknown variable name"
      end if
   end function SET_INT_NODE_TRACER

! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_NODE_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_NODE_TRACER      ! different de 0 si erreur
      type(NODE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_NODE_TRACER = 0
      MessageErreur          = ""

      if (associated(Instance%NB_CHILD)) then
         taille = size(Instance%NB_CHILD)
         if (taille > 0) then
            deallocate(Instance%NB_CHILD, STAT=err)
            if (err /= 0) then
               DESALLOUE_NODE_TRACER = err
               MessageErreur = 'Unable to deallocate NODE_TRACER_T.NB_CHILD'
               return
            endif
         endif
      endif
      nullify(Instance%NB_PARENT)
      if (associated(Instance%NB_PARENT)) then
         taille = size(Instance%NB_PARENT)
         if (taille > 0) then
            deallocate(Instance%NB_PARENT, STAT=err)
            if (err /= 0) then
               DESALLOUE_NODE_TRACER = err
               MessageErreur = 'Unable to deallocate NODE_TRACER_T.NB_PARENT'
               return
            endif
         endif
      endif
      nullify(Instance%NB_PARENT)
      if (associated(Instance%CHILD)) then
         taille = size(Instance%CHILD, 1)
         if (taille > 0) then
            deallocate(Instance%CHILD, STAT=err)
            if (err /= 0) then
               DESALLOUE_NODE_TRACER = err
               MessageErreur = 'Unable to deallocate NODE_TRACER_T.CHILD'
               return
            endif
         endif
      endif
      nullify(Instance%PARENT)
      if (associated(Instance%PARENT)) then
         taille = size(Instance%PARENT, 1)
         if (taille > 0) then
            deallocate(Instance%PARENT, STAT=err)
            if (err /= 0) then
               DESALLOUE_NODE_TRACER = err
               MessageErreur = 'Unable to deallocate NODE_TRACER_T.PARENT'
               return
            endif
         endif
      endif
      nullify(Instance%PARENT)

    end function DESALLOUE_NODE_TRACER

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_NODE_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_NODE_TRACER      ! different de 0 si erreur
      type(NODE_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_NODE_TRACER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      nullify(Instance%NB_CHILD)
      nullify(Instance%NB_PARENT)
      nullify(Instance%CHILD)
      nullify(Instance%PARENT)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_NODE_TRACER

 end module M_NODE_TRACER_T
