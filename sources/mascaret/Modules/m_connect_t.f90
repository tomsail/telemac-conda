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

module M_CONNECT_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================

TYPE CONNECT_T
  sequence
  integer, dimension(:),   pointer :: OrigineBief          => null() ! Num. de la sect. origine du bief
  integer, dimension(:),   pointer :: FinBief              => null() ! Num. de la sect. fin     du bief
  integer, dimension(:),   pointer :: NbBiefConfluence     => null() ! Nombre de biefs d'une confluence
  integer, dimension(:,:), pointer :: NumBiefConfluence    => null() ! Num. du bief    d'une confluence
                                                                     ! (NbConfluence , NbMaxBiefConfluence)
  integer, dimension(:,:), pointer :: NumSectionConfluence => null() ! Num. de section d'une confluence
                                                          ! (NbConfluence , NbMaxBiefConfluence)
  integer, dimension(:),   pointer :: NumBiefExtLibre      => null() ! Num. de bief    de l'extr. libre
  integer, dimension(:),   pointer :: NumSectionExtLibre   => null() ! Num. de section de l'extr. libre

END TYPE CONNECT_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_CONNECT(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

      tabNomVar(i)         ="Model.Connect.FirstNdNum"
      tabDescriptionVar(i) ="Number of the first node for a reach"
      i=i+1
      tabNomVar(i)         ="Model.Connect.LastNdNum"
      tabDescriptionVar(i) ="Number of the last node for a reach"
      i=i+1
      tabNomVar(i)         ="Model.Connect.NumReachJunction"
      tabDescriptionVar(i) ="The number of reaches for a junction"
      i=i+1
      tabNomVar(i)         ="Model.Connect.ReachNum"
      tabDescriptionVar(i) ="The numbers of the reaches for a junction"
      i=i+1
      tabNomVar(i)         ="Model.Connect.NodeNum"
      tabDescriptionVar(i) ="Node number for a junction"
      i=i+1
      tabNomVar(i)         ="Model.Connect.ReachNumFreeOutflow"
      tabDescriptionVar(i) ="Reach number for the free outflow"
      i=i+1
      tabNomVar(i)         ="Model.Connect.NodeNumFreeOutflow"
      tabDescriptionVar(i) ="Node number for the free outflow"
      i=i+1
      return

    end subroutine GET_TAB_VAR_CONNECT

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_CONNECT(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none


      integer                          :: GET_TYPE_VAR_CONNECT     ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_CONNECT = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""

       if ( index(NomVar, 'Model.Connect.FirstNdNum') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Connect.LastNdNum') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Connect.NumReachJunction') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Connect.ReachNumFreeOutflow') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Connect.ReachNum') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 2
       else if ( index(NomVar, 'Model.Connect.NodeNumFreeOutflow') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Connect.NodeNum') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 2
      else
        GET_TYPE_VAR_CONNECT = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_CONNECT - Unknown variable name"
      end if

    end function GET_TYPE_VAR_CONNECT

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_CONNECT(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_CONNECT         ! different de 0 si erreur
      type(CONNECT_T),        intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_CONNECT = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Connect.FirstNdNum') > 0) then
         if (ASSOCIATED(Instance%OrigineBief)) then
            taille1 = size(Instance%OrigineBief)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Connect.LastNdNum') > 0) then
         if (ASSOCIATED(Instance%FinBief)) then
            taille1 = size(Instance%FinBief)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Connect.NumReachJunction') > 0) then
         if (ASSOCIATED(Instance%NbBiefConfluence)) then
            taille1 = size(Instance%NbBiefConfluence)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Connect.ReachNumFreeOutflow') > 0) then
         if (ASSOCIATED(Instance%NumBiefExtLibre)) then
            taille1 = size(Instance%NumBiefExtLibre)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Connect.ReachNum') > 0) then
         if (ASSOCIATED(Instance%NumBiefConfluence)) then
            taille1 = size(Instance%NumBiefConfluence, 1)
            taille2 = size(Instance%NumBiefConfluence, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.Connect.NodeNumFreeOutflow') > 0) then
         if (ASSOCIATED(Instance%NumSectionExtLibre)) then
            taille1 = size(Instance%NumSectionExtLibre)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Connect.NodeNum') > 0) then
         if (ASSOCIATED(Instance%NumSectionConfluence)) then
            taille1 = size(Instance%NumSectionConfluence, 1)
            taille2 = size(Instance%NumSectionConfluence, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else
         GET_TAILLE_VAR_CONNECT = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_CONNECT - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_CONNECT

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_CONNECT(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_CONNECT         ! different de 0 si erreur
      type(CONNECT_T),        intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_CONNECT = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.Connect.FirstNdNum') > 0) then
        if (ASSOCIATED(Instance%OrigineBief)) then
           t1 = size(Instance%OrigineBief)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%OrigineBief, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONNECT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to deallocate CONNECT_T.OrigineBief'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%OrigineBief) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%OrigineBief(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONNECT = err
              MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to allocate CONNECT_T.OrigineBief'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Connect.LastNdNum') > 0) then
        if (ASSOCIATED(Instance%FinBief)) then
           t1 = size(Instance%FinBief)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%FinBief, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONNECT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to deallocate CONNECT_T.FinBief'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%FinBief) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%FinBief(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONNECT = err
              MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to allocate CONNECT_T.FinBief'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Connect.NumReachJunction') > 0) then
        if (ASSOCIATED(Instance%NbBiefConfluence)) then
           t1 = size(Instance%NbBiefConfluence)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%NbBiefConfluence, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONNECT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to deallocate CONNECT_T.NbBiefConfluence'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%NbBiefConfluence) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%NbBiefConfluence(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONNECT = err
              MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to allocate CONNECT_T.NbBiefConfluence'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Connect.ReachNumFreeOutflow') > 0) then
        if (ASSOCIATED(Instance%NumBiefExtLibre)) then
           t1 = size(Instance%NumBiefExtLibre)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%NumBiefExtLibre, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONNECT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to deallocate CONNECT_T.NumBiefExtLibre'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%NumBiefExtLibre) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%NumBiefExtLibre(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONNECT = err
              MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to allocate CONNECT_T.NumBiefExtLibre'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Connect.ReachNum') > 0) then
        if (ASSOCIATED(Instance%NumBiefConfluence)) then
           t1 = size(Instance%NumBiefConfluence, 1)
           t2 = size(Instance%NumBiefConfluence, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%NumBiefConfluence, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONNECT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to deallocate CONNECT_T.NumBiefConfluence'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%NumBiefConfluence).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%NumBiefConfluence(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONNECT = err
              MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to allocate CONNECT_T.NumBiefConfluence'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Connect.NodeNumFreeOutflow') > 0) then
        if (ASSOCIATED(Instance%NumSectionExtLibre)) then
           t1 = size(Instance%NumSectionExtLibre)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%NumSectionExtLibre, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONNECT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to deallocate CONNECT_T.NumSectionExtLibre'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%NumSectionExtLibre) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%NumSectionExtLibre(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONNECT = err
              MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to allocate CONNECT_T.NumSectionExtLibre'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Connect.NodeNum') > 0) then
        if (ASSOCIATED(Instance%NumSectionConfluence)) then
           t1 = size(Instance%NumSectionConfluence, 1)
           t2 = size(Instance%NumSectionConfluence, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%NumSectionConfluence, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONNECT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to deallocate CONNECT_T.NumSectionConfluence'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%NumSectionConfluence).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%NumSectionConfluence(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONNECT = err
              MessageErreur = 'SET_TAILLE_VAR_CONNECT : Unable to allocate CONNECT_T.NumSectionConfluence'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_CONNECT = 1
         MessageErreur         = "SET_TAILLE_VAR_CONNECT - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_CONNECT

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_INT_CONNECT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_CONNECT            ! different de 0 si erreur
      type(CONNECT_T),        intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_CONNECT = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Connect.FirstNdNum') > 0) then
         valeur = Instance%OrigineBief(index1)
      else if ( index(NomVar, 'Model.Connect.LastNdNum') > 0) then
         valeur = Instance%FinBief(index1)
      else if ( index(NomVar, 'Model.Connect.NumReachJunction') > 0) then
         valeur = Instance%NbBiefConfluence(index1)
      else if ( index(NomVar, 'Model.Connect.ReachNumFreeOutflow') > 0) then
         valeur = Instance%NumBiefExtLibre(index1)
      else if ( index(NomVar, 'Model.Connect.ReachNum') > 0) then
         valeur = Instance%NumBiefConfluence(index1, index2)
      else if ( index(NomVar, 'Model.Connect.NodeNumFreeOutflow') > 0) then
         valeur = Instance%NumSectionExtLibre(index1)
      else if ( index(NomVar, 'Model.Connect.NodeNum') > 0) then
         valeur = Instance%NumSectionConfluence(index1, index2)
      else
         GET_INT_CONNECT = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_CONNECT - Unknown variable name"
      end if
   end function GET_INT_CONNECT



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_INT_CONNECT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_CONNECT            ! different de 0 si erreur
      type(CONNECT_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_CONNECT = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Connect.FirstNdNum') > 0) then
         Instance%OrigineBief(index1) = valeur
      else if ( index(NomVar, 'Model.Connect.LastNdNum') > 0) then
         Instance%FinBief(index1) = valeur
      else if ( index(NomVar, 'Model.Connect.NumReachJunction') > 0) then
         Instance%NbBiefConfluence(index1) = valeur
      else if ( index(NomVar, 'Model.Connect.ReachNumFreeOutflow') > 0) then
         Instance%NumBiefExtLibre(index1) = valeur
      else if ( index(NomVar, 'Model.Connect.ReachNum') > 0) then
         Instance%NumBiefConfluence(index1, index2) = valeur
      else if ( index(NomVar, 'Model.Connect.NodeNumFreeOutflow') > 0) then
         Instance%NumSectionExtLibre(index1) = valeur
      else if ( index(NomVar, 'Model.Connect.NodeNum') > 0) then
         Instance%NumSectionConfluence(index1, index2) = valeur
      else
         SET_INT_CONNECT = 1
         MessageErreur         = "SET_INT_CONNECT - Unknown variable name"
      end if
   end function SET_INT_CONNECT



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_CONNECT(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_CONNECT          ! different de 0 si erreur
      type(CONNECT_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_CONNECT = 0
      MessageErreur     = ""
      err               = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%OrigineBief)) then
          taille = SIZE(Instance%OrigineBief)
          if (taille > 0) then
              DEALLOCATE(Instance%OrigineBief, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONNECT = err
                  MessageErreur = 'Unable to deallocate CONNECT_T.OrigineBief'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%OrigineBief)
      if (ASSOCIATED(Instance%FinBief)) then
          taille = SIZE(Instance%FinBief)
          if (taille > 0) then
              DEALLOCATE(Instance%FinBief, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONNECT = err
                  MessageErreur = 'Unable to deallocate CONNECT_T.FinBief'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%FinBief)
      if (ASSOCIATED(Instance%NbBiefConfluence)) then
          taille = SIZE(Instance%NbBiefConfluence)
          if (taille > 0) then
              DEALLOCATE(Instance%NbBiefConfluence, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONNECT = err
                  MessageErreur = 'Unable to deallocate CONNECT_T.NbBiefConfluence'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%NbBiefConfluence)
      if (ASSOCIATED(Instance%NumBiefConfluence)) then
          taille = SIZE(Instance%NumBiefConfluence, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%NumBiefConfluence, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONNECT = err
                  MessageErreur = 'Unable to deallocate CONNECT_T.NumBiefConfluence'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%NumBiefConfluence)
      if (ASSOCIATED(Instance%NumSectionConfluence)) then
          taille = SIZE(Instance%NumSectionConfluence, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%NumSectionConfluence, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONNECT = err
                  MessageErreur = 'Unable to deallocate CONNECT_T.NumSectionConfluence'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%NumSectionConfluence)
      if (ASSOCIATED(Instance%NumBiefExtLibre)) then
          taille = SIZE(Instance%NumBiefExtLibre)
          if (taille > 0) then
              DEALLOCATE(Instance%NumBiefExtLibre, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONNECT = err
                  MessageErreur = 'Unable to deallocate CONNECT_T.NumBiefExtLibre'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%NumBiefExtLibre)
      if (ASSOCIATED(Instance%NumSectionExtLibre)) then
          taille = SIZE(Instance%NumSectionExtLibre)
          if (taille > 0) then
              DEALLOCATE(Instance%NumSectionExtLibre, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONNECT = err
                  MessageErreur = 'Unable to deallocate CONNECT_T.NumSectionExtLibre'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%NumSectionExtLibre)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_CONNECT

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_CONNECT(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_CONNECT          ! different de 0 si erreur
      type(CONNECT_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_CONNECT = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%OrigineBief)
      NULLIFY(Instance%FinBief)
      NULLIFY(Instance%NbBiefConfluence)
      NULLIFY(Instance%NumBiefConfluence)
      NULLIFY(Instance%NumSectionConfluence)
      NULLIFY(Instance%NumBiefExtLibre)
      NULLIFY(Instance%NumSectionExtLibre)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_CONNECT

end module M_CONNECT_T
