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

module M_LOI_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================

  use M_PRECISION

  ! Constantes reperant le  type de "loi"
  !--------------------------------------


  integer     , parameter :: LOI_TYPE_HYDROGRAMME      = 1 !  Q = f(T)
  integer     , parameter :: LOI_TYPE_LIMNIGRAMME      = 2 !  Z = f(T)
  integer     , parameter :: LOI_TYPE_LIMNHYDROGRAMME  = 3 !  Q et Z = f(T)
  integer     , parameter :: LOI_TYPE_TARAGE_Z_Q       = 4 !  Z = f(Q)
  integer     , parameter :: LOI_TYPE_TARAGE_Q_Z       = 5 !  Q = f(Z)
  integer     , parameter :: LOI_TYPE_ZAMONT_ZAVAL_Q   = 6 !  ZAM = f(ZAV,Q)
  integer     , parameter :: LOI_TYPE_ZINF_ZSUP_T      = 7 !  ZINF et ZSUP = f(T)

  integer     , parameter :: LOI_TYPE_NB_MAX = 7

  integer     , parameter :: LOI_UNITE_SECONDE = 1 !  Seconde "S"
  integer     , parameter :: LOI_UNITE_MINUTE  = 2 !  Minute  "M"
  integer     , parameter :: LOI_UNITE_HEURE   = 3 !  Heure   "H"
  integer     , parameter :: LOI_UNITE_JOUR    = 4 !  Jour    "J"

  integer     , parameter :: LOI_UNITE_NB_MAX = 4

  type LOI_T
     sequence
     character(30)                         :: Nom
     integer                               :: Type
     real(DOUBLE), dimension(:)  , pointer :: Temps     => null()
     real(DOUBLE), dimension(:)  , pointer :: Cote      => null()
     real(DOUBLE), dimension(:)  , pointer :: Debit     => null()

     real(DOUBLE), dimension(:)  , pointer :: CoteSup   => null()
     real(DOUBLE), dimension(:)  , pointer :: CoteInf   => null()
     real(DOUBLE), dimension(:)  , pointer :: CoteAval  => null()
     real(DOUBLE), dimension(:,:), pointer :: CoteAmont => null()

  end type LOI_T

contains
   ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_LOI(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele
        tabNomVar(i)         ="Model.Graph.Name"
        tabDescriptionVar(i) ="Name of the graph"
        i=i+1
        tabNomVar(i)         ="Model.Graph.Type"
        tabDescriptionVar(i) =&
        "Graph type : 1->Q=f(T) 2->Z=f(T) 3->Q,Z=f(T) 4->Z=f(Q) 5->Q=f(Z) 6->Zus=f(Zds,Q) 7->Zinf,Zsup=f(T)"
        i=i+1
        tabNomVar(i)         ="Model.Graph.Time"
        tabDescriptionVar(i) ="Time value"
        i=i+1
        tabNomVar(i)         ="Model.Graph.Level"
        tabDescriptionVar(i) ="Level value"
        i=i+1
        tabNomVar(i)         ="Model.Graph.Discharge"
        tabDescriptionVar(i) ="Discharge value"
        i=i+1
        tabNomVar(i)         ="Model.Graph.SupLevel"
        tabDescriptionVar(i) ="Superior level"
        i=i+1
        tabNomVar(i)         ="Model.Graph.InfLevel"
        tabDescriptionVar(i) ="Inferior level"
        i=i+1
        tabNomVar(i)         ="Model.Graph.DownLevel"
        tabDescriptionVar(i) ="Downstream level"
        i=i+1
        tabNomVar(i)         ="Model.Graph.UpLevel"
        tabDescriptionVar(i) ="Upstream level"
        i=i+1

      return

    end subroutine GET_TAB_VAR_LOI

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_LOI(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_LOI    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_LOI = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

       if ( index(NomVar, 'Model.Graph.Name') > 0) then
          TypeVar = 'STRING'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Graph.Type') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Graph.Time') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Graph.Level') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Graph.Discharge') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Graph.SupLevel') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Graph.InfLevel') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Graph.DownLevel') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Graph.UpLevel') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
      else
        GET_TYPE_VAR_LOI = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_LOI - Unknown variable name"
      end if


    end function GET_TYPE_VAR_LOI

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_LOI(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_LOI             ! different de 0 si erreur
      type(LOI_T),            intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_LOI = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Graph.Name') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Graph.Type') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Graph.Time') > 0) then
         if (ASSOCIATED(Instance%Temps)) then
            taille1 = size(Instance%Temps)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Graph.Level') > 0) then
         if (ASSOCIATED(Instance%Cote)) then
            taille1 = size(Instance%Cote)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Graph.Discharge') > 0) then
         if (ASSOCIATED(Instance%Debit)) then
            taille1 = size(Instance%Debit)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Graph.SupLevel') > 0) then
         if (ASSOCIATED(Instance%CoteSup)) then
            taille1 = size(Instance%CoteSup)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Graph.InfLevel') > 0) then
         if (ASSOCIATED(Instance%CoteInf)) then
            taille1 = size(Instance%CoteInf)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Graph.DownLevel') > 0) then
         if (ASSOCIATED(Instance%CoteAval)) then
            taille1 = size(Instance%CoteAval)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Graph.UpLevel') > 0) then
         if (ASSOCIATED(Instance%CoteAmont)) then
            taille1 = size(Instance%CoteAmont, 1)
            taille2 = size(Instance%CoteAmont, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else
         GET_TAILLE_VAR_LOI = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_LOI - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_LOI

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_LOI(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_LOI             ! different de 0 si erreur
      type(LOI_T),            intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_LOI = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.Graph.Time') > 0) then
        if (ASSOCIATED(Instance%Temps)) then
           t1 = size(Instance%Temps)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Temps, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_LOI = err
                 MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to deallocate LOI_T.Temps'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Temps) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Temps(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_LOI = err
              MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to allocate LOI_T.Temps'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Graph.Level') > 0) then
        if (ASSOCIATED(Instance%Cote)) then
           t1 = size(Instance%Cote)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Cote, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_LOI = err
                 MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to deallocate LOI_T.Cote'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Cote) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Cote(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_LOI = err
              MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to allocate LOI_T.Cote'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Graph.Discharge') > 0) then
        if (ASSOCIATED(Instance%Debit)) then
           t1 = size(Instance%Debit)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Debit, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_LOI = err
                 MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to deallocate LOI_T.Debit'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Debit) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Debit(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_LOI = err
              MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to allocate LOI_T.Debit'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Graph.SupLevel') > 0) then
        if (ASSOCIATED(Instance%CoteSup)) then
           t1 = size(Instance%CoteSup)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%CoteSup, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_LOI = err
                 MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to deallocate LOI_T.CoteSup'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%CoteSup) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%CoteSup(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_LOI = err
              MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to allocate LOI_T.CoteSup'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Graph.InfLevel') > 0) then
        if (ASSOCIATED(Instance%CoteInf)) then
           t1 = size(Instance%CoteInf)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%CoteInf, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_LOI = err
                 MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to deallocate LOI_T.CoteInf'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%CoteInf) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%CoteInf(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_LOI = err
              MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to allocate LOI_T.CoteInf'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Graph.DownLevel') > 0) then
        if (ASSOCIATED(Instance%CoteAval)) then
           t1 = size(Instance%CoteAval)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%CoteAval, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_LOI = err
                 MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to deallocate LOI_T.CoteAval'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%CoteAval) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%CoteAval(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_LOI = err
              MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to allocate LOI_T.CoteAval'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Graph.UpLevel') > 0) then
        if (ASSOCIATED(Instance%CoteAmont)) then
           t1 = size(Instance%CoteAmont, 1)
           t2 = size(Instance%CoteAmont, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%CoteAmont, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_LOI = err
                 MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to deallocate LOI_T.CoteAmont'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%CoteAmont).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%CoteAmont(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_LOI = err
              MessageErreur = 'SET_TAILLE_VAR_LOI : Unable to allocate LOI_T.CoteAmont'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_LOI = 1
         MessageErreur         = "SET_TAILLE_VAR_LOI - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_LOI

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_LOI(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_LOI             ! different de 0 si erreur
      type(LOI_T),            intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_LOI = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Graph.Time') > 0) then
         valeur = Instance%Temps(index1)
      else if ( index(NomVar, 'Model.Graph.Level') > 0) then
         valeur = Instance%Cote(index1)
      else if ( index(NomVar, 'Model.Graph.Discharge') > 0) then
         valeur = Instance%Debit(index1)
      else if ( index(NomVar, 'Model.Graph.SupLevel') > 0) then
         valeur = Instance%CoteSup(index1)
      else if ( index(NomVar, 'Model.Graph.InfLevel') > 0) then
         valeur = Instance%CoteInf(index1)
      else if ( index(NomVar, 'Model.Graph.DownLevel') > 0) then
         valeur = Instance%CoteAval(index1)
      else if ( index(NomVar, 'Model.Graph.UpLevel') > 0) then
         valeur = Instance%CoteAmont(index1, index2)
      else
         GET_DOUBLE_LOI = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_LOI - Unknown variable name"
      end if
   end function GET_DOUBLE_LOI


   function GET_INT_LOI(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_LOI                ! different de 0 si erreur
      type(LOI_T),            intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_LOI = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Graph.Type') > 0) then
         valeur = Instance%Type
      else
         GET_INT_LOI = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_LOI - Unknown variable name"
      end if
   end function GET_INT_LOI


   function GET_STRING_LOI(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_LOI             ! different de 0 si erreur
      type(LOI_T),            intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_LOI = 0
      valeur                = ""
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Graph.Name') > 0) then
         valeur = Instance%Nom
      else
         GET_STRING_LOI = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_LOI - Unknown variable name"
      end if
   end function GET_STRING_LOI



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_LOI(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_LOI             ! different de 0 si erreur
      type(LOI_T),            intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_LOI = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Graph.Time') > 0) then
         Instance%Temps(index1) = valeur
      else if ( index(NomVar, 'Model.Graph.Level') > 0) then
         Instance%Cote(index1) = valeur
      else if ( index(NomVar, 'Model.Graph.Discharge') > 0) then
         Instance%Debit(index1) = valeur
      else if ( index(NomVar, 'Model.Graph.SupLevel') > 0) then
         Instance%CoteSup(index1) = valeur
      else if ( index(NomVar, 'Model.Graph.InfLevel') > 0) then
         Instance%CoteInf(index1) = valeur
      else if ( index(NomVar, 'Model.Graph.DownLevel') > 0) then
         Instance%CoteAval(index1) = valeur
      else if ( index(NomVar, 'Model.Graph.UpLevel') > 0) then
         Instance%CoteAmont(index1, index2) = valeur
      else
         SET_DOUBLE_LOI = 1
         MessageErreur         = "SET_DOUBLE_LOI - Unknown variable name"
      end if
   end function SET_DOUBLE_LOI


   function SET_INT_LOI(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_LOI                ! different de 0 si erreur
      type(LOI_T),            intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_LOI = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Graph.Type') > 0) then
         Instance%Type = valeur
      else
         SET_INT_LOI = 1
         MessageErreur         = "SET_INT_LOI - Unknown variable name"
      end if
   end function SET_INT_LOI


   function SET_STRING_LOI(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_LOI             ! different de 0 si erreur
      type(LOI_T),            intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_LOI = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Graph.Name') > 0) then
         Instance%Nom = valeur(1:30)
      else
         SET_STRING_LOI = 1
         MessageErreur         = "SET_STRING_LOI - Unknown variable name"
      end if
   end function SET_STRING_LOI



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_LOI(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_LOI              ! different de 0 si erreur
      type(LOI_T),            intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_LOI = 0
      MessageErreur = ""
      err           = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%Temps)) then
          taille = SIZE(Instance%Temps)
          if (taille > 0) then
              DEALLOCATE(Instance%Temps, STAT=err)
              if (err /= 0) then
                  DESALLOUE_LOI = err
                  MessageErreur = 'Unable to deallocate LOI_T.Temps'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Temps)
      if (ASSOCIATED(Instance%Cote)) then
          taille = SIZE(Instance%Cote)
          if (taille > 0) then
              DEALLOCATE(Instance%Cote, STAT=err)
              if (err /= 0) then
                  DESALLOUE_LOI = err
                  MessageErreur = 'Unable to deallocate LOI_T.Cote'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Cote)
      if (ASSOCIATED(Instance%Debit)) then
          taille = SIZE(Instance%Debit)
          if (taille > 0) then
              DEALLOCATE(Instance%Debit, STAT=err)
              if (err /= 0) then
                  DESALLOUE_LOI = err
                  MessageErreur = 'Unable to deallocate LOI_T.Debit'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Debit)
      if (ASSOCIATED(Instance%CoteSup)) then
          taille = SIZE(Instance%CoteSup)
          if (taille > 0) then
              DEALLOCATE(Instance%CoteSup, STAT=err)
              if (err /= 0) then
                  DESALLOUE_LOI = err
                  MessageErreur = 'Unable to deallocate LOI_T.CoteSup'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%CoteSup)
      if (ASSOCIATED(Instance%CoteInf)) then
          taille = SIZE(Instance%CoteInf)
          if (taille > 0) then
              DEALLOCATE(Instance%CoteInf, STAT=err)
              if (err /= 0) then
                  DESALLOUE_LOI = err
                  MessageErreur = 'Unable to deallocate LOI_T.CoteInf'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%CoteInf)
      if (ASSOCIATED(Instance%CoteAval)) then
          taille = SIZE(Instance%CoteAval)
          if (taille > 0) then
              DEALLOCATE(Instance%CoteAval, STAT=err)
              if (err /= 0) then
                  DESALLOUE_LOI = err
                  MessageErreur = 'Unable to deallocate LOI_T.CoteAval'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%CoteAval)
      if (ASSOCIATED(Instance%CoteAmont)) then
          taille = SIZE(Instance%CoteAmont, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%CoteAmont, STAT=err)
              if (err /= 0) then
                  DESALLOUE_LOI = err
                  MessageErreur = 'Unable to deallocate LOI_T.CoteAmont'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%CoteAmont)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_LOI

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_LOI(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_LOI              ! different de 0 si erreur
      type(LOI_T),            intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_LOI = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%Temps)
      NULLIFY(Instance%Cote)
      NULLIFY(Instance%Debit)
      NULLIFY(Instance%CoteSup)
      NULLIFY(Instance%CoteInf)
      NULLIFY(Instance%CoteAval)
      NULLIFY(Instance%CoteAmont)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_LOI

end module M_LOI_T
