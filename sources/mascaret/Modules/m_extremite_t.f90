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

module M_EXTREMITE_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
use M_PRECISION

  ! Constantes reperant le  type de condition a la limite

  integer     , parameter :: CONDITION_TYPE_DEBIT_IMPOSE  = 1
  integer     , parameter :: CONDITION_TYPE_COTE_IMPOSE   = 2
  integer     , parameter :: CONDITION_TYPE_COTE_DEBIT    = 3
  integer     , parameter :: CONDITION_TYPE_DEBIT_COTE    = 4
  integer     , parameter :: CONDITION_TYPE_ZAVAL_QAMONT  = 5
  integer     , parameter :: CONDITION_TYPE_SORTIE_LIBRE  = 6
  integer     , parameter :: CONDITION_TYPE_NORMALE       = 7
  integer     , parameter :: CONDITION_TYPE_COTE_DEBIT_IMPOSES = 8

  integer     , parameter :: CONDITION_TYPE_NB_MAX        = 8

TYPE EXTREMITE_T
  sequence
  character(30) :: Nom              ! Nom de l'extremite
  real(DOUBLE)  :: PenteFond        ! pente du fond a cette extremite
  integer       :: NumeroLoi        ! Numero de la loi associe a l'extremite
  integer       :: Type             ! Type de la CL (1, 2, 3, 4, 5, 6 ou 7)
  real(DOUBLE), dimension(:), pointer :: PtZ => null()    ! Points Z de la loi
  real(DOUBLE), dimension(:), pointer :: PtQ => null()    ! Points Q de la loi
END TYPE EXTREMITE_T


contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_EXTREMITE(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.Boundary.Name"
        tabDescriptionVar(i) ="Name of a boundary"
        i=i+1
        tabNomVar(i)         ="Model.Boundary.Slope"
        tabDescriptionVar(i) ="Slope of the bottom"
        i=i+1
        tabNomVar(i)         ="Model.Boundary.GraphNum"
        tabDescriptionVar(i) ="Graph number"
        i=i+1
        tabNomVar(i)         ="Model.Boundary.Type"
        tabDescriptionVar(i) ="Type of the boundary (flow or stage hydrograph, rating curve, open ,etc.)"
        i=i+1
        tabNomVar(i)         ="Model.Boundary.PtZ"
        tabDescriptionVar(i) ="Z points of the graph (level (m))"
        i=i+1
        tabNomVar(i)         ="Model.Boundary.PtQ"
        tabDescriptionVar(i) ="Q points of the graph (discharge (m3/s))"
        i=i+1
      return

    end subroutine GET_TAB_VAR_EXTREMITE

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_EXTREMITE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_EXTREMITE    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_EXTREMITE = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""


       if ( index(NomVar, 'Model.Boundary.Name') > 0) then
          TypeVar = 'STRING'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Boundary.Slope') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Boundary.GraphNum') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Boundary.Type') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Boundary.PtZ') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Boundary.PtQ') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
      else
        GET_TYPE_VAR_EXTREMITE = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_EXTREMITE - Unknown variable name"
      end if


    end function GET_TYPE_VAR_EXTREMITE

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_EXTREMITE(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_EXTREMITE       ! different de 0 si erreur
      type(EXTREMITE_T),      intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_EXTREMITE = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Boundary.Name') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Boundary.Slope') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Boundary.GraphNum') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Boundary.Type') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Boundary.PtZ') > 0) then
         if (ASSOCIATED(Instance%PtZ)) then
            taille1 = size(Instance%PtZ)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if (index(NomVar, 'Model.Boundary.PtQ') > 0) then
         if (ASSOCIATED(Instance%PtQ)) then
            taille1 = size(Instance%PtQ)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_EXTREMITE = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_EXTREMITE - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_EXTREMITE

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_EXTREMITE(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_EXTREMITE       ! different de 0 si erreur
      type(EXTREMITE_T),      intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_EXTREMITE = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.Boundary.PtZ') > 0) then
        if (ASSOCIATED(Instance%PtZ)) then
           t1 = size(Instance%PtZ)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%PtZ, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_EXTREMITE = err
                 MessageErreur = 'SET_TAILLE_VAR_EXTREMITE : Unable to deallocate EXTREMITE_T.PtZ'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PtZ) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%PtZ(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_EXTREMITE = err
              MessageErreur = 'SET_TAILLE_VAR_EXTREMITE : Unable to allocate EXTREMITE_T.PtZ'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Boundary.PtQ') > 0) then
        if (ASSOCIATED(Instance%PtQ)) then
           t1 = size(Instance%PtQ)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%PtQ, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_EXTREMITE = err
                 MessageErreur = 'SET_TAILLE_VAR_EXTREMITE : Unable to deallocate EXTREMITE_T.PtQ'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PtQ) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%PtQ(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_EXTREMITE = err
              MessageErreur = 'SET_TAILLE_VAR_EXTREMITE : Unable to allocate EXTREMITE_T.PtQ'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_EXTREMITE = 1
         MessageErreur         = "SET_TAILLE_VAR_EXTREMITE - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_EXTREMITE

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_EXTREMITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_EXTREMITE       ! different de 0 si erreur
      type(EXTREMITE_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_EXTREMITE = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Boundary.Slope') > 0) then
         valeur = Instance%PenteFond
      else if ( index(NomVar, 'Model.Boundary.PtZ') > 0) then
         valeur = Instance%PtZ(index1)
      else if ( index(NomVar, 'Model.Boundary.PtQ') > 0) then
         valeur = Instance%PtQ(index1)
      else
         GET_DOUBLE_EXTREMITE = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_EXTREMITE - Unknown variable name"
      end if
   end function GET_DOUBLE_EXTREMITE


   function GET_INT_EXTREMITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_EXTREMITE          ! different de 0 si erreur
      type(EXTREMITE_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_EXTREMITE = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Boundary.GraphNum') > 0) then
         valeur = Instance%NumeroLoi
      else if ( index(NomVar, 'Model.Boundary.Type') > 0) then
         valeur = Instance%Type
      else
         GET_INT_EXTREMITE = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_EXTREMITE - Unknown variable name"
      end if
   end function GET_INT_EXTREMITE


   function GET_STRING_EXTREMITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_EXTREMITE       ! different de 0 si erreur
      type(EXTREMITE_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_EXTREMITE = 0
      valeur                = ""
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Boundary.Name') > 0) then
         valeur = Instance%Nom
      else
         GET_STRING_EXTREMITE = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_EXTREMITE - Unknown variable name"
      end if
   end function GET_STRING_EXTREMITE



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_EXTREMITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_EXTREMITE       ! different de 0 si erreur
      type(EXTREMITE_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_EXTREMITE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Boundary.Slope') > 0) then
         Instance%PenteFond = valeur
      else if ( index(NomVar, 'Model.Boundary.PtZ') > 0) then
         Instance%PtZ(index1) = valeur
      else if ( index(NomVar, 'Model.Boundary.PtQ') > 0) then
         Instance%PtQ(index1) = valeur
      else
         SET_DOUBLE_EXTREMITE = 1
         MessageErreur         = "SET_DOUBLE_EXTREMITE - Unknown variable name"
      end if
   end function SET_DOUBLE_EXTREMITE


   function SET_INT_EXTREMITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_EXTREMITE          ! different de 0 si erreur
      type(EXTREMITE_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_EXTREMITE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Boundary.GraphNum') > 0) then
         Instance%NumeroLoi = valeur
      else if ( index(NomVar, 'Model.Boundary.Type') > 0) then
         Instance%Type = valeur
      else
         SET_INT_EXTREMITE = 1
         MessageErreur         = "SET_INT_EXTREMITE - Unknown variable name"
      end if
   end function SET_INT_EXTREMITE


   function SET_STRING_EXTREMITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_EXTREMITE       ! different de 0 si erreur
      type(EXTREMITE_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_EXTREMITE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Boundary.Name') > 0) then
         Instance%Nom = valeur(1:30)
      else
         SET_STRING_EXTREMITE = 1
         MessageErreur         = "SET_STRING_EXTREMITE - Unknown variable name"
      end if
   end function SET_STRING_EXTREMITE



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_EXTREMITE(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_EXTREMITE        ! different de 0 si erreur
      type(EXTREMITE_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_EXTREMITE = 0
      MessageErreur       = ""
      err                 = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%PtZ)) then
          taille = SIZE(Instance%PtZ)
          if (taille > 0) then
              DEALLOCATE(Instance%PtZ, STAT=err)
              if (err /= 0) then
                  DESALLOUE_EXTREMITE = err
                  MessageErreur = 'Unable to deallocate EXTREMITE_T.PtZ'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PtZ)
      if (ASSOCIATED(Instance%PtQ)) then
          taille = SIZE(Instance%PtQ)
          if (taille > 0) then
              DEALLOCATE(Instance%PtQ, STAT=err)
              if (err /= 0) then
                  DESALLOUE_EXTREMITE = err
                  MessageErreur = 'Unable to deallocate EXTREMITE_T.PtQ'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PtQ)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_EXTREMITE

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_EXTREMITE(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_EXTREMITE        ! different de 0 si erreur
      type(EXTREMITE_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_EXTREMITE = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%PtZ)
      NULLIFY(Instance%PtQ)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_EXTREMITE

end module M_EXTREMITE_T
