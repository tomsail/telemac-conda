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

module M_CONFLUENT_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

!=========================== Declarations ==============================

use M_PRECISION

Type CONFLUENT_T
  sequence
  character(30)                        :: Nom            ! Nom du confluent
  integer                              :: NbAffluent     ! Nombre d'affluents du confluent
  real(DOUBLE), dimension(:), pointer  :: AbscisseAfflu  => null() ! Abscisse des affluents
  real(DOUBLE), dimension(:), pointer  :: OrdonneeAfflu  => null() ! Ordonnee des affluents
  real(DOUBLE), dimension(:), pointer  :: AngleAfflu     => null() ! Angle des affluents
  integer     , dimension(:), pointer  :: Isec           => null() ! Indice de section au niveau de  l'affluent
  integer     , dimension(:), pointer  :: Isecvo         => null() ! Indice de section au niveau de  l'affluent
  integer     , dimension(:), pointer  :: Finbie         => null() ! Fin du bief
end type CONFLUENT_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_CONFLUENT(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.Junction.Name"
        tabDescriptionVar(i) ="Name of the junction"
        i=i+1
        tabNomVar(i)         ="Model.Connect.ReachNum"
        tabDescriptionVar(i) ="The number of reaches"
        i=i+1
        tabNomVar(i)         ="Model.Junction.Abscissa"
        tabDescriptionVar(i) ="Abscissa of the reaches"
        i=i+1
        tabNomVar(i)         ="Model.Junction.Ordinate"
        tabDescriptionVar(i) ="Ordinate of the reaches"
        i=i+1
        tabNomVar(i)         ="Model.Junction.Angle"
        tabDescriptionVar(i) ="Angle of the reaches"
        i=i+1
        tabNomVar(i)         ="Model.Junction.Isec"
        tabDescriptionVar(i) ="Node number for the reaches"
        i=i+1
        tabNomVar(i)         ="Model.Junction.Isecvo"
        tabDescriptionVar(i) ="Node number for the reaches"
        i=i+1
        tabNomVar(i)         ="Model.Junction.EndReach"
        tabDescriptionVar(i) ="End of the reach"
        i=i+1

      return

    end subroutine GET_TAB_VAR_CONFLUENT

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_CONFLUENT(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_CONFLUENT    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_CONFLUENT = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""

       if ( index(NomVar, 'Model.Junction.Name') > 0) then
          TypeVar = 'STRING'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Connect.ReachNum') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Junction.Abscissa') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Junction.Ordinate') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Junction.Angle') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Junction.Isecvo') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Junction.Isec') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Junction.EndReach') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
      else
        GET_TYPE_VAR_CONFLUENT = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_CONFLUENT - Unknown variable name"
      end if


    end function GET_TYPE_VAR_CONFLUENT

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_CONFLUENT(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_CONFLUENT       ! different de 0 si erreur
      type(CONFLUENT_T),      intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_CONFLUENT = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Junction.Name') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Connect.ReachNum') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Junction.Abscissa') > 0) then
         if (ASSOCIATED(Instance%AbscisseAfflu)) then
            taille1 = size(Instance%AbscisseAfflu)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Junction.Ordinate') > 0) then
         if (ASSOCIATED(Instance%OrdonneeAfflu)) then
            taille1 = size(Instance%OrdonneeAfflu)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Junction.Angle') > 0) then
         if (ASSOCIATED(Instance%AngleAfflu)) then
            taille1 = size(Instance%AngleAfflu)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Junction.Isecvo') > 0) then
         if (ASSOCIATED(Instance%Isecvo)) then
            taille1 = size(Instance%Isecvo)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Junction.Isec') > 0) then
         if (ASSOCIATED(Instance%Isec)) then
            taille1 = size(Instance%Isec)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Junction.EndReach') > 0) then
         if (ASSOCIATED(Instance%Finbie)) then
            taille1 = size(Instance%Finbie)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_CONFLUENT = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_CONFLUENT - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_CONFLUENT

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_CONFLUENT(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_CONFLUENT       ! different de 0 si erreur
      type(CONFLUENT_T),      intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_CONFLUENT = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.Junction.Abscissa') > 0) then
        if (ASSOCIATED(Instance%AbscisseAfflu)) then
           t1 = size(Instance%AbscisseAfflu)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%AbscisseAfflu, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONFLUENT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to deallocate CONFLUENT_T.AbscisseAfflu'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%AbscisseAfflu) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%AbscisseAfflu(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONFLUENT = err
              MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to allocate CONFLUENT_T.AbscisseAfflu'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Junction.Ordinate') > 0) then
        if (ASSOCIATED(Instance%OrdonneeAfflu)) then
           t1 = size(Instance%OrdonneeAfflu)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%OrdonneeAfflu, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONFLUENT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to deallocate CONFLUENT_T.OrdonneeAfflu'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%OrdonneeAfflu) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%OrdonneeAfflu(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONFLUENT = err
              MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to allocate CONFLUENT_T.OrdonneeAfflu'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Junction.Angle') > 0) then
        if (ASSOCIATED(Instance%AngleAfflu)) then
           t1 = size(Instance%AngleAfflu)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%AngleAfflu, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONFLUENT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to deallocate CONFLUENT_T.AngleAfflu'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%AngleAfflu) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%AngleAfflu(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONFLUENT = err
              MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to allocate CONFLUENT_T.AngleAfflu'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Junction.Isecvo') > 0) then
        if (ASSOCIATED(Instance%Isecvo)) then
           t1 = size(Instance%Isecvo)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Isecvo, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONFLUENT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to deallocate CONFLUENT_T.Isecvo'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Isecvo) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Isecvo(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONFLUENT = err
              MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to allocate CONFLUENT_T.Isecvo'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Junction.Isec') > 0) then
        if (ASSOCIATED(Instance%Isec)) then
           t1 = size(Instance%Isec)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Isec, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONFLUENT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to deallocate CONFLUENT_T.Isec'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Isec) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Isec(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONFLUENT = err
              MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to allocate CONFLUENT_T.Isec'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Junction.EndReach') > 0) then
        if (ASSOCIATED(Instance%Finbie)) then
           t1 = size(Instance%Finbie)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Finbie, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_CONFLUENT = err
                 MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to deallocate CONFLUENT_T.Finbie'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Finbie) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Finbie(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_CONFLUENT = err
              MessageErreur = 'SET_TAILLE_VAR_CONFLUENT : Unable to allocate CONFLUENT_T.Finbie'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_CONFLUENT = 1
         MessageErreur         = "SET_TAILLE_VAR_CONFLUENT - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_CONFLUENT

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_CONFLUENT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_CONFLUENT       ! different de 0 si erreur
      type(CONFLUENT_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_CONFLUENT = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Junction.Abscissa') > 0) then
         valeur = Instance%AbscisseAfflu(index1)
      else if ( index(NomVar, 'Model.Junction.Ordinate') > 0) then
         valeur = Instance%OrdonneeAfflu(index1)
      else if ( index(NomVar, 'Model.Junction.Angle') > 0) then
         valeur = Instance%AngleAfflu(index1)
      else
         GET_DOUBLE_CONFLUENT = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_CONFLUENT - Unknown variable name"
      end if
   end function GET_DOUBLE_CONFLUENT


   function GET_INT_CONFLUENT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_CONFLUENT          ! different de 0 si erreur
      type(CONFLUENT_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_CONFLUENT = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Connect.ReachNum') > 0) then
         valeur = Instance%NbAffluent
      else if ( index(NomVar, 'Model.Junction.Isecvo') > 0) then
         valeur = Instance%Isecvo(index1)
      else if ( index(NomVar, 'Model.Junction.Isec') > 0) then
         valeur = Instance%Isec(index1)
      else if ( index(NomVar, 'Model.Junction.EndReach') > 0) then
         valeur = Instance%Finbie(index1)
      else
         GET_INT_CONFLUENT = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_CONFLUENT - Unknown variable name"
      end if
   end function GET_INT_CONFLUENT


   function GET_STRING_CONFLUENT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_CONFLUENT       ! different de 0 si erreur
      type(CONFLUENT_T),      intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_CONFLUENT = 0
      valeur                = ""
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Junction.Name') > 0) then
         valeur = Instance%Nom
      else
         GET_STRING_CONFLUENT = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_CONFLUENT - Unknown variable name"
      end if
   end function GET_STRING_CONFLUENT



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_CONFLUENT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_CONFLUENT       ! different de 0 si erreur
      type(CONFLUENT_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_CONFLUENT = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Junction.Abscissa') > 0) then
         Instance%AbscisseAfflu(index1) = valeur
      else if ( index(NomVar, 'Model.Junction.Ordinate') > 0) then
         Instance%OrdonneeAfflu(index1) = valeur
      else if ( index(NomVar, 'Model.Junction.Angle') > 0) then
         Instance%AngleAfflu(index1) = valeur
      else
         SET_DOUBLE_CONFLUENT = 1
         MessageErreur         = "SET_DOUBLE_CONFLUENT - Unknown variable name"
      end if
   end function SET_DOUBLE_CONFLUENT


   function SET_INT_CONFLUENT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_CONFLUENT          ! different de 0 si erreur
      type(CONFLUENT_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_CONFLUENT = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Connect.ReachNum') > 0) then
         Instance%NbAffluent = valeur
      else if ( index(NomVar, 'Model.Junction.Isecvo') > 0) then
         Instance%Isecvo(index1) = valeur
      else if ( index(NomVar, 'Model.Junction.Isec') > 0) then
         Instance%Isec(index1) = valeur
      else if ( index(NomVar, 'Model.Junction.EndReach') > 0) then
         Instance%Finbie(index1) = valeur
      else
         SET_INT_CONFLUENT = 1
         MessageErreur         = "SET_INT_CONFLUENT - Unknown variable name"
      end if
   end function SET_INT_CONFLUENT


   function SET_STRING_CONFLUENT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_CONFLUENT       ! different de 0 si erreur
      type(CONFLUENT_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_CONFLUENT = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Junction.Name') > 0) then
         Instance%Nom = valeur(1:30)
      else
         SET_STRING_CONFLUENT = 1
         MessageErreur         = "SET_STRING_CONFLUENT - Unknown variable name"
      end if
   end function SET_STRING_CONFLUENT



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_CONFLUENT(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_CONFLUENT        ! different de 0 si erreur
      type(CONFLUENT_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_CONFLUENT = 0
      MessageErreur       = ""
      err                 = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%AbscisseAfflu)) then
          taille = SIZE(Instance%AbscisseAfflu)
          if (taille > 0) then
              DEALLOCATE(Instance%AbscisseAfflu, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONFLUENT = err
                  MessageErreur = 'Unable to deallocate CONFLUENT_T.AbscisseAfflu'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%AbscisseAfflu)
      if (ASSOCIATED(Instance%OrdonneeAfflu)) then
          taille = SIZE(Instance%OrdonneeAfflu)
          if (taille > 0) then
              DEALLOCATE(Instance%OrdonneeAfflu, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONFLUENT = err
                  MessageErreur = 'Unable to deallocate CONFLUENT_T.OrdonneeAfflu'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%OrdonneeAfflu)
      if (ASSOCIATED(Instance%AngleAfflu)) then
          taille = SIZE(Instance%AngleAfflu)
          if (taille > 0) then
              DEALLOCATE(Instance%AngleAfflu, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONFLUENT = err
                  MessageErreur = 'Unable to deallocate CONFLUENT_T.AngleAfflu'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%AngleAfflu)
      if (ASSOCIATED(Instance%Isec)) then
          taille = SIZE(Instance%Isec)
          if (taille > 0) then
              DEALLOCATE(Instance%Isec, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONFLUENT = err
                  MessageErreur = 'Unable to deallocate CONFLUENT_T.Isec'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Isec)
      if (ASSOCIATED(Instance%Isecvo)) then
          taille = SIZE(Instance%Isecvo)
          if (taille > 0) then
              DEALLOCATE(Instance%Isecvo, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONFLUENT = err
                  MessageErreur = 'Unable to deallocate CONFLUENT_T.Isecvo'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Isecvo)
      if (ASSOCIATED(Instance%Finbie)) then
          taille = SIZE(Instance%Finbie)
          if (taille > 0) then
              DEALLOCATE(Instance%Finbie, STAT=err)
              if (err /= 0) then
                  DESALLOUE_CONFLUENT = err
                  MessageErreur = 'Unable to deallocate CONFLUENT_T.Finbie'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Finbie)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_CONFLUENT

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_CONFLUENT(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_CONFLUENT        ! different de 0 si erreur
      type(CONFLUENT_T),      intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_CONFLUENT = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%AbscisseAfflu)
      NULLIFY(Instance%OrdonneeAfflu)
      NULLIFY(Instance%AngleAfflu)
      NULLIFY(Instance%Isec)
      NULLIFY(Instance%Isecvo)
      NULLIFY(Instance%Finbie)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_CONFLUENT

end module M_CONFLUENT_T
