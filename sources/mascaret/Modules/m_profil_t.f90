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

module M_PROFIL_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
  use M_PRECISION

  TYPE PROFIL_T
     sequence
     character(30)               :: Nom        ! Nom du profil
     real(DOUBLE)                :: AbsRel     ! Abscisse relative du profil
     real(DOUBLE)                :: AbsAbs     ! Abscisse absolue du profil
     integer                     :: NumBief    ! Numero de la branche
     character(30)               :: NomBief    ! Nom de la branche
     integer                     :: NbPas      ! Nombre de pas de planimetrage
     real(DOUBLE)                :: Pas        ! Valeur du pas de planimetrage

     real(DOUBLE)                :: Zref       ! Point bas du profil
     real(DOUBLE), dimension(2)  :: ZRive      ! Cote rive du profil

     real(DOUBLE), dimension(:), pointer  :: X   => null()       ! Point X
     real(DOUBLE), dimension(:), pointer  :: Y   => null()       ! Point Y
     integer, dimension(2)                :: LimiteMin ! Limites lit mineur du profil
     integer, dimension(2)                :: LimiteMaj ! Limites lit majeur du profil

     real(DOUBLE)  :: CoeffFrottMin          ! Frottement mineur
     real(DOUBLE)  :: CoeffFrottMaj          ! Frottement majeur

  END TYPE PROFIL_T


contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_PROFIL(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.CrossSection.Name"
        tabDescriptionVar(i) ="Name of the cross section"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.RelAbs"
        tabDescriptionVar(i) ="Relative abscissa of the cross section"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.AbsAbs"
        tabDescriptionVar(i) ="Absolute abscissa of the cross section"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.ReachNum"
        tabDescriptionVar(i) ="Reach number of the cross section"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.ReachName"
        tabDescriptionVar(i) ="Name of the reach"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.NumStep"
        tabDescriptionVar(i) ="Number of steps of the vertical discretisation"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.Step"
        tabDescriptionVar(i) ="Step value for the vertical discretisation"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.Zbot"
        tabDescriptionVar(i) ="Bottom level of the cross section"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.Zbank"
        tabDescriptionVar(i) ="Bank level of the cross section"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.X"
        tabDescriptionVar(i) ="X points"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.Y"
        tabDescriptionVar(i) ="Y points"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.Bound1"
        tabDescriptionVar(i) ="Boundary of the main channel"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.Bound2"
        tabDescriptionVar(i) ="Boundary of the floodplain"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.FricCoef1"
        tabDescriptionVar(i) ="Friction coefficient of the main channel"
        i=i+1
        tabNomVar(i)         ="Model.CrossSection.FricCoef2"
        tabDescriptionVar(i) ="Friction coefficient of the floodplain"
        i=i+1
      return

    end subroutine GET_TAB_VAR_PROFIL

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_PROFIL(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_PROFIL      ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_PROFIL = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""

       if ( index(NomVar, 'Model.CrossSection.Name') > 0) then
          TypeVar = 'STRING'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CrossSection.RelAbs') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CrossSection.AbsAbs') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CrossSection.ReachNum') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CrossSection.ReachName') > 0) then
         TypeVar = 'STRING'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CrossSection.NumStep') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CrossSection.Step') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CrossSection.Zbot') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CrossSection.Zbank') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.CrossSection.X') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.CrossSection.Y') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.CrossSection.Bound1') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.CrossSection.Bound2') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.CrossSection.FricCoef1') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CrossSection.FricCoef2') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else
        GET_TYPE_VAR_PROFIL = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_PROFIL - Unknown variable name"
      end if


    end function GET_TYPE_VAR_PROFIL

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_PROFIL(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_PROFIL          ! different de 0 si erreur
      type(PROFIL_T),         intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_PROFIL = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.CrossSection.Name') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.RelAbs') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.AbsAbs') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.ReachNum') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.ReachName') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.NumStep') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.Step') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.Zbot') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.Zbank') > 0) then
         taille1 = size(Instance%ZRive)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.X') > 0) then
         if (ASSOCIATED(Instance%X)) then
            taille1 = size(Instance%X)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.Y') > 0) then
         if (ASSOCIATED(Instance%Y)) then
            taille1 = size(Instance%Y)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.Bound1') > 0) then
         taille1 = size(Instance%LimiteMin)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.Bound2') > 0) then
         taille1 = size(Instance%LimiteMaj)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.FricCoef1') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CrossSection.FricCoef2') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_PROFIL = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_PROFIL - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_PROFIL

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_PROFIL(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_PROFIL          ! different de 0 si erreur
      type(PROFIL_T),         intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_PROFIL = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.CrossSection.X') > 0) then
        if (ASSOCIATED(Instance%X)) then
           t1 = size(Instance%X)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%X, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL : Unable to deallocate PROFIL_T.X'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%X) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%X(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL : Unable to allocate PROFIL_T.X'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.CrossSection.Y') > 0) then
        if (ASSOCIATED(Instance%Y)) then
           t1 = size(Instance%Y)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Y, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL : Unable to deallocate PROFIL_T.Y'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Y) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Y(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL : Unable to allocate PROFIL_T.Y'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_PROFIL = 1
         MessageErreur         = "SET_TAILLE_VAR_PROFIL - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_PROFIL

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_PROFIL(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_PROFIL          ! different de 0 si erreur
      type(PROFIL_T),         intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_PROFIL = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.CrossSection.RelAbs') > 0) then
         valeur = Instance%AbsRel
      else if ( index(NomVar, 'Model.CrossSection.AbsAbs') > 0) then
         valeur = Instance%AbsAbs
      else if ( index(NomVar, 'Model.CrossSection.Step') > 0) then
         valeur = Instance%Pas
      else if ( index(NomVar, 'Model.CrossSection.Zbot') > 0) then
         valeur = Instance%Zref
      else if ( index(NomVar, 'Model.CrossSection.Zbank') > 0) then
         valeur = Instance%ZRive(index1)
      else if ( index(NomVar, 'Model.CrossSection.X') > 0) then
         valeur = Instance%X(index1)
      else if ( index(NomVar, 'Model.CrossSection.Y') > 0) then
         valeur = Instance%Y(index1)
      else if ( index(NomVar, 'Model.CrossSection.FricCoef1') > 0) then
         valeur = Instance%CoeffFrottMin
      else if ( index(NomVar, 'Model.CrossSection.FricCoef2') > 0) then
         valeur = Instance%CoeffFrottMaj
      else
         GET_DOUBLE_PROFIL = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_PROFIL - Unknown variable name"
      end if
   end function GET_DOUBLE_PROFIL


   function GET_INT_PROFIL(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_PROFIL             ! different de 0 si erreur
      type(PROFIL_T),         intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_PROFIL = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.CrossSection.ReachNum') > 0) then
         valeur = Instance%NumBief
      else if ( index(NomVar, 'Model.CrossSection.NumStep') > 0) then
         valeur = Instance%NbPas
      else if ( index(NomVar, 'Model.CrossSection.Bound1') > 0) then
         valeur = Instance%LimiteMin(index1)
      else if ( index(NomVar, 'Model.CrossSection.Bound2') > 0) then
         valeur = Instance%LimiteMaj(index1)
      else
         GET_INT_PROFIL = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_PROFIL - Unknown variable name"
      end if
   end function GET_INT_PROFIL


   function GET_STRING_PROFIL(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_PROFIL          ! different de 0 si erreur
      type(PROFIL_T),         intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_PROFIL = 0
      valeur                = ""
      MessageErreur          = ""

      if ( index(NomVar, 'Model.CrossSection.Name') > 0) then
         valeur = Instance%Nom
      else if ( index(NomVar, 'Model.CrossSection.ReachName') > 0) then
         valeur = Instance%NomBief
      else
         GET_STRING_PROFIL = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_PROFIL - Unknown variable name"
      end if
   end function GET_STRING_PROFIL



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_PROFIL(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_PROFIL          ! different de 0 si erreur
      type(PROFIL_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_PROFIL = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.CrossSection.RelAbs') > 0) then
         Instance%AbsRel = valeur
      else if ( index(NomVar, 'Model.CrossSection.AbsAbs') > 0) then
         Instance%AbsAbs = valeur
      else if ( index(NomVar, 'Model.CrossSection.Step') > 0) then
         Instance%Pas = valeur
      else if ( index(NomVar, 'Model.CrossSection.Zbot') > 0) then
         Instance%Zref = valeur
      else if ( index(NomVar, 'Model.CrossSection.Zbank') > 0) then
         Instance%ZRive(index1) = valeur
      else if ( index(NomVar, 'Model.CrossSection.X') > 0) then
         Instance%X(index1) = valeur
      else if ( index(NomVar, 'Model.CrossSection.Y') > 0) then
         Instance%Y(index1) = valeur
      else if ( index(NomVar, 'Model.CrossSection.FricCoef1') > 0) then
         Instance%CoeffFrottMin = valeur
      else if ( index(NomVar, 'Model.CrossSection.FricCoef2') > 0) then
         Instance%CoeffFrottMaj = valeur
      else
         SET_DOUBLE_PROFIL = 1
         MessageErreur         = "SET_DOUBLE_PROFIL - Unknown variable name"
      end if
   end function SET_DOUBLE_PROFIL


   function SET_INT_PROFIL(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_PROFIL             ! different de 0 si erreur
      type(PROFIL_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_PROFIL = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.CrossSection.ReachNum') > 0) then
         Instance%NumBief = valeur
      else if ( index(NomVar, 'Model.CrossSection.NumStep') > 0) then
         Instance%NbPas = valeur
      else if ( index(NomVar, 'Model.CrossSection.Bound1') > 0) then
         Instance%LimiteMin(index1) = valeur
      else if ( index(NomVar, 'Model.CrossSection.Bound2') > 0) then
         Instance%LimiteMaj(index1) = valeur
      else
         SET_INT_PROFIL = 1
         MessageErreur         = "SET_INT_PROFIL - Unknown variable name"
      end if
   end function SET_INT_PROFIL


   function SET_STRING_PROFIL(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_PROFIL          ! different de 0 si erreur
      type(PROFIL_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_PROFIL = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.CrossSection.Name') > 0) then
         Instance%Nom = valeur(1:255)
      else if ( index(NomVar, 'Model.CrossSection.ReachName') > 0) then
         Instance%NomBief = valeur(1:255)
      else
         SET_STRING_PROFIL = 1
         MessageErreur         = "SET_STRING_PROFIL - Unknown variable name"
      end if
   end function SET_STRING_PROFIL



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_PROFIL(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_PROFIL           ! different de 0 si erreur
      type(PROFIL_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_PROFIL = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%X)) then
          taille = SIZE(Instance%X)
          if (taille > 0) then
              DEALLOCATE(Instance%X, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL = err
                  MessageErreur = 'Unable to deallocate PROFIL_T.X'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%X)
      if (ASSOCIATED(Instance%Y)) then
          taille = SIZE(Instance%Y)
          if (taille > 0) then
              DEALLOCATE(Instance%Y, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL = err
                  MessageErreur = 'Unable to deallocate PROFIL_T.Y'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Y)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_PROFIL

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_PROFIL(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_PROFIL           ! different de 0 si erreur
      type(PROFIL_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_PROFIL = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%X)
      NULLIFY(Instance%Y)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_PROFIL

end module M_PROFIL_T
