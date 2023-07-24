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

module M_METEO_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION
   ! Constantes reperant le  type de "loi"
   !--------------------------------------
   integer     , parameter :: METEO_UNITE_SECONDE = 1 !  Seconde "S"
   integer     , parameter :: METEO_UNITE_MINUTE  = 2 !  Minute  "M"
   integer     , parameter :: METEO_UNITE_HEURE   = 3 !  Heure   "H"
   integer     , parameter :: METEO_UNITE_JOUR    = 4 !  Jour    "J"
   integer     , parameter :: METEO_UNITE_NB_MAX  = 4

   type METEO_T
      sequence
      character(30)                         :: Nom
      real(DOUBLE), dimension(:)  , pointer :: Temps => null()
      ! parametres meteo pour Eutro et Biomass
      real(DOUBLE), dimension(:)  , pointer :: Temp => null()
      real(DOUBLE), dimension(:)  , pointer :: I0 => null()
      ! parametres meteo pour Thermic
      real(DOUBLE), dimension(:)  , pointer :: T_Air => null()
      real(DOUBLE), dimension(:)  , pointer :: P_Vap => null()
      real(DOUBLE), dimension(:)  , pointer :: Vit_Vent => null()
      real(DOUBLE), dimension(:)  , pointer :: Nebulo => null()
      real(DOUBLE), dimension(:)  , pointer :: Ray3 => null()
      real(DOUBLE), dimension(:)  , pointer :: P_atm => null()
   end type METEO_T

contains

   ! Retourne les noms des champs du type ainsi qu'une description
   subroutine GET_TAB_VAR_METEO(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="Model.Tracer.Weather.Name"
        tabDescriptionVar(i) ="Names of the variables for the weather"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Weather.Time"
        tabDescriptionVar(i) ="Time instance of weather variables"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Weather.Temperature"
        tabDescriptionVar(i) ="Temperature"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Weather.I0"
        tabDescriptionVar(i) ="I0"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Weather.AirTemp"
        tabDescriptionVar(i) ="Air temperature"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Weather.AirPress"
        tabDescriptionVar(i) ="Vapour tension"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Weather.WindSpeed"
        tabDescriptionVar(i) ="Wind speed"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Weather.Nebulosity"
        tabDescriptionVar(i) ="Nebulosity"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Weather.Radiation"
        tabDescriptionVar(i) ="Radiation"
        i=i+1
        tabNomVar(i)         ="Model.Tracer.Weather.AtmPress"
        tabDescriptionVar(i) ="Atmospheric pressure"
        i=i+1

        return

     end subroutine GET_TAB_VAR_METEO

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_METEO(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_METEO ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_TRACER sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_METEO = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'Model.Tracer.Weather.Name') > 0) then
         TypeVar = 'STRING'
         dimVar                = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.Time') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Weather.Temperature') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Weather.I0') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Weather.AirTemp') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Weather.AirPress') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Weather.WindSpeed') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Weather.Nebulosity') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Weather.Radiation') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'Model.Tracer.Weather.AtmPress') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else
         GET_TYPE_VAR_METEO = 1
         TypeVar = "?"
         Categorie             = "MODEL"
         Modifiable            = .false.
         dimVar                = -1
         MessageErreur         = "GET_TYPE_VAR_METEO - Unknown variable name"
      end if

    end function GET_TYPE_VAR_METEO

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_METEO(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_METEO     ! different de 0 si erreur
      type(METEO_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du source
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur
      GET_TAILLE_VAR_METEO = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Weather.Name') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.Time') > 0) then
         taille1 = size(Instance%Temps)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.Temperature') > 0) then
         taille1 = size(Instance%Temp)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.I0') > 0) then
         taille1 = size(Instance%I0)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.AirTemp') > 0) then
         taille1 = size(Instance%T_Air)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.AirPress') > 0) then
         taille1 = size(Instance%P_Vap)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.WindSpeed') > 0) then
         taille1 = size(Instance%Vit_Vent)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.Nebulosity') > 0) then
         taille1 = size(Instance%Nebulo)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.Radiation') > 0) then
         taille1 = size(Instance%Ray3)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Tracer.Weather.AtmPress') > 0) then
         taille1 = size(Instance%P_atm)
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_METEO = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_METEO - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_METEO

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................
   function SET_TAILLE_VAR_METEO(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_METEO     ! different de 0 si erreur
      type(METEO_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, err

      SET_TAILLE_VAR_METEO = 0
      MessageErreur          = ""
      !------------------------------------------------------------------------------
      ! Fin des appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type primitif
      !------------------------------------------------------------------------------
      if ( index(NomVar, 'Model.Tracer.Weather.Time') > 0) then
        if (ASSOCIATED(Instance%Temps)) then
           t1 = size(Instance%Temps)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Temps, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_METEO = err
                 MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to deallocate MODEL_METEO_T.Time'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Temps) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Temps(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_METEO = err
              MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to allocate MODEL_METEO_T.Time'
              return
           endif
        endif
     else if ( index(NomVar, 'Model.Tracer.Weather.Temperature') > 0) then
        if (ASSOCIATED(Instance%Temp)) then
           t1 = size(Instance%Temp)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Temp, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_METEO = err
                 MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to deallocate MODEL_METEO_T.Temperature'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Temp) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Temp(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_METEO = err
              MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to allocate MODEL_METEO_T.Temperature'
              return
           endif
        endif
     else if ( index(NomVar, 'Model.Tracer.Weather.I0') > 0) then
        if (ASSOCIATED(Instance%I0)) then
           t1 = size(Instance%I0)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%I0, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_METEO = err
                 MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to deallocate MODEL_METEO_T.I0'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%I0) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%I0(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_METEO = err
              MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to allocate MODEL_METEO_T.I0'
              return
           endif
        endif
     else if ( index(NomVar, 'Model.Tracer.Weather.AirTemp') > 0) then
        if (ASSOCIATED(Instance%T_Air)) then
           t1 = size(Instance%T_Air)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%T_Air, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_METEO = err
                 MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to deallocate MODEL_METEO_T.AirTemp'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%T_Air) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%T_Air(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_METEO = err
              MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to allocate MODEL_METEO_T.AirTemp'
              return
           endif
        endif
     else if ( index(NomVar, 'Model.Tracer.Weather.AirPress') > 0) then
        if (ASSOCIATED(Instance%P_Vap)) then
           t1 = size(Instance%P_Vap)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%P_Vap, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_METEO = err
                 MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to deallocate MODEL_METEO_T.AirPress'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%P_Vap) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%P_Vap(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_METEO = err
              MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to allocate MODEL_METEO_T.AirPress'
              return
           endif
        endif
     else if ( index(NomVar, 'Model.Tracer.Weather.WindSpeed') > 0) then
        if (ASSOCIATED(Instance%Vit_Vent)) then
           t1 = size(Instance%Vit_Vent)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Vit_Vent, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_METEO = err
                 MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to deallocate MODEL_METEO_T.WindSpeed'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Vit_Vent) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Vit_Vent(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_METEO = err
              MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to allocate MODEL_METEO_T.WindSpeed'
              return
           endif
        endif
     else if ( index(NomVar, 'Model.Tracer.Weather.Nebulosity') > 0) then
        if (ASSOCIATED(Instance%Nebulo)) then
           t1 = size(Instance%Nebulo)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Nebulo, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_METEO = err
                 MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to deallocate MODEL_METEO_T.Nebulosity'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Nebulo) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Nebulo(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_METEO = err
              MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to allocate MODEL_METEO_T.Nebulosity'
              return
           endif
        endif
     else if ( index(NomVar, 'Model.Tracer.Weather.Radiation') > 0) then
        if (ASSOCIATED(Instance%Ray3)) then
           t1 = size(Instance%Ray3)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Ray3, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_METEO = err
                 MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to deallocate MODEL_METEO_T.Radiation'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Ray3) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Ray3(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_METEO = err
              MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to allocate MODEL_METEO_T.Radiation'
              return
           endif
        endif
     else if ( index(NomVar, 'Model.Tracer.Weather.AtmPress') > 0) then
        if (ASSOCIATED(Instance%P_atm)) then
           t1 = size(Instance%P_atm)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%P_atm, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_METEO = err
                 MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to deallocate MODEL_METEO_T.AtmPress'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%P_atm) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%P_atm(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_METEO = err
              MessageErreur = 'SET_TAILLE_VAR_METEO : Unable to allocate MODEL_METEO_T.AtmPress'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------
      else
         SET_TAILLE_VAR_METEO = 1
         MessageErreur         = "SET_TAILLE_VAR_METEO - Unknown variable name"
      end if

    end function SET_TAILLE_VAR_METEO

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_METEO(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_METEO     ! different de 0 si erreur
      type(METEO_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_METEO = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Weather.Time') > 0) then
         valeur = Instance%Temps(index1)
      else if ( index(NomVar, 'Model.Tracer.Weather.Temperature') > 0) then
         valeur = Instance%Temp(index1)
      else if ( index(NomVar, 'Model.Tracer.Weather.I0') > 0) then
         valeur = Instance%I0(index1)
      else if ( index(NomVar, 'Model.Tracer.Weather.AirTemp') > 0) then
         valeur = Instance%T_Air(index1)
      else if ( index(NomVar, 'Model.Tracer.Weather.AirPress') > 0) then
         valeur = Instance%P_Vap(index1)
      else if ( index(NomVar, 'Model.Tracer.Weather.WindSpeed') > 0) then
         valeur = Instance%Vit_Vent(index1)
      else if ( index(NomVar, 'Model.Tracer.Weather.Nebulosity') > 0) then
         valeur = Instance%Nebulo(index1)
      else if ( index(NomVar, 'Model.Tracer.Weather.Radiation') > 0) then
         valeur = Instance%Ray3(index1)
      else if ( index(NomVar, 'Model.Tracer.Weather.AtmPress') > 0) then
         valeur = Instance%P_atm(index1)
      else
         GET_DOUBLE_METEO = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_METEO - Unknown variable name"
      end if
   end function GET_DOUBLE_METEO

   function GET_STRING_METEO(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_METEO ! different de 0 si erreur
      type(METEO_T),intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_METEO = 0
      valeur                = ""
      MessageErreur          = ""

      if (INDEX(NomVar,'Model.Tracer.Weather.Name') > 0) then
         valeur = Instance%Nom
      else
         GET_STRING_METEO = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_METEO - Unknown variable name"
      end if
   end function GET_STRING_METEO

! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_METEO(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_METEO     ! different de 0 si erreur
      type(METEO_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_METEO = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Tracer.Weather.Time') > 0) then
         Instance%Temps(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.Weather.Temperature') > 0) then
         Instance%Temp(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.Weather.I0') > 0) then
         Instance%I0(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.Weather.AirTemp') > 0) then
         Instance%T_Air(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.Weather.AirPress') > 0) then
         Instance%P_Vap(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.Weather.WindSpeed') > 0) then
         Instance%Vit_Vent(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.Weather.Nebulosity') > 0) then
         Instance%Nebulo(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.Weather.Radiation') > 0) then
         Instance%Ray3(index1) = valeur
      else if ( index(NomVar, 'Model.Tracer.Weather.AtmPress') > 0) then
         Instance%P_atm(index1) = valeur
      else
         SET_DOUBLE_METEO = 1
         MessageErreur         = "SET_DOUBLE_METEO - Unknown variable name"
      end if
   end function SET_DOUBLE_METEO

   function SET_STRING_METEO(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_METEO ! different de 0 si erreur
      type(METEO_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_METEO = 0
      MessageErreur          = ""

      if (INDEX(NomVar,'Model.Tracer.Weather.Name') > 0) then
         Instance%Nom = TRIM(valeur)
        else
         SET_STRING_METEO = 1
         MessageErreur         = "SET_STRING_METEO - Unknown variable name"
      end if
   end function SET_STRING_METEO

! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_METEO(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_METEO      ! different de 0 si erreur
      type(METEO_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_METEO = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%Temps)) then
          taille = SIZE(Instance%Temps)
          if (taille > 0) then
              DEALLOCATE(Instance%Temps, STAT=err)
              if (err /= 0) then
                  DESALLOUE_METEO = err
                  MessageErreur = 'Unable to deallocate MODEL_METEO_T.Time'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Temps)
      if (ASSOCIATED(Instance%Temp)) then
          taille = SIZE(Instance%Temp)
          if (taille > 0) then
              DEALLOCATE(Instance%Temp, STAT=err)
              if (err /= 0) then
                  DESALLOUE_METEO = err
                  MessageErreur = 'Unable to deallocate MODEL_METEO_T.Temperature'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Temp)
      if (ASSOCIATED(Instance%I0)) then
          taille = SIZE(Instance%I0)
          if (taille > 0) then
              DEALLOCATE(Instance%I0, STAT=err)
              if (err /= 0) then
                  DESALLOUE_METEO = err
                  MessageErreur = 'Unable to deallocate MODEL_METEO_T.I0'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%I0)
      if (ASSOCIATED(Instance%T_Air)) then
          taille = SIZE(Instance%T_Air)
          if (taille > 0) then
              DEALLOCATE(Instance%T_Air, STAT=err)
              if (err /= 0) then
                  DESALLOUE_METEO = err
                  MessageErreur = 'Unable to deallocate MODEL_METEO_T.AirTemp'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%T_Air)
      if (ASSOCIATED(Instance%P_Vap)) then
          taille = SIZE(Instance%P_Vap)
          if (taille > 0) then
              DEALLOCATE(Instance%P_Vap, STAT=err)
              if (err /= 0) then
                  DESALLOUE_METEO = err
                  MessageErreur = 'Unable to deallocate MODEL_METEO_T.AirPress'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%P_Vap)
      if (ASSOCIATED(Instance%Vit_Vent)) then
          taille = SIZE(Instance%Vit_Vent)
          if (taille > 0) then
              DEALLOCATE(Instance%Vit_Vent, STAT=err)
              if (err /= 0) then
                  DESALLOUE_METEO = err
                  MessageErreur = 'Unable to deallocate MODEL_METEO_T.WindSpeed'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Vit_Vent)
      if (ASSOCIATED(Instance%Nebulo)) then
          taille = SIZE(Instance%Nebulo)
          if (taille > 0) then
              DEALLOCATE(Instance%Nebulo, STAT=err)
              if (err /= 0) then
                  DESALLOUE_METEO = err
                  MessageErreur = 'Unable to deallocate MODEL_METEO_T.Nebulosity'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Nebulo)
      if (ASSOCIATED(Instance%Ray3)) then
          taille = SIZE(Instance%Ray3)
          if (taille > 0) then
              DEALLOCATE(Instance%Ray3, STAT=err)
              if (err /= 0) then
                  DESALLOUE_METEO = err
                  MessageErreur = 'Unable to deallocate MODEL_METEO_T.Radiation'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Ray3)
      if (ASSOCIATED(Instance%P_atm)) then
          taille = SIZE(Instance%P_atm)
          if (taille > 0) then
              DEALLOCATE(Instance%P_atm, STAT=err)
              if (err /= 0) then
                  DESALLOUE_METEO = err
                  MessageErreur = 'Unable to deallocate MODEL_METEO_T.AtmPress'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%P_atm)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_METEO

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_METEO(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_METEO      ! different de 0 si erreur
      type(METEO_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_METEO = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%Temps)
      NULLIFY(Instance%Temp)
      NULLIFY(Instance%I0)
      NULLIFY(Instance%T_Air)
      NULLIFY(Instance%P_Vap)
      NULLIFY(Instance%Vit_Vent)
      NULLIFY(Instance%Nebulo)
      NULLIFY(Instance%Ray3)
      NULLIFY(Instance%P_atm)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_METEO

end module M_METEO_T
