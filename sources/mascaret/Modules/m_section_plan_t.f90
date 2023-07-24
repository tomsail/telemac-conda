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

module M_SECTION_PLAN_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

  !***********************************************************************
  !
  ! Type pour les variables de sections planimetrees
  !
  !***********************************************************************

  !=========================== Declarations ==============================

  use M_PRECISION

  type SECTION_PLAN_T

     sequence

     real(DOUBLE), dimension(:,:), pointer :: S      => null() ! Section mouillee totale
     real(DOUBLE), dimension(:,:), pointer :: S1     => null() ! Section mouillee lit mineur
     real(DOUBLE), dimension(:,:), pointer :: S2     => null() ! Section mouillee lit majeur
     real(DOUBLE), dimension(:,:), pointer :: SS     => null() ! Section de stockage
     real(DOUBLE), dimension(:,:), pointer :: S1GEO  => null() ! Termes non hydroastatiques (Scube)
     real(DOUBLE), dimension(:,:), pointer :: CELER  => null() !
     real(DOUBLE), dimension(:,:), pointer :: B      => null() ! Largeur au miroir totale
     real(DOUBLE), dimension(:,:), pointer :: INV    => null() ! Invariant de Riemann
     real(DOUBLE), dimension(:,:), pointer :: INTE   => null() !
     real(DOUBLE), dimension(:,:), pointer :: DYDX   => null() ! dY/dx|s
     real(DOUBLE), dimension(:,:), pointer :: PRESS  => null() ! Pression
     real(DOUBLE), dimension(:,:), pointer :: DEB    => null() ! Debitance
     real(DOUBLE), dimension(:,:), pointer :: DEB1   => null() ! Debitance mineure
     real(DOUBLE), dimension(:,:), pointer :: DEB2   => null() ! Debitance majeure

! Maillage decale
     real(DOUBLE), dimension(:,:), pointer :: SD     => null() ! Section mouillees totale
     real(DOUBLE), dimension(:,:), pointer :: SD1    => null() ! Section mouillees mineur
     real(DOUBLE), dimension(:,:), pointer :: SD2    => null() ! Section mouillees majeur
     real(DOUBLE), dimension(:,:), pointer :: PRESSD => null() ! Pression
     real(DOUBLE), dimension(:,:), pointer :: BD     => null() ! Largeur au miroir totale
     real(DOUBLE), dimension(:,:), pointer :: DEBD   => null() ! Debitance totale

  end type SECTION_PLAN_T

contains
   ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_SECTION_PLAN(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                        :: i                 ! indiceTableaux
      character(len= 40), dimension(*)               :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)               :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.VDSection.S"
        tabDescriptionVar(i) ="Total wetted area"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.S1"
        tabDescriptionVar(i) ="Main channel wetted area"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.S2"
        tabDescriptionVar(i) ="Floodplain wetted area"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.SS"
        tabDescriptionVar(i) ="Ineffective flow area"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.S1GEO"
        tabDescriptionVar(i) ="Non-hydrostatic termsTermes (Scube)"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.CELER"
        tabDescriptionVar(i) ="Celerity"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.B"
        tabDescriptionVar(i) ="Total width"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.INV"
        tabDescriptionVar(i) ="Riemann invariants"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.INTE"
        tabDescriptionVar(i) ="INTE AIGEO"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.DYDX"
        tabDescriptionVar(i) ="dY/dx|s"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.PRESS"
        tabDescriptionVar(i) ="Pressure"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.DEB"
        tabDescriptionVar(i) ="Conveyance"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.DEB1"
        tabDescriptionVar(i) ="Main channel conveyance"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.DEB2"
        tabDescriptionVar(i) ="Floodplain conveyance"
        i=i+1
        ! Maillage decale
        tabNomVar(i)         ="Model.VDSection.SD"
        tabDescriptionVar(i) ="Shifted - Total wetted area"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.SD1"
        tabDescriptionVar(i) ="Shifted - Main channel area"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.SD2"
        tabDescriptionVar(i) ="Shifted - Floodplain"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.PRESSD"
        tabDescriptionVar(i) ="Shifted  - Pressure"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.BD"
        tabDescriptionVar(i) ="Shifted - Total width"
        i=i+1
        tabNomVar(i)         ="Model.VDSection.DEBD"
        tabDescriptionVar(i) ="Shifted - Total conveyance"
        i=i+1
      return

    end subroutine GET_TAB_VAR_SECTION_PLAN

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_SECTION_PLAN(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_SECTION_PLAN    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_SECTION_PLAN = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""

       if ( index(NomVar, 'Model.VDSection.S1GEO') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.S1') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.S2') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.SS') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.S') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.CELER') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.B') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.INV') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.INTE') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.DYDX') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.PRESS') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.DEB1') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.DEB2') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.DEBD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.DEB') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.SD1') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.SD2') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.SD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.PRESSD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDSection.BD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
      else
        GET_TYPE_VAR_SECTION_PLAN = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_SECTION_PLAN - Unknown variable name"
      end if


    end function GET_TYPE_VAR_SECTION_PLAN

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_SECTION_PLAN(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_SECTION_PLAN    ! different de 0 si erreur
      type(SECTION_PLAN_T),   intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_SECTION_PLAN = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.VDSection.S1GEO') > 0) then
         if (ASSOCIATED(Instance%S1GEO)) then
            taille1 = size(Instance%S1GEO, 1)
            taille2 = size(Instance%S1GEO, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.S1') > 0) then
         if (ASSOCIATED(Instance%S1)) then
            taille1 = size(Instance%S1, 1)
            taille2 = size(Instance%S1, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.S2') > 0) then
         if (ASSOCIATED(Instance%S2)) then
            taille1 = size(Instance%S2, 1)
            taille2 = size(Instance%S2, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.SS') > 0) then
         if (ASSOCIATED(Instance%SS)) then
            taille1 = size(Instance%SS, 1)
            taille2 = size(Instance%SS, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.S') > 0) then
         if (ASSOCIATED(Instance%S)) then
            taille1 = size(Instance%S, 1)
            taille2 = size(Instance%S, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.CELER') > 0) then
         if (ASSOCIATED(Instance%CELER)) then
            taille1 = size(Instance%CELER, 1)
            taille2 = size(Instance%CELER, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.B') > 0) then
         if (ASSOCIATED(Instance%B)) then
            taille1 = size(Instance%B, 1)
            taille2 = size(Instance%B, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.INV') > 0) then
         if (ASSOCIATED(Instance%INV)) then
            taille1 = size(Instance%INV, 1)
            taille2 = size(Instance%INV, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.INTE') > 0) then
         if (ASSOCIATED(Instance%INTE)) then
            taille1 = size(Instance%INTE, 1)
            taille2 = size(Instance%INTE, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.DYDX') > 0) then
         if (ASSOCIATED(Instance%DYDX)) then
            taille1 = size(Instance%DYDX, 1)
            taille2 = size(Instance%DYDX, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.PRESS') > 0) then
         if (ASSOCIATED(Instance%PRESS)) then
            taille1 = size(Instance%PRESS, 1)
            taille2 = size(Instance%PRESS, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.DEB1') > 0) then
         if (ASSOCIATED(Instance%DEB1)) then
            taille1 = size(Instance%DEB1, 1)
            taille2 = size(Instance%DEB1, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.DEB2') > 0) then
         if (ASSOCIATED(Instance%DEB2)) then
            taille1 = size(Instance%DEB2, 1)
            taille2 = size(Instance%DEB2, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.DEBD') > 0) then
         if (ASSOCIATED(Instance%DEBD)) then
            taille1 = size(Instance%DEBD, 1)
            taille2 = size(Instance%DEBD, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.DEB') > 0) then
         if (ASSOCIATED(Instance%DEB)) then
            taille1 = size(Instance%DEB, 1)
            taille2 = size(Instance%DEB, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.SD1') > 0) then
         if (ASSOCIATED(Instance%SD1)) then
            taille1 = size(Instance%SD1, 1)
            taille2 = size(Instance%SD1, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.SD2') > 0) then
         if (ASSOCIATED(Instance%SD2)) then
            taille1 = size(Instance%SD2, 1)
            taille2 = size(Instance%SD2, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.SD') > 0) then
         if (ASSOCIATED(Instance%SD)) then
            taille1 = size(Instance%SD, 1)
            taille2 = size(Instance%SD, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.PRESSD') > 0) then
         if (ASSOCIATED(Instance%PRESSD)) then
            taille1 = size(Instance%PRESSD, 1)
            taille2 = size(Instance%PRESSD, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDSection.BD') > 0) then
         if (ASSOCIATED(Instance%BD)) then
            taille1 = size(Instance%BD, 1)
            taille2 = size(Instance%BD, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else
         GET_TAILLE_VAR_SECTION_PLAN = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_SECTION_PLAN - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_SECTION_PLAN

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_SECTION_PLAN(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_SECTION_PLAN    ! different de 0 si erreur
      type(SECTION_PLAN_T),   intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_SECTION_PLAN = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.VDSection.S1GEO') > 0) then
        if (ASSOCIATED(Instance%S1GEO)) then
           t1 = size(Instance%S1GEO, 1)
           t2 = size(Instance%S1GEO, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%S1GEO, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.S1GEO'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S1GEO).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%S1GEO(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.S1GEO'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.S1') > 0) then
        if (ASSOCIATED(Instance%S1)) then
           t1 = size(Instance%S1, 1)
           t2 = size(Instance%S1, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%S1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.S1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S1).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%S1(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.S1'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.S2') > 0) then
        if (ASSOCIATED(Instance%S2)) then
           t1 = size(Instance%S2, 1)
           t2 = size(Instance%S2, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%S2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.S2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S2).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%S2(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.S2'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.SS') > 0) then
        if (ASSOCIATED(Instance%SS)) then
           t1 = size(Instance%SS, 1)
           t2 = size(Instance%SS, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%SS, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.SS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SS).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%SS(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.SS'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.S') > 0) then
        if (ASSOCIATED(Instance%S)) then
           t1 = size(Instance%S, 1)
           t2 = size(Instance%S, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%S, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.S'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%S(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.S'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.CELER') > 0) then
        if (ASSOCIATED(Instance%CELER)) then
           t1 = size(Instance%CELER, 1)
           t2 = size(Instance%CELER, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%CELER, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.CELER'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%CELER).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%CELER(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.CELER'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.B') > 0) then
        if (ASSOCIATED(Instance%B)) then
           t1 = size(Instance%B, 1)
           t2 = size(Instance%B, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%B, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.B'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%B).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%B(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.B'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.INV') > 0) then
        if (ASSOCIATED(Instance%INV)) then
           t1 = size(Instance%INV, 1)
           t2 = size(Instance%INV, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%INV, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.INV'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%INV).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%INV(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.INV'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.INTE') > 0) then
        if (ASSOCIATED(Instance%INTE)) then
           t1 = size(Instance%INTE, 1)
           t2 = size(Instance%INTE, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%INTE, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.INTE'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%INTE).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%INTE(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.INTE'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.DYDX') > 0) then
        if (ASSOCIATED(Instance%DYDX)) then
           t1 = size(Instance%DYDX, 1)
           t2 = size(Instance%DYDX, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%DYDX, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.DYDX'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DYDX).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%DYDX(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.DYDX'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.PRESS') > 0) then
        if (ASSOCIATED(Instance%PRESS)) then
           t1 = size(Instance%PRESS, 1)
           t2 = size(Instance%PRESS, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%PRESS, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.PRESS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PRESS).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%PRESS(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.PRESS'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.DEB1') > 0) then
        if (ASSOCIATED(Instance%DEB1)) then
           t1 = size(Instance%DEB1, 1)
           t2 = size(Instance%DEB1, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%DEB1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.DEB1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DEB1).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%DEB1(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.DEB1'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.DEB2') > 0) then
        if (ASSOCIATED(Instance%DEB2)) then
           t1 = size(Instance%DEB2, 1)
           t2 = size(Instance%DEB2, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%DEB2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.DEB2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DEB2).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%DEB2(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.DEB2'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.DEBD') > 0) then
        if (ASSOCIATED(Instance%DEBD)) then
           t1 = size(Instance%DEBD, 1)
           t2 = size(Instance%DEBD, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%DEBD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.DEBD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DEBD).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%DEBD(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.DEBD'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.DEB') > 0) then
        if (ASSOCIATED(Instance%DEB)) then
           t1 = size(Instance%DEB, 1)
           t2 = size(Instance%DEB, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%DEB, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.DEB'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DEB).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%DEB(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.DEB'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.SD1') > 0) then
        if (ASSOCIATED(Instance%SD1)) then
           t1 = size(Instance%SD1, 1)
           t2 = size(Instance%SD1, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%SD1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.SD1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SD1).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%SD1(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.SD1'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.SD2') > 0) then
        if (ASSOCIATED(Instance%SD2)) then
           t1 = size(Instance%SD2, 1)
           t2 = size(Instance%SD2, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%SD2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.SD2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SD2).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%SD2(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.SD2'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.SD') > 0) then
        if (ASSOCIATED(Instance%SD)) then
           t1 = size(Instance%SD, 1)
           t2 = size(Instance%SD, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%SD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.SD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SD).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%SD(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.SD'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.PRESSD') > 0) then
        if (ASSOCIATED(Instance%PRESSD)) then
           t1 = size(Instance%PRESSD, 1)
           t2 = size(Instance%PRESSD, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%PRESSD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.PRESSD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PRESSD).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%PRESSD(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.PRESSD'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDSection.BD') > 0) then
        if (ASSOCIATED(Instance%BD)) then
           t1 = size(Instance%BD, 1)
           t2 = size(Instance%BD, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%BD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SECTION_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to deallocate SECTION_PLAN_T.BD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%BD).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%BD(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SECTION_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_SECTION_PLAN : Unable to allocate SECTION_PLAN_T.BD'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_SECTION_PLAN = 1
         MessageErreur         = "SET_TAILLE_VAR_SECTION_PLAN - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_SECTION_PLAN

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_SECTION_PLAN(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_SECTION_PLAN    ! different de 0 si erreur
      type(SECTION_PLAN_T),   intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_SECTION_PLAN = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.VDSection.S1GEO') > 0) then
         valeur = Instance%S1GEO(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.S1') > 0) then
         valeur = Instance%S1(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.S2') > 0) then
         valeur = Instance%S2(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.SS') > 0) then
         valeur = Instance%SS(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.S') > 0) then
         valeur = Instance%S(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.CELER') > 0) then
         valeur = Instance%CELER(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.B') > 0) then
         valeur = Instance%B(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.INV') > 0) then
         valeur = Instance%INV(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.INTE') > 0) then
         valeur = Instance%INTE(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.DYDX') > 0) then
         valeur = Instance%DYDX(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.PRESS') > 0) then
         valeur = Instance%PRESS(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.DEB1') > 0) then
         valeur = Instance%DEB1(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.DEB2') > 0) then
         valeur = Instance%DEB2(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.DEBD') > 0) then
         valeur = Instance%DEBD(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.DEB') > 0) then
         valeur = Instance%DEB(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.SD1') > 0) then
         valeur = Instance%SD1(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.SD2') > 0) then
         valeur = Instance%SD2(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.SD') > 0) then
         valeur = Instance%SD(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.PRESSD') > 0) then
         valeur = Instance%PRESSD(index1, index2)
      else if ( index(NomVar, 'Model.VDSection.BD') > 0) then
         valeur = Instance%BD(index1, index2)
      else
         GET_DOUBLE_SECTION_PLAN = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_SECTION_PLAN - Unknown variable name"
      end if
   end function GET_DOUBLE_SECTION_PLAN



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_SECTION_PLAN(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_SECTION_PLAN    ! different de 0 si erreur
      type(SECTION_PLAN_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_SECTION_PLAN = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.VDSection.S1GEO') > 0) then
         Instance%S1GEO(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.S1') > 0) then
         Instance%S1(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.S2') > 0) then
         Instance%S2(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.SS') > 0) then
         Instance%SS(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.S') > 0) then
         Instance%S(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.CELER') > 0) then
         Instance%CELER(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.B') > 0) then
         Instance%B(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.INV') > 0) then
         Instance%INV(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.INTE') > 0) then
         Instance%INTE(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.DYDX') > 0) then
         Instance%DYDX(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.PRESS') > 0) then
         Instance%PRESS(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.DEB1') > 0) then
         Instance%DEB1(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.DEB2') > 0) then
         Instance%DEB2(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.DEBD') > 0) then
         Instance%DEBD(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.DEB') > 0) then
         Instance%DEB(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.SD1') > 0) then
         Instance%SD1(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.SD2') > 0) then
         Instance%SD2(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.SD') > 0) then
         Instance%SD(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.PRESSD') > 0) then
         Instance%PRESSD(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDSection.BD') > 0) then
         Instance%BD(index1, index2) = valeur
      else
         SET_DOUBLE_SECTION_PLAN = 1
         MessageErreur         = "SET_DOUBLE_SECTION_PLAN - Unknown variable name"
      end if
   end function SET_DOUBLE_SECTION_PLAN



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_SECTION_PLAN(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_SECTION_PLAN     ! different de 0 si erreur
      type(SECTION_PLAN_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_SECTION_PLAN = 0
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%S)) then
          taille = SIZE(Instance%S, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%S, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.S'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S)
      if (ASSOCIATED(Instance%S1)) then
          taille = SIZE(Instance%S1, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%S1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.S1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S1)
      if (ASSOCIATED(Instance%S2)) then
          taille = SIZE(Instance%S2, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%S2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.S2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S2)
      if (ASSOCIATED(Instance%SS)) then
          taille = SIZE(Instance%SS, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%SS, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.SS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SS)
      if (ASSOCIATED(Instance%S1GEO)) then
          taille = SIZE(Instance%S1GEO, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%S1GEO, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.S1GEO'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S1GEO)
      if (ASSOCIATED(Instance%CELER)) then
          taille = SIZE(Instance%CELER, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%CELER, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.CELER'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%CELER)
      if (ASSOCIATED(Instance%B)) then
          taille = SIZE(Instance%B, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%B, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.B'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%B)
      if (ASSOCIATED(Instance%INV)) then
          taille = SIZE(Instance%INV, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%INV, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.INV'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%INV)
      if (ASSOCIATED(Instance%INTE)) then
          taille = SIZE(Instance%INTE, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%INTE, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.INTE'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%INTE)
      if (ASSOCIATED(Instance%DYDX)) then
          taille = SIZE(Instance%DYDX, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%DYDX, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.DYDX'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DYDX)
      if (ASSOCIATED(Instance%PRESS)) then
          taille = SIZE(Instance%PRESS, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%PRESS, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.PRESS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PRESS)
      if (ASSOCIATED(Instance%DEB)) then
          taille = SIZE(Instance%DEB, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%DEB, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.DEB'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DEB)
      if (ASSOCIATED(Instance%DEB1)) then
          taille = SIZE(Instance%DEB1, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%DEB1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.DEB1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DEB1)
      if (ASSOCIATED(Instance%DEB2)) then
          taille = SIZE(Instance%DEB2, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%DEB2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.DEB2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DEB2)
      if (ASSOCIATED(Instance%SD)) then
          taille = SIZE(Instance%SD, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%SD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.SD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SD)
      if (ASSOCIATED(Instance%SD1)) then
          taille = SIZE(Instance%SD1, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%SD1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.SD1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SD1)
      if (ASSOCIATED(Instance%SD2)) then
          taille = SIZE(Instance%SD2, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%SD2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.SD2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SD2)
      if (ASSOCIATED(Instance%PRESSD)) then
          taille = SIZE(Instance%PRESSD, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%PRESSD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.PRESSD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PRESSD)
      if (ASSOCIATED(Instance%BD)) then
          taille = SIZE(Instance%BD, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%BD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.BD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%BD)
      if (ASSOCIATED(Instance%DEBD)) then
          taille = SIZE(Instance%DEBD, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%DEBD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SECTION_PLAN = err
                  MessageErreur = 'Unable to deallocate SECTION_PLAN_T.DEBD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DEBD)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_SECTION_PLAN

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_SECTION_PLAN(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_SECTION_PLAN     ! different de 0 si erreur
      type(SECTION_PLAN_T),   intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_SECTION_PLAN = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%S)
      NULLIFY(Instance%S1)
      NULLIFY(Instance%S2)
      NULLIFY(Instance%SS)
      NULLIFY(Instance%S1GEO)
      NULLIFY(Instance%CELER)
      NULLIFY(Instance%B)
      NULLIFY(Instance%INV)
      NULLIFY(Instance%INTE)
      NULLIFY(Instance%DYDX)
      NULLIFY(Instance%PRESS)
      NULLIFY(Instance%DEB)
      NULLIFY(Instance%DEB1)
      NULLIFY(Instance%DEB2)
      NULLIFY(Instance%SD)
      NULLIFY(Instance%SD1)
      NULLIFY(Instance%SD2)
      NULLIFY(Instance%PRESSD)
      NULLIFY(Instance%BD)
      NULLIFY(Instance%DEBD)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_SECTION_PLAN

end module M_SECTION_PLAN_T
