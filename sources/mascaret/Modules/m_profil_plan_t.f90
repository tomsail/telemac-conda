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

module M_PROFIL_PLAN_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !***********************************************************************
   !
   ! Type pour les variables de profil planimetrees
   !
   !***********************************************************************

   !=========================== Declarations ==============================
  use M_PRECISION

  type PROFIL_PLAN_T

     sequence

     real(DOUBLE), dimension(:,:), pointer :: B1   => null() ! Largeur au miroir lit mineur
     real(DOUBLE), dimension(:,:), pointer :: B2   => null() ! Largeur au miroir lit majeur
     real(DOUBLE), dimension(:,:), pointer :: BS   => null() ! Largeur au miroir zone stockage
     real(DOUBLE), dimension(:,:), pointer :: P1   => null() ! Perimetre mouille  lit mineur
     real(DOUBLE), dimension(:,:), pointer :: P2   => null() ! Perimetre mouille  lit majeur
     real(DOUBLE), dimension(:,:), pointer :: S1   => null() ! Section   mouillee lit mineur
     real(DOUBLE), dimension(:,:), pointer :: S2   => null() ! Section   mouillee lit majeur
     real(DOUBLE), dimension(:,:), pointer :: S2G  => null() ! Section   mouillee lit maj gauche
     real(DOUBLE), dimension(:,:), pointer :: SS   => null() ! Section mouillee zone stockage

     ! Pour MASCARET
     real(DOUBLE), dimension(:,:), pointer :: C     => null() ! Celerite
     real(DOUBLE), dimension(:,:), pointer :: Deb1  => null() ! Debitance mineur
     real(DOUBLE), dimension(:,:), pointer :: Deb2  => null() ! Debitance majeur
     real(DOUBLE), dimension(:,:), pointer :: Pr    => null() ! Pression
     real(DOUBLE), dimension(:,:), pointer :: Inv   => null() ! Invariants de Riemann
     real(DOUBLE), dimension(:,:), pointer :: S1D   => null() ! Section mouillee mineur maillage decale
     real(DOUBLE), dimension(:,:), pointer :: S2D   => null() ! Section mouillee majeur maillage decale
     real(DOUBLE), dimension(:,:), pointer :: SSD   => null() ! Section mouillee stockage maillage decale
     real(DOUBLE), dimension(:,:), pointer :: PrD   => null() ! Pression maillage decale
     real(DOUBLE), dimension(:,:), pointer :: BD    => null() ! Largeur au miroir maillage decale
     real(DOUBLE), dimension(:,:), pointer :: DebD  => null() ! Debitance maillage decale

  end type PROFIL_PLAN_T


contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_PROFIL_PLAN(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.VDCrossSection.B1"
        tabDescriptionVar(i) ="Main channel width"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.B2"
        tabDescriptionVar(i) ="Floodplain width"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.BS"
        tabDescriptionVar(i) ="Width of the ineffective flow area"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.P1"
        tabDescriptionVar(i) ="Main channel wetted perimeter"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.P2"
        tabDescriptionVar(i) ="Floodplain wetted perimeter"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.S1"
        tabDescriptionVar(i) ="Main channel wetted area"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.S2"
        tabDescriptionVar(i) ="Floodplain wetted area"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.S2G"
        tabDescriptionVar(i) ="Left floodplain wetted area"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.SS"
        tabDescriptionVar(i) ="Wetted ineffective flow area"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.Conv1"
        tabDescriptionVar(i) ="Conveyance of the main channel"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.Conv2"
        tabDescriptionVar(i) ="Conveyance of the floodplain"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.C"
        tabDescriptionVar(i) ="Celerity"
        i=i+1
         tabNomVar(i)         ="Model.VDCrossSection.Pr"
        tabDescriptionVar(i) ="Pressure"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.Inv"
        tabDescriptionVar(i) ="Riemann invariant"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.S1D"
        tabDescriptionVar(i) ="Shifted mesh - Main channel wetted area"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.S2D"
        tabDescriptionVar(i) ="Shifted mesh - Floodplain wetted area"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.SSD"
        tabDescriptionVar(i) ="Shifted mesh - Ineffective flow area"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.PrD"
        tabDescriptionVar(i) ="Shifted mesh - Pressure"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.BD"
        tabDescriptionVar(i) ="Shifted mesh - Width"
        i=i+1
        tabNomVar(i)         ="Model.VDCrossSection.DebD"
        tabDescriptionVar(i) ="Shifted mesh - Conveyance"
        i=i+1


      return
    end subroutine GET_TAB_VAR_PROFIL_PLAN

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_PROFIL_PLAN(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_PROFIL_PLAN    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_PROFIL_PLAN = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""


       if ( index(NomVar, 'Model.VDCrossSection.B1') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.B2') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.BS') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.P1') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.P2') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.S1') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.S2') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.S2G') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.SS') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.Conv1') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.Conv2') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.C') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.Pr') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.Inv') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.S1D') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.S2D') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.SSD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.PrD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.BD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.VDCrossSection.DebD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
      else
        GET_TYPE_VAR_PROFIL_PLAN = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_PROFIL_PLAN - Unknown variable name"
      end if


    end function GET_TYPE_VAR_PROFIL_PLAN

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_PROFIL_PLAN(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_PROFIL_PLAN     ! different de 0 si erreur
      type(PROFIL_PLAN_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_PROFIL_PLAN = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.VDCrossSection.B1') > 0) then
         if (ASSOCIATED(Instance%B1)) then
            taille1 = size(Instance%B1, 1)
            taille2 = size(Instance%B1, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.B2') > 0) then
         if (ASSOCIATED(Instance%B2)) then
            taille1 = size(Instance%B2, 1)
            taille2 = size(Instance%B2, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.BS') > 0) then
         if (ASSOCIATED(Instance%BS)) then
            taille1 = size(Instance%BS, 1)
            taille2 = size(Instance%BS, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.P1') > 0) then
         if (ASSOCIATED(Instance%P1)) then
            taille1 = size(Instance%P1, 1)
            taille2 = size(Instance%P1, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.P2') > 0) then
         if (ASSOCIATED(Instance%P2)) then
            taille1 = size(Instance%P2, 1)
            taille2 = size(Instance%P2, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.S1') > 0) then
         if (ASSOCIATED(Instance%S1)) then
            taille1 = size(Instance%S1, 1)
            taille2 = size(Instance%S1, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.S2') > 0) then
         if (ASSOCIATED(Instance%S2)) then
            taille1 = size(Instance%S2, 1)
            taille2 = size(Instance%S2, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.S2G') > 0) then
         if (ASSOCIATED(Instance%S2G)) then
            taille1 = size(Instance%S2G, 1)
            taille2 = size(Instance%S2G, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.SS') > 0) then
         if (ASSOCIATED(Instance%SS)) then
            taille1 = size(Instance%SS, 1)
            taille2 = size(Instance%SS, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.Conv1') > 0) then
         if (ASSOCIATED(Instance%Deb1)) then
            taille1 = size(Instance%Deb1, 1)
            taille2 = size(Instance%Deb1, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.Conv2') > 0) then
         if (ASSOCIATED(Instance%Deb2)) then
            taille1 = size(Instance%Deb2, 1)
            taille2 = size(Instance%Deb2, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.C') > 0) then
         if (ASSOCIATED(Instance%C)) then
            taille1 = size(Instance%C, 1)
            taille2 = size(Instance%C, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.Pr') > 0) then
         if (ASSOCIATED(Instance%Pr)) then
            taille1 = size(Instance%Pr, 1)
            taille2 = size(Instance%Pr, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.Inv') > 0) then
         if (ASSOCIATED(Instance%Inv)) then
            taille1 = size(Instance%Inv, 1)
            taille2 = size(Instance%Inv, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.S1D') > 0) then
         if (ASSOCIATED(Instance%S1D)) then
            taille1 = size(Instance%S1D, 1)
            taille2 = size(Instance%S1D, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.S2D') > 0) then
         if (ASSOCIATED(Instance%S2D)) then
            taille1 = size(Instance%S2D, 1)
            taille2 = size(Instance%S2D, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.SSD') > 0) then
         if (ASSOCIATED(Instance%SSD)) then
            taille1 = size(Instance%SSD, 1)
            taille2 = size(Instance%SSD, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.PrD') > 0) then
         if (ASSOCIATED(Instance%PrD)) then
            taille1 = size(Instance%PrD, 1)
            taille2 = size(Instance%PrD, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.BD') > 0) then
         if (ASSOCIATED(Instance%BD)) then
            taille1 = size(Instance%BD, 1)
            taille2 = size(Instance%BD, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.VDCrossSection.DebD') > 0) then
         if (ASSOCIATED(Instance%DebD)) then
            taille1 = size(Instance%DebD, 1)
            taille2 = size(Instance%DebD, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else
         GET_TAILLE_VAR_PROFIL_PLAN = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_PROFIL_PLAN - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_PROFIL_PLAN

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_PROFIL_PLAN(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_PROFIL_PLAN     ! different de 0 si erreur
      type(PROFIL_PLAN_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_PROFIL_PLAN = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.VDCrossSection.B1') > 0) then
        if (ASSOCIATED(Instance%B1)) then
           t1 = size(Instance%B1, 1)
           t2 = size(Instance%B1, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%B1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.B1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%B1).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%B1(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.B1'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.B2') > 0) then
        if (ASSOCIATED(Instance%B2)) then
           t1 = size(Instance%B2, 1)
           t2 = size(Instance%B2, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%B2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.B2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%B2).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%B2(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.B2'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.BS') > 0) then
        if (ASSOCIATED(Instance%BS)) then
           t1 = size(Instance%BS, 1)
           t2 = size(Instance%BS, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%BS, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.BS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%BS).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%BS(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.BS'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.P1') > 0) then
        if (ASSOCIATED(Instance%P1)) then
           t1 = size(Instance%P1, 1)
           t2 = size(Instance%P1, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%P1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.P1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%P1).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%P1(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.P1'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.P2') > 0) then
        if (ASSOCIATED(Instance%P2)) then
           t1 = size(Instance%P2, 1)
           t2 = size(Instance%P2, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%P2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.P2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%P2).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%P2(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.P2'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.S1') > 0) then
        if (ASSOCIATED(Instance%S1)) then
           t1 = size(Instance%S1, 1)
           t2 = size(Instance%S1, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%S1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.S1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S1).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%S1(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.S1'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.S2') > 0) then
        if (ASSOCIATED(Instance%S2)) then
           t1 = size(Instance%S2, 1)
           t2 = size(Instance%S2, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%S2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.S2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S2).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%S2(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.S2'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.S2G') > 0) then
        if (ASSOCIATED(Instance%S2G)) then
           t1 = size(Instance%S2G, 1)
           t2 = size(Instance%S2G, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%S2G, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.S2G'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S2G).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%S2G(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.S2G'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.SS') > 0) then
        if (ASSOCIATED(Instance%SS)) then
           t1 = size(Instance%SS, 1)
           t2 = size(Instance%SS, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%SS, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.SS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SS).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%SS(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.SS'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.Conv1') > 0) then
        if (ASSOCIATED(Instance%Deb1)) then
           t1 = size(Instance%Deb1, 1)
           t2 = size(Instance%Deb1, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%Deb1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.Deb1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Deb1).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%Deb1(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.Deb1'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.Conv2') > 0) then
        if (ASSOCIATED(Instance%Deb2)) then
           t1 = size(Instance%Deb2, 1)
           t2 = size(Instance%Deb2, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%Deb2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.Deb2'
                 return
              endif
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.C') > 0) then
        if (ASSOCIATED(Instance%C)) then
           t1 = size(Instance%C, 1)
           t2 = size(Instance%C, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%C, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.C'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%C).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%C(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.C'
              return
           endif
        endif
        if (.not.ASSOCIATED(Instance%Deb2).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%Deb2(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.Deb2'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.Pr') > 0) then
        if (ASSOCIATED(Instance%Pr)) then
           t1 = size(Instance%Pr, 1)
           t2 = size(Instance%Pr, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%Pr, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.Pr'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Pr).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%Pr(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.Pr'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.Inv') > 0) then
        if (ASSOCIATED(Instance%Inv)) then
           t1 = size(Instance%Inv, 1)
           t2 = size(Instance%Inv, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%Inv, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.Inv'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Inv).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%Inv(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.Inv'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.S1D') > 0) then
        if (ASSOCIATED(Instance%S1D)) then
           t1 = size(Instance%S1D, 1)
           t2 = size(Instance%S1D, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%S1D, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.S1D'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S1D).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%S1D(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.S1D'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.S2D') > 0) then
        if (ASSOCIATED(Instance%S2D)) then
           t1 = size(Instance%S2D, 1)
           t2 = size(Instance%S2D, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%S2D, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.S2D'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S2D).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%S2D(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.S2D'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.SSD') > 0) then
        if (ASSOCIATED(Instance%SSD)) then
           t1 = size(Instance%SSD, 1)
           t2 = size(Instance%SSD, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%SSD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.SSD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SSD).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%SSD(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.SSD'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.PrD') > 0) then
        if (ASSOCIATED(Instance%PrD)) then
           t1 = size(Instance%PrD, 1)
           t2 = size(Instance%PrD, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%PrD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.PrD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PrD).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%PrD(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.PrD'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.BD') > 0) then
        if (ASSOCIATED(Instance%BD)) then
           t1 = size(Instance%BD, 1)
           t2 = size(Instance%BD, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%BD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.BD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%BD).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%BD(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.BD'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.VDCrossSection.DebD') > 0) then
        if (ASSOCIATED(Instance%DebD)) then
           t1 = size(Instance%DebD, 1)
           t2 = size(Instance%DebD, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%DebD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_PROFIL_PLAN = err
                 MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to deallocate PROFIL_PLAN_T.DebD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DebD).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%DebD(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_PROFIL_PLAN = err
              MessageErreur = 'SET_TAILLE_VAR_PROFIL_PLAN : Unable to allocate PROFIL_PLAN_T.DebD'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_PROFIL_PLAN = 1
         MessageErreur         = "SET_TAILLE_VAR_PROFIL_PLAN - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_PROFIL_PLAN

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_PROFIL_PLAN(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_PROFIL_PLAN     ! different de 0 si erreur
      type(PROFIL_PLAN_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_PROFIL_PLAN = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.VDCrossSection.B1') > 0) then
         valeur = Instance%B1(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.B2') > 0) then
         valeur = Instance%B2(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.BS') > 0) then
         valeur = Instance%BS(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.P1') > 0) then
         valeur = Instance%P1(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.P2') > 0) then
         valeur = Instance%P2(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.S1') > 0) then
         valeur = Instance%S1(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.S2') > 0) then
         valeur = Instance%S2(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.S2G') > 0) then
         valeur = Instance%S2G(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.SS') > 0) then
         valeur = Instance%SS(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.Conv1') > 0) then
         valeur = Instance%Deb1(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.Conv2') > 0) then
         valeur = Instance%Deb2(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.C') > 0) then
         valeur = Instance%C(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.Pr') > 0) then
         valeur = Instance%Pr(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.Inv') > 0) then
         valeur = Instance%Inv(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.S1D') > 0) then
         valeur = Instance%S1D(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.S2D') > 0) then
         valeur = Instance%S2D(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.SSD') > 0) then
         valeur = Instance%SSD(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.PrD') > 0) then
         valeur = Instance%PrD(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.BD') > 0) then
         valeur = Instance%BD(index1, index2)
      else if ( index(NomVar, 'Model.VDCrossSection.DebD') > 0) then
         valeur = Instance%DebD(index1, index2)
      else
         GET_DOUBLE_PROFIL_PLAN = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_PROFIL_PLAN - Unknown variable name"
      end if
   end function GET_DOUBLE_PROFIL_PLAN



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_PROFIL_PLAN(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_PROFIL_PLAN     ! different de 0 si erreur
      type(PROFIL_PLAN_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_PROFIL_PLAN = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.VDCrossSection.B1') > 0) then
         Instance%B1(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.B2') > 0) then
         Instance%B2(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.BS') > 0) then
         Instance%BS(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.P1') > 0) then
         Instance%P1(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.P2') > 0) then
         Instance%P2(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.S1') > 0) then
         Instance%S1(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.S2') > 0) then
         Instance%S2(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.S2G') > 0) then
         Instance%S2G(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.SS') > 0) then
         Instance%SS(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.Conv1') > 0) then
         Instance%Deb1(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.Conv2') > 0) then
         Instance%Deb2(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.C') > 0) then
         Instance%C(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.Pr') > 0) then
         Instance%Pr(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.Inv') > 0) then
         Instance%Inv(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.S1D') > 0) then
         Instance%S1D(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.S2D') > 0) then
         Instance%S2D(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.SSD') > 0) then
         Instance%SSD(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.PrD') > 0) then
         Instance%PrD(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.BD') > 0) then
         Instance%BD(index1, index2) = valeur
      else if ( index(NomVar, 'Model.VDCrossSection.DebD') > 0) then
         Instance%DebD(index1, index2) = valeur
      else
         SET_DOUBLE_PROFIL_PLAN = 1
         MessageErreur         = "SET_DOUBLE_PROFIL_PLAN - Unknown variable name"
      end if
   end function SET_DOUBLE_PROFIL_PLAN



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_PROFIL_PLAN(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_PROFIL_PLAN      ! different de 0 si erreur
      type(PROFIL_PLAN_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_PROFIL_PLAN = 0
      MessageErreur         = ""
      err                   = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%B1)) then
          taille = SIZE(Instance%B1, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%B1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.B1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%B1)
      if (ASSOCIATED(Instance%B2)) then
          taille = SIZE(Instance%B2, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%B2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.B2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%B2)
      if (ASSOCIATED(Instance%BS)) then
          taille = SIZE(Instance%BS, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%BS, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.BS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%BS)
      if (ASSOCIATED(Instance%P1)) then
          taille = SIZE(Instance%P1, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%P1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.P1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%P1)
      if (ASSOCIATED(Instance%P2)) then
          taille = SIZE(Instance%P2, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%P2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.P2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%P2)
      if (ASSOCIATED(Instance%S1)) then
          taille = SIZE(Instance%S1, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%S1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.S1'
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
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.S2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S2)
      if (ASSOCIATED(Instance%S2G)) then
          taille = SIZE(Instance%S2G, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%S2G, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.S2G'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S2G)
      if (ASSOCIATED(Instance%SS)) then
          taille = SIZE(Instance%SS, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%SS, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.SS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SS)
      if (ASSOCIATED(Instance%C)) then
          taille = SIZE(Instance%C, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%C, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.C'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%C)
      if (ASSOCIATED(Instance%Deb1)) then
          taille = SIZE(Instance%Deb1, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%Deb1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.Deb1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Deb1)
      if (ASSOCIATED(Instance%Deb2)) then
          taille = SIZE(Instance%Deb2, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%Deb2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.Deb2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Deb2)
      if (ASSOCIATED(Instance%Pr)) then
          taille = SIZE(Instance%Pr, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%Pr, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.Pr'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Pr)
      if (ASSOCIATED(Instance%Inv)) then
          taille = SIZE(Instance%Inv, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%Inv, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.Inv'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Inv)
      if (ASSOCIATED(Instance%S1D)) then
          taille = SIZE(Instance%S1D, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%S1D, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.S1D'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S1D)
      if (ASSOCIATED(Instance%S2D)) then
          taille = SIZE(Instance%S2D, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%S2D, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.S2D'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S2D)
      if (ASSOCIATED(Instance%SSD)) then
          taille = SIZE(Instance%SSD, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%SSD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.SSD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SSD)
      if (ASSOCIATED(Instance%PrD)) then
          taille = SIZE(Instance%PrD, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%PrD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.PrD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PrD)
      if (ASSOCIATED(Instance%BD)) then
          taille = SIZE(Instance%BD, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%BD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.BD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%BD)
      if (ASSOCIATED(Instance%DebD)) then
          taille = SIZE(Instance%DebD, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%DebD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_PROFIL_PLAN = err
                  MessageErreur = 'Unable to deallocate PROFIL_PLAN_T.DebD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DebD)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_PROFIL_PLAN

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_PROFIL_PLAN(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_PROFIL_PLAN      ! different de 0 si erreur
      type(PROFIL_PLAN_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_PROFIL_PLAN = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%B1)
      NULLIFY(Instance%B2)
      NULLIFY(Instance%BS)
      NULLIFY(Instance%P1)
      NULLIFY(Instance%P2)
      NULLIFY(Instance%S1)
      NULLIFY(Instance%S2)
      NULLIFY(Instance%S2G)
      NULLIFY(Instance%SS)
      NULLIFY(Instance%C)
      NULLIFY(Instance%Deb1)
      NULLIFY(Instance%Deb2)
      NULLIFY(Instance%Pr)
      NULLIFY(Instance%Inv)
      NULLIFY(Instance%S1D)
      NULLIFY(Instance%S2D)
      NULLIFY(Instance%SSD)
      NULLIFY(Instance%PrD)
      NULLIFY(Instance%BD)
      NULLIFY(Instance%DebD)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_PROFIL_PLAN

end module M_PROFIL_PLAN_T
