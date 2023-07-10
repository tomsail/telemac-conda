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
module M_ETAT_TRACER_T

!=========================== Declarations ==============================

use M_PRECISION

TYPE ETAT_TRACER_T

    sequence

    real(DOUBLE), dimension(:,:), pointer :: Ctraceur => null() ! Concentration du traceur no i

    real(DOUBLE), dimension(:), pointer :: QT => null()       ! Debit total (Q1+Q2)
    real(DOUBLE), dimension(:), pointer :: ST => null()       ! Surface mouillee totale (S1+S2)
    real(DOUBLE), dimension(:), pointer :: BT => null()       ! Largeur totale (B1+B2)
    real(DOUBLE), dimension(:), pointer :: QT_ANT => null()   ! Debit total au pas de temps anterieur
    real(DOUBLE), dimension(:), pointer :: ST_ANT => null()   ! Surface mouillee totale au pas de temps anterieur
    real(DOUBLE), dimension(:), pointer :: BT_ANT => null()   ! Largeur totale au pas de temps anterieur

    real(DOUBLE), dimension(:),   pointer :: NbCourant => null() !
    real(DOUBLE), dimension(:,:), pointer :: MASS   => null()    !
    real(DOUBLE), dimension(:,:), pointer :: FLUMAS => null()    !
    real(DOUBLE), dimension(:,:), pointer :: FLUENT => null()    !
    real(DOUBLE), dimension(:,:), pointer :: FLUSOR => null()    !
    real(DOUBLE), dimension(:,:), pointer :: FLUSRC => null()    !

END TYPE ETAT_TRACER_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_ETAT_TRACER(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="State.Tracer.Concentration"
        tabDescriptionVar(i) ="Concentration of the constituant"
        i=i+1
        tabNomVar(i)         ="State.Tracer.Q"
        tabDescriptionVar(i) ="Total discharge"
        i=i+1
        tabNomVar(i)         ="State.Tracer.WetArea"
        tabDescriptionVar(i) ="Wet area"
        i=i+1
        tabNomVar(i)         ="State.Tracer.Width"
        tabDescriptionVar(i) ="Main channel width"
        i=i+1
        tabNomVar(i)         ="State.Tracer.Qprior"
        tabDescriptionVar(i) ="Prior values of the total discharge"
        i=i+1
        tabNomVar(i)         ="State.Tracer.WetAreaPrior"
        tabDescriptionVar(i) ="Prior values of the wet area"
        i=i+1
        tabNomVar(i)         ="State.Tracer.WidthPrior"
        tabDescriptionVar(i) ="Prior values of the hydraulic width"
        i=i+1
        tabNomVar(i)         ="State.Tracer.CourantNumber"
        tabDescriptionVar(i) ="Courant number"
        i=i+1
        tabNomVar(i)         ="State.Tracer.Mass"
        tabDescriptionVar(i) ="Mass of the constituant"
        i=i+1
        tabNomVar(i)         ="State.Tracer.MassFlux"
        tabDescriptionVar(i) ="Flux of mass"
        i=i+1
        tabNomVar(i)         ="State.Tracer.InMass"
        tabDescriptionVar(i) ="Incoming mass values"
        i=i+1
        tabNomVar(i)         ="State.Tracer.OutMass"
        tabDescriptionVar(i) ="Outcoming mass values"
        i=i+1
        tabNomVar(i)         ="State.Tracer.PtMass"
        tabDescriptionVar(i) ="Local mass value"
        i=i+1

        return

    end subroutine GET_TAB_VAR_ETAT_TRACER

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_ETAT_TRACER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_ETAT_TRACER ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_ETAT_TRACER = 0
      TypeVar               = ""
      Categorie             = "STATE"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'State.Tracer.Concentration') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 2
      else if ( index(NomVar, 'State.Tracer.Q') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Tracer.WetArea') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Tracer.Width') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Tracer.Qprior') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Tracer.WetAreaPrior') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Tracer.WidthPrior') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Tracer.CourantNumber') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar, 'State.Tracer.MassFlux') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 2
      else if ( index(NomVar, 'State.Tracer.Mass') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 2
      else if ( index(NomVar, 'State.Tracer.InMass') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 2
      else if ( index(NomVar, 'State.Tracer.OutMass') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 2
      else if ( index(NomVar, 'State.Tracer.PtMass') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 2
      else
         GET_TYPE_VAR_ETAT_TRACER = 1
         TypeVar = "?"
         Categorie             = "STATE"
         Modifiable            = .false.
         dimVar                = -1
         MessageErreur         = "GET_TYPE_VAR_ETAT_TRACER - Unknown variable name"
      end if


    end function GET_TYPE_VAR_ETAT_TRACER

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_ETAT_TRACER(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_ETAT_TRACER     ! different de 0 si erreur
      type(ETAT_TRACER_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_ETAT_TRACER = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.Tracer.Concentration') > 0) then
         taille1 = size(Instance%Ctraceur, 1)
         taille2 = size(Instance%Ctraceur, 2)
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.Q') > 0) then
         taille1 = size(Instance%QT)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.WetArea') > 0) then
         taille1 = size(Instance%ST)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.Width') > 0) then
         taille1 = size(Instance%BT)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.Qprior') > 0) then
         taille1 = size(Instance%QT_ANT)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.WetAreaPrior') > 0) then
         taille1 = size(Instance%ST_ANT)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.WidthPrior') > 0) then
         taille1 = size(Instance%BT_ANT)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.CourantNumber') > 0) then
         taille1 = size(Instance%NbCourant)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.MassFlux') > 0) then
         taille1 = size(Instance%FLUMAS, 1)
         taille2 = size(Instance%FLUMAS, 2)
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.Mass') > 0) then
         taille1 = size(Instance%MASS, 1)
         taille2 = size(Instance%MASS, 2)
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.InMass') > 0) then
         taille1 = size(Instance%FLUENT, 1)
         taille2 = size(Instance%FLUENT, 2)
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.OutMass') > 0) then
         taille1 = size(Instance%FLUSOR, 1)
         taille2 = size(Instance%FLUSOR, 2)
         taille3 = 0
      else if ( index(NomVar, 'State.Tracer.PtMass') > 0) then
         taille1 = size(Instance%FLUSRC, 1)
         taille2 = size(Instance%FLUSRC, 2)
         taille3 = 0
      else
         GET_TAILLE_VAR_ETAT_TRACER = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_ETAT_TRACER - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_ETAT_TRACER

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_ETAT_TRACER(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_ETAT_TRACER     ! different de 0 si erreur
      type(ETAT_TRACER_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_ETAT_TRACER = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'State.Tracer.Concentration') > 0) then
         if (associated(Instance%Ctraceur)) then
            t1 = size(Instance%Ctraceur, 1)
            t2 = size(Instance%Ctraceur, 2)
            if ( (t1 /= NewT1).or.(t2 /= NewT2) ) then
               deallocate(Instance%Ctraceur, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.Ctraceur'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%Ctraceur) .or. (t1 /= NewT1).or.(t2/=NewT2)) then
            allocate(Instance%Ctraceur(NewT1, NewT2), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.Ctraceur'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.Q') > 0) then
         if (associated(Instance%QT)) then
            t1 = size(Instance%QT)
            if (t1 /= NewT1) then
               deallocate(Instance%QT, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.QT'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%QT) .or. (t1 /= NewT1)) then
            allocate(Instance%QT(NewT1), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.QT'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.WetArea') > 0) then
         if (associated(Instance%ST)) then
            t1 = size(Instance%ST)
            if (t1 /= NewT1) then
               deallocate(Instance%ST, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.ST'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%ST) .or. (t1 /= NewT1)) then
            allocate(Instance%ST(NewT1), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.ST'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.Width') > 0) then
         if (associated(Instance%BT)) then
            t1 = size(Instance%BT)
            if (t1 /= NewT1) then
               deallocate(Instance%BT, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.BT'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%BT) .or. (t1 /= NewT1)) then
            allocate(Instance%BT(NewT1), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.BT'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.Qprior') > 0) then
         if (associated(Instance%QT_ANT)) then
            t1 = size(Instance%QT_ANT)
            if (t1 /= NewT1) then
               deallocate(Instance%QT_ANT, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.QT_ANT'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%QT_ANT) .or. (t1 /= NewT1)) then
            allocate(Instance%QT_ANT(NewT1), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.QT_ANT'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.WetAreaPrior') > 0) then
         if (associated(Instance%ST_ANT)) then
            t1 = size(Instance%ST_ANT)
            if (t1 /= NewT1) then
               deallocate(Instance%ST_ANT, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.ST_ANT'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%ST_ANT) .or. (t1 /= NewT1)) then
            allocate(Instance%ST_ANT(NewT1), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.ST_ANT'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.WidthPrior') > 0) then
         if (associated(Instance%BT_ANT)) then
            t1 = size(Instance%BT_ANT)
            if (t1 /= NewT1) then
               deallocate(Instance%BT_ANT, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.BT_ANT'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%BT_ANT) .or. (t1 /= NewT1)) then
            allocate(Instance%BT_ANT(NewT1), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.BT_ANT'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.CourantNumber') > 0) then
         if (associated(Instance%NbCourant)) then
            t1 = size(Instance%NbCourant)
            if (t1 /= NewT1) then
               deallocate(Instance%NbCourant, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.NbCourant'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%NbCourant) .or. (t1 /= NewT1)) then
            allocate(Instance%NbCourant(NewT1), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.NbCourant'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.MassFlux') > 0) then
         if (associated(Instance%FLUMAS)) then
            t1 = size(Instance%FLUMAS, 1)
            t2 = size(Instance%FLUMAS, 2)
            if ( (t1 /= NewT1).or.(t2 /= NewT2) ) then
               deallocate(Instance%FLUMAS, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.FLUMAS'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%FLUMAS) .or. (t1 /= NewT1).or.(t2/=NewT2)) then
            allocate(Instance%FLUMAS(NewT1, NewT2), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.FLUMAS'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.Mass') > 0) then
         if (associated(Instance%MASS)) then
            t1 = size(Instance%MASS, 1)
            t2 = size(Instance%MASS, 2)
            if ( (t1 /= NewT1).or.(t2 /= NewT2) ) then
               deallocate(Instance%MASS, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.MASS'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%MASS) .or. (t1 /= NewT1).or.(t2/=NewT2)) then
            allocate(Instance%MASS(NewT1, NewT2), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.MASS'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.InMass') > 0) then
         if (associated(Instance%FLUENT)) then
            t1 = size(Instance%FLUENT, 1)
            t2 = size(Instance%FLUENT, 2)
            if ( (t1 /= NewT1).or.(t2 /= NewT2) ) then
               deallocate(Instance%FLUENT, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.FLUENT'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%FLUENT) .or. (t1 /= NewT1).or.(t2/=NewT2)) then
            allocate(Instance%FLUENT(NewT1, NewT2), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.FLUENT'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.OutMass') > 0) then
         if (associated(Instance%FLUSOR)) then
            t1 = size(Instance%FLUSOR, 1)
            t2 = size(Instance%FLUSOR, 2)
            if ( (t1 /= NewT1).or.(t2 /= NewT2) ) then
               deallocate(Instance%FLUSOR, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.FLUSOR'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%FLUSOR) .or. (t1 /= NewT1).or.(t2/=NewT2)) then
            allocate(Instance%FLUSOR(NewT1, NewT2), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.FLUSOR'
               return
            endif
         endif
      else if ( index(NomVar, 'State.Tracer.PtMass') > 0) then
         if (associated(Instance%FLUSRC)) then
            t1 = size(Instance%FLUSRC, 1)
            t2 = size(Instance%FLUSRC, 2)
            if ( (t1 /= NewT1).or.(t2 /= NewT2) ) then
               deallocate(Instance%FLUSRC, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_TRACER = err
                  MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to deallocate ETAT_TRACER_T.FLUSRC'
                  return
               endif
            endif
         endif
         if (.not.associated(Instance%FLUSRC) .or. (t1 /= NewT1).or.(t2/=NewT2)) then
            allocate(Instance%FLUSRC(NewT1, NewT2), STAT=err)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_TRACER = err
               MessageErreur = 'SET_TAILLE_VAR_ETAT_TRACER : Unable to allocate ETAT_TRACER_T.FLUSRC'
               return
            endif
         endif
         !--------------------------------------------------------------------
         ! Fin de la modification de la taille des pointers de types primitifs
         !--------------------------------------------------------------------
      else
         SET_TAILLE_VAR_ETAT_TRACER = 1
         MessageErreur         = "SET_TAILLE_VAR_ETAT_TRACER - Unknown variable name"
      end if


   end function SET_TAILLE_VAR_ETAT_TRACER

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_ETAT_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_ETAT_TRACER     ! different de 0 si erreur
      type(ETAT_TRACER_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_ETAT_TRACER = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'State.Tracer.Concentration') > 0) then
         valeur = Instance%Ctraceur(index1, index2)
      else if ( index(NomVar, 'State.Tracer.Q') > 0) then
         valeur = Instance%QT(index1)
      else if ( index(NomVar, 'State.Tracer.WetArea') > 0) then
         valeur = Instance%ST(index1)
      else if ( index(NomVar, 'State.Tracer.Width') > 0) then
         valeur = Instance%BT(index1)
      else if ( index(NomVar, 'State.Tracer.Qprior') > 0) then
         valeur = Instance%QT_ANT(index1)
      else if ( index(NomVar, 'State.Tracer.WetAreaPrior') > 0) then
         valeur = Instance%ST_ANT(index1)
      else if ( index(NomVar, 'State.Tracer.WidthPrior') > 0) then
         valeur = Instance%BT_ANT(index1)
      else if ( index(NomVar, 'State.Tracer.CourantNumber') > 0) then
         valeur = Instance%NbCourant(index1)
      else if ( index(NomVar, 'State.Tracer.MassFlux') > 0) then
         valeur = Instance%FLUMAS(index1, index2)
      else if ( index(NomVar, 'State.Tracer.Mass') > 0) then
         valeur = Instance%MASS(index1, index2)
      else if ( index(NomVar, 'State.Tracer.InMass') > 0) then
         valeur = Instance%FLUENT(index1, index2)
      else if ( index(NomVar, 'State.Tracer.OutMass') > 0) then
         valeur = Instance%FLUSOR(index1, index2)
      else if ( index(NomVar, 'State.Tracer.PtMass') > 0) then
         valeur = Instance%FLUSRC(index1, index2)
      else
         GET_DOUBLE_ETAT_TRACER = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_ETAT_TRACER - Unknown variable name"
      end if
   end function GET_DOUBLE_ETAT_TRACER

! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_ETAT_TRACER(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_ETAT_TRACER     ! different de 0 si erreur
      type(ETAT_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_ETAT_TRACER = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.Tracer.Concentration') > 0) then
         Instance%Ctraceur(index1, index2) = valeur
      else if ( index(NomVar, 'State.Tracer.Q') > 0) then
         Instance%QT(index1) = valeur
      else if ( index(NomVar, 'State.Tracer.WetArea') > 0) then
         Instance%ST(index1) = valeur
      else if ( index(NomVar, 'State.Tracer.Width') > 0) then
         Instance%BT(index1) = valeur
      else if ( index(NomVar, 'State.Tracer.Qprior') > 0) then
         Instance%QT_ANT(index1) = valeur
      else if ( index(NomVar, 'State.Tracer.WetAreaPrior') > 0) then
         Instance%ST_ANT(index1) = valeur
      else if ( index(NomVar, 'State.Tracer.WidthPrior') > 0) then
         Instance%BT_ANT(index1) = valeur
      else if ( index(NomVar, 'State.Tracer.CourantNumber') > 0) then
         Instance%NbCourant(index1) = valeur
      else if ( index(NomVar, 'State.Tracer.MassFlux') > 0) then
         Instance%FLUMAS(index1, index2) = valeur
      else if ( index(NomVar, 'State.Tracer.Mass') > 0) then
         Instance%MASS(index1, index2) = valeur
      else if ( index(NomVar, 'State.Tracer.InMass') > 0) then
         Instance%FLUENT(index1, index2) = valeur
      else if ( index(NomVar, 'State.Tracer.OutMass') > 0) then
         Instance%FLUSOR(index1, index2) = valeur
      else if ( index(NomVar, 'State.Tracer.PtMass') > 0) then
         Instance%FLUSRC(index1, index2) = valeur
      else
         SET_DOUBLE_ETAT_TRACER = 1
         MessageErreur         = "SET_DOUBLE_ETAT_TRACER - Unknown variable name"
      end if
   end function SET_DOUBLE_ETAT_TRACER


! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_ETAT_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_ETAT_TRACER      ! different de 0 si erreur
      type(ETAT_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err

      DESALLOUE_ETAT_TRACER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (associated(Instance%Ctraceur)) then
         taille = size(Instance%Ctraceur, 1)
         if (taille > 0) then
            deallocate(Instance%Ctraceur, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.Ctraceur'
               return
            endif
         endif
      endif
      nullify(Instance%Ctraceur)
      if (associated(Instance%QT)) then
         taille = size(Instance%QT)
         if (taille > 0) then
            deallocate(Instance%QT, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.QT'
               return
            endif
         endif
      endif
      nullify(Instance%QT)
      if (associated(Instance%ST)) then
         taille = size(Instance%ST)
         if (taille > 0) then
            deallocate(Instance%ST, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.ST'
               return
            endif
         endif
      endif
      nullify(Instance%ST)
      if (associated(Instance%BT)) then
         taille = size(Instance%BT)
         if (taille > 0) then
            deallocate(Instance%BT, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.BT'
               return
            endif
         endif
      endif
      nullify(Instance%BT)
      if (associated(Instance%QT_ANT)) then
         taille = size(Instance%QT_ANT)
         if (taille > 0) then
            deallocate(Instance%QT_ANT, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.QT_ANT'
               return
            endif
         endif
      endif
      nullify(Instance%QT_ANT)
      if (associated(Instance%ST_ANT)) then
         taille = size(Instance%ST_ANT)
         if (taille > 0) then
            deallocate(Instance%ST_ANT, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.ST_ANT'
               return
            endif
         endif
      endif
      nullify(Instance%ST_ANT)
      if (associated(Instance%BT_ANT)) then
         taille = size(Instance%BT_ANT)
         if (taille > 0) then
            deallocate(Instance%BT_ANT, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.BT_ANT'
               return
            endif
         endif
      endif
      nullify(Instance%BT_ANT)
      if (associated(Instance%NbCourant)) then
         taille = size(Instance%NbCourant)
         if (taille > 0) then
            deallocate(Instance%NbCourant, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.NbCourant'
               return
            endif
         endif
      endif
      nullify(Instance%NbCourant)
      if (associated(Instance%MASS)) then
         taille = size(Instance%MASS, 1)
         if (taille > 0) then
            deallocate(Instance%MASS, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.MASS'
               return
            endif
         endif
      endif
      nullify(Instance%MASS)
      if (associated(Instance%FLUMAS)) then
         taille = size(Instance%FLUMAS, 1)
         if (taille > 0) then
            deallocate(Instance%FLUMAS, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.FLUMAS'
               return
            endif
         endif
      endif
      nullify(Instance%FLUMAS)
      if (associated(Instance%FLUENT)) then
         taille = size(Instance%FLUENT, 1)
         if (taille > 0) then
            deallocate(Instance%FLUENT, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.FLUENT'
               return
            endif
         endif
      endif
      nullify(Instance%FLUENT)
      if (associated(Instance%FLUSOR)) then
         taille = size(Instance%FLUSOR, 1)
         if (taille > 0) then
            deallocate(Instance%FLUSOR, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.FLUSOR'
               return
            endif
         endif
      endif
      nullify(Instance%FLUSOR)
      if (associated(Instance%FLUSRC)) then
         taille = size(Instance%FLUSRC, 1)
         if (taille > 0) then
            deallocate(Instance%FLUSRC, STAT=err)
            if (err /= 0) then
               DESALLOUE_ETAT_TRACER = err
               MessageErreur = 'Unable to deallocate ETAT_TRACER_T.FLUSRC'
               return
            endif
         endif
      endif
      nullify(Instance%FLUSRC)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_ETAT_TRACER

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_ETAT_TRACER(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_ETAT_TRACER      ! different de 0 si erreur
      type(ETAT_TRACER_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_ETAT_TRACER = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      nullify(Instance%Ctraceur)
      nullify(Instance%QT)
      nullify(Instance%ST)
      nullify(Instance%BT)
      nullify(Instance%QT_ANT)
      nullify(Instance%ST_ANT)
      nullify(Instance%BT_ANT)
      nullify(Instance%NbCourant)
      nullify(Instance%MASS)
      nullify(Instance%FLUMAS)
      nullify(Instance%FLUENT)
      nullify(Instance%FLUSOR)
      nullify(Instance%FLUSRC)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_ETAT_TRACER

end module M_ETAT_TRACER_T
