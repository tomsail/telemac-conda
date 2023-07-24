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

module M_SINGULARITE_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
use M_PRECISION

! Constantes reperant le type de singularite

integer, parameter :: SINGULARITE_TYPE_ZAMONT_ZAVAL_Q = 1
integer, parameter :: SINGULARITE_TYPE_ZAMONT_Q       = 2
integer, parameter :: SINGULARITE_TYPE_PROFIL_CRETE   = 3
integer, parameter :: SINGULARITE_TYPE_CRETE_COEFF    = 4
integer, parameter :: SINGULARITE_TYPE_Z_T            = 5
integer, parameter :: SINGULARITE_TYPE_Q_ZAMONT       = 6
integer, parameter :: SINGULARITE_TYPE_Q_ZAVAL        = 7
integer, parameter :: SINGULARITE_TYPE_VANNE          = 8

integer, parameter :: SINGULARITE_TYPE_NB_MAX         = 8


TYPE SINGULARITE_T
  sequence
  character(30) :: Nom          ! Nom du seuil ou du barrage
  integer       :: Numero       ! Numero du seuil
  integer       :: NumBranche   ! Numero de la branche
  real(DOUBLE)  :: AbscisseRel  ! Abscisse relative
  integer       :: Section      ! Numero de la section
  integer       :: Type         ! Type
  logical       :: EtatEfface   ! Etat efface
  Integer       :: Epaisseur_seuil ! Seuil epais ou mince (1 seuil epais)
  real(DOUBLE)  :: CoteCrete    ! Cote de crete
  real(DOUBLE)  :: CoeffDebit   ! Coefficient de debit
  real(DOUBLE)  :: CoteRupture  ! Cote de rupture
  real (DOUBLE) :: Pente        ! Pente d'ouverture de la breche
  real (DOUBLE) :: Debit        ! Debit constant a travers la singularite
  real(DOUBLE)  :: LargeurVanne ! Largeur de la vanne
  integer       :: NumeroLoi    ! Numero de la loi

  real(DOUBLE), dimension(:)  , pointer :: PtZ      => null() ! Points Zamont de la loi
  real(DOUBLE), dimension(:)  , pointer :: PtQ      => null() ! Points Q de la loi
  real(DOUBLE), dimension(:,:), pointer :: PtZamont => null() ! Points Zamont de la loi
  real(DOUBLE), dimension(:)  , pointer :: PtZaval  => null() ! Points Zaval de la loi
  real(DOUBLE)                          :: PtZsup   ! Cote sup de la vanne
  real(DOUBLE)                          :: PtZinf   ! Cote inf de la vanne
  real(DOUBLE), dimension(:)  , pointer :: PtX      => null() ! Abscisses de la crete
  real(DOUBLE), dimension(:)  , pointer :: PtY      => null() ! Cotes     de la crete

end type SINGULARITE_T

contains
   ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_SINGULARITE(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.Weir.Name"
        tabDescriptionVar(i) ="Name of the weir or dam"
        i=i+1
        tabNomVar(i)         ="Model.Weir.Number"
        tabDescriptionVar(i) ="Weir number"
        i=i+1
        tabNomVar(i)         ="Model.Weir.ReachNum"
        tabDescriptionVar(i) ="Numero de la branche"
        i=i+1
        tabNomVar(i)         ="Model.Weir.RelAbscissa"
        tabDescriptionVar(i) ="Relative abscissa"
        i=i+1
        tabNomVar(i)         ="Model.Weir.Node"
        tabDescriptionVar(i) ="Node number"
        i=i+1
        tabNomVar(i)         ="Model.Weir.Type"
        tabDescriptionVar(i) ="Type"
        i=i+1
        tabNomVar(i)         ="Model.Weir.State"
        tabDescriptionVar(i) ="Enabled or disabled"
        i=i+1
        tabNomVar(i)         ="Model.Weir.Thickness"
        tabDescriptionVar(i) ="Thick or thin"
        i=i+1
        tabNomVar(i)         ="Model.Weir.CrestLevel"
        tabDescriptionVar(i) ="Crest level (m)"
        i=i+1
        tabNomVar(i)         ="Model.Weir.DischCoef"
        tabDescriptionVar(i) ="Discharge coefficient"
        i=i+1
        tabNomVar(i)         ="Model.Weir.BrkLevel"
        tabDescriptionVar(i) ="Breaking water level"
        i=i+1
        tabNomVar(i)         ="Model.Weir.Slope"
        tabDescriptionVar(i) ="Gradient of crest lowering (m/s)"
        i=i+1
        tabNomVar(i)         ="Model.Weir.Discharge"
        tabDescriptionVar(i) ="Constant discharge (m3/s)"
        i=i+1
        tabNomVar(i)         ="Model.Weir.GateLength"
        tabDescriptionVar(i) ="Length of the gate"
        i=i+1
        tabNomVar(i)         ="Model.Weir.GraphNum"
        tabDescriptionVar(i) ="Graph number"
        i=i+1
        tabNomVar(i)         ="Model.Weir.PtZ"
        tabDescriptionVar(i) ="Z points (levels) for the graph"
        i=i+1
        tabNomVar(i)         ="Model.Weir.PtQ"
        tabDescriptionVar(i) ="Q points (discharges) for the graph"
        i=i+1
        tabNomVar(i)         ="Model.Weir.PtZus"
        tabDescriptionVar(i) ="Z upstream points (levels) for the graph"
        i=i+1
        tabNomVar(i)         ="Model.Weir.PtZds"
        tabDescriptionVar(i) ="Z downstream points (levels) for the graph"
        i=i+1
        tabNomVar(i)         ="Model.Weir.PtZup"
        tabDescriptionVar(i) ="Upper levels of the gate"
        i=i+1
        tabNomVar(i)         ="Model.Weir.PtZlo"
        tabDescriptionVar(i) ="Lower levels of the gate"
        i=i+1
        tabNomVar(i)         ="Model.Weir.PtX"
        tabDescriptionVar(i) ="Crest abscissa"
        i=i+1
        tabNomVar(i)         ="Model.Weir.PtY"
        tabDescriptionVar(i) ="Crest ordinate"
        i=i+1

      return

    end subroutine GET_TAB_VAR_SINGULARITE

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_SINGULARITE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_SINGULARITE    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_SINGULARITE = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

       if ( index(NomVar, 'Model.Weir.Name') > 0) then
          TypeVar = 'STRING'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.Number') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.ReachNum') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.RelAbscissa') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.Node') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.Type') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.State') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.Thickness') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.CrestLevel') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.DischCoef') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.BrkLevel') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.Slope') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.Discharge') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.GateLength') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.GraphNum') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.PtQ') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Weir.PtZus') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.Weir.PtZds') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Weir.PtZup') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.PtZlo') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Weir.PtZ') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Weir.PtX') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Weir.PtY') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
      else
        GET_TYPE_VAR_SINGULARITE = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_SINGULARITE - Unknown variable name"
      end if


    end function GET_TYPE_VAR_SINGULARITE

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_SINGULARITE(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_SINGULARITE     ! different de 0 si erreur
      type(SINGULARITE_T),    intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_SINGULARITE = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Weir.Name') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.Number') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.ReachNum') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.RelAbscissa') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.Node') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.Type') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.State') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.Thickness') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.CrestLevel') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.DischCoef') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.BrkLevel') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.Slope') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.Discharge') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.GateLength') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.GraphNum') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.PtQ') > 0) then
         if (ASSOCIATED(Instance%PtQ)) then
            taille1 = size(Instance%PtQ)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.PtZus') > 0) then
         if (ASSOCIATED(Instance%PtZamont)) then
            taille1 = size(Instance%PtZamont, 1)
            taille2 = size(Instance%PtZamont, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.PtZds') > 0) then
         if (ASSOCIATED(Instance%PtZaval)) then
            taille1 = size(Instance%PtZaval)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.PtZup') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.PtZlo') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.PtZ') > 0) then
         if (ASSOCIATED(Instance%PtZ)) then
            taille1 = size(Instance%PtZ)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.PtX') > 0) then
         if (ASSOCIATED(Instance%PtX)) then
            taille1 = size(Instance%PtX)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Weir.PtY') > 0) then
         if (ASSOCIATED(Instance%PtY)) then
            taille1 = size(Instance%PtY)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_SINGULARITE = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_SINGULARITE - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_SINGULARITE

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_SINGULARITE(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_SINGULARITE     ! different de 0 si erreur
      type(SINGULARITE_T),    intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_SINGULARITE = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.Weir.PtQ') > 0) then
        if (ASSOCIATED(Instance%PtQ)) then
           t1 = size(Instance%PtQ)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%PtQ, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SINGULARITE = err
                 MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to deallocate SINGULARITE_T.PtQ'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PtQ) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%PtQ(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SINGULARITE = err
              MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to allocate SINGULARITE_T.PtQ'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Weir.PtZus') > 0) then
        if (ASSOCIATED(Instance%PtZamont)) then
           t1 = size(Instance%PtZamont, 1)
           t2 = size(Instance%PtZamont, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%PtZamont, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SINGULARITE = err
                 MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to deallocate SINGULARITE_T.PtZamont'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PtZamont).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%PtZamont(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SINGULARITE = err
              MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to allocate SINGULARITE_T.PtZamont'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Weir.PtZds') > 0) then
        if (ASSOCIATED(Instance%PtZaval)) then
           t1 = size(Instance%PtZaval)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%PtZaval, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SINGULARITE = err
                 MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to deallocate SINGULARITE_T.PtZaval'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PtZaval) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%PtZaval(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SINGULARITE = err
              MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to allocate SINGULARITE_T.PtZaval'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Weir.PtZ') > 0) then
        if (ASSOCIATED(Instance%PtZ)) then
           t1 = size(Instance%PtZ)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%PtZ, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SINGULARITE = err
                 MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to deallocate SINGULARITE_T.PtZ'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PtZ) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%PtZ(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SINGULARITE = err
              MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to allocate SINGULARITE_T.PtZ'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Weir.PtX') > 0) then
        if (ASSOCIATED(Instance%PtX)) then
           t1 = size(Instance%PtX)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%PtX, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SINGULARITE = err
                 MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to deallocate SINGULARITE_T.PtX'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PtX) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%PtX(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SINGULARITE = err
              MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to allocate SINGULARITE_T.PtX'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.Weir.PtY') > 0) then
        if (ASSOCIATED(Instance%PtY)) then
           t1 = size(Instance%PtY)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%PtY, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_SINGULARITE = err
                 MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to deallocate SINGULARITE_T.PtY'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PtY) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%PtY(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_SINGULARITE = err
              MessageErreur = 'SET_TAILLE_VAR_SINGULARITE : Unable to allocate SINGULARITE_T.PtY'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_SINGULARITE = 1
         MessageErreur         = "SET_TAILLE_VAR_SINGULARITE - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_SINGULARITE

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_SINGULARITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_SINGULARITE     ! different de 0 si erreur
      type(SINGULARITE_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_SINGULARITE = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Weir.RelAbscissa') > 0) then
         valeur = Instance%AbscisseRel
      else if ( index(NomVar, 'Model.Weir.CrestLevel') > 0) then
         valeur = Instance%CoteCrete
      else if ( index(NomVar, 'Model.Weir.DischCoef') > 0) then
         valeur = Instance%CoeffDebit
      else if ( index(NomVar, 'Model.Weir.BrkLevel') > 0) then
         valeur = Instance%CoteRupture
      else if ( index(NomVar, 'Model.Weir.Slope') > 0) then
         valeur = Instance%Pente
      else if ( index(NomVar, 'Model.Weir.Discharge') > 0) then
         valeur = Instance%Debit
      else if ( index(NomVar, 'Model.Weir.GateLength') > 0) then
         valeur = Instance%LargeurVanne
      else if ( index(NomVar, 'Model.Weir.PtQ') > 0) then
         valeur = Instance%PtQ(index1)
      else if ( index(NomVar, 'Model.Weir.PtZus') > 0) then
         valeur = Instance%PtZamont(index1, index2)
      else if ( index(NomVar, 'Model.Weir.PtZds') > 0) then
         valeur = Instance%PtZaval(index1)
      else if ( index(NomVar, 'Model.Weir.PtZup') > 0) then
         valeur = Instance%PtZsup
      else if ( index(NomVar, 'Model.Weir.PtZlo') > 0) then
         valeur = Instance%PtZinf
      else if ( index(NomVar, 'Model.Weir.PtZ') > 0) then
         valeur = Instance%PtZ(index1)
      else if ( index(NomVar, 'Model.Weir.PtX') > 0) then
         valeur = Instance%PtX(index1)
      else if ( index(NomVar, 'Model.Weir.PtY') > 0) then
         valeur = Instance%PtY(index1)
      else
         GET_DOUBLE_SINGULARITE = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_SINGULARITE - Unknown variable name"
      end if
   end function GET_DOUBLE_SINGULARITE


   function GET_INT_SINGULARITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_SINGULARITE        ! different de 0 si erreur
      type(SINGULARITE_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_SINGULARITE = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Weir.Number') > 0) then
         valeur = Instance%Numero
      else if ( index(NomVar, 'Model.Weir.ReachNum') > 0) then
         valeur = Instance%NumBranche
      else if ( index(NomVar, 'Model.Weir.Node') > 0) then
         valeur = Instance%Section
      else if ( index(NomVar, 'Model.Weir.Type') > 0) then
         valeur = Instance%Type
      else if ( index(NomVar, 'Model.Weir.Thickness') > 0) then
         valeur = Instance%Epaisseur_seuil
      else if ( index(NomVar, 'Model.Weir.GraphNum') > 0) then
         valeur = Instance%NumeroLoi
      else
         GET_INT_SINGULARITE = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_SINGULARITE - Unknown variable name"
      end if
   end function GET_INT_SINGULARITE


   function GET_BOOL_SINGULARITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_BOOL_SINGULARITE       ! different de 0 si erreur
      type(SINGULARITE_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(out):: valeur                     ! valeur du logical de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_BOOL_SINGULARITE = 0
      valeur                = .FALSE.
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Weir.State') > 0) then
         valeur = Instance%EtatEfface
      else
         GET_BOOL_SINGULARITE = 1
         valeur                = .FALSE.
         MessageErreur         = "GET_BOOL_SINGULARITE - Unknown variable name"
      end if
   end function GET_BOOL_SINGULARITE


   function GET_STRING_SINGULARITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_SINGULARITE     ! different de 0 si erreur
      type(SINGULARITE_T),    intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_SINGULARITE = 0
      valeur                = ""
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Weir.Name') > 0) then
         valeur = Instance%Nom
      else
         GET_STRING_SINGULARITE = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_SINGULARITE - Unknown variable name"
      end if
   end function GET_STRING_SINGULARITE



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_SINGULARITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_SINGULARITE     ! different de 0 si erreur
      type(SINGULARITE_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_SINGULARITE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Weir.RelAbscissa') > 0) then
         Instance%AbscisseRel = valeur
      else if ( index(NomVar, 'Model.Weir.CrestLevel') > 0) then
         Instance%CoteCrete = valeur
      else if ( index(NomVar, 'Model.Weir.DischCoef') > 0) then
         Instance%CoeffDebit = valeur
      else if ( index(NomVar, 'Model.Weir.BrkLevel') > 0) then
         Instance%CoteRupture = valeur
      else if ( index(NomVar, 'Model.Weir.Slope') > 0) then
         Instance%Pente = valeur
      else if ( index(NomVar, 'Model.Weir.Discharge') > 0) then
         Instance%Debit = valeur
      else if ( index(NomVar, 'Model.Weir.GateLength') > 0) then
         Instance%LargeurVanne = valeur
      else if ( index(NomVar, 'Model.Weir.PtQ') > 0) then
         Instance%PtQ(index1) = valeur
      else if ( index(NomVar, 'Model.Weir.PtZus') > 0) then
         Instance%PtZamont(index1, index2) = valeur
      else if ( index(NomVar, 'Model.Weir.PtZds') > 0) then
         Instance%PtZaval(index1) = valeur
      else if ( index(NomVar, 'Model.Weir.PtZup') > 0) then
         Instance%PtZsup = valeur
      else if ( index(NomVar, 'Model.Weir.PtZlo') > 0) then
         Instance%PtZinf = valeur
      else if ( index(NomVar, 'Model.Weir.PtZ') > 0) then
         Instance%PtZ(index1) = valeur
      else if ( index(NomVar, 'Model.Weir.PtX') > 0) then
         Instance%PtX(index1) = valeur
      else if ( index(NomVar, 'Model.Weir.PtY') > 0) then
         Instance%PtY(index1) = valeur
      else
         SET_DOUBLE_SINGULARITE = 1
         MessageErreur         = "SET_DOUBLE_SINGULARITE - Unknown variable name"
      end if
   end function SET_DOUBLE_SINGULARITE


   function SET_INT_SINGULARITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_SINGULARITE        ! different de 0 si erreur
      type(SINGULARITE_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_SINGULARITE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Weir.Number') > 0) then
         Instance%Numero = valeur
      else if ( index(NomVar, 'Model.Weir.ReachNum') > 0) then
         Instance%NumBranche = valeur
      else if ( index(NomVar, 'Model.Weir.Node') > 0) then
         Instance%Section = valeur
      else if ( index(NomVar, 'Model.Weir.Type') > 0) then
         Instance%Type = valeur
      else if ( index(NomVar, 'Model.Weir.Thickness') > 0) then
         Instance%Epaisseur_seuil = valeur
      else if ( index(NomVar, 'Model.Weir.GraphNum') > 0) then
         Instance%NumeroLoi = valeur
      else
         SET_INT_SINGULARITE = 1
         MessageErreur         = "SET_INT_SINGULARITE - Unknown variable name"
      end if
   end function SET_INT_SINGULARITE


   function SET_BOOL_SINGULARITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_BOOL_SINGULARITE       ! different de 0 si erreur
      type(SINGULARITE_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(in) :: valeur                     ! valeur du logical de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_BOOL_SINGULARITE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Weir.State') > 0) then
         Instance%EtatEfface = valeur
      else
         SET_BOOL_SINGULARITE = 1
         MessageErreur         = "SET_BOOL_SINGULARITE - Unknown variable name"
      end if
   end function SET_BOOL_SINGULARITE


   function SET_STRING_SINGULARITE(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_SINGULARITE     ! different de 0 si erreur
      type(SINGULARITE_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_SINGULARITE = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Weir.Name') > 0) then
         Instance%Nom = valeur(1:30)
      else
         SET_STRING_SINGULARITE = 1
         MessageErreur         = "SET_STRING_SINGULARITE - Unknown variable name"
      end if
   end function SET_STRING_SINGULARITE



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_SINGULARITE(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_SINGULARITE      ! different de 0 si erreur
      type(SINGULARITE_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_SINGULARITE = 0
      MessageErreur         = ""
      err                   = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%PtZ)) then
          taille = SIZE(Instance%PtZ)
          if (taille > 0) then
              DEALLOCATE(Instance%PtZ, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SINGULARITE = err
                  MessageErreur = 'Unable to deallocate SINGULARITE_T.PtZ'
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
                  DESALLOUE_SINGULARITE = err
                  MessageErreur = 'Unable to deallocate SINGULARITE_T.PtQ'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PtQ)
      if (ASSOCIATED(Instance%PtZamont)) then
          taille = SIZE(Instance%PtZamont, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%PtZamont, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SINGULARITE = err
                  MessageErreur = 'Unable to deallocate SINGULARITE_T.PtZamont'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PtZamont)
      if (ASSOCIATED(Instance%PtZaval)) then
          taille = SIZE(Instance%PtZaval)
          if (taille > 0) then
              DEALLOCATE(Instance%PtZaval, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SINGULARITE = err
                  MessageErreur = 'Unable to deallocate SINGULARITE_T.PtZaval'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PtZaval)
      if (ASSOCIATED(Instance%PtX)) then
          taille = SIZE(Instance%PtX)
          if (taille > 0) then
              DEALLOCATE(Instance%PtX, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SINGULARITE = err
                  MessageErreur = 'Unable to deallocate SINGULARITE_T.PtX'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PtX)
      if (ASSOCIATED(Instance%PtY)) then
          taille = SIZE(Instance%PtY)
          if (taille > 0) then
              DEALLOCATE(Instance%PtY, STAT=err)
              if (err /= 0) then
                  DESALLOUE_SINGULARITE = err
                  MessageErreur = 'Unable to deallocate SINGULARITE_T.PtY'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PtY)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_SINGULARITE

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_SINGULARITE(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_SINGULARITE      ! different de 0 si erreur
      type(SINGULARITE_T),    intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_SINGULARITE = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%PtZ)
      NULLIFY(Instance%PtQ)
      NULLIFY(Instance%PtZamont)
      NULLIFY(Instance%PtZaval)
      NULLIFY(Instance%PtX)
      NULLIFY(Instance%PtY)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_SINGULARITE

end module M_SINGULARITE_T
