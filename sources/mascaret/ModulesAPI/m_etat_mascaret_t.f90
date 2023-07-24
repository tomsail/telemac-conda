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

module M_ETAT_MASCARET_T
!***********************************************************************
! PROGICIEL : MASCARET        J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

!=========================== Declarations ==============================

use M_PRECISION
use M_ETAT_LIAISON_T           ! Type ETAT_LIAISON_T
use M_ETAT_CASIER_T            ! Type ETAT_CASIER_T
use M_REZOMAT_T                ! Definition du type REZOMAT_T
use M_SAUVE_T                  ! Definition du type SAUVE_T
use M_ETAT_TRACER_T            ! Type ETAT_TRACER_T

Type ETAT_MASCARET_T
    sequence
    real(DOUBLE), dimension(:), pointer        :: DPDZ2    => null()
    real(DOUBLE), dimension(:), pointer        :: DPDZ1    => null()
    real(DOUBLE), dimension(:), pointer        :: QDeverse => null()
    real(DOUBLE), dimension(:), pointer        :: Qinjec   => null()
    real(DOUBLE), dimension(:,:), pointer      :: Airs     => null()
    real(DOUBLE), dimension(:,:,:), pointer    :: W        => null()
    real(DOUBLE), dimension(:), pointer        :: YNode    => null()
    real(DOUBLE), dimension(:), pointer        :: CNode    => null()
    real(DOUBLE), dimension(:), pointer        :: UNode    => null()
    real(DOUBLE), dimension(:), pointer        :: XFron    => null()
    real(DOUBLE), dimension(:), pointer        :: RH2      => null()
    real(DOUBLE), dimension(:), pointer        :: RH1      => null()
    real(DOUBLE), dimension(:), pointer        :: BS       => null()
    real(DOUBLE), dimension(:), pointer        :: B2       => null()
    real(DOUBLE), dimension(:), pointer        :: B1       => null()
    real(DOUBLE), dimension(:), pointer        :: P2       => null()
    real(DOUBLE), dimension(:), pointer        :: P1       => null()
    real(DOUBLE), dimension(:), pointer        :: Froude   => null()
    real(DOUBLE), dimension(:), pointer        :: Beta     => null()
    real(DOUBLE), dimension(:), pointer        :: S2       => null()
    real(DOUBLE), dimension(:), pointer        :: S1       => null()
    real(DOUBLE), dimension(:), pointer        :: SS       => null() ! Section stockage
    real(DOUBLE), dimension(:), pointer        :: Q2       => null()
    real(DOUBLE), dimension(:), pointer        :: Q1       => null()
    real(DOUBLE), dimension(:), pointer        :: V1       => null() ! Vitesse dans le lit mineur
    real(DOUBLE), dimension(:), pointer        :: V2       => null() ! Vitesse dans le lit majeur
    real(DOUBLE), dimension(:), pointer        :: Y        => null() ! Hauteur d'eau
    real(DOUBLE), dimension(:), pointer        :: VOL      => null() ! Volume du lit actif
    real(DOUBLE), dimension(:), pointer        :: VOLS     => null() ! Volume de stockage
    real(DOUBLE)                               :: tempsPrecedent = 0.
    integer                                    :: numPasTps = 0 ! num_pas : numero de pas de temps
    integer                                    :: phaseSimulation !
    real(DOUBLE)                               :: DT = 0. ! pas te temps
    real(DOUBLE), dimension(:), pointer        :: Q        => null()
    real(DOUBLE), dimension(:), pointer        :: Z        => null()
    type(ETAT_LIAISON_T),     dimension(:), pointer :: Liaisons => null()
    type(ETAT_CASIER_T),      dimension(:), pointer :: Casiers  => null()
    type(ETAT_TRACER_T)                        :: Tracer              ! Structure pour les traceurs
    integer     , dimension(:), pointer        :: JGNODE    => null() ! indice de planimetrage GNode
    integer     , dimension(:), pointer        :: JDNODE    => null() ! indice de planimetrage JDNODE
    integer     , dimension(:), pointer        :: IFIGE     => null() ! indice de planimetrage Fige
    real(DOUBLE), dimension(:,:), pointer      :: FLUX      => null() ! Flux pour le solveur de Roe
    real(DOUBLE), dimension(:)  , pointer      :: DebitFlux => null() ! Flux de masse
    real(DOUBLE)                               :: DTRezo = 0.     ! Pas de temps REZO
    type(REZOMAT_T)                            :: MatriceRezo ! Matrice du reseau
    integer                                    :: NBARAD   = 0  ! NBARAD
    integer     , dimension(:), pointer        :: IDEB  => null() ! LIMITE DE DEBUT DE LA ZONE DE CALCUL PAR BIEF
    integer     , dimension(:), pointer        :: IFIN  => null() ! LIMITE DE FIN DE LA ZONE DE CALCUL PAR BIEF
    integer     , dimension(:), pointer        :: ITEM0 => null() ! ITEM0
    type(Sauve_T)                              :: Sauve
    real(DOUBLE), dimension(:), pointer        :: ZINIT => null() ! Ligne initiale

end type ETAT_MASCARET_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_ETAT_MASCARET(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

        tabNomVar(i)         ="State.DPDZ2"
        tabDescriptionVar(i) ="Derivative of the floodplain wetted perimeter with respect to the level"
        i=i+1
        tabNomVar(i)         ="State.DPDZ1"
        tabDescriptionVar(i) ="Derivative of main channel wetted perimeter with respect to the level"
        i=i+1
        tabNomVar(i)         ="State.Qspilled"
        tabDescriptionVar(i) ="Spilled discharge (lateral weir)"
        i=i+1
        tabNomVar(i)         ="State.Qinflow"
        tabDescriptionVar(i) ="Inflow discharge"
        i=i+1
        tabNomVar(i)         ="State.Airs"
        tabDescriptionVar(i) ="State of the 2D junction"
        i=i+1
        tabNomVar(i)         ="State.W"
        tabDescriptionVar(i) ="State of the 2D junction"
        i=i+1
        tabNomVar(i)         ="State.YNode"
        tabDescriptionVar(i) ="Water depth (super-critical kernel)"
        i=i+1
        tabNomVar(i)         ="State.CNode"
        tabDescriptionVar(i) ="Celerity (super-critical kernel)"
        i=i+1
        tabNomVar(i)         ="State.UNode"
        tabDescriptionVar(i) ="Speed (super-critical kernel)"
        i=i+1
        tabNomVar(i)         ="State.XFron"
        tabDescriptionVar(i) ="Front wave position"
        i=i+1
        tabNomVar(i)         ="State.RH2"
        tabDescriptionVar(i) ="Hydraulic radius of the floodplain"
        i=i+1
        tabNomVar(i)         ="State.RH1"
        tabDescriptionVar(i) ="Hydraulic radius of the main channel"
        i=i+1
        tabNomVar(i)         ="State.BS"
        tabDescriptionVar(i) ="Width of the ineffective flow area"
        i=i+1
        tabNomVar(i)         ="State.B2"
        tabDescriptionVar(i) ="Width of the floodplain"
        i=i+1
        tabNomVar(i)         ="State.B1"
        tabDescriptionVar(i) ="Width of the main channelr"
        i=i+1
        tabNomVar(i)         ="State.P2"
        tabDescriptionVar(i) ="Floodplain wetted perimeter"
        i=i+1
        tabNomVar(i)         ="State.P1"
        tabDescriptionVar(i) ="Main channel wetted perimeter"
        i=i+1
        tabNomVar(i)         ="State.Froude"
        tabDescriptionVar(i) ="Froude number"
        i=i+1
        tabNomVar(i)         ="State.Beta"
        tabDescriptionVar(i) ="beta coefficient of the Debord's formula"
        i=i+1
        tabNomVar(i)         ="State.S2"
        tabDescriptionVar(i) ="Floodplain wetted area"
        i=i+1
        tabNomVar(i)         ="State.S1"
        tabDescriptionVar(i) ="Main channel wetted area"
        i=i+1
        tabNomVar(i)         ="State.SS"
        tabDescriptionVar(i) ="Wetted ineffective flow area"
        i=i+1
        tabNomVar(i)         ="State.Q2"
        tabDescriptionVar(i) ="Floodplain discharge"
        i=i+1
        tabNomVar(i)         ="State.Q1"
        tabDescriptionVar(i) ="Main channel discharge"
        i=i+1
        tabNomVar(i)         ="State.V1"
        tabDescriptionVar(i) ="Velocity in main channel"
        i=i+1
        tabNomVar(i)         ="State.V2"
        tabDescriptionVar(i) ="Velocity in floodplain"
        i=i+1
        tabNomVar(i)         ="State.Y"
        tabDescriptionVar(i) ="Water depth"
        i=i+1
        tabNomVar(i)         ="State.VOL"
        tabDescriptionVar(i) ="Volume of the active channel"
        i=i+1
        tabNomVar(i)         ="State.VOLS"
        tabDescriptionVar(i) ="Volume of the ineffective flow areas"
        i=i+1
        tabNomVar(i)         ="State.PreviousTime"
        tabDescriptionVar(i) ="Previous Time"
        i=i+1
        tabNomVar(i)         ="State.TimeStepNum"
        tabDescriptionVar(i) ="Time step number"
        i=i+1
        tabNomVar(i)         ="State.SimulPhase"
        tabDescriptionVar(i) ="Indicator of the simulation phase"
        i=i+1
        tabNomVar(i)         ="State.DT"
        tabDescriptionVar(i) ="Time step (s)"
        i=i+1
        tabNomVar(i)         ="State.Q"
        tabDescriptionVar(i) ="Total discharge (m3/s)"
        i=i+1
        tabNomVar(i)         ="State.Z"
        tabDescriptionVar(i) ="Water level (m)"
        i=i+1

        ! --- ETAT_LIAISON_T ---
        call GET_TAB_VAR_ETAT_LIAISON(i, tabNomVar, tabDescriptionVar)

        ! --- ETAT_CASIER_T ---

        call GET_TAB_VAR_ETAT_CASIER(i, tabNomVar, tabDescriptionVar)

        ! --- ETAT_TRACER_T ---

        call GET_TAB_VAR_ETAT_TRACER(i, tabNomVar, tabDescriptionVar)

        ! --- RACINE Etat Mascaret (suite) ---
        tabNomVar(i)         ="State.JGNODE"
        tabDescriptionVar(i) ="index of the vertical discretisation GNode"
        i=i+1
        tabNomVar(i)         ="State.JDNODE"
        tabDescriptionVar(i) ="index of the vertical discretisation DNODE"
        i=i+1
        tabNomVar(i)         ="State.IFIGE"
        tabDescriptionVar(i) ="index of the vertical discretisation  Fige"
        i=i+1
        tabNomVar(i)         ="State.FLUX"
        tabDescriptionVar(i) ="Flux of the Roe's solver (FV)"
        i=i+1
        tabNomVar(i)         ="State.Flow"
        tabDescriptionVar(i) ="Mass flow"
        i=i+1
        tabNomVar(i)         ="State.DTRezo"
        tabDescriptionVar(i) ="Time step (REZO - subcritical kernel)"
        i=i+1

        ! --- REZOMAT_T ---
        call GET_TAB_VAR_REZOMAT(i, tabNomVar, tabDescriptionVar)

        ! --- RACINE Etat Mascaret (suite 2) ---
        tabNomVar(i)         ="State.NBARAD"
        tabDescriptionVar(i) ="Number of downstream dams (super-critical kernel)"
        i=i+1
        tabNomVar(i)         ="State.IDEB"
        tabDescriptionVar(i) ="Beginning of the computation area per reach (super-critical kernel)"
        i=i+1
        tabNomVar(i)         ="State.IFIN"
        tabDescriptionVar(i) ="End of the computation area per reach (super-critical kernel)"
        i=i+1
        tabNomVar(i)         ="State.ITEM0"
        tabDescriptionVar(i) ="ITEM0 (super-critical kernel)"
        i=i+1
        ! --- SAUVE_T ---
        call GET_TAB_VAR_SAUVE(i, tabNomVar, tabDescriptionVar)

        tabNomVar(i)         ="State.ZINIT"
        tabDescriptionVar(i) ="Initial water level (super-critical kernel)"
        i=i+1


      return

    end subroutine GET_TAB_VAR_ETAT_MASCARET

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_ETAT_MASCARET(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_ETAT_MASCARET    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_ETAT_MASCARET = 0
      TypeVar               = ""
      Categorie             = "STATE"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar,'State.DPDZ2') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.DPDZ1') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.Qspilled') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.Qinflow') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.Airs') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 2
      else if ( index(NomVar,'State.W') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 3
      else if ( index(NomVar,'State.YNode') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.CNode') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.UNode') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.XFron') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.RH2') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.RH1') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.BS') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.B2') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.B1') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.P2') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.P1') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.Froude') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.Beta') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.S2') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.S1') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.SS') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.Q2') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.Q1') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.V1') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.V2') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.Y') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.VOLS') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.VOL') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.PreviousTime') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar,'State.TimeStepNum') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar,'State.SimulPhase') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar,'State.DTRezo') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar,'State.DT') > 0) then
         TypeVar = 'DOUBLE'
         dimVar                = 0
      else if ( index(NomVar,'State.Q') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.ZINIT') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( index(NomVar,'State.Z') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( INDEX(NomVar,'State.Link.') > 0) then
          GET_TYPE_VAR_ETAT_MASCARET = GET_TYPE_VAR_ETAT_LIAISON(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
      else if ( INDEX(NomVar,'State.StoArea.') > 0) then
          GET_TYPE_VAR_ETAT_MASCARET = GET_TYPE_VAR_ETAT_CASIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
      else if ( INDEX(NomVar,'State.Tracer.') > 0) then
          GET_TYPE_VAR_ETAT_MASCARET = GET_TYPE_VAR_ETAT_TRACER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else if ( index(NomVar,'State.JGNODE') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 1
      else if ( index(NomVar,'State.JDNODE') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 1
      else if ( index(NomVar,'State.IFIGE') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 1
      else if ( index(NomVar,'State.FLUX') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 2
      else if ( index(NomVar,'State.Flow') > 0) then
         TypeVar = 'TABDOUBLE'
         dimVar                = 1
      else if ( INDEX(NomVar,'State.Rezomat.') > 0) then
          GET_TYPE_VAR_ETAT_MASCARET = GET_TYPE_VAR_REZOMAT(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)

      else if ( index(NomVar,'State.NBARAD') > 0) then
         TypeVar = 'INT'
         dimVar                = 0
      else if ( index(NomVar,'State.IDEB') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 1
      else if ( index(NomVar,'State.IFIN') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 1
      else if ( index(NomVar,'State.ITEM0') > 0) then
         TypeVar = 'TABINT'
         dimVar                = 1
      else if ( INDEX(NomVar,'State.Save.') > 0) then
          GET_TYPE_VAR_ETAT_MASCARET = GET_TYPE_VAR_SAUVE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)

      else
        GET_TYPE_VAR_ETAT_MASCARET = 1
        TypeVar = "?"
        Categorie             = "STATE"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_ETAT_MASCARET - Unknown variable name "//TRIM(NomVar)
      end if


    end function GET_TYPE_VAR_ETAT_MASCARET

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_ETAT_MASCARET(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_ETAT_MASCARET   ! different de 0 si erreur
      type(ETAT_MASCARET_T),  intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      GET_TAILLE_VAR_ETAT_MASCARET = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar,'State.DPDZ2') > 0) then
         if (ASSOCIATED(Instance%DPDZ2)) then
            taille1 = size(Instance%DPDZ2)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.DPDZ1') > 0) then
         if (ASSOCIATED(Instance%DPDZ1)) then
            taille1 = size(Instance%DPDZ1)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Qspilled') > 0) then
         if (ASSOCIATED(Instance%QDeverse)) then
            taille1 = size(Instance%QDeverse)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Qinflow') > 0) then
         if (ASSOCIATED(Instance%Qinjec)) then
            taille1 = size(Instance%Qinjec)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Airs') > 0) then
         if (ASSOCIATED(Instance%Airs)) then
            taille1 = size(Instance%Airs, 1)
            taille2 = size(Instance%Airs, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar,'State.W') > 0) then
         if (ASSOCIATED(Instance%W)) then
            taille1 = size(Instance%W, 1)
            taille2 = size(Instance%W, 2)
            taille3 = size(Instance%W, 3)
         else
            taille1 = 0
            taille2 = 0
            taille3 = 0
         endif
      else if ( index(NomVar,'State.YNode') > 0) then
         if (ASSOCIATED(Instance%YNode)) then
            taille1 = size(Instance%YNode)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.CNode') > 0) then
         if (ASSOCIATED(Instance%CNode)) then
            taille1 = size(Instance%CNode)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.UNode') > 0) then
         if (ASSOCIATED(Instance%UNode)) then
            taille1 = size(Instance%UNode)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.XFron') > 0) then
         if (ASSOCIATED(Instance%XFron)) then
            taille1 = size(Instance%XFron)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.RH2') > 0) then
         if (ASSOCIATED(Instance%RH2)) then
            taille1 = size(Instance%RH2)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.RH1') > 0) then
         if (ASSOCIATED(Instance%RH1)) then
            taille1 = size(Instance%RH1)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.BS') > 0) then
         if (ASSOCIATED(Instance%BS)) then
            taille1 = size(Instance%BS)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.B2') > 0) then
         if (ASSOCIATED(Instance%B2)) then
            taille1 = size(Instance%B2)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.B1') > 0) then
         if (ASSOCIATED(Instance%B1)) then
            taille1 = size(Instance%B1)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.P2') > 0) then
         if (ASSOCIATED(Instance%P2)) then
            taille1 = size(Instance%P2)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.P1') > 0) then
         if (ASSOCIATED(Instance%P1)) then
            taille1 = size(Instance%P1)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Froude') > 0) then
         if (ASSOCIATED(Instance%Froude)) then
            taille1 = size(Instance%Froude)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Beta') > 0) then
         if (ASSOCIATED(Instance%Beta)) then
            taille1 = size(Instance%Beta)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.S2') > 0) then
         if (ASSOCIATED(Instance%S2)) then
            taille1 = size(Instance%S2)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.S1') > 0) then
         if (ASSOCIATED(Instance%S1)) then
            taille1 = size(Instance%S1)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.SS') > 0) then
         if (ASSOCIATED(Instance%SS)) then
            taille1 = size(Instance%SS)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Q2') > 0) then
         if (ASSOCIATED(Instance%Q2)) then
            taille1 = size(Instance%Q2)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Q1') > 0) then
         if (ASSOCIATED(Instance%Q1)) then
            taille1 = size(Instance%Q1)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.V1') > 0) then
         if (ASSOCIATED(Instance%V1)) then
            taille1 = size(Instance%V1)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.V2') > 0) then
         if (ASSOCIATED(Instance%V2)) then
            taille1 = size(Instance%V2)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Y') > 0) then
         if (ASSOCIATED(Instance%Y)) then
            taille1 = size(Instance%Y)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.VOLS') > 0) then
         if (ASSOCIATED(Instance%VOLS)) then
            taille1 = size(Instance%VOLS)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.VOL') > 0) then
         if (ASSOCIATED(Instance%VOL)) then
            taille1 = size(Instance%VOL)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.PreviousTime') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.TimeStepNum') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.SimulPhase') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.DTRezo') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.DT') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Q') > 0) then
         if (ASSOCIATED(Instance%Q)) then
            taille1 = size(Instance%Q)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.ZINIT') > 0) then
         if (ASSOCIATED(Instance%ZINIT)) then
            taille1 = size(Instance%ZINIT)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.Z') > 0) then
         if (ASSOCIATED(Instance%Z)) then
            taille1 = size(Instance%Z)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.JGNODE') > 0) then
         if (ASSOCIATED(Instance%JGNODE)) then
            taille1 = size(Instance%JGNODE)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.JDNODE') > 0) then
         if (ASSOCIATED(Instance%JDNODE)) then
            taille1 = size(Instance%JDNODE)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.IFIGE') > 0) then
         if (ASSOCIATED(Instance%IFIGE)) then
            taille1 = size(Instance%IFIGE)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.FLUX') > 0) then
         if (ASSOCIATED(Instance%FLUX)) then
            taille1 = size(Instance%FLUX, 1)
            taille2 = size(Instance%FLUX, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar,'State.Flow') > 0) then
         if (ASSOCIATED(Instance%DebitFlux)) then
            taille1 = size(Instance%DebitFlux)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.NBARAD') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.IDEB') > 0) then
         if (ASSOCIATED(Instance%IDEB)) then
            taille1 = size(Instance%IDEB)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.IFIN') > 0) then
         if (ASSOCIATED(Instance%IFIN)) then
            taille1 = size(Instance%IFIN)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar,'State.ITEM0') > 0) then
         if (ASSOCIATED(Instance%ITEM0)) then
            taille1 = size(Instance%ITEM0)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if (INDEX(NomVar,'State.Link.') > 0) then
         if (ASSOCIATED(Instance%Liaisons)) then
            taille1 = size(Instance%Liaisons)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             GET_TAILLE_VAR_ETAT_MASCARET = GET_TAILLE_VAR_ETAT_LIAISON(Instance%Liaisons(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'State.StoArea.') > 0) then
         if (ASSOCIATED(Instance%Casiers)) then
            taille1 = size(Instance%Casiers)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             GET_TAILLE_VAR_ETAT_MASCARET = GET_TAILLE_VAR_ETAT_CASIER(Instance%Casiers(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'State.Tracer.') > 0) then
         GET_TAILLE_VAR_ETAT_MASCARET = GET_TAILLE_VAR_ETAT_TRACER(Instance%Tracer,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'State.Rezomat.') > 0) then
         GET_TAILLE_VAR_ETAT_MASCARET = GET_TAILLE_VAR_REZOMAT(Instance%MatriceRezo,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'State.Save.') > 0) then
         GET_TAILLE_VAR_ETAT_MASCARET = GET_TAILLE_VAR_SAUVE(Instance%Sauve,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else
         GET_TAILLE_VAR_ETAT_MASCARET = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_ETAT_MASCARET - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_ETAT_MASCARET

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_ETAT_MASCARET(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_ETAT_MASCARET   ! different de 0 si erreur
      type(ETAT_MASCARET_T),  intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err
      integer i, bidon
      character(LEN=256)                 :: MessageErreurType

      SET_TAILLE_VAR_ETAT_MASCARET = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar,'State.DPDZ2') > 0) then
        if (ASSOCIATED(Instance%DPDZ2)) then
           t1 = size(Instance%DPDZ2)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%DPDZ2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.DPDZ2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DPDZ2) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%DPDZ2(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.DPDZ2'
              return
           endif
        endif
      else if ( index(NomVar,'State.DPDZ1') > 0) then
        if (ASSOCIATED(Instance%DPDZ1)) then
           t1 = size(Instance%DPDZ1)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%DPDZ1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.DPDZ1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DPDZ1) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%DPDZ1(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.DPDZ1'
              return
           endif
        endif
      else if ( index(NomVar,'State.Qspilled') > 0) then
        if (ASSOCIATED(Instance%QDeverse)) then
           t1 = size(Instance%QDeverse)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%QDeverse, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.QDeverse'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%QDeverse) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%QDeverse(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.QDeverse'
              return
           endif
        endif
      else if ( index(NomVar,'State.Qinflow') > 0) then
        if (ASSOCIATED(Instance%Qinjec)) then
           t1 = size(Instance%Qinjec)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Qinjec, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.Qinjec'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Qinjec) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Qinjec(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.Qinjec'
              return
           endif
        endif
     else if ( index(NomVar,'State.Airs') > 0) then
        if (ASSOCIATED(Instance%Airs)) then
           t1 = size(Instance%Airs, 1)
           t2 = size(Instance%Airs, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%Airs, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.Airs'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Airs).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%Airs(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.Airs'
              return
           endif
        endif
      else if ( index(NomVar,'State.W') > 0) then
        if (ASSOCIATED(Instance%W)) then
           t1 = size(Instance%W, 1)
           t2 = size(Instance%W, 2)
           t3 = size(Instance%W, 3)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2).OR.(t3 /= NewT3) ) then
              DEALLOCATE(Instance%W, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.W'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%W).OR.(t1/=NewT1).OR.(t2/=NewT2).OR.(t3/=NewT3)) then
           ALLOCATE(Instance%W(NewT1, NewT2, NewT3), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.W'
              return
           endif
        endif
      else if ( index(NomVar,'State.YNode') > 0) then
        if (ASSOCIATED(Instance%YNode)) then
           t1 = size(Instance%YNode)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%YNode, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.YNode'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%YNode) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%YNode(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.YNode'
              return
           endif
        endif
      else if ( index(NomVar,'State.CNode') > 0) then
        if (ASSOCIATED(Instance%CNode)) then
           t1 = size(Instance%CNode)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%CNode, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.CNode'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%CNode) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%CNode(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.CNode'
              return
           endif
        endif
      else if ( index(NomVar,'State.UNode') > 0) then
        if (ASSOCIATED(Instance%UNode)) then
           t1 = size(Instance%UNode)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%UNode, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.UNode'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%UNode) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%UNode(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.UNode'
              return
           endif
        endif
      else if ( index(NomVar,'State.XFron') > 0) then
        if (ASSOCIATED(Instance%XFron)) then
           t1 = size(Instance%XFron)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%XFron, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.XFron'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%XFron) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%XFron(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.XFron'
              return
           endif
        endif
      else if ( index(NomVar,'State.RH2') > 0) then
        if (ASSOCIATED(Instance%RH2)) then
           t1 = size(Instance%RH2)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%RH2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.RH2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%RH2) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%RH2(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.RH2'
              return
           endif
        endif
      else if ( index(NomVar,'State.RH1') > 0) then
        if (ASSOCIATED(Instance%RH1)) then
           t1 = size(Instance%RH1)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%RH1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.RH1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%RH1) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%RH1(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.RH1'
              return
           endif
        endif
      else if ( index(NomVar,'State.BS') > 0) then
        if (ASSOCIATED(Instance%BS)) then
           t1 = size(Instance%BS)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%BS, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.BS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%BS) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%BS(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.BS'
              return
           endif
        endif
      else if ( index(NomVar,'State.B2') > 0) then
        if (ASSOCIATED(Instance%B2)) then
           t1 = size(Instance%B2)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%B2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.B2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%B2) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%B2(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.B2'
              return
           endif
        endif
      else if ( index(NomVar,'State.B1') > 0) then
        if (ASSOCIATED(Instance%B1)) then
           t1 = size(Instance%B1)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%B1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.B1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%B1) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%B1(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.B1'
              return
           endif
        endif
      else if ( index(NomVar,'State.P2') > 0) then
        if (ASSOCIATED(Instance%P2)) then
           t1 = size(Instance%P2)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%P2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.P2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%P2) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%P2(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.P2'
              return
           endif
        endif
      else if ( index(NomVar,'State.P1') > 0) then
        if (ASSOCIATED(Instance%P1)) then
           t1 = size(Instance%P1)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%P1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.P1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%P1) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%P1(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.P1'
              return
           endif
        endif
      else if ( index(NomVar,'State.Froude') > 0) then
        if (ASSOCIATED(Instance%Froude)) then
           t1 = size(Instance%Froude)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Froude, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.Froude'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Froude) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Froude(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.Froude'
              return
           endif
        endif
      else if ( index(NomVar,'State.Beta') > 0) then
        if (ASSOCIATED(Instance%Beta)) then
           t1 = size(Instance%Beta)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Beta, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.Beta'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Beta) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Beta(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.Beta'
              return
           endif
        endif
      else if ( index(NomVar,'State.S2') > 0) then
        if (ASSOCIATED(Instance%S2)) then
           t1 = size(Instance%S2)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%S2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.S2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S2) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%S2(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.S2'
              return
           endif
        endif
      else if ( index(NomVar,'State.S1') > 0) then
        if (ASSOCIATED(Instance%S1)) then
           t1 = size(Instance%S1)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%S1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.S1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%S1) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%S1(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.S1'
              return
           endif
        endif
      else if ( index(NomVar,'State.SS') > 0) then
        if (ASSOCIATED(Instance%SS)) then
           t1 = size(Instance%SS)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%SS, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.SS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SS) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%SS(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.SS'
              return
           endif
        endif
      else if ( index(NomVar,'State.Q2') > 0) then
        if (ASSOCIATED(Instance%Q2)) then
           t1 = size(Instance%Q2)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Q2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.Q2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Q2) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Q2(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.Q2'
              return
           endif
        endif
      else if ( index(NomVar,'State.Q1') > 0) then
        if (ASSOCIATED(Instance%Q1)) then
           t1 = size(Instance%Q1)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Q1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.Q1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Q1) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Q1(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.Q1'
              return
           endif
        endif
      else if ( index(NomVar,'State.V1') > 0) then
        if (ASSOCIATED(Instance%V1)) then
           t1 = size(Instance%V1)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%V1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.V1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%V1) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%V1(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.V1'
              return
           endif
        endif
      else if ( index(NomVar,'State.V2') > 0) then
        if (ASSOCIATED(Instance%V2)) then
           t1 = size(Instance%V2)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%V2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.V2'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%V2) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%V2(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.V2'
              return
           endif
        endif
      else if ( index(NomVar,'State.Y') > 0) then
        if (ASSOCIATED(Instance%Y)) then
           t1 = size(Instance%Y)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Y, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.Y'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Y) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Y(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.Y'
              return
           endif
        endif
      else if ( index(NomVar,'State.VOLS') > 0) then
        if (ASSOCIATED(Instance%VOLS)) then
           t1 = size(Instance%VOLS)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%VOLS, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.VOLS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%VOLS) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%VOLS(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.VOLS'
              return
           endif
        endif
      else if ( index(NomVar,'State.VOL') > 0) then
        if (ASSOCIATED(Instance%VOL)) then
           t1 = size(Instance%VOL)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%VOL, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.VOL'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%VOL) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%VOL(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.VOL'
              return
           endif
        endif
      else if ( index(NomVar,'State.Q') > 0) then
        if (ASSOCIATED(Instance%Q)) then
           t1 = size(Instance%Q)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Q, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.Q'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Q) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Q(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.Q'
              return
           endif
        endif
      else if ( index(NomVar,'State.ZINIT') > 0) then
        if (ASSOCIATED(Instance%ZINIT)) then
           t1 = size(Instance%ZINIT)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%ZINIT, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.ZINIT'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%ZINIT) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%ZINIT(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.ZINIT'
              return
           endif
        endif
      else if ( index(NomVar,'State.Z') > 0) then
        if (ASSOCIATED(Instance%Z)) then
           t1 = size(Instance%Z)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Z, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.Z'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Z) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Z(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.Z'
              return
           endif
        endif
      else if ( index(NomVar,'State.JGNODE') > 0) then
        if (ASSOCIATED(Instance%JGNODE)) then
           t1 = size(Instance%JGNODE)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%JGNODE, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.JGNODE'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%JGNODE) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%JGNODE(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.JGNODE'
              return
           endif
        endif
      else if ( index(NomVar,'State.JDNODE') > 0) then
        if (ASSOCIATED(Instance%JDNODE)) then
           t1 = size(Instance%JDNODE)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%JDNODE, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.JDNODE'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%JDNODE) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%JDNODE(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.JDNODE'
              return
           endif
        endif
      else if ( index(NomVar,'State.IFIGE') > 0) then
        if (ASSOCIATED(Instance%IFIGE)) then
           t1 = size(Instance%IFIGE)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%IFIGE, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.IFIGE'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%IFIGE) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%IFIGE(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.IFIGE'
              return
           endif
        endif
      else if ( index(NomVar,'State.FLUX') > 0) then
        if (ASSOCIATED(Instance%FLUX)) then
           t1 = size(Instance%FLUX, 1)
           t2 = size(Instance%FLUX, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%FLUX, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.FLUX'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%FLUX).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%FLUX(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.FLUX'
              return
           endif
        endif
      else if ( index(NomVar,'State.Flow') > 0) then
        if (ASSOCIATED(Instance%DebitFlux)) then
           t1 = size(Instance%DebitFlux)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%DebitFlux, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.DebitFlux'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DebitFlux) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%DebitFlux(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.DebitFlux'
              return
           endif
        endif
      else if ( index(NomVar,'State.IDEB') > 0) then
        if (ASSOCIATED(Instance%IDEB)) then
           t1 = size(Instance%IDEB)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%IDEB, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.IDEB'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%IDEB) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%IDEB(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.IDEB'
              return
           endif
        endif
      else if ( index(NomVar,'State.IFIN') > 0) then
        if (ASSOCIATED(Instance%IFIN)) then
           t1 = size(Instance%IFIN)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%IFIN, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.IFIN'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%IFIN) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%IFIN(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.IFIN'
              return
           endif
        endif
      else if ( index(NomVar,'State.ITEM0') > 0) then
        if (ASSOCIATED(Instance%ITEM0)) then
           t1 = size(Instance%ITEM0)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%ITEM0, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to deallocate ETAT_MASCARET_T.ITEM0'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%ITEM0) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%ITEM0(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_ETAT_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_ETAT_MASCARET : Unable to allocate ETAT_MASCARET_T.ITEM0'
              return
           endif
        endif

      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      !-----------------------------------------------------------------------
      ! Appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type derive
      !-----------------------------------------------------------------------
      else if (INDEX(NomVar,'State.Link.') > 0) then
         if (ASSOCIATED(Instance%Liaisons)) then
            t1 = SIZE(Instance%Liaisons)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_ETAT_LIAISON(Instance%Liaisons(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_ETAT_MASCARET = err
                     MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Liaisons(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Liaisons, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Liaisons'
                  return
               endif
               ALLOCATE(Instance%Liaisons(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_ETAT_LIAISON(Instance%Liaisons(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_ETAT_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Liaisons'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Liaisons pas 'associated'
            ALLOCATE(Instance%Liaisons(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_ETAT_LIAISON(Instance%Liaisons(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Liaisons'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_ETAT_LIAISON(Instance%Liaisons(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_MASCARET = err
                MessageErreur = 'Unable to change the size of Instance%Liaisons'
                return
            endif
         enddo
      else if (INDEX(NomVar,'State.StoArea.') > 0) then
         if (ASSOCIATED(Instance%Casiers)) then
            t1 = SIZE(Instance%Casiers)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_ETAT_CASIER(Instance%Casiers(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_ETAT_MASCARET = err
                     MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Casiers(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Casiers, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Casiers'
                  return
               endif
               ALLOCATE(Instance%Casiers(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_ETAT_CASIER(Instance%Casiers(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_ETAT_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Casiers'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Casiers pas 'associated'
            ALLOCATE(Instance%Casiers(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_ETAT_CASIER(Instance%Casiers(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_ETAT_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Casiers'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_ETAT_CASIER(Instance%Casiers(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_ETAT_MASCARET = err
                MessageErreur = 'Unable to change the size of Instance%Casiers'
                return
            endif
         enddo
      else if (INDEX(NomVar,'State.Tracer.') > 0) then
         err = SET_TAILLE_VAR_ETAT_TRACER(Instance%Tracer, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_ETAT_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%Tracer'
            return
         endif
      else if (INDEX(NomVar,'State.Rezomat.') > 0) then
         err = SET_TAILLE_VAR_REZOMAT(Instance%MatriceRezo, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_ETAT_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%MatriceRezo'
            return
         endif
      else if (INDEX(NomVar,'State.Save.') > 0) then
         err = SET_TAILLE_VAR_SAUVE(Instance%Sauve, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_ETAT_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%Sauve'
            return
         endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type derive
      !--------------------------------------------------------------------------------
      else
         SET_TAILLE_VAR_ETAT_MASCARET = 1
         MessageErreur         = "SET_TAILLE_VAR_ETAT_MASCARET - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_ETAT_MASCARET

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_ETAT_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_ETAT_MASCARET   ! different de 0 si erreur
      type(ETAT_MASCARET_T),  intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      GET_DOUBLE_ETAT_MASCARET = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar,'State.DPDZ2') > 0) then
         valeur = Instance%DPDZ2(index1)
      else if ( index(NomVar,'State.DPDZ1') > 0) then
         valeur = Instance%DPDZ1(index1)
      else if ( index(NomVar,'State.Qspilled') > 0) then
         valeur = Instance%QDeverse(index1)
      else if ( index(NomVar,'State.Qinflow') > 0) then
         valeur = Instance%Qinjec(index1)
      else if ( index(NomVar,'State.Airs') > 0) then
         valeur = Instance%Airs(index1, index2)
      else if ( index(NomVar,'State.W') > 0) then
         valeur = Instance%W(index1, index2, index3)
      else if ( index(NomVar,'State.YNode') > 0) then
         valeur = Instance%YNode(index1)
      else if ( index(NomVar,'State.CNode') > 0) then
         valeur = Instance%CNode(index1)
      else if ( index(NomVar,'State.UNode') > 0) then
         valeur = Instance%UNode(index1)
      else if ( index(NomVar,'State.XFron') > 0) then
         valeur = Instance%XFron(index1)
      else if ( index(NomVar,'State.RH2') > 0) then
         valeur = Instance%RH2(index1)
      else if ( index(NomVar,'State.RH1') > 0) then
         valeur = Instance%RH1(index1)
      else if ( index(NomVar,'State.BS') > 0) then
         valeur = Instance%BS(index1)
      else if ( index(NomVar,'State.B2') > 0) then
         valeur = Instance%B2(index1)
      else if ( index(NomVar,'State.B1') > 0) then
         valeur = Instance%B1(index1)
      else if ( index(NomVar,'State.P2') > 0) then
         valeur = Instance%P2(index1)
      else if ( index(NomVar,'State.P1') > 0) then
         valeur = Instance%P1(index1)
      else if ( index(NomVar,'State.Froude') > 0) then
         valeur = Instance%Froude(index1)
      else if ( index(NomVar,'State.Beta') > 0) then
         valeur = Instance%Beta(index1)
      else if ( index(NomVar,'State.S2') > 0) then
         valeur = Instance%S2(index1)
      else if ( index(NomVar,'State.S1') > 0) then
         valeur = Instance%S1(index1)
      else if ( index(NomVar,'State.SS') > 0) then
         valeur = Instance%SS(index1)
      else if ( index(NomVar,'State.Q2') > 0) then
         valeur = Instance%Q2(index1)
      else if ( index(NomVar,'State.Q1') > 0) then
         valeur = Instance%Q1(index1)
      else if ( index(NomVar,'State.V1') > 0) then
         valeur = Instance%V1(index1)
      else if ( index(NomVar,'State.V2') > 0) then
         valeur = Instance%V2(index1)
      else if ( index(NomVar,'State.Y') > 0) then
         valeur = Instance%Y(index1)
      else if ( index(NomVar,'State.VOLS') > 0) then
         valeur = Instance%VOLS(index1)
      else if ( index(NomVar,'State.VOL') > 0) then
         valeur = Instance%VOL(index1)
      else if ( index(NomVar,'State.PreviousTime') > 0) then
         valeur = Instance%tempsPrecedent
      else if ( index(NomVar,'State.DTRezo') > 0) then
         valeur = Instance%DTRezo
      else if ( index(NomVar,'State.DT') > 0) then
         valeur = Instance%DT
      else if ( index(NomVar,'State.Q') > 0) then
         valeur = Instance%Q(index1)
      else if ( index(NomVar,'State.ZINIT') > 0) then
         valeur = Instance%ZINIT(index1)
      else if ( index(NomVar,'State.Z') > 0) then
         valeur = Instance%Z(index1)
      else if ( index(NomVar,'State.FLUX') > 0) then
         valeur = Instance%FLUX(index1, index2)
      else if ( index(NomVar,'State.Flow') > 0) then
         valeur = Instance%DebitFlux(index1)
      else if (INDEX(NomVar,'State.Link.') > 0) then
           GET_DOUBLE_ETAT_MASCARET = GET_DOUBLE_ETAT_LIAISON(instance%Liaisons(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.StoArea.') > 0) then
           GET_DOUBLE_ETAT_MASCARET = GET_DOUBLE_ETAT_CASIER(instance%Casiers(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.Tracer.') > 0) then
           GET_DOUBLE_ETAT_MASCARET = GET_DOUBLE_ETAT_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.Rezomat.') > 0) then
           GET_DOUBLE_ETAT_MASCARET = GET_DOUBLE_REZOMAT(instance%MatriceRezo, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.Save.') > 0) then
           GET_DOUBLE_ETAT_MASCARET = GET_DOUBLE_SAUVE(instance%Sauve, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         GET_DOUBLE_ETAT_MASCARET = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_ETAT_MASCARET - Unknown variable name"
      end if
   end function GET_DOUBLE_ETAT_MASCARET


   function GET_INT_ETAT_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_ETAT_MASCARET      ! different de 0 si erreur
      type(ETAT_MASCARET_T),  intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_ETAT_MASCARET = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar,'State.TimeStepNum') > 0) then
         valeur = Instance%numPasTps
      else if ( index(NomVar,'State.SimulPhase') > 0) then
         valeur = Instance%phaseSimulation
      else if ( index(NomVar,'State.JGNODE') > 0) then
         valeur = Instance%JGNODE(index1)
      else if ( index(NomVar,'State.JDNODE') > 0) then
         valeur = Instance%JDNODE(index1)
      else if ( index(NomVar,'State.IFIGE') > 0) then
         valeur = Instance%IFIGE(index1)
      else if ( index(NomVar,'State.NBARAD') > 0) then
         valeur = Instance%NBARAD
      else if ( index(NomVar,'State.IDEB') > 0) then
         valeur = Instance%IDEB(index1)
      else if ( index(NomVar,'State.IFIN') > 0) then
         valeur = Instance%IFIN(index1)
      else if ( index(NomVar,'State.ITEM0') > 0) then
         valeur = Instance%ITEM0(index1)
      else if (INDEX(NomVar,'State.Rezomat.') > 0) then
           GET_INT_ETAT_MASCARET = GET_INT_REZOMAT(instance%MatriceRezo, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         GET_INT_ETAT_MASCARET = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_ETAT_MASCARET - Unknown variable name"
      end if
   end function GET_INT_ETAT_MASCARET



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_ETAT_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_ETAT_MASCARET   ! different de 0 si erreur
      type(ETAT_MASCARET_T),  intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      SET_DOUBLE_ETAT_MASCARET = 0
      MessageErreur          = ""

      if ( index(NomVar,'State.DPDZ2') > 0) then
         Instance%DPDZ2(index1) = valeur
      else if ( index(NomVar,'State.DPDZ1') > 0) then
         Instance%DPDZ1(index1) = valeur
      else if ( index(NomVar,'State.Qspilled') > 0) then
         Instance%QDeverse(index1) = valeur
      else if ( index(NomVar,'State.Qinflow') > 0) then
         Instance%Qinjec(index1) = valeur
      else if ( index(NomVar,'State.Airs') > 0) then
         Instance%Airs(index1, index2) = valeur
      else if ( index(NomVar,'State.W') > 0) then
         Instance%W(index1, index2, index3) = valeur
      else if ( index(NomVar,'State.YNode') > 0) then
         Instance%YNode(index1) = valeur
      else if ( index(NomVar,'State.CNode') > 0) then
         Instance%CNode(index1) = valeur
      else if ( index(NomVar,'State.UNode') > 0) then
         Instance%UNode(index1) = valeur
      else if ( index(NomVar,'State.XFron') > 0) then
         Instance%XFron(index1) = valeur
      else if ( index(NomVar,'State.RH2') > 0) then
         Instance%RH2(index1) = valeur
      else if ( index(NomVar,'State.RH1') > 0) then
         Instance%RH1(index1) = valeur
      else if ( index(NomVar,'State.BS') > 0) then
         Instance%BS(index1) = valeur
      else if ( index(NomVar,'State.B2') > 0) then
         Instance%B2(index1) = valeur
      else if ( index(NomVar,'State.B1') > 0) then
         Instance%B1(index1) = valeur
      else if ( index(NomVar,'State.P2') > 0) then
         Instance%P2(index1) = valeur
      else if ( index(NomVar,'State.P1') > 0) then
         Instance%P1(index1) = valeur
      else if ( index(NomVar,'State.Froude') > 0) then
         Instance%Froude(index1) = valeur
      else if ( index(NomVar,'State.Beta') > 0) then
         Instance%Beta(index1) = valeur
      else if ( index(NomVar,'State.S2') > 0) then
         Instance%S2(index1) = valeur
      else if ( index(NomVar,'State.S1') > 0) then
         Instance%S1(index1) = valeur
      else if ( index(NomVar,'State.SS') > 0) then
         Instance%SS(index1) = valeur
      else if ( index(NomVar,'State.Q2') > 0) then
         Instance%Q2(index1) = valeur
      else if ( index(NomVar,'State.Q1') > 0) then
         Instance%Q1(index1) = valeur
      else if ( index(NomVar,'State.V1') > 0) then
         Instance%V1(index1) = valeur
      else if ( index(NomVar,'State.V2') > 0) then
         Instance%V2(index1) = valeur
      else if ( index(NomVar,'State.Y') > 0) then
         Instance%Y(index1) = valeur
      else if ( index(NomVar,'State.VOLS') > 0) then
         Instance%VOLS(index1) = valeur
      else if ( index(NomVar,'State.VOL') > 0) then
         Instance%VOL(index1) = valeur
      else if ( index(NomVar,'State.PreviousTime') > 0) then
         Instance%tempsPrecedent = valeur
      else if ( index(NomVar,'State.DTRezo') > 0) then
         Instance%DTRezo = valeur
      else if ( index(NomVar,'State.DT') > 0) then
         Instance%DT = valeur
      else if ( index(NomVar,'State.Q') > 0) then
         Instance%Q(index1) = valeur
      else if ( index(NomVar,'State.ZINIT') > 0) then
         Instance%ZINIT(index1) = valeur
      else if ( index(NomVar,'State.Z') > 0) then
         Instance%Z(index1) = valeur
      else if ( index(NomVar,'State.FLUX') > 0) then
         Instance%FLUX(index1, index2) = valeur
      else if ( index(NomVar,'State.Flow') > 0) then
         Instance%DebitFlux(index1) = valeur
      else if (INDEX(NomVar,'State.Link.') > 0) then
           SET_DOUBLE_ETAT_MASCARET = SET_DOUBLE_ETAT_LIAISON(instance%Liaisons(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.StoArea.') > 0) then
           SET_DOUBLE_ETAT_MASCARET = SET_DOUBLE_ETAT_CASIER(instance%Casiers(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.Tracer.') > 0) then
           SET_DOUBLE_ETAT_MASCARET = SET_DOUBLE_ETAT_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.Rezomat.') > 0) then
           SET_DOUBLE_ETAT_MASCARET = SET_DOUBLE_REZOMAT(instance%MatriceRezo, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'State.Save.') > 0) then
           SET_DOUBLE_ETAT_MASCARET = SET_DOUBLE_SAUVE(instance%Sauve, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         SET_DOUBLE_ETAT_MASCARET = 1
         MessageErreur         = "SET_DOUBLE_ETAT_MASCARET - Unknown variable name"
      end if
   end function SET_DOUBLE_ETAT_MASCARET


   function SET_INT_ETAT_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_ETAT_MASCARET      ! different de 0 si erreur
      type(ETAT_MASCARET_T),  intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_ETAT_MASCARET = 0
      MessageErreur          = ""

      if ( index(NomVar,'State.TimeStepNum') > 0) then
         Instance%numPasTps = valeur
      else if ( index(NomVar,'State.SimulPhase') > 0) then
         Instance%phaseSimulation = valeur
      else if ( index(NomVar,'State.JGNODE') > 0) then
         Instance%JGNODE(index1) = valeur
      else if ( index(NomVar,'State.JDNODE') > 0) then
         Instance%JDNODE(index1) = valeur
      else if ( index(NomVar,'State.IFIGE') > 0) then
         Instance%IFIGE(index1) = valeur
      else if ( index(NomVar,'State.NBARAD') > 0) then
         Instance%NBARAD = valeur
      else if ( index(NomVar,'State.IDEB') > 0) then
         Instance%IDEB(index1) = valeur
      else if ( index(NomVar,'State.IFIN') > 0) then
         Instance%IFIN(index1) = valeur
      else if ( index(NomVar,'State.ITEM0') > 0) then
         Instance%ITEM0(index1) = valeur
      else if (INDEX(NomVar,'State.Rezomat.') > 0) then
           SET_INT_ETAT_MASCARET = SET_INT_REZOMAT(instance%MatriceRezo, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         SET_INT_ETAT_MASCARET = 1
         MessageErreur         = "SET_INT_ETAT_MASCARET - Unknown variable name"
      end if
   end function SET_INT_ETAT_MASCARET



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_ETAT_MASCARET(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_ETAT_MASCARET    ! different de 0 si erreur
      type(ETAT_MASCARET_T),  intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      integer                            :: i
      character(LEN=256)                 :: MessageErreurType
      DESALLOUE_ETAT_MASCARET = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%DPDZ2)) then
          taille = SIZE(Instance%DPDZ2)
          if (taille > 0) then
              DEALLOCATE(Instance%DPDZ2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.DPDZ2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DPDZ2)
      if (ASSOCIATED(Instance%DPDZ1)) then
          taille = SIZE(Instance%DPDZ1)
          if (taille > 0) then
              DEALLOCATE(Instance%DPDZ1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.DPDZ1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DPDZ1)
      if (ASSOCIATED(Instance%QDeverse)) then
          taille = SIZE(Instance%QDeverse)
          if (taille > 0) then
              DEALLOCATE(Instance%QDeverse, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.QDeverse'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%QDeverse)
      if (ASSOCIATED(Instance%Qinjec)) then
          taille = SIZE(Instance%Qinjec)
          if (taille > 0) then
              DEALLOCATE(Instance%Qinjec, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Qinjec'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Qinjec)
      if (ASSOCIATED(Instance%Airs)) then
          taille = SIZE(Instance%Airs, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%Airs, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Airs'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Airs)
      if (ASSOCIATED(Instance%W)) then
          taille = SIZE(Instance%W, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%W, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.W'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%W)
      if (ASSOCIATED(Instance%YNode)) then
          taille = SIZE(Instance%YNode)
          if (taille > 0) then
              DEALLOCATE(Instance%YNode, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.YNode'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%YNode)
      if (ASSOCIATED(Instance%CNode)) then
          taille = SIZE(Instance%CNode)
          if (taille > 0) then
              DEALLOCATE(Instance%CNode, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.CNode'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%CNode)
      if (ASSOCIATED(Instance%UNode)) then
          taille = SIZE(Instance%UNode)
          if (taille > 0) then
              DEALLOCATE(Instance%UNode, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.UNode'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%UNode)
      if (ASSOCIATED(Instance%XFron)) then
          taille = SIZE(Instance%XFron)
          if (taille > 0) then
              DEALLOCATE(Instance%XFron, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.XFron'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%XFron)
      if (ASSOCIATED(Instance%RH2)) then
          taille = SIZE(Instance%RH2)
          if (taille > 0) then
              DEALLOCATE(Instance%RH2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.RH2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%RH2)
      if (ASSOCIATED(Instance%RH1)) then
          taille = SIZE(Instance%RH1)
          if (taille > 0) then
              DEALLOCATE(Instance%RH1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.RH1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%RH1)
      if (ASSOCIATED(Instance%BS)) then
          taille = SIZE(Instance%BS)
          if (taille > 0) then
              DEALLOCATE(Instance%BS, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.BS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%BS)
      if (ASSOCIATED(Instance%B2)) then
          taille = SIZE(Instance%B2)
          if (taille > 0) then
              DEALLOCATE(Instance%B2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.B2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%B2)
      if (ASSOCIATED(Instance%B1)) then
          taille = SIZE(Instance%B1)
          if (taille > 0) then
              DEALLOCATE(Instance%B1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.B1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%B1)
      if (ASSOCIATED(Instance%P2)) then
          taille = SIZE(Instance%P2)
          if (taille > 0) then
              DEALLOCATE(Instance%P2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.P2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%P2)
      if (ASSOCIATED(Instance%P1)) then
          taille = SIZE(Instance%P1)
          if (taille > 0) then
              DEALLOCATE(Instance%P1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.P1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%P1)
      if (ASSOCIATED(Instance%Froude)) then
          taille = SIZE(Instance%Froude)
          if (taille > 0) then
              DEALLOCATE(Instance%Froude, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Froude'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Froude)
      if (ASSOCIATED(Instance%Beta)) then
          taille = SIZE(Instance%Beta)
          if (taille > 0) then
              DEALLOCATE(Instance%Beta, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Beta'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Beta)
      if (ASSOCIATED(Instance%S2)) then
          taille = SIZE(Instance%S2)
          if (taille > 0) then
              DEALLOCATE(Instance%S2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.S2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S2)
      if (ASSOCIATED(Instance%S1)) then
          taille = SIZE(Instance%S1)
          if (taille > 0) then
              DEALLOCATE(Instance%S1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.S1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%S1)
      if (ASSOCIATED(Instance%SS)) then
          taille = SIZE(Instance%SS)
          if (taille > 0) then
              DEALLOCATE(Instance%SS, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.SS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SS)
      if (ASSOCIATED(Instance%Q2)) then
          taille = SIZE(Instance%Q2)
          if (taille > 0) then
              DEALLOCATE(Instance%Q2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Q2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Q2)
      if (ASSOCIATED(Instance%Q1)) then
          taille = SIZE(Instance%Q1)
          if (taille > 0) then
              DEALLOCATE(Instance%Q1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Q1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Q1)
      if (ASSOCIATED(Instance%V1)) then
          taille = SIZE(Instance%V1)
          if (taille > 0) then
              DEALLOCATE(Instance%V1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.V1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%V1)
      if (ASSOCIATED(Instance%V2)) then
          taille = SIZE(Instance%V2)
          if (taille > 0) then
              DEALLOCATE(Instance%V2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.V2'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%V2)
      if (ASSOCIATED(Instance%Y)) then
          taille = SIZE(Instance%Y)
          if (taille > 0) then
              DEALLOCATE(Instance%Y, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Y'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Y)
      if (ASSOCIATED(Instance%VOL)) then
          taille = SIZE(Instance%VOL)
          if (taille > 0) then
              DEALLOCATE(Instance%VOL, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.VOL'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%VOL)
      if (ASSOCIATED(Instance%VOLS)) then
          taille = SIZE(Instance%VOLS)
          if (taille > 0) then
              DEALLOCATE(Instance%VOLS, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.VOLS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%VOLS)
      if (ASSOCIATED(Instance%Q)) then
          taille = SIZE(Instance%Q)
          if (taille > 0) then
              DEALLOCATE(Instance%Q, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Q'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Q)
      if (ASSOCIATED(Instance%Z)) then
          taille = SIZE(Instance%Z)
          if (taille > 0) then
              DEALLOCATE(Instance%Z, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Z'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Z)
      if (ASSOCIATED(Instance%JGNODE)) then
          taille = SIZE(Instance%JGNODE)
          if (taille > 0) then
              DEALLOCATE(Instance%JGNODE, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.JGNODE'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%JGNODE)
      if (ASSOCIATED(Instance%JDNODE)) then
          taille = SIZE(Instance%JDNODE)
          if (taille > 0) then
              DEALLOCATE(Instance%JDNODE, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.JDNODE'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%JDNODE)
      if (ASSOCIATED(Instance%IFIGE)) then
          taille = SIZE(Instance%IFIGE)
          if (taille > 0) then
              DEALLOCATE(Instance%IFIGE, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.IFIGE'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%IFIGE)
      if (ASSOCIATED(Instance%FLUX)) then
          taille = SIZE(Instance%FLUX, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%FLUX, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.FLUX'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%FLUX)
      if (ASSOCIATED(Instance%DebitFlux)) then
          taille = SIZE(Instance%DebitFlux)
          if (taille > 0) then
              DEALLOCATE(Instance%DebitFlux, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.DebitFlux'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DebitFlux)
      if (ASSOCIATED(Instance%IDEB)) then
          taille = SIZE(Instance%IDEB)
          if (taille > 0) then
              DEALLOCATE(Instance%IDEB, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.IDEB'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%IDEB)
      if (ASSOCIATED(Instance%IFIN)) then
          taille = SIZE(Instance%IFIN)
          if (taille > 0) then
              DEALLOCATE(Instance%IFIN, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.IFIN'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%IFIN)
      if (ASSOCIATED(Instance%ITEM0)) then
          taille = SIZE(Instance%ITEM0)
          if (taille > 0) then
              DEALLOCATE(Instance%ITEM0, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.ITEM0'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%ITEM0)
      if (ASSOCIATED(Instance%ZINIT)) then
          taille = SIZE(Instance%ZINIT)
          if (taille > 0) then
              DEALLOCATE(Instance%ZINIT, STAT=err)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.ZINIT'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%ZINIT)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

      !-----------------------------------------------------------------------
      ! Appels aux fonctions desalloue des membres de type derive
      !-----------------------------------------------------------------------
      if (ASSOCIATED(Instance%Liaisons)) then
          taille = SIZE(Instance%Liaisons)
          DO i=1, taille
              err = DESALLOUE_ETAT_LIAISON(Instance%Liaisons(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Liaisons(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Liaisons, STAT=err)
          if (err /= 0) then
              DESALLOUE_ETAT_MASCARET = err
              MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Liaisons'
              return
          endif
          NULLIFY(Instance%Liaisons)
      endif
      if (ASSOCIATED(Instance%Casiers)) then
          taille = SIZE(Instance%Casiers)
          DO i=1, taille
              err = DESALLOUE_ETAT_CASIER(Instance%Casiers(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_ETAT_MASCARET = err
                  MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Casiers(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Casiers, STAT=err)
          if (err /= 0) then
              DESALLOUE_ETAT_MASCARET = err
              MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Casiers'
              return
          endif
          NULLIFY(Instance%Casiers)
      endif
      err = DESALLOUE_ETAT_TRACER(Instance%Tracer, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_ETAT_MASCARET = err
          MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Tracer'
      endif
      err = DESALLOUE_REZOMAT(Instance%MatriceRezo, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_ETAT_MASCARET = err
          MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.MatriceRezo'
      endif
      err = DESALLOUE_SAUVE(Instance%Sauve, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_ETAT_MASCARET = err
          MessageErreur = 'Unable to deallocate ETAT_MASCARET_T.Sauve'
      endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions desalloue des membres de type derive
      !--------------------------------------------------------------------------------
   end function DESALLOUE_ETAT_MASCARET

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_ETAT_MASCARET(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_ETAT_MASCARET    ! different de 0 si erreur
      type(ETAT_MASCARET_T),  intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: err
      character(LEN=256)                 :: MessageErreurType
      NULLIFIER_ETAT_MASCARET = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%DPDZ2)
      NULLIFY(Instance%DPDZ1)
      NULLIFY(Instance%QDeverse)
      NULLIFY(Instance%Qinjec)
      NULLIFY(Instance%Airs)
      NULLIFY(Instance%W)
      NULLIFY(Instance%YNode)
      NULLIFY(Instance%CNode)
      NULLIFY(Instance%UNode)
      NULLIFY(Instance%XFron)
      NULLIFY(Instance%RH2)
      NULLIFY(Instance%RH1)
      NULLIFY(Instance%BS)
      NULLIFY(Instance%B2)
      NULLIFY(Instance%B1)
      NULLIFY(Instance%P2)
      NULLIFY(Instance%P1)
      NULLIFY(Instance%Froude)
      NULLIFY(Instance%Beta)
      NULLIFY(Instance%S2)
      NULLIFY(Instance%S1)
      NULLIFY(Instance%SS)
      NULLIFY(Instance%Q2)
      NULLIFY(Instance%Q1)
      NULLIFY(Instance%V1)
      NULLIFY(Instance%V2)
      NULLIFY(Instance%Y)
      NULLIFY(Instance%VOL)
      NULLIFY(Instance%VOLS)
      NULLIFY(Instance%Q)
      NULLIFY(Instance%Z)
      NULLIFY(Instance%JGNODE)
      NULLIFY(Instance%JDNODE)
      NULLIFY(Instance%IFIGE)
      NULLIFY(Instance%FLUX)
      NULLIFY(Instance%DebitFlux)
      NULLIFY(Instance%IDEB)
      NULLIFY(Instance%IFIN)
      NULLIFY(Instance%ITEM0)
      NULLIFY(Instance%ZINIT)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

      !-----------------------------------------------------------------------
      ! Appels aux fonctions nullifier des membres de type derive
      !-----------------------------------------------------------------------
      NULLIFY(Instance%Liaisons)
      NULLIFY(Instance%Casiers)
      err = NULLIFIER_REZOMAT(Instance%MatriceRezo, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_ETAT_MASCARET = err
          MessageErreur = 'Unable to nullify ETAT_MASCARET_T.MatriceRezo'
          return
      endif
      err = NULLIFIER_SAUVE(Instance%Sauve, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_ETAT_MASCARET = err
          MessageErreur = 'Unable to nullify ETAT_MASCARET_T.Sauve'
          return
      endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions nullifier des membres de type derive
      !--------------------------------------------------------------------------------
   end function NULLIFIER_ETAT_MASCARET

end module M_ETAT_MASCARET_T
