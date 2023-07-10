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

module M_MODELE_MASCARET_T
!***********************************************************************
! PROGICIEL : MASCARET        J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

!=========================== Declarations ==============================

use M_PRECISION

use M_APPORT_T            ! Type APPORT_T
use M_BARRAGE_T           ! Type BARRAGE_T
use M_CONFLUENT_T         ! Type CONFLUENT_T
use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
use M_DEVERSOIR_T         ! Type DEVERSOIR_T
use M_ERREUR_T            ! Type ERREUR_T
use M_EXTREMITE_T         ! Type EXTREMITE_T
use M_FICHIER_T           ! Type FICHIER_T
use M_LOI_T               ! Type LOI_T
use M_PROFIL_T            ! Type PROFIL_T
use M_PROFIL_PLAN_T       ! Type PROFIL_PLAN_T
use M_SECTION_T           ! Type SECTION_PLAN_T
use M_SECTION_PLAN_T      ! Type SECTION_T
use M_SINGULARITE_T       ! Type SINGULARITE_T
use M_ZONE_SECHE_T        ! Type ZONE_SECHE_T
use M_ZONE_FROT_T         ! Type ZONE_FROT_T
use M_SAUVE_T             ! Type SAUVE_T
use M_CASIER_T            ! Type CASIER_T
use M_LIAISON_T           ! Type LIAISON_T
use M_MODELE_TRACER_T     ! Type MODELE_TRACER_T
use M_APPORT_PLUIE_T      ! Type APPORT_PLUIE_T

use M_INDEX_VARIABLE_C    ! Index des variables : definie la constante NB_TOT_VAR


Type MODELE_MASCARET_T
    sequence
    type(CONNECT_T)                            :: Connect
    type(PROFIL_T),      dimension(:), pointer :: Profils
    type(SECTION_PLAN_T)                       :: SectionPlan
    type(ZONE_SECHE_T),  dimension(:), pointer :: ZonesSeches
    type(ZONE_FROT_T),   dimension(:), pointer :: ZonesFrottement
    type(LOI_T),         dimension(:), pointer :: LoisHydrau
    type(SINGULARITE_T), dimension(:), pointer :: Singularites
    type(DEVERSOIR_T),   dimension(:), pointer :: Deversoirs
    type(FICHIER_T)                            :: FichierListing
    type(LIAISON_T),     dimension(:), pointer :: Liaisons
    type(FICHIER_T)                            :: FichierResultat
    type(FICHIER_T)                            :: FichierResultat2
    type(FICHIER_T)                            :: FichierGeomCasier
    type(FICHIER_T)                            :: FichierResuCasier
    type(FICHIER_T)                            :: FichierResuLiaison
    type(FICHIER_T)                            :: FichierListingCasier
    type(FICHIER_T)                            :: FichierListingLiaison
    type(EXTREMITE_T),   dimension(:), pointer :: Extremites
    type(CASIER_T),      dimension(:), pointer :: Casiers
    type(MODELE_TRACER_T)                      :: Tracer
    type(CONFLUENT_T),   dimension(:), pointer :: Confluents
    type(APPORT_T),      dimension(:), pointer :: Apports
    type(BARRAGE_T)                            :: Barrage
    type(APPORT_PLUIE_T),dimension(:), pointer :: ApportsPluie
    type(PROFIL_PLAN_T)                        :: ProfilPlan
    logical                                    :: ProfAbs
    logical                                    :: PerteElargissementTrans
    logical                                    :: ImplicitTrans
    logical                                    :: PerteChargeConfluent
    logical                                    :: OptionCasier
    logical                                    :: OptionTracer
    logical                                    :: OptionCourlis
    logical, dimension(NB_TOT_VAR)             :: VarSto
    logical, dimension(NB_TOT_VAR)             :: VarCalc
    logical                                    :: ImpressionCalcul
    logical                                    :: ImpressionPlanim
    logical                                    :: RepriseCalcul
    logical                                    :: PresenceZoneStockage
    logical                                    :: InterpolLinStrickler
    logical                                    :: DebProgressifZS
    logical                                    :: DebProgressifLM
    logical                                    :: FrottParoiVerticale
    logical                                    :: PasTempsVariable
    logical                                    :: FrottementImplicite
    logical                                    :: CalculValidation
    logical                                    :: OndeSubm
    real(DOUBLE), dimension(:), pointer        :: ZREF
    real(DOUBLE), dimension(:), pointer        :: RDC
    real(DOUBLE), dimension(:), pointer        :: RGC
    real(DOUBLE), dimension(6,6,5)             :: abaque
    real(DOUBLE)                               :: HEPS
    real(DOUBLE), dimension(:), pointer        :: CF2
    real(DOUBLE), dimension(:), pointer        :: CF1
    real(DOUBLE), dimension(:), pointer        :: PCSing
    real(DOUBLE), dimension(:), pointer        :: XDT
    real(DOUBLE), dimension(:), pointer        :: X
    real(DOUBLE)                               :: CourantObj
    real(DOUBLE)                               :: TempsMaximum
    real(DOUBLE)                               :: TempsInitial
    real(DOUBLE)                               :: DT
    real(DOUBLE)                               :: FroudeLim
    real(DOUBLE)                               :: DZArriveeFront
    integer                                    :: FormatResu2
    integer     , dimension(:), pointer        :: SectionStockage
    integer                                    :: OptionStockage
    integer                                    :: FormatResu
    integer                                    :: PremierPasStocke
    integer                                    :: PasImpression
    integer                                    :: PasStockage
    integer     , dimension(:), pointer        :: Algorithme
    integer                                    :: TypeMaillage
    integer     , dimension(:), pointer        :: IDT
    integer                                    :: FormatGeom
    integer                                    :: LoiFrottement
    integer                                    :: NbPasTemps
    real(DOUBLE)                               :: Cote_max_controle
    integer                                    :: Section_controle
    integer                                    :: CritereArret
    integer                                    :: Regime
    integer                                    :: ModeleLit
    integer                                    :: TypeValidation
    integer                                    :: Noyau
    integer                                    :: VersionCode
    character(LEN=255)                          :: TitreCas
    real(DOUBLE), dimension(:), pointer        :: DZ
    real(DOUBLE), dimension(:), pointer        :: XD
    real(DOUBLE), dimension(:), pointer        :: DZD
    integer     , dimension(:), pointer        :: ProfDebBief
    integer     , dimension(:), pointer        :: ProfFinBief
    real(DOUBLE), dimension(:), pointer        :: absc_rel_ext_deb_bief
    real(DOUBLE), dimension(:), pointer        :: absc_rel_ext_fin_bief
    logical                                    :: Opt
    real(DOUBLE), dimension(:,:), pointer      :: F1
    logical                                    :: Boussinesq
    logical                                    :: NoConvection
    integer                                    :: CQMV
    logical                                    :: decentrement

end type MODELE_MASCARET_T

contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_MODELE_MASCARET(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)               :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)               :: tabDescriptionVar ! Tableau des description de variable du modele

        ! --- M_CONNECT_T ---
        call GET_TAB_VAR_CONNECT(i, tabNomVar, tabDescriptionVar)

        !  --- PROFIL_T ---
        call GET_TAB_VAR_PROFIL(i, tabNomVar, tabDescriptionVar)

        ! --- SECTION_PLAN_T ---
        call GET_TAB_VAR_SECTION_PLAN(i, tabNomVar, tabDescriptionVar)

        ! --- ZONE_SECHE_T ---
        call GET_TAB_VAR_ZONE_SECHE(i, tabNomVar, tabDescriptionVar)

        ! --- ZONE_FROT_T ---
        call GET_TAB_VAR_ZONE_FROT(i, tabNomVar, tabDescriptionVar)

        ! --- LOI_T ---
        call GET_TAB_VAR_LOI(i, tabNomVar, tabDescriptionVar)

        ! --- SINGULARITE_T ---
        call GET_TAB_VAR_SINGULARITE(i, tabNomVar, tabDescriptionVar)

        ! --- DEVERSOIR_T ---
        call GET_TAB_VAR_DEVERSOIR(i, tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierListing ---
        call GET_TAB_VAR_FICHIER(i, 'Listing',tabNomVar, tabDescriptionVar)

        ! --- LIAISON_T ---
        call GET_TAB_VAR_LIAISON(i, tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierResultat ---
        call GET_TAB_VAR_FICHIER(i, 'Result',tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierResultat2 ---
        call GET_TAB_VAR_FICHIER(i, 'Result2',tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierGeomCasier ---
        call GET_TAB_VAR_FICHIER(i, 'GeoStoArea',tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierResultatCasier ---
        call GET_TAB_VAR_FICHIER(i, 'ResultStoArea',tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierResuLiaison ---
        call GET_TAB_VAR_FICHIER(i, 'ResultLink',tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierListingCasier ---
        call GET_TAB_VAR_FICHIER(i, 'ListingStoArea',tabNomVar, tabDescriptionVar)

        ! --- FICHIER_T FichierListingLiaison ---
        call GET_TAB_VAR_FICHIER(i, 'ListingLink',tabNomVar, tabDescriptionVar)

        ! --- EXTREMITE_T ---
        call GET_TAB_VAR_EXTREMITE(i, tabNomVar, tabDescriptionVar)

        ! --- CASIER_T ---
        call GET_TAB_VAR_CASIER(i, tabNomVar, tabDescriptionVar)

        ! --- MODELE_TRACER_T ---
        call GET_TAB_VAR_MODELE_TRACER(i, tabNomVar, tabDescriptionVar)

        ! --- CONFLUENT_T ---
        call GET_TAB_VAR_CONFLUENT(i, tabNomVar, tabDescriptionVar)

        ! --- APPORT_T ---
        call GET_TAB_VAR_APPORT(i, tabNomVar, tabDescriptionVar)

        ! --- BARRAGE_T ---
        call GET_TAB_VAR_BARRAGE(i, tabNomVar, tabDescriptionVar)

        ! --- APPORT_PLUIE_T ---
        call GET_TAB_VAR_APPORT_PLUIE(i, tabNomVar, tabDescriptionVar)

        ! --- PROFIL_PLAN_T ---
        call GET_TAB_VAR_PROFIL_PLAN(i, tabNomVar, tabDescriptionVar)

        ! --- Racine Modele Mascaret ---
        tabNomVar(i)         ="Model.CSectionAbsX"
        tabDescriptionVar(i) ="Cross section in absolute abscissa?"
        i=i+1
        tabNomVar(i)         ="Model.HeadLossEnlarg"
        tabDescriptionVar(i) ="Automatic head loss in case of enlargement?"
        i=i+1
        tabNomVar(i)         ="Model.ImpSupCritKern"
        tabDescriptionVar(i) ="Implicitation of the super-critical kernel?"
        i=i+1
        tabNomVar(i)         ="Model.HeadLossJunc"
        tabDescriptionVar(i) ="Automatic head losses at junctions?"
        i=i+1
        tabNomVar(i)         ="Model.StoArea"
        tabDescriptionVar(i) ="Storage areas activation?"
        i=i+1
        tabNomVar(i)         ="Model.TracerOn"
        tabDescriptionVar(i) ="Tracer activation?"
        i=i+1
        tabNomVar(i)         ="Model.RecVar"
        tabDescriptionVar(i) ="Recorded variables"
        i=i+1
        tabNomVar(i)         ="Model.CompVar"
        tabDescriptionVar(i) ="Computed variables"
        i=i+1
        tabNomVar(i)         ="Model.PrintComp"
        tabDescriptionVar(i) ="Write the listing along with the computation progress"
        i=i+1
        tabNomVar(i)         ="Model.PrintVertCSection"
        tabDescriptionVar(i) ="Write the vertical discretisation of the cross sections"
        i=i+1
        tabNomVar(i)         ="Model.HotStart"
        tabDescriptionVar(i) ="Hot start?"
        i=i+1
        tabNomVar(i)         ="Model.InefFlowArea"
        tabDescriptionVar(i) ="Ineffective flow area?"
        i=i+1
        tabNomVar(i)         ="Model.InterpFriction"
        tabDescriptionVar(i) ="Linear interpolation of the friction coefficients?"
        i=i+1
        tabNomVar(i)         ="Model.ProgOverFlowIFA"
        tabDescriptionVar(i) ="Type of progressive overflow for the ineffective flow areas"
        i=i+1
        tabNomVar(i)         ="Model.ProgOverFlowFP"
        tabDescriptionVar(i) ="Type of progressive overflow for the floodplains"
        i=i+1
        tabNomVar(i)         ="Model.FricVertWall"
        tabDescriptionVar(i) ="Conservation of friction on the vertical walls?"
        i=i+1
        tabNomVar(i)         ="Model.VarTimeStep"
        tabDescriptionVar(i) ="Variable time step?"
        i=i+1
        tabNomVar(i)         ="Model.ImpFric"
        tabDescriptionVar(i) ="Implicitation of friction?"
        i=i+1
        tabNomVar(i)         ="Model.ValidComp"
        tabDescriptionVar(i) ="Validate the computation?"
        i=i+1
        tabNomVar(i)         ="Model.DamBrkFldWave"
        tabDescriptionVar(i) ="Computation of a dam break flood wave?"
        i=i+1
        tabNomVar(i)         ="Model.Zbot"
        tabDescriptionVar(i) ="Bottom level (m)"
        i=i+1
        tabNomVar(i)         ="Model.LevRightBk"
        tabDescriptionVar(i) ="Level of the right bank (m)"
        i=i+1
        tabNomVar(i)         ="Model.LevLeftBk"
        tabDescriptionVar(i) ="Level of the left bank (m)"
        i=i+1
        tabNomVar(i)         ="Model.Abac"
        tabDescriptionVar(i) ="Abacus for the head losses at junctions"
        i=i+1
        tabNomVar(i)         ="Model.Heps"
        tabDescriptionVar(i) ="Miniam water depth (m)"
        i=i+1
        tabNomVar(i)         ="Model.FricCoefFP"
        tabDescriptionVar(i) ="Friction coefficients for the floodplains"
        i=i+1
        tabNomVar(i)         ="Model.FricCoefMainCh"
        tabDescriptionVar(i) ="Friction coefficients for the main channel"
        i=i+1
        tabNomVar(i)         ="Model.LocalHeadLoss"
        tabDescriptionVar(i) ="Local head losses"
        i=i+1
        tabNomVar(i)         ="Model.XDT"
        tabDescriptionVar(i) ="Relative position of nodes / cross sections"
        i=i+1
        tabNomVar(i)         ="Model.X"
        tabDescriptionVar(i) ="Abscissa of nodes (m)"
        i=i+1
        tabNomVar(i)         ="Model.CourantNum"
        tabDescriptionVar(i) ="Maximal Courant number"
        i=i+1
        tabNomVar(i)         ="Model.MaxCompTime"
        tabDescriptionVar(i) ="Maximal computation time (s)"
        i=i+1
        tabNomVar(i)         ="Model.MaxControlZ"
        tabDescriptionVar(i) ="Maximal control water level (m)"
        i=i+1
        tabNomVar(i)         ="Model.InitTime"
        tabDescriptionVar(i) ="Initial time of the computation (s)"
        i=i+1
        tabNomVar(i)         ="Model.DT"
        tabDescriptionVar(i) ="Time step (s)"
        i=i+1
        tabNomVar(i)         ="Model.LimFroude"
        tabDescriptionVar(i) ="Limit Froude number for the boundary conditions"
        i=i+1
        tabNomVar(i)         ="Model.DZwave"
        tabDescriptionVar(i) ="Level threshold identifying the wave (m)"
        i=i+1
        tabNomVar(i)         ="Model.ResultFmt2"
        tabDescriptionVar(i) ="Result format #2"
        i=i+1
        tabNomVar(i)         ="Model.NodeRes"
        tabDescriptionVar(i) ="Nodes where the results are recorded"
        i=i+1
        tabNomVar(i)         ="Model.RecOption"
        tabDescriptionVar(i) ="Option for the records"
        i=i+1
        tabNomVar(i)         ="Model.ResultFmt"
        tabDescriptionVar(i) ="Result format"
        i=i+1
        tabNomVar(i)         ="Model.RecNbFstTimeStep"
        tabDescriptionVar(i) ="Recording : number of the first time step to write"
        i=i+1
        tabNomVar(i)         ="Model.RecNTimeStep"
        tabDescriptionVar(i) ="Recording : write data all N time steps "
        i=i+1
        tabNomVar(i)         ="Model.RecListNTimeStep"
        tabDescriptionVar(i) ="Recording : write to the listing file all N time steps"
        i=i+1
        tabNomVar(i)         ="Model.AlgoNet"
        tabDescriptionVar(i) ="Algorithm for the network solution"
        i=i+1
        tabNomVar(i)         ="Model.1DMesh"
        tabDescriptionVar(i) ="Computational method for the 1D mesh generator"
        i=i+1
        tabNomVar(i)         ="Model.IDT"
        tabDescriptionVar(i) ="Upstream cross section of a node"
        i=i+1
        tabNomVar(i)         ="Model.GeoFileFmt"
        tabDescriptionVar(i) ="Format of the geometry file"
        i=i+1
        tabNomVar(i)         ="Model.FricLaw"
        tabDescriptionVar(i) ="Type of friction law"
        i=i+1
        tabNomVar(i)         ="Model.MaxNbTimeStep"
        tabDescriptionVar(i) ="Maximal number of time steps"
        i=i+1
        tabNomVar(i)         ="Model.ControlSection"
        tabDescriptionVar(i) ="Control section number"
        i=i+1
        tabNomVar(i)         ="Model.StopCriteria"
        tabDescriptionVar(i) ="Criteria for stopping calculations"
        i=i+1
        tabNomVar(i)         ="Model.Regime"
        tabDescriptionVar(i) ="Regime : steady or unsteady"
        i=i+1
        tabNomVar(i)         ="Model.CSectionLayout"
        tabDescriptionVar(i) ="Cross section layout"
        i=i+1
        tabNomVar(i)         ="Model.ValidType"
        tabDescriptionVar(i) ="Type of validation (number)"
        i=i+1
        tabNomVar(i)         ="Model.Kernel"
        tabDescriptionVar(i) ="Computational kernel : Steady, Unsteady subcritical or Super-critical"
        i=i+1
        tabNomVar(i)         ="Model.Version"
        tabDescriptionVar(i) ="MASCARET version"
        i=i+1
        tabNomVar(i)         ="Model.Title"
        tabDescriptionVar(i) ="Title of the computation"
        i=i+1
        tabNomVar(i)         ="Model.DZ"
        tabDescriptionVar(i) ="Features of the nodes"
        i=i+1
        tabNomVar(i)         ="Model.XD"
        tabDescriptionVar(i) ="Abscissa of interfaces"
        i=i+1
        tabNomVar(i)         ="Model.DZD"
        tabDescriptionVar(i) ="Vertical discretisation of interfaces"
        i=i+1
        tabNomVar(i)         ="Model.FirstCSReach"
        tabDescriptionVar(i) ="First cross section of a reach"
        i=i+1
        tabNomVar(i)         ="Model.LastCSReach"
        tabDescriptionVar(i) ="Last cross section of a reach"
        i=i+1
        tabNomVar(i)         ="Model.RelXFirstNdReach"
        tabDescriptionVar(i) ="Relative abscissa of the first node of a reach"
        i=i+1
        tabNomVar(i)         ="Model.RelXLastNdReach"
        tabDescriptionVar(i) ="Relative abscissa of the last node of a reach"
        i=i+1
        tabNomVar(i)         ="Model.Opt"
        tabDescriptionVar(i) ="Optimisation of the super-critical kernel"
        i=i+1
        tabNomVar(i)         ="Model.F1"
        tabDescriptionVar(i) ="Pulse function"
        i=i+1
        tabNomVar(i)         ="Model.Boussinesq"
        tabDescriptionVar(i) ="Non-hydrostatic terms for the super-critical kernel?"
        i=i+1
        tabNomVar(i)         ="Model.NoConvection"
        tabDescriptionVar(i) ="Reduced momentum equation for sub-critical kernel?"
        i=i+1
        tabNomVar(i)         ="Model.CQMV"
        tabDescriptionVar(i) ="Lateral inflow in the momentum equation?"
        i=i+1
        tabNomVar(i)         ="Model.decentrement"
        tabDescriptionVar(i) ="Option to use upwind numerical scheme"
        i=i+1

      return

    end subroutine GET_TAB_VAR_MODELE_MASCARET

    ! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_MODELE_MASCARET(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_MODELE_MASCARET    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_MODELE_MASCARET = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .TRUE.
      dimVar                = 0
      MessageErreur         = ""

      if ( index(NomVar, 'Model.CSectionAbsX') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.HeadLossEnlarg') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ImpSupCritKern') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.HeadLossJunc') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StoArea') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.TracerOn') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.RecVar') > 0) then
          TypeVar = 'TABBOOL'
          dimVar                = 1
       else if ( index(NomVar, 'Model.CompVar') > 0) then
          TypeVar = 'TABBOOL'
          dimVar                = 1
       else if ( index(NomVar, 'Model.PrintComp') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.PrintVertCSection') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.HotStart') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.InefFlowArea') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.InterpFriction') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ProgOverFlowIFA') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ProgOverFlowFP') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.FricVertWall') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.VarTimeStep') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ImpFric') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ValidComp') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.DamBrkFldWave') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Zbot') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.LevRightBk') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.LevLeftBk') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Abac') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 3
       else if ( index(NomVar, 'Model.Heps') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.FricCoefFP') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.FricCoefMainCh') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.LocalHeadLoss') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.XDT') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.XD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.X') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.CourantNum') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.MaxCompTime') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.MaxControlZ') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.InitTime') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.DT') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.LimFroude') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.DZwave') > 0) then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ResultFmt2') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.NodeRes') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.RecOption') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ResultFmt') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.RecNbFstTimeStep') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.RecNTimeStep') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.RecListNTimeStep') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.AlgoNet') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.1DMesh') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.IDT') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.GeoFileFmt') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.FricLaw') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.MaxNbTimeStep') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ControlSection') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.StopCriteria') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Regime') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CSectionLayout') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.ValidType') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Kernel') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Version') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.Title') > 0) then
          TypeVar = 'STRING'
          dimVar                = 0
       else if ( index(NomVar, 'Model.DZD') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.DZ') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.FirstCSReach') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.LastCSReach') > 0) then
          TypeVar = 'TABINT'
          dimVar                = 1
       else if ( index(NomVar, 'Model.RelXFirstNdReach') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.RelXLastNdReach') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 1
       else if ( index(NomVar, 'Model.Opt') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.F1') > 0) then
          TypeVar = 'TABDOUBLE'
          dimVar                = 2
       else if ( index(NomVar, 'Model.Boussinesq') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
      else if ( index(NomVar, 'Model.NoConvection') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( index(NomVar, 'Model.CQMV') > 0) then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( index(NomVar, 'Model.decentrement') > 0) then
          TypeVar = 'BOOL'
          dimVar                = 0
       else if ( INDEX(NomVar,'Model.Connect.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_CONNECT(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.CrossSection.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_PROFIL(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.VDSection.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_SECTION_PLAN(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.DryArea.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_ZONE_SECHE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.FrictionZone.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_ZONE_FROT(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.Graph.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_LOI(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.Weir.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_SINGULARITE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.LateralWeir.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_DEVERSOIR(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.File.Listing.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.Link.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_LIAISON(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.File.Result2.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.File.Result.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.File.GeoStoArea.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.File.ResultStoArea.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.File.ResultLink.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.File.ListingStoArea.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.File.ListingLink.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_FICHIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.Boundary.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_EXTREMITE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.StorageArea.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_CASIER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.Junction.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_CONFLUENT(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.Tracer.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_MODELE_TRACER(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.Inflow.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_APPORT(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.Dam.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_BARRAGE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
       else if ( INDEX(NomVar,'Model.ExternalInflow.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_APPORT_PLUIE(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
          dimVar = dimVar + 1
       else if ( INDEX(NomVar,'Model.VDCrossSection.') > 0) then
          GET_TYPE_VAR_MODELE_MASCARET = GET_TYPE_VAR_PROFIL_PLAN(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      else
        GET_TYPE_VAR_MODELE_MASCARET = 1
        TypeVar = "?"
        Categorie             = "STATE"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_MODELE_MASCARET - Unknown variable name : "//NomVar
      end if


    end function GET_TYPE_VAR_MODELE_MASCARET

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_MODELE_MASCARET(Instance, NomVar, index1, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_MODELE_MASCARET ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in) :: index1                         ! valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Liaisons, Extremites, Casiers et Confluents
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      GET_TAILLE_VAR_MODELE_MASCARET = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.CSectionAbsX') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.HeadLossEnlarg') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ImpSupCritKern') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.HeadLossJunc') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StoArea') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.TracerOn') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.RecVar') > 0) then
         taille1 = size(Instance%VarSto)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CompVar') > 0) then
         taille1 = size(Instance%VarCalc)
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.PrintComp') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.PrintVertCSection') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.HotStart') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.InefFlowArea') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.InterpFriction') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ProgOverFlowIFA') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ProgOverFlowFP') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.FricVertWall') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.VarTimeStep') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ImpFric') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ValidComp') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.DamBrkFldWave') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Zbot') > 0) then
         if (ASSOCIATED(Instance%ZREF)) then
            taille1 = size(Instance%ZREF)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.LevRightBk') > 0) then
         if (ASSOCIATED(Instance%RDC)) then
            taille1 = size(Instance%RDC)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.LevLeftBk') > 0) then
         if (ASSOCIATED(Instance%RGC)) then
            taille1 = size(Instance%RGC)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Abac') > 0) then
         taille1 = size(Instance%abaque, 1)
         taille2 = size(Instance%abaque, 2)
         taille3 = size(Instance%abaque, 3)
      else if ( index(NomVar, 'Model.Heps') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.FricCoefFP') > 0) then
         if (ASSOCIATED(Instance%CF2)) then
            taille1 = size(Instance%CF2)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.FricCoefMainCh') > 0) then
         if (ASSOCIATED(Instance%CF1)) then
            taille1 = size(Instance%CF1)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.LocalHeadLoss') > 0) then
         if (ASSOCIATED(Instance%PCSing)) then
            taille1 = size(Instance%PCSing)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.XDT') > 0) then
         if (ASSOCIATED(Instance%XDT)) then
            taille1 = size(Instance%XDT)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.XD') > 0) then
         if (ASSOCIATED(Instance%XD)) then
            taille1 = size(Instance%XD)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.X') > 0) then
         if (ASSOCIATED(Instance%X)) then
            taille1 = size(Instance%X)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CourantNum') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.MaxCompTime') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.MaxControlZ') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.InitTime') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.DT') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.LimFroude') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.DZwave') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ResultFmt2') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.NodeRes') > 0) then
         if (ASSOCIATED(Instance%SectionStockage)) then
            taille1 = size(Instance%SectionStockage)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.RecOption') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ResultFmt') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.RecNbFstTimeStep') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.RecNTimeStep') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.RecListNTimeStep') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.AlgoNet') > 0) then
         if (ASSOCIATED(Instance%Algorithme)) then
            taille1 = size(Instance%Algorithme)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.1DMesh') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.IDT') > 0) then
         if (ASSOCIATED(Instance%IDT)) then
            taille1 = size(Instance%IDT)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.GeoFileFmt') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.FricLaw') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.MaxNbTimeStep') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ControlSection') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.StopCriteria') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Regime') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CSectionLayout') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.ValidType') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Kernel') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Version') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Title') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.DZD') > 0) then
         if (ASSOCIATED(Instance%DZD)) then
            taille1 = size(Instance%DZD)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.DZ') > 0) then
         if (ASSOCIATED(Instance%DZ)) then
            taille1 = size(Instance%DZ)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.FirstCSReach') > 0) then
         if (ASSOCIATED(Instance%ProfDebBief)) then
            taille1 = size(Instance%ProfDebBief)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.LastCSReach') > 0) then
         if (ASSOCIATED(Instance%ProfFinBief)) then
            taille1 = size(Instance%ProfFinBief)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.RelXFirstNdReach') > 0) then
         if (ASSOCIATED(Instance%absc_rel_ext_deb_bief)) then
            taille1 = size(Instance%absc_rel_ext_deb_bief)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.RelXLastNdReach') > 0) then
         if (ASSOCIATED(Instance%absc_rel_ext_fin_bief)) then
            taille1 = size(Instance%absc_rel_ext_fin_bief)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.Opt') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.F1') > 0) then
         if (ASSOCIATED(Instance%F1)) then
            taille1 = size(Instance%F1, 1)
            taille2 = size(Instance%F1, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'Model.Boussinesq') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.NoConvection') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.CQMV') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'Model.decentrement') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if (INDEX(NomVar,'Model.Connect.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_CONNECT(Instance%Connect,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.CrossSection.') > 0) then
         if (ASSOCIATED(Instance%Profils)) then
            taille1 = size(Instance%Profils)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                 GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_PROFIL(Instance%Profils(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                 GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_PROFIL(Instance%Profils(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.VDSection.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_SECTION_PLAN(Instance%SectionPlan,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.DryArea.') > 0) then
         if (ASSOCIATED(Instance%ZonesSeches)) then
            taille1 = size(Instance%ZonesSeches)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_ZONE_SECHE(Instance%ZonesSeches(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.FrictionZone.') > 0) then
         if (ASSOCIATED(Instance%ZonesFrottement)) then
            taille1 = size(Instance%ZonesFrottement)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_ZONE_FROT(Instance%ZonesFrottement(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.Graph.') > 0) then
         if (ASSOCIATED(Instance%LoisHydrau)) then
            taille1 = size(Instance%LoisHydrau)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_LOI(Instance%LoisHydrau(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_LOI(Instance%LoisHydrau(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.Weir.') > 0) then
         if (ASSOCIATED(Instance%Singularites)) then
            taille1 = size(Instance%Singularites)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_SINGULARITE(Instance%Singularites(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_SINGULARITE(Instance%Singularites(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.LateralWeir.') > 0) then
         if (ASSOCIATED(Instance%Deversoirs)) then
            taille1 = size(Instance%Deversoirs)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_DEVERSOIR(Instance%Deversoirs(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_DEVERSOIR(Instance%Deversoirs(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.File.Listing.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%FichierListing,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.') > 0) then
         if (ASSOCIATED(Instance%Liaisons)) then
            taille1 = size(Instance%Liaisons)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_LIAISON(Instance%Liaisons(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_LIAISON(Instance%Liaisons(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.File.Result2.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%FichierResultat2,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Result.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%FichierResultat,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.GeoStoArea.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%FichierGeomCasier,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultStoArea.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%FichierResuCasier,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultLink.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%FichierResuLiaison,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%Tracer%FichierResuTracer,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingStoArea.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%FichierListingCasier,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingLink.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%FichierListingLiaison,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%Tracer%FichierListingTracer,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_FICHIER(Instance%Tracer%FichierConcInit,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.Boundary.') > 0) then
         if (ASSOCIATED(Instance%Extremites)) then
            taille1 = size(Instance%Extremites)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_EXTREMITE(Instance%Extremites(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_EXTREMITE(Instance%Extremites(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
        else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.StorageArea.') > 0) then
         if (ASSOCIATED(Instance%Casiers)) then
            taille1 = size(Instance%Casiers)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_CASIER(Instance%Casiers(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_CASIER(Instance%Casiers(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.Tracer.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_MODELE_TRACER(Instance%Tracer, NomVar, &
                                               index1, taille1, taille2, taille3, MessageErreur)
      else if (index(NomVar,'Model.Junction.') > 0) then
         if (ASSOCIATED(Instance%Confluents)) then
            taille1 = size(Instance%Confluents)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             if ((index1 > 1).AND.(index1 <= taille1)) then
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_CONFLUENT(Instance%Confluents(index1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             else
                GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_CONFLUENT(Instance%Confluents(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
             endif
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.Inflow.') > 0) then
         if (ASSOCIATED(Instance%Apports)) then
            taille1 = size(Instance%Apports)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_APPORT(Instance%Apports(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.Dam.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_BARRAGE(Instance%Barrage,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else if (INDEX(NomVar,'Model.ExternalInflow.') > 0) then
         if (ASSOCIATED(Instance%ApportsPluie)) then
            taille1 = size(Instance%ApportsPluie)
         else
            taille1 = 0
         endif
         if (taille1 > 0) then
             GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_APPORT_PLUIE(Instance%ApportsPluie(1),&
                                                   NomVar, taille2, taille3, bidon1, MessageErreur)
         else
             taille2 = 0
             taille3 = 0
         end if
      else if (INDEX(NomVar,'Model.VDCrossSection.') > 0) then
         GET_TAILLE_VAR_MODELE_MASCARET = GET_TAILLE_VAR_PROFIL_PLAN(Instance%ProfilPlan,&
                                               NomVar, taille1, taille2, taille3, MessageErreur)
      else
         GET_TAILLE_VAR_MODELE_MASCARET = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_MODELE_MASCARET - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_MODELE_MASCARET

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_MODELE_MASCARET(Instance, NomVar, index1, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_MODELE_MASCARET ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: index1                         ! Valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Liaisons, Extremites, Casiers et Confluents
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err
      integer i, bidon
      character(LEN=256)                 :: MessageErreurType

      SET_TAILLE_VAR_MODELE_MASCARET = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'Model.Zbot') > 0) then
        if (ASSOCIATED(Instance%ZREF)) then
           t1 = size(Instance%ZREF)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%ZREF, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.ZBOT'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%ZREF) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%ZREF(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.ZBOT'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.LevRightBk') > 0) then
        if (ASSOCIATED(Instance%RDC)) then
           t1 = size(Instance%RDC)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%RDC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.LEVRIGHTBK'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%RDC) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%RDC(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.LEVRIGHTBK'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.LevLeftBk') > 0) then
        if (ASSOCIATED(Instance%RGC)) then
           t1 = size(Instance%RGC)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%RGC, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.LEVLEFTBK'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%RGC) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%RGC(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.LEVLEFTBK'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.FricCoefFP') > 0) then
        if (ASSOCIATED(Instance%CF2)) then
           t1 = size(Instance%CF2)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%CF2, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.FRICCOEFFP'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%CF2) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%CF2(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.FRICCOEFFP'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.FricCoefMainCh') > 0) then
        if (ASSOCIATED(Instance%CF1)) then
           t1 = size(Instance%CF1)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%CF1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.FRICCOEFMAINCH'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%CF1) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%CF1(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.FRICCOEFMAINCH'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.LocalHeadLoss') > 0) then
        if (ASSOCIATED(Instance%PCSing)) then
           t1 = size(Instance%PCSing)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%PCSing, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.LOCALHEADLOSS'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%PCSing) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%PCSing(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.LOCALHEADLOSS'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.XDT') > 0) then
        if (ASSOCIATED(Instance%XDT)) then
           t1 = size(Instance%XDT)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%XDT, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.XDT'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%XDT) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%XDT(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.XDT'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.XD') > 0) then
        if (ASSOCIATED(Instance%XD)) then
           t1 = size(Instance%XD)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%XD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.XD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%XD) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%XD(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.XD'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.X') > 0) then
        if (ASSOCIATED(Instance%X)) then
           t1 = size(Instance%X)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%X, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.X'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%X) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%X(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.X'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.NodeRes') > 0) then
        if (ASSOCIATED(Instance%SectionStockage)) then
           t1 = size(Instance%SectionStockage)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%SectionStockage, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.NODERES'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SectionStockage) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%SectionStockage(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.NODERES'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.AlgoNet') > 0) then
        if (ASSOCIATED(Instance%Algorithme)) then
           t1 = size(Instance%Algorithme)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%Algorithme, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.ALGONET'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%Algorithme) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%Algorithme(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.ALGONET'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.IDT') > 0) then
        if (ASSOCIATED(Instance%IDT)) then
           t1 = size(Instance%IDT)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%IDT, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.IDT'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%IDT) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%IDT(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.IDT'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.DZD') > 0) then
        if (ASSOCIATED(Instance%DZD)) then
           t1 = size(Instance%DZD)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%DZD, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.DZD'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DZD) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%DZD(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.DZD'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.DZ') > 0) then
        if (ASSOCIATED(Instance%DZ)) then
           t1 = size(Instance%DZ)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%DZ, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.DZ'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%DZ) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%DZ(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.DZ'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.FirstCSReach') > 0) then
        if (ASSOCIATED(Instance%ProfDebBief)) then
           t1 = size(Instance%ProfDebBief)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%ProfDebBief, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.FIRSTCSREACH'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%ProfDebBief) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%ProfDebBief(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.FIRSTCSREACH'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.LastCSReach') > 0) then
        if (ASSOCIATED(Instance%ProfFinBief)) then
           t1 = size(Instance%ProfFinBief)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%ProfFinBief, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.LASTCSREACH'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%ProfFinBief) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%ProfFinBief(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.LASTCSREACH'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.RelXFirstNdReach') > 0) then
        if (ASSOCIATED(Instance%absc_rel_ext_deb_bief)) then
           t1 = size(Instance%absc_rel_ext_deb_bief)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%absc_rel_ext_deb_bief, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = &
                 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.RELXFIRSTNDREACH'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%absc_rel_ext_deb_bief) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%absc_rel_ext_deb_bief(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.RELXFIRSTNDREACH'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.RelXLastNdReach') > 0) then
        if (ASSOCIATED(Instance%absc_rel_ext_fin_bief)) then
           t1 = size(Instance%absc_rel_ext_fin_bief)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%absc_rel_ext_fin_bief, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = &
                     'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.RELXLASTNDREACH'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%absc_rel_ext_fin_bief) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%absc_rel_ext_fin_bief(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.RELXLASTNDREACH'
              return
           endif
        endif
      else if ( index(NomVar, 'Model.F1') > 0) then
        if (ASSOCIATED(Instance%F1)) then
           t1 = size(Instance%F1, 1)
           t2 = size(Instance%F1, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%F1, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                 MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to deallocate MODEL_MASCARET_T.F1'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%F1).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%F1(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'SET_TAILLE_VAR_MODELE_MASCARET : Unable to allocate MODEL_MASCARET_T.F1'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      !-----------------------------------------------------------------------
      ! Appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type derive
      !-----------------------------------------------------------------------
      else if (INDEX(NomVar,'Model.Connect.') > 0) then
         err = SET_TAILLE_VAR_CONNECT(Instance%Connect, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%Connect'
            return
         endif
      else if (INDEX(NomVar,'Model.CrossSection.') > 0) then
         if (ASSOCIATED(Instance%Profils)) then
            t1 = SIZE(Instance%Profils)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_PROFIL(Instance%Profils(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.CROSSSECTION(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Profils, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.CROSSSECTION'
                  return
               endif
               ALLOCATE(Instance%Profils(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_PROFIL(Instance%Profils(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Profils'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Profils pas 'associated'
            ALLOCATE(Instance%Profils(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_PROFIL(Instance%Profils(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Profils'
                  return
              endif
            enddo
         endif

     ! partie non generee automatiquement mais a la main
         if (SIZE(Instance%Profils)/=0) then
           err = SET_TAILLE_VAR_PROFIL(Instance%Profils(index1), NomVar, NewT2, NewT3, Bidon, MessageErreurType)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'Unable to change the size of Instance%Profils'
              return
           endif
         endif
      ! fin de la partie non generee automatiquement
       else if (INDEX(NomVar,'Model.VDSection.') > 0) then
         err = SET_TAILLE_VAR_SECTION_PLAN(Instance%SectionPlan, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%SectionPlan'
            return
         endif
      else if (INDEX(NomVar,'Model.DryArea.') > 0) then
         if (ASSOCIATED(Instance%ZonesSeches)) then
            t1 = SIZE(Instance%ZonesSeches)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_ZONE_SECHE(Instance%ZonesSeches(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.DRYAREA(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%ZonesSeches, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.DRYAREA'
                  return
               endif
               ALLOCATE(Instance%ZonesSeches(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_ZONE_SECHE(Instance%ZonesSeches(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%ZonesSeches'
                     return
                  endif
               enddo
            endif
         else  ! Instance%ZonesSeches pas 'associated'
            ALLOCATE(Instance%ZonesSeches(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_ZONE_SECHE(Instance%ZonesSeches(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%ZonesSeches'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_ZONE_SECHE(Instance%ZonesSeches(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_MODELE_MASCARET = err
                MessageErreur = 'Unable to change the size of Instance%ZonesSeches'
                return
            endif
         enddo
      else if (INDEX(NomVar,'Model.FrictionZone.') > 0) then
         if (ASSOCIATED(Instance%ZonesFrottement)) then
            t1 = SIZE(Instance%ZonesFrottement)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_ZONE_FROT(Instance%ZonesFrottement(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FRICTIONAREA(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%ZonesFrottement, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FRICTIONAREA'
                  return
               endif
               ALLOCATE(Instance%ZonesFrottement(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_ZONE_FROT(Instance%ZonesFrottement(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%ZonesFrottement'
                     return
                  endif
               enddo
            endif
         else  ! Instance%ZonesFrottement pas 'associated'
            ALLOCATE(Instance%ZonesFrottement(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_ZONE_FROT(Instance%ZonesFrottement(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%ZonesFrottement'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_ZONE_FROT(Instance%ZonesFrottement(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_MODELE_MASCARET = err
                MessageErreur = 'Unable to change the size of Instance%ZonesFrottement'
                return
            endif
         enddo
      else if (INDEX(NomVar,'Model.Graph.') > 0) then
         if (ASSOCIATED(Instance%LoisHydrau)) then
            t1 = SIZE(Instance%LoisHydrau)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_LOI(Instance%LoisHydrau(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.GRAPH(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%LoisHydrau, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.GRAPH'
                  return
               endif
               ALLOCATE(Instance%LoisHydrau(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_LOI(Instance%LoisHydrau(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%LoisHydrau'
                     return
                  endif
               enddo
            endif
         else  ! Instance%LoisHydrau pas 'associated'
            ALLOCATE(Instance%LoisHydrau(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_LOI(Instance%LoisHydrau(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%LoisHydrau'
                  return
              endif
            enddo
         endif
         ! partie non generee automatiquement mais a la main
         if (SIZE(Instance%LoisHydrau)/=0) then
           err = SET_TAILLE_VAR_LOI(Instance%LoisHydrau(index1), NomVar, NewT2, NewT3, Bidon, MessageErreurType)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'Unable to change the size of Instance%LoisHydrau'
              return
           endif
         endif
         ! fin de la partie non generee automatiquement

      else if (INDEX(NomVar,'Model.Weir.') > 0) then
         if (ASSOCIATED(Instance%Singularites)) then
            t1 = SIZE(Instance%Singularites)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_SINGULARITE(Instance%Singularites(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.WEIR(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Singularites, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.WEIR'
                  return
               endif
               ALLOCATE(Instance%Singularites(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_SINGULARITE(Instance%Singularites(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Singularites'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Singularites pas 'associated'
            ALLOCATE(Instance%Singularites(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_SINGULARITE(Instance%Singularites(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Singularites'
                  return
              endif
            enddo
         endif
        ! partie non generee automatiquement mais a la main
         if (SIZE(Instance%Singularites)/=0) then
           err = SET_TAILLE_VAR_SINGULARITE(Instance%Singularites(index1), NomVar, NewT2, NewT3, Bidon, MessageErreurType)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'Unable to change the size of Instance%Singularites'
              return
           endif
         endif
        ! fin de la partie non generee automatiquement

      else if (INDEX(NomVar,'Model.LateralWeir.') > 0) then
         if (ASSOCIATED(Instance%Deversoirs)) then
            t1 = SIZE(Instance%Deversoirs)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_DEVERSOIR(Instance%Deversoirs(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LATERALWEIR(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Deversoirs, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LATERALWEIR'
                  return
               endif
               ALLOCATE(Instance%Deversoirs(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_DEVERSOIR(Instance%Deversoirs(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Deversoirs'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Deversoirs pas 'associated'
            ALLOCATE(Instance%Deversoirs(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_DEVERSOIR(Instance%Deversoirs(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Deversoirs'
                  return
              endif
            enddo
         endif
        ! partie non generee automatiquement mais a la main
         if (SIZE(Instance%Deversoirs)/=0) then
           err = SET_TAILLE_VAR_DEVERSOIR(Instance%Deversoirs(index1), NomVar, NewT2, NewT3, Bidon, MessageErreurType)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'Unable to change the size of Instance%Deversoirs'
              return
           endif
         endif
        ! fin de la partie non generee automatiquement

       else if (INDEX(NomVar,'Model.File.Listing.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%FichierListing, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%FichierListing'
            return
         endif
      else if (INDEX(NomVar,'Model.Link.') > 0) then
         if (ASSOCIATED(Instance%Liaisons)) then
            t1 = SIZE(Instance%Liaisons)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_LIAISON(Instance%Liaisons(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LINK(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Liaisons, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LINK'
                  return
               endif
               ALLOCATE(Instance%Liaisons(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_LIAISON(Instance%Liaisons(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Liaisons'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Liaisons pas 'associated'
            ALLOCATE(Instance%Liaisons(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_LIAISON(Instance%Liaisons(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Liaisons'
                  return
              endif
            enddo
         endif
         ! partie non generee automatiquement mais a la main
         if (SIZE(Instance%Liaisons)/=0) then
           err = SET_TAILLE_VAR_LIAISON(Instance%Liaisons(index1), NomVar, NewT2, NewT3, Bidon, MessageErreurType)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'Unable to change the size of Instance%Liaisons'
              return
           endif
         endif
         ! fin de la partie non generee automatiquement

      else if (INDEX(NomVar,'Model.File.Result2.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%FichierResultat2, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%FichierResultat2'
            return
         endif
      else if (INDEX(NomVar,'Model.File.Result.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%FichierResultat, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%FichierResultat'
            return
         endif
      else if (INDEX(NomVar,'Model.File.GeoStoArea.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%FichierGeomCasier, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%FichierGeomCasier'
            return
         endif
      else if (INDEX(NomVar,'Model.File.ResultStoArea.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%FichierResuCasier, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%FichierResuCasier'
            return
         endif
      else if (INDEX(NomVar,'Model.File.ResultLink.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%FichierResuLiaison, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%FichierResuLiaison'
            return
         endif
      else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%Tracer%FichierResuTracer, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%Tracer%FichierResuTracer'
            return
         endif
      else if (INDEX(NomVar,'Model.File.ListingStoArea.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%FichierListingCasier, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%FichierListingCasier'
            return
         endif
      else if (INDEX(NomVar,'Model.File.ListingLink.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%FichierListingLiaison, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%FichierListingLiaison'
            return
         endif
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%Tracer%FichierListingTracer, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%FichierListingTracer'
            return
         endif
      else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
         err = SET_TAILLE_VAR_FICHIER(Instance%Tracer%FichierConcInit, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%Tracer%FichierConcInit'
            return
         endif
      else if (INDEX(NomVar,'Model.Boundary.') > 0) then
         if (ASSOCIATED(Instance%Extremites)) then
            t1 = SIZE(Instance%Extremites)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_EXTREMITE(Instance%Extremites(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.BOUNDARY(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Extremites, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.BOUNDARY'
                  return
               endif
               ALLOCATE(Instance%Extremites(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_EXTREMITE(Instance%Extremites(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Extremites'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Extremites pas 'associated'
            ALLOCATE(Instance%Extremites(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_EXTREMITE(Instance%Extremites(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Extremites'
                  return
              endif
            enddo
         endif
         ! partie non generee automatiquement mais a la main
         if (SIZE(Instance%Extremites)/=0) then
           err = SET_TAILLE_VAR_EXTREMITE(Instance%Extremites(index1), NomVar, NewT2, NewT3, Bidon, MessageErreurType)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'Unable to change the size of Instance%Extremites'
              return
           endif
         endif
         ! fin de la partie non generee automatiquement

      else if (INDEX(NomVar,'Model.StorageArea.') > 0) then
         if (ASSOCIATED(Instance%Casiers)) then
            t1 = SIZE(Instance%Casiers)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_CASIER(Instance%Casiers(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.STORAGEAREA(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Casiers, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.STORAGEAREA'
                  return
               endif
               ALLOCATE(Instance%Casiers(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_CASIER(Instance%Casiers(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Casiers'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Casiers pas 'associated'
            ALLOCATE(Instance%Casiers(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_CASIER(Instance%Casiers(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Casiers'
                  return
              endif
            enddo
         endif
         ! partie non generee automatiquement mais a la main
         if (SIZE(Instance%Casiers)/=0) then
           err = SET_TAILLE_VAR_CASIER(Instance%Casiers(index1), NomVar, NewT2, NewT3, Bidon, MessageErreurType)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'Unable to change the size of Instance%Casiers'
              return
           endif
         endif
         ! fin de la partie non generee automatiquement
       else if (INDEX(NomVar,'Model.Tracer.') > 0) then
         err = SET_TAILLE_VAR_MODELE_TRACER(Instance%Tracer, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%SectionPlan'
            return
         endif
      else if (index(NomVar,'Model.Junction.') > 0) then
         if (ASSOCIATED(Instance%Confluents)) then
            t1 = SIZE(Instance%Confluents)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_CONFLUENT(Instance%Confluents(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.JUNCTION(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Confluents, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.JUNCTION'
                  return
               endif
               ALLOCATE(Instance%Confluents(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_CONFLUENT(Instance%Confluents(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Confluents'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Confluents pas 'associated'
            ALLOCATE(Instance%Confluents(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_CONFLUENT(Instance%Confluents(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Confluents'
                  return
              endif
            enddo
         endif
         ! partie non generee automatiquement mais a la main
         if (SIZE(Instance%Confluents)/=0) then
           err = SET_TAILLE_VAR_CONFLUENT(Instance%Confluents(index1), NomVar, NewT2, NewT3, Bidon, MessageErreurType)
           if (err /= 0) then
              SET_TAILLE_VAR_MODELE_MASCARET = err
              MessageErreur = 'Unable to change the size of Instance%Confluents'
              return
           endif
         endif
         ! fin de la partie non generee automatiquement
      else if (INDEX(NomVar,'Model.Inflow.') > 0) then
         if (ASSOCIATED(Instance%Apports)) then
            t1 = SIZE(Instance%Apports)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_APPORT(Instance%Apports(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.INFLOW(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%Apports, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.INFLOW'
                  return
               endif
               ALLOCATE(Instance%Apports(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_APPORT(Instance%Apports(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%Apports'
                     return
                  endif
               enddo
            endif
         else  ! Instance%Apports pas 'associated'
            ALLOCATE(Instance%Apports(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_APPORT(Instance%Apports(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%Apports'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_APPORT(Instance%Apports(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_MODELE_MASCARET = err
                MessageErreur = 'Unable to change the size of Instance%Apports'
                return
            endif
         enddo
      else if (INDEX(NomVar,'Model.Dam.') > 0) then
         err = SET_TAILLE_VAR_BARRAGE(Instance%Barrage, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%Barrage'
            return
         endif
      else if (INDEX(NomVar,'Model.ExternalInflow.') > 0) then
         if (ASSOCIATED(Instance%ApportsPluie)) then
            t1 = SIZE(Instance%ApportsPluie)
            if (t1 /= NewT1) then
               DO i=1, t1
                  err = DESALLOUE_APPORT_PLUIE(Instance%ApportsPluie(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.EXTERNALINFLOW(i)'
                     return
                  endif
               enddo
               DEALLOCATE(Instance%ApportsPluie, STAT=err)
               if (err /= 0) then
                  SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.EXTERNALINFLOW'
                  return
               endif
               ALLOCATE(Instance%ApportsPluie(NewT1), STAT=err)
               DO i=1, NewT1
                  err = NULLIFIER_APPORT_PLUIE(Instance%ApportsPluie(i), MessageErreurType)
                  if (err /= 0) then
                     SET_TAILLE_VAR_MODELE_MASCARET = err
                     MessageErreur = 'Unable to nullify the pointers Instance%ApportsPluie'
                     return
                  endif
               enddo
            endif
         else  ! Instance%ApportsPluie pas 'associated'
            ALLOCATE(Instance%ApportsPluie(NewT1), STAT=err)
            DO i=1, NewT1
              err = NULLIFIER_APPORT_PLUIE(Instance%ApportsPluie(i), MessageErreurType)
              if (err /= 0) then
                 SET_TAILLE_VAR_MODELE_MASCARET = err
                  MessageErreur = 'Unable to nullify the pointers Instance%ApportsPluie'
                  return
              endif
            enddo
         endif
         DO i=1, NewT1
            err = SET_TAILLE_VAR_APPORT_PLUIE(Instance%ApportsPluie(i), &
                                 NomVar, NewT2, NewT3, Bidon, MessageErreurType)
            if (err /= 0) then
               SET_TAILLE_VAR_MODELE_MASCARET = err
                MessageErreur = 'Unable to change the size of Instance%ApportsPluie'
                return
            endif
         enddo
      else if (INDEX(NomVar,'Model.VDCrossSection.') > 0) then
         err = SET_TAILLE_VAR_PROFIL_PLAN(Instance%ProfilPlan, &
                                   NomVar, NewT1, NewT2, NewT3, MessageErreurType)
         if (err /= 0) then
            SET_TAILLE_VAR_MODELE_MASCARET = err
            MessageErreur = 'Unable to change the size of Instance%ProfilPlan'
            return
         endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions SET_TAILLE_VAR_XXXX des membres de type derive
      !--------------------------------------------------------------------------------
      else
         SET_TAILLE_VAR_MODELE_MASCARET = 1
         MessageErreur         = "SET_TAILLE_VAR_MODELE_MASCARET - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_MODELE_MASCARET

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_MODELE_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_MODELE_MASCARET ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      GET_DOUBLE_MODELE_MASCARET = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Zbot') > 0) then
         valeur = Instance%ZREF(index1)
      else if ( index(NomVar, 'Model.LevRightBk') > 0) then
         valeur = Instance%RDC(index1)
      else if ( index(NomVar, 'Model.LevLeftBk') > 0) then
         valeur = Instance%RGC(index1)
      else if ( index(NomVar, 'Model.Abac') > 0) then
         valeur = Instance%abaque(index1, index2, index3)
      else if ( index(NomVar, 'Model.Heps') > 0) then
         valeur = Instance%HEPS
      else if ( index(NomVar, 'Model.FricCoefFP') > 0) then
         valeur = Instance%CF2(index1)
      else if ( index(NomVar, 'Model.FricCoefMainCh') > 0) then
         valeur = Instance%CF1(index1)
      else if ( index(NomVar, 'Model.LocalHeadLoss') > 0) then
         valeur = Instance%PCSing(index1)
      else if ( index(NomVar, 'Model.XDT') > 0) then
         valeur = Instance%XDT(index1)
      else if ( index(NomVar, 'Model.XD') > 0) then
         valeur = Instance%XD(index1)
      else if ( index(NomVar, 'Model.X') > 0) then
         valeur = Instance%X(index1)
      else if ( index(NomVar, 'Model.CourantNum') > 0) then
         valeur = Instance%CourantObj
      else if ( index(NomVar, 'Model.MaxCompTime') > 0) then
         valeur = Instance%TempsMaximum
      else if ( index(NomVar, 'Model.MaxControlZ') > 0) then
         valeur = Instance%Cote_max_controle
      else if ( index(NomVar, 'Model.InitTime') > 0) then
         valeur = Instance%TempsInitial
      else if ( index(NomVar, 'Model.DT') > 0) then
         valeur = Instance%DT
      else if ( index(NomVar, 'Model.LimFroude') > 0) then
         valeur = Instance%FroudeLim
      else if ( index(NomVar, 'Model.DZwave') > 0) then
         valeur = Instance%DZArriveeFront
      else if ( index(NomVar, 'Model.DZD') > 0) then
         valeur = Instance%DZD(index1)
      else if ( index(NomVar, 'Model.DZ') > 0) then
         valeur = Instance%DZ(index1)
      else if ( index(NomVar, 'Model.RelXFirstNdReach') > 0) then
         valeur = Instance%absc_rel_ext_deb_bief(index1)
      else if ( index(NomVar, 'Model.RelXLastNdReach') > 0) then
         valeur = Instance%absc_rel_ext_fin_bief(index1)
      else if ( index(NomVar, 'Model.F1') > 0) then
         valeur = Instance%F1(index1, index2)
      else if (INDEX(NomVar,'Model.CrossSection.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_PROFIL(instance%Profils(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.VDSection.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_SECTION_PLAN(instance%SectionPlan, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Graph.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_LOI(instance%LoisHydrau(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Weir.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_SINGULARITE(instance%Singularites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.LateralWeir.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_DEVERSOIR(instance%Deversoirs(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_LIAISON(instance%Liaisons(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Boundary.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_EXTREMITE(instance%Extremites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.StorageArea.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_CASIER(instance%Casiers(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_MODELE_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Junction.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_CONFLUENT(instance%Confluents(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Inflow.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_APPORT(instance%Apports(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Dam.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_BARRAGE(instance%Barrage, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.ExternalInflow.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_APPORT_PLUIE(instance%ApportsPluie(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.VDCrossSection.') > 0) then
           GET_DOUBLE_MODELE_MASCARET = GET_DOUBLE_PROFIL_PLAN(instance%ProfilPlan, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         GET_DOUBLE_MODELE_MASCARET = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_MODELE_MASCARET - Unknown variable name"
      end if
   end function GET_DOUBLE_MODELE_MASCARET


   function GET_INT_MODELE_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_MODELE_MASCARET    ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      GET_INT_MODELE_MASCARET = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'Model.ResultFmt2') > 0) then
         valeur = Instance%FormatResu2
      else if ( index(NomVar, 'Model.NodeRes') > 0) then
         valeur = Instance%SectionStockage(index1)
      else if ( index(NomVar, 'Model.RecOption') > 0) then
         valeur = Instance%OptionStockage
      else if ( index(NomVar, 'Model.ResultFmt') > 0) then
         valeur = Instance%FormatResu
      else if ( index(NomVar, 'Model.RecNbFstTimeStep') > 0) then
         valeur = Instance%PremierPasStocke
      else if ( index(NomVar, 'Model.RecNTimeStep') > 0) then
         valeur = Instance%PasImpression
      else if ( index(NomVar, 'Model.RecListNTimeStep') > 0) then
         valeur = Instance%PasStockage
      else if ( index(NomVar, 'Model.AlgoNet') > 0) then
         valeur = Instance%Algorithme(index1)
      else if ( index(NomVar, 'Model.1DMesh') > 0) then
         valeur = Instance%TypeMaillage
      else if ( index(NomVar, 'Model.IDT') > 0) then
         valeur = Instance%IDT(index1)
      else if ( index(NomVar, 'Model.GeoFileFmt') > 0) then
         valeur = Instance%FormatGeom
      else if ( index(NomVar, 'Model.FricLaw') > 0) then
         valeur = Instance%LoiFrottement
      else if ( index(NomVar, 'Model.MaxNbTimeStep') > 0) then
         valeur = Instance%NbPasTemps
      else if ( index(NomVar, 'Model.ControlSection') > 0) then
         valeur = Instance%Section_controle
      else if ( index(NomVar, 'Model.StopCriteria') > 0) then
         valeur = Instance%CritereArret
      else if ( index(NomVar, 'Model.Regime') > 0) then
         valeur = Instance%Regime
      else if ( index(NomVar, 'Model.CSectionLayout') > 0) then
         valeur = Instance%ModeleLit
      else if ( index(NomVar, 'Model.ValidType') > 0) then
         valeur = Instance%TypeValidation
      else if ( index(NomVar, 'Model.Kernel') > 0) then
         valeur = Instance%Noyau
      else if ( index(NomVar, 'Model.Version') > 0) then
         valeur = Instance%VersionCode
      else if ( index(NomVar, 'Model.FirstCSReach') > 0) then
         valeur = Instance%ProfDebBief(index1)
      else if ( index(NomVar, 'Model.LastCSReach') > 0) then
         valeur = Instance%ProfFinBief(index1)
      else if ( index(NomVar, 'Model.CQMV') > 0) then
         valeur = Instance%CQMV
       else if (INDEX(NomVar,'Model.Connect.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_CONNECT(instance%Connect, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.CrossSection.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_PROFIL(instance%Profils(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.DryArea.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_ZONE_SECHE(instance%ZonesSeches(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.FrictionZone.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_ZONE_FROT(instance%ZonesFrottement(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Graph.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_LOI(instance%LoisHydrau(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Weir.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_SINGULARITE(instance%Singularites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.LateralWeir.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_DEVERSOIR(instance%Deversoirs(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Listing.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%FichierListing, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_LIAISON(instance%Liaisons(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Result2.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%FichierResultat2, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Result.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%FichierResultat, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.GeoStoArea.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%FichierGeomCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultStoArea.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%FichierResuCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultLink.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%FichierResuLiaison, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%Tracer%FichierResuTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingStoArea.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%FichierListingCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingLink.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%FichierListingLiaison, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%Tracer%FichierListingTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_FICHIER(instance%Tracer%FichierConcInit, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Boundary.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_EXTREMITE(instance%Extremites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.StorageArea.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_CASIER(instance%Casiers(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_MODELE_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Junction.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_CONFLUENT(instance%Confluents(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Inflow.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_APPORT(instance%Apports(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Dam.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_BARRAGE(instance%Barrage, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.ExternalInflow.') > 0) then
           GET_INT_MODELE_MASCARET = GET_INT_APPORT_PLUIE(instance%ApportsPluie(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else
         GET_INT_MODELE_MASCARET = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_MODELE_MASCARET - Unknown variable name"
      end if
   end function GET_INT_MODELE_MASCARET


   function GET_BOOL_MODELE_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_BOOL_MODELE_MASCARET   ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(out):: valeur                     ! valeur du logical de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      GET_BOOL_MODELE_MASCARET = 0
      valeur                = .FALSE.
      MessageErreur          = ""

      if ( index(NomVar, 'Model.CSectionAbsX') > 0) then
         valeur = Instance%ProfAbs
      else if ( index(NomVar, 'Model.HeadLossEnlarg') > 0) then
         valeur = Instance%PerteElargissementTrans
      else if ( index(NomVar, 'Model.ImpSupCritKern') > 0) then
         valeur = Instance%ImplicitTrans
      else if ( index(NomVar, 'Model.HeadLossJunc') > 0) then
         valeur = Instance%PerteChargeConfluent
      else if ( index(NomVar, 'Model.StoArea') > 0) then
         valeur = Instance%OptionCasier
      else if ( index(NomVar, 'Model.TracerOn') > 0) then
         valeur = Instance%OptionTracer
      else if ( index(NomVar, 'Model.RecVar') > 0) then
         valeur = Instance%VarSto(index1)
      else if ( index(NomVar, 'Model.CompVar') > 0) then
         valeur = Instance%VarCalc(index1)
      else if ( index(NomVar, 'Model.PrintComp') > 0) then
         valeur = Instance%ImpressionCalcul
      else if ( index(NomVar, 'Model.PrintVertCSection') > 0) then
         valeur = Instance%ImpressionPlanim
      else if ( index(NomVar, 'Model.HotStart') > 0) then
         valeur = Instance%RepriseCalcul
      else if ( index(NomVar, 'Model.InefFlowArea') > 0) then
         valeur = Instance%PresenceZoneStockage
      else if ( index(NomVar, 'Model.InterpFriction') > 0) then
         valeur = Instance%InterpolLinStrickler
      else if ( index(NomVar, 'Model.ProgOverFlowIFA') > 0) then
         valeur = Instance%DebProgressifZS
      else if ( index(NomVar, 'Model.ProgOverFlowFP') > 0) then
         valeur = Instance%DebProgressifLM
      else if ( index(NomVar, 'Model.FricVertWall') > 0) then
         valeur = Instance%FrottParoiVerticale
      else if ( index(NomVar, 'Model.VarTimeStep') > 0) then
         valeur = Instance%PasTempsVariable
      else if ( index(NomVar, 'Model.ImpFric') > 0) then
         valeur = Instance%FrottementImplicite
      else if ( index(NomVar, 'Model.ValidComp') > 0) then
         valeur = Instance%CalculValidation
      else if ( index(NomVar, 'Model.DamBrkFldWave') > 0) then
         valeur = Instance%OndeSubm
      else if ( index(NomVar, 'Model.Opt') > 0) then
         valeur = Instance%Opt
      else if ( index(NomVar, 'Model.Boussinesq') > 0) then
         valeur = Instance%Boussinesq
      else if ( index(NomVar, 'Model.NoConvection') > 0) then
         valeur = Instance%NoConvection
      else if (INDEX(NomVar,'Model.Weir.') > 0) then
           GET_BOOL_MODELE_MASCARET = GET_BOOL_SINGULARITE(instance%Singularites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.') > 0) then
           GET_BOOL_MODELE_MASCARET = GET_BOOL_MODELE_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         GET_BOOL_MODELE_MASCARET = 1
         valeur                = .FALSE.
         MessageErreur         = "GET_BOOL_MODELE_MASCARET - Unknown variable name"
      end if
   end function GET_BOOL_MODELE_MASCARET


   function GET_STRING_MODELE_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_MODELE_MASCARET ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      GET_STRING_MODELE_MASCARET = 0
      valeur                = ""
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Title') > 0) then
         valeur = Instance%TitreCas
      else if (INDEX(NomVar,'Model.CrossSection.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_PROFIL(instance%Profils(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Graph.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_LOI(instance%LoisHydrau(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Weir.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_SINGULARITE(instance%Singularites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.LateralWeir.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_DEVERSOIR(instance%Deversoirs(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Listing.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%FichierListing, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Result2.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%FichierResultat2, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Result.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%FichierResultat, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.GeoStoArea.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%FichierGeomCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultStoArea.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%FichierResuCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultLink.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%FichierResuLiaison, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%Tracer%FichierResuTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingStoArea.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%FichierListingCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingLink.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%FichierListingLiaison, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%Tracer%FichierListingTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_FICHIER(instance%Tracer%FichierConcInit, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Boundary.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_EXTREMITE(instance%Extremites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_MODELE_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Junction.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_CONFLUENT(instance%Confluents(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Inflow.') > 0) then
           GET_STRING_MODELE_MASCARET = GET_STRING_APPORT(instance%Apports(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else
         GET_STRING_MODELE_MASCARET = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_MODELE_MASCARET - Unknown variable name"
      end if
   end function GET_STRING_MODELE_MASCARET



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_MODELE_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_MODELE_MASCARET ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      SET_DOUBLE_MODELE_MASCARET = 0
      MessageErreur          = ""

      if ( index(NomVar,'Model.Zbot') > 0) then
         Instance%ZREF(index1) = valeur
      else if ( index(NomVar,'Model.LevRightBk') > 0) then
         Instance%RDC(index1) = valeur
      else if ( index(NomVar,'Model.LevLeftBk') > 0) then
         Instance%RGC(index1) = valeur
      else if ( index(NomVar,'Model.Abac') > 0) then
         Instance%abaque(index1, index2, index3) = valeur
      else if ( index(NomVar,'Model.Heps') > 0) then
         Instance%HEPS = valeur
      else if ( index(NomVar,'Model.FricCoefFP') > 0) then
         Instance%CF2(index1) = valeur
      else if ( index(NomVar,'Model.FricCoefMainCh') > 0) then
         Instance%CF1(index1) = valeur
      else if ( index(NomVar,'Model.LocalHeadLoss') > 0) then
         Instance%PCSing(index1) = valeur
      else if ( index(NomVar,'Model.XDT') > 0) then
         Instance%XDT(index1) = valeur
      else if ( index(NomVar,'Model.XD') > 0) then
            Instance%XD(index1) = valeur
      else if ( index(NomVar,'Model.X') > 0) then
         Instance%X(index1) = valeur
      else if ( index(NomVar,'Model.CourantNum') > 0) then
         Instance%CourantObj = valeur
      else if ( index(NomVar,'Model.MaxCompTime') > 0) then
         Instance%TempsMaximum = valeur
      else if ( index(NomVar,'Model.MaxControlZ') > 0) then
         Instance%Cote_max_controle = valeur
      else if ( index(NomVar,'Model.InitTime') > 0) then
         Instance%TempsInitial = valeur
      else if ( index(NomVar,'Model.DT') > 0) then
         Instance%DT = valeur
      else if ( index(NomVar,'Model.LimFroude') > 0) then
         Instance%FroudeLim = valeur
      else if ( index(NomVar,'Model.DZwave') > 0) then
         Instance%DZArriveeFront = valeur
      else if ( index(NomVar,'Model.DZD') > 0) then
            Instance%DZD(index1) = valeur
      else if ( index(NomVar,'Model.DZ') > 0) then
         Instance%DZ(index1) = valeur
      else if ( index(NomVar,'Model.RelXFirstNdReach') > 0) then
         Instance%absc_rel_ext_deb_bief(index1) = valeur
      else if ( index(NomVar,'Model.RelXLastNdReach') > 0) then
         Instance%absc_rel_ext_fin_bief(index1) = valeur
      else if ( index(NomVar,'Model.F1') > 0) then
         Instance%F1(index1, index2) = valeur
      else if (INDEX(NomVar,'Model.CrossSection.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_PROFIL(instance%Profils(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.VDSection.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_SECTION_PLAN(instance%SectionPlan, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Graph.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_LOI(instance%LoisHydrau(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Weir.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_SINGULARITE(instance%Singularites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.LateralWeir.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_DEVERSOIR(instance%Deversoirs(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_LIAISON(instance%Liaisons(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Boundary.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_EXTREMITE(instance%Extremites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.StorageArea.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_CASIER(instance%Casiers(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_MODELE_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Junction.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_CONFLUENT(instance%Confluents(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Inflow.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_APPORT(instance%Apports(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Dam.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_BARRAGE(instance%Barrage, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.ExternalInflow.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_APPORT_PLUIE(instance%ApportsPluie(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.VDCrossSection.') > 0) then
           SET_DOUBLE_MODELE_MASCARET = SET_DOUBLE_PROFIL_PLAN(instance%ProfilPlan, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         SET_DOUBLE_MODELE_MASCARET = 1
         MessageErreur         = "SET_DOUBLE_MODELE_MASCARET - Unknown variable name"
      end if
   end function SET_DOUBLE_MODELE_MASCARET


   function SET_INT_MODELE_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_MODELE_MASCARET    ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      SET_INT_MODELE_MASCARET = 0
      MessageErreur          = ""

      if ( index(NomVar,'Model.ResultFmt2') > 0) then
         Instance%FormatResu2 = valeur
      else if ( index(NomVar,'Model.NodeRes') > 0) then
         Instance%SectionStockage(index1) = valeur
      else if ( index(NomVar,'Model.RecOption') > 0) then
         Instance%OptionStockage = valeur
      else if ( index(NomVar,'Model.ResultFmt') > 0) then
         Instance%FormatResu = valeur
      else if ( index(NomVar,'Model.RecNbFstTimeStep') > 0) then
         Instance%PremierPasStocke = valeur
      else if ( index(NomVar,'Model.RecNTimeStep') > 0) then
         Instance%PasImpression = valeur
      else if ( index(NomVar,'Model.RecListNTimeStep') > 0) then
         Instance%PasStockage = valeur
      else if ( index(NomVar,'Model.AlgoNet') > 0) then
         Instance%Algorithme(index1) = valeur
      else if ( index(NomVar,'Model.1DMesh') > 0) then
         Instance%TypeMaillage = valeur
      else if ( index(NomVar,'Model.IDT') > 0) then
         Instance%IDT(index1) = valeur
      else if ( index(NomVar,'Model.GeoFileFmt') > 0) then
         Instance%FormatGeom = valeur
      else if ( index(NomVar,'Model.FricLaw') > 0) then
         Instance%LoiFrottement = valeur
      else if ( index(NomVar,'Model.MaxNbTimeStep') > 0) then
         Instance%NbPasTemps = valeur
      else if ( index(NomVar,'Model.ControlSection') > 0) then
         Instance%Section_controle = valeur
      else if ( index(NomVar,'Model.StopCriteria') > 0) then
         Instance%CritereArret = valeur
      else if ( index(NomVar,'Model.Regime') > 0) then
         Instance%Regime = valeur
      else if ( index(NomVar,'Model.CSectionLayout') > 0) then
         Instance%ModeleLit = valeur
      else if ( index(NomVar,'Model.ValidType') > 0) then
         Instance%TypeValidation = valeur
      else if ( index(NomVar,'Model.Kernel') > 0) then
         Instance%Noyau = valeur
      else if ( index(NomVar,'Model.Version') > 0) then
         Instance%VersionCode = valeur
      else if ( index(NomVar,'Model.FirstCSReach') > 0) then
         Instance%ProfDebBief(index1) = valeur
      else if ( index(NomVar,'Model.LastCSReach') > 0) then
         Instance%ProfFinBief(index1) = valeur
      else if ( index(NomVar,'Model.CQMV') > 0) then
         Instance%CQMV = valeur
       else if (INDEX(NomVar,'Model.Connect.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_CONNECT(instance%Connect, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.CrossSection.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_PROFIL(instance%Profils(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.DryArea.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_ZONE_SECHE(instance%ZonesSeches(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.FrictionZone.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_ZONE_FROT(instance%ZonesFrottement(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Graph.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_LOI(instance%LoisHydrau(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Weir.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_SINGULARITE(instance%Singularites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.LateralWeir.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_DEVERSOIR(instance%Deversoirs(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Listing.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%FichierListing, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Link.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_LIAISON(instance%Liaisons(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Result2.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%FichierResultat2, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Result.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%FichierResultat, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.GeoStoArea.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%FichierGeomCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultStoArea.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%FichierResuCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultLink.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%FichierResuLiaison, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%Tracer%FichierResuTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingStoArea.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%FichierListingCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingLink.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%FichierListingLiaison, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%Tracer%FichierListingTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_FICHIER(instance%Tracer%FichierConcInit, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Boundary.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_EXTREMITE(instance%Extremites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.StorageArea.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_CASIER(instance%Casiers(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_MODELE_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Junction.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_CONFLUENT(instance%Confluents(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Inflow.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_APPORT(instance%Apports(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Dam.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_BARRAGE(instance%Barrage, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.ExternalInflow.') > 0) then
           SET_INT_MODELE_MASCARET = SET_INT_APPORT_PLUIE(instance%ApportsPluie(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else
         SET_INT_MODELE_MASCARET = 1
         MessageErreur         = "SET_INT_MODELE_MASCARET - Unknown variable name"
      end if
   end function SET_INT_MODELE_MASCARET


   function SET_BOOL_MODELE_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_BOOL_MODELE_MASCARET   ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      logical,                intent(in) :: valeur                     ! valeur du logical de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      SET_BOOL_MODELE_MASCARET = 0
      MessageErreur          = ""

      if ( index(NomVar,'Model.CSectionAbsX') > 0) then
         Instance%ProfAbs = valeur
      else if ( index(NomVar,'Model.HeadLossEnlarg') > 0) then
         Instance%PerteElargissementTrans = valeur
      else if ( index(NomVar,'Model.ImpSupCritKern') > 0) then
         Instance%ImplicitTrans = valeur
      else if ( index(NomVar,'Model.HeadLossJunc') > 0) then
         Instance%PerteChargeConfluent = valeur
      else if ( index(NomVar,'Model.TracerOn') > 0) then
         Instance%OptionTracer = valeur
      else if ( index(NomVar,'Model.RecVar') > 0) then
         Instance%VarSto(index1) = valeur
      else if ( index(NomVar,'Model.CompVar') > 0) then
         Instance%VarCalc(index1) = valeur
      else if ( index(NomVar,'Model.PrintComp') > 0) then
         Instance%ImpressionCalcul = valeur
      else if ( index(NomVar,'Model.PrintVertCSection') > 0) then
         Instance%ImpressionPlanim = valeur
      else if ( index(NomVar,'Model.HotStart') > 0) then
         Instance%RepriseCalcul = valeur
      else if ( index(NomVar,'Model.InefFlowArea') > 0) then
         Instance%PresenceZoneStockage = valeur
      else if ( index(NomVar,'Model.InterpFriction') > 0) then
         Instance%InterpolLinStrickler = valeur
      else if ( index(NomVar,'Model.ProgOverFlowIFA') > 0) then
         Instance%DebProgressifZS = valeur
      else if ( index(NomVar,'Model.ProgOverFlowFP') > 0) then
         Instance%DebProgressifLM = valeur
      else if ( index(NomVar,'Model.FricVertWall') > 0) then
         Instance%FrottParoiVerticale = valeur
      else if ( index(NomVar,'Model.VarTimeStep') > 0) then
         Instance%PasTempsVariable = valeur
      else if ( index(NomVar,'Model.ImpFric') > 0) then
         Instance%FrottementImplicite = valeur
      else if ( index(NomVar,'Model.ValidComp') > 0) then
         Instance%CalculValidation = valeur
      else if ( index(NomVar,'Model.DamBrkFldWave') > 0) then
         Instance%OndeSubm = valeur
      else if ( index(NomVar,'Model.Opt') > 0) then
         Instance%Opt = valeur
      else if ( index(NomVar,'Model.Boussinesq') > 0) then
         Instance%Boussinesq = valeur
      else if ( index(NomVar,'Model.NoConvection') > 0) then
         Instance%NoConvection = valeur
      else if (INDEX(NomVar,'Model.Weir.') > 0) then
           SET_BOOL_MODELE_MASCARET = SET_BOOL_SINGULARITE(instance%Singularites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.') > 0) then
           SET_BOOL_MODELE_MASCARET = SET_BOOL_MODELE_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else
         SET_BOOL_MODELE_MASCARET = 1
         MessageErreur         = "SET_BOOL_MODELE_MASCARET - Unknown variable name"
      end if
   end function SET_BOOL_MODELE_MASCARET


   function SET_STRING_MODELE_MASCARET(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_MODELE_MASCARET ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur
      integer                            :: bidon1                         ! variable locale non utilise

      SET_STRING_MODELE_MASCARET = 0
      MessageErreur          = ""

      if ( index(NomVar, 'Model.Title') > 0) then
         Instance%TitreCas = valeur(1:255)
      else if (INDEX(NomVar,'Model.CrossSection.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_PROFIL(instance%Profils(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Graph.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_LOI(instance%LoisHydrau(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Weir.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_SINGULARITE(instance%Singularites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.LateralWeir.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_DEVERSOIR(instance%Deversoirs(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Listing.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%FichierListing, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Result2.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%FichierResultat2, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Result.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%FichierResultat, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.GeoStoArea.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%FichierGeomCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultStoArea.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%FichierResuCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ResultLink.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%FichierResuLiaison, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Result.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%Tracer%FichierResuTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingStoArea.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%FichierListingCasier, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.ListingLink.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%FichierListingLiaison, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.Listing.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%Tracer%FichierListingTracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.File.Tracer.IniConc.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_FICHIER(instance%Tracer%FichierConcInit, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Boundary.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_EXTREMITE(instance%Extremites(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Tracer.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_MODELE_TRACER(instance%Tracer, NomVar, index1,&
                                         index2, index3, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Junction.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_CONFLUENT(instance%Confluents(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else if (INDEX(NomVar,'Model.Inflow.') > 0) then
           SET_STRING_MODELE_MASCARET = SET_STRING_APPORT(instance%Apports(index1), NomVar, index2,&
                                         index3, bidon1, valeur, MessageErreur)
      else
         SET_STRING_MODELE_MASCARET = 1
         MessageErreur         = "SET_STRING_MODELE_MASCARET - Unknown variable name"
      end if
   end function SET_STRING_MODELE_MASCARET


! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_MODELE_MASCARET(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_MODELE_MASCARET  ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      integer                            :: i
      character(LEN=256)                 :: MessageErreurType
      DESALLOUE_MODELE_MASCARET = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
      if (ASSOCIATED(Instance%ZREF)) then
          taille = SIZE(Instance%ZREF)
          if (taille > 0) then
              DEALLOCATE(Instance%ZREF, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.ZBOT'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%ZREF)
      if (ASSOCIATED(Instance%RDC)) then
          taille = SIZE(Instance%RDC)
          if (taille > 0) then
              DEALLOCATE(Instance%RDC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LEVRIGHTBK'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%RDC)
      if (ASSOCIATED(Instance%RGC)) then
          taille = SIZE(Instance%RGC)
          if (taille > 0) then
              DEALLOCATE(Instance%RGC, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LEVLEFTBK'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%RGC)
      if (ASSOCIATED(Instance%CF2)) then
          taille = SIZE(Instance%CF2)
          if (taille > 0) then
              DEALLOCATE(Instance%CF2, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FRICCOEFFP'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%CF2)
      if (ASSOCIATED(Instance%CF1)) then
          taille = SIZE(Instance%CF1)
          if (taille > 0) then
              DEALLOCATE(Instance%CF1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FRICCOEFMAINCH'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%CF1)
      if (ASSOCIATED(Instance%PCSing)) then
          taille = SIZE(Instance%PCSing)
          if (taille > 0) then
              DEALLOCATE(Instance%PCSing, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LOCALHEADLOSS'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%PCSing)
      if (ASSOCIATED(Instance%XDT)) then
          taille = SIZE(Instance%XDT)
          if (taille > 0) then
              DEALLOCATE(Instance%XDT, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.XDT'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%XDT)
      if (ASSOCIATED(Instance%X)) then
          taille = SIZE(Instance%X)
          if (taille > 0) then
              DEALLOCATE(Instance%X, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.X'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%X)
      if (ASSOCIATED(Instance%SectionStockage)) then
          taille = SIZE(Instance%SectionStockage)
          if (taille > 0) then
              DEALLOCATE(Instance%SectionStockage, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.NODERES'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SectionStockage)
      if (ASSOCIATED(Instance%Algorithme)) then
          taille = SIZE(Instance%Algorithme)
          if (taille > 0) then
              DEALLOCATE(Instance%Algorithme, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.ALGONET'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%Algorithme)
      if (ASSOCIATED(Instance%IDT)) then
          taille = SIZE(Instance%IDT)
          if (taille > 0) then
              DEALLOCATE(Instance%IDT, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.IDT'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%IDT)
      if (ASSOCIATED(Instance%DZ)) then
          taille = SIZE(Instance%DZ)
          if (taille > 0) then
              DEALLOCATE(Instance%DZ, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.DZ'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DZ)
      if (ASSOCIATED(Instance%XD)) then
          taille = SIZE(Instance%XD)
          if (taille > 0) then
              DEALLOCATE(Instance%XD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.XD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%XD)
      if (ASSOCIATED(Instance%DZD)) then
          taille = SIZE(Instance%DZD)
          if (taille > 0) then
              DEALLOCATE(Instance%DZD, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.DZD'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%DZD)
      if (ASSOCIATED(Instance%ProfDebBief)) then
          taille = SIZE(Instance%ProfDebBief)
          if (taille > 0) then
              DEALLOCATE(Instance%ProfDebBief, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FIRSTCSREACH'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%ProfDebBief)
      if (ASSOCIATED(Instance%ProfFinBief)) then
          taille = SIZE(Instance%ProfFinBief)
          if (taille > 0) then
              DEALLOCATE(Instance%ProfFinBief, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LASTCSREACH'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%ProfFinBief)
      if (ASSOCIATED(Instance%absc_rel_ext_deb_bief)) then
          taille = SIZE(Instance%absc_rel_ext_deb_bief)
          if (taille > 0) then
              DEALLOCATE(Instance%absc_rel_ext_deb_bief, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.RELXFIRSTNDREACH'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%absc_rel_ext_deb_bief)
      if (ASSOCIATED(Instance%absc_rel_ext_fin_bief)) then
          taille = SIZE(Instance%absc_rel_ext_fin_bief)
          if (taille > 0) then
              DEALLOCATE(Instance%absc_rel_ext_fin_bief, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.RELXLASTNDREACH'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%absc_rel_ext_fin_bief)
      if (ASSOCIATED(Instance%F1)) then
          taille = SIZE(Instance%F1, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%F1, STAT=err)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.F1'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%F1)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

      !-----------------------------------------------------------------------
      ! Appels aux fonctions desalloue des membres de type derive
      !-----------------------------------------------------------------------
      err = DESALLOUE_CONNECT(Instance%Connect, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.CONNECT'
      endif
      if (ASSOCIATED(Instance%Profils)) then
          taille = SIZE(Instance%Profils)
          DO i=1, taille
              err = DESALLOUE_PROFIL(Instance%Profils(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.CROSSSECTION(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Profils, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.CROSSSECTION'
              return
          endif
          NULLIFY(Instance%Profils)
      endif
      err = DESALLOUE_SECTION_PLAN(Instance%SectionPlan, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.VDSECTION'
      endif
      if (ASSOCIATED(Instance%ZonesSeches)) then
          taille = SIZE(Instance%ZonesSeches)
          DO i=1, taille
              err = DESALLOUE_ZONE_SECHE(Instance%ZonesSeches(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.DRYAREA(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%ZonesSeches, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.DRYAREA'
              return
          endif
          NULLIFY(Instance%ZonesSeches)
      endif
      if (ASSOCIATED(Instance%ZonesFrottement)) then
          taille = SIZE(Instance%ZonesFrottement)
          DO i=1, taille
              err = DESALLOUE_ZONE_FROT(Instance%ZonesFrottement(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FRICTIONAREA(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%ZonesFrottement, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FRICTIONAREA'
              return
          endif
          NULLIFY(Instance%ZonesFrottement)
      endif
      if (ASSOCIATED(Instance%LoisHydrau)) then
          taille = SIZE(Instance%LoisHydrau)
          DO i=1, taille
              err = DESALLOUE_LOI(Instance%LoisHydrau(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.GRAPH(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%LoisHydrau, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.GRAPH'
              return
          endif
          NULLIFY(Instance%LoisHydrau)
      endif
      if (ASSOCIATED(Instance%Singularites)) then
          taille = SIZE(Instance%Singularites)
          DO i=1, taille
              err = DESALLOUE_SINGULARITE(Instance%Singularites(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.WEIR(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Singularites, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.WEIR'
              return
          endif
          NULLIFY(Instance%Singularites)
      endif
      if (ASSOCIATED(Instance%Deversoirs)) then
          taille = SIZE(Instance%Deversoirs)
          DO i=1, taille
              err = DESALLOUE_DEVERSOIR(Instance%Deversoirs(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LATERALWEIR(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Deversoirs, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LATERALWEIR'
              return
          endif
          NULLIFY(Instance%Deversoirs)
      endif
      err = DESALLOUE_FICHIER(Instance%FichierListing, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILELISTING'
      endif
      if (ASSOCIATED(Instance%Liaisons)) then
          taille = SIZE(Instance%Liaisons)
          DO i=1, taille
              err = DESALLOUE_LIAISON(Instance%Liaisons(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LINK(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Liaisons, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.LINK'
              return
          endif
          NULLIFY(Instance%Liaisons)
      endif
      err = DESALLOUE_FICHIER(Instance%FichierResultat, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.RESULT'
      endif
      err = DESALLOUE_FICHIER(Instance%FichierResultat2, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.RESULT2'
      endif
      err = DESALLOUE_FICHIER(Instance%FichierGeomCasier, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.GEOSTOAREA'
      endif
      err = DESALLOUE_FICHIER(Instance%FichierResuCasier, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.RESULTSTOAREA'
      endif
      err = DESALLOUE_FICHIER(Instance%FichierResuLiaison, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.RESULTLINK'
      endif
      err = DESALLOUE_FICHIER(Instance%Tracer%FichierResuTracer, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.RESUTTRACER'
      endif
      err = DESALLOUE_FICHIER(Instance%FichierListingCasier, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.RESULTLISTINGSTOAREA'
      endif
      err = DESALLOUE_FICHIER(Instance%FichierListingLiaison, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.LISTINGLINK'
      endif
      err = DESALLOUE_FICHIER(Instance%Tracer%FichierListingTracer, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.LISTINGTRACER'
      endif
      err = DESALLOUE_FICHIER(Instance%Tracer%FichierConcInit, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.FILE.CONCINITRACER'
      endif
      if (ASSOCIATED(Instance%Extremites)) then
          taille = SIZE(Instance%Extremites)
          DO i=1, taille
              err = DESALLOUE_EXTREMITE(Instance%Extremites(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.BOUNDARY(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Extremites, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.BOUNDARY'
              return
          endif
          NULLIFY(Instance%Extremites)
      endif
      if (ASSOCIATED(Instance%Casiers)) then
          taille = SIZE(Instance%Casiers)
          DO i=1, taille
              err = DESALLOUE_CASIER(Instance%Casiers(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.STORAGEAREA(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Casiers, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.STORAGEAREA'
              return
          endif
          NULLIFY(Instance%Casiers)
      endif
      err = DESALLOUE_MODELE_TRACER(Instance%Tracer, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.TRACER'
      endif
      if (ASSOCIATED(Instance%Confluents)) then
          taille = SIZE(Instance%Confluents)
          DO i=1, taille
              err = DESALLOUE_CONFLUENT(Instance%Confluents(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.JUNCTION(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Confluents, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.JUNCTION'
              return
          endif
          NULLIFY(Instance%Confluents)
      endif
      if (ASSOCIATED(Instance%Apports)) then
          taille = SIZE(Instance%Apports)
          DO i=1, taille
              err = DESALLOUE_APPORT(Instance%Apports(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.INFLOW(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%Apports, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.INFLOW'
              return
          endif
          NULLIFY(Instance%Apports)
      endif
      err = DESALLOUE_BARRAGE(Instance%Barrage, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.DAM'
      endif
      if (ASSOCIATED(Instance%ApportsPluie)) then
          taille = SIZE(Instance%ApportsPluie)
          DO i=1, taille
              err = DESALLOUE_APPORT_PLUIE(Instance%ApportsPluie(i), MessageErreurType)
              if (err /= 0) then
                  DESALLOUE_MODELE_MASCARET = err
                  MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.EXTERNALINFLOW(i)'
                  return
              endif
          enddo
          DEALLOCATE(Instance%ApportsPluie, STAT=err)
          if (err /= 0) then
              DESALLOUE_MODELE_MASCARET = err
              MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.EXTERNALINFLOW'
              return
          endif
          NULLIFY(Instance%ApportsPluie)
      endif
      err = DESALLOUE_PROFIL_PLAN(Instance%ProfilPlan, MessageErreurType)
      if (err /= 0) then
          DESALLOUE_MODELE_MASCARET = err
          MessageErreur = 'Unable to deallocate MODEL_MASCARET_T.VDCROSSSECTION'
      endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions desalloue des membres de type derive
      !--------------------------------------------------------------------------------
   end function DESALLOUE_MODELE_MASCARET

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_MODELE_MASCARET(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_MODELE_MASCARET  ! different de 0 si erreur
      type(MODELE_MASCARET_T),intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: err
      character(LEN=256)                 :: MessageErreurType
      NULLIFIER_MODELE_MASCARET = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%ZREF)
      NULLIFY(Instance%RDC)
      NULLIFY(Instance%RGC)
      NULLIFY(Instance%CF2)
      NULLIFY(Instance%CF1)
      NULLIFY(Instance%PCSing)
      NULLIFY(Instance%XDT)
      NULLIFY(Instance%X)
      NULLIFY(Instance%SectionStockage)
      NULLIFY(Instance%Algorithme)
      NULLIFY(Instance%IDT)
      NULLIFY(Instance%DZ)
      NULLIFY(Instance%XD)
      NULLIFY(Instance%DZD)
      NULLIFY(Instance%ProfDebBief)
      NULLIFY(Instance%ProfFinBief)
      NULLIFY(Instance%absc_rel_ext_deb_bief)
      NULLIFY(Instance%absc_rel_ext_fin_bief)
      NULLIFY(Instance%F1)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

      !-----------------------------------------------------------------------
      ! Appels aux fonctions nullifier des membres de type derive
      !-----------------------------------------------------------------------
      err = NULLIFIER_CONNECT(Instance%Connect, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.CONNECT'
          return
      endif
      NULLIFY(Instance%Profils)
      err = NULLIFIER_SECTION_PLAN(Instance%SectionPlan, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.VDSECTION'
          return
      endif
      NULLIFY(Instance%ZonesSeches)
      NULLIFY(Instance%ZonesFrottement)
      NULLIFY(Instance%LoisHydrau)
      NULLIFY(Instance%Singularites)
      NULLIFY(Instance%Deversoirs)
      err = NULLIFIER_FICHIER(Instance%FichierListing, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILELISTING'
          return
      endif
      NULLIFY(Instance%Liaisons)
      err = NULLIFIER_FICHIER(Instance%FichierResultat, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.RESULT'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%FichierResultat2, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.RESULT2'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%FichierGeomCasier, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.GEOSTOAREA'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%FichierResuCasier, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.RESULTSTOAREA'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%FichierResuLiaison, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.RESULTLINK'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%Tracer%FichierResuTracer, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.RESULTTRACER'
          return
      endif
       err = NULLIFIER_FICHIER(Instance%FichierListingCasier, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.RESULTLISTINGSTOAREA'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%FichierListingLiaison, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.LISTINGLINK'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%Tracer%FichierListingTracer, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.LISTINGTRACER'
          return
      endif
      err = NULLIFIER_FICHIER(Instance%Tracer%FichierConcInit, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.FILE.CONCINITRACER'
          return
      endif
      NULLIFY(Instance%Extremites)
      NULLIFY(Instance%Casiers)
      err = NULLIFIER_MODELE_TRACER(Instance%Tracer, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.TRACER'
          return
      endif
      NULLIFY(Instance%Confluents)
      NULLIFY(Instance%Apports)
      err = NULLIFIER_BARRAGE(Instance%Barrage, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.DAM'
          return
      endif
      NULLIFY(Instance%ApportsPluie)
      err = NULLIFIER_PROFIL_PLAN(Instance%ProfilPlan, MessageErreurType)
      if (err /= 0) then
          NULLIFIER_MODELE_MASCARET = err
          MessageErreur = 'Unable to nullify MODEL_MASCARET_T.VDCROSSSECTION'
          return
      endif
      !--------------------------------------------------------------------------------
      ! Fin des appels aux fonctions nullifier des membres de type derive
      !--------------------------------------------------------------------------------
   end function NULLIFIER_MODELE_MASCARET


end module M_MODELE_MASCARET_T
