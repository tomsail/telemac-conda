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

module M_PRETRAIT_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  PRETRAIT                                       ( &

      VersionCode, Noyau                                       , &
      FichierModele, FichierMotCle                             , &
      OptionCasier                                             , &
      OptionCourlis, FichierMotCleCourlis, FichierDicoCourlis  , &
      OndeSubm                                                 , &
      decentrement                                             , &
      CalculValidation, TypeValidation                         , &
      Regime, ModeleLit                                        , &
      FrottParoiVerticale, PerteChargeConfluent                , &
      DebProgressifLM, DebProgressifZS                         , &
      DZArriveeFront                                           , &
      FroudeLim, FrottementImplicite, Impli_Trans , Opt        , &
      PerteElargissementTrans, Boussinesq, NoConvection, CQMV  , &
      Prof_Abs , HEPS                                          , &
      DT, TempsInitial, CritereArret, NbPasTemps, TempsMaximum , &
      Section_controle ,Cote_max_controle                      , &
      PasTempsVariable, CourantObj                             , &
      FichierGeom, FormatGeom, Profil, PresenceZoneStockage    , &
      X, IDT, XDT                                              , &
      FichierMaillage, FichierSauveMaillage, TypeMaillage      , &
      Connect                                                  , &
      Z, Q                                                     , &
      CF1, CF2, InterpLinCoeffFrott, LoiFrottement             , &
      RepriseCalcul                                            , &
      FichierRepriseEcr, FichierRepriseLec                     , &
      FichierLigne                                             , &
      ZoneSeche                                                , &
      ZoneFrot                                                 , &
      TitreCas                                                 , &
      ImpressionPlani, ImpressionCalcul                        , &
      PasStockage, PasImpression                               , &
      PremierPasStocke                                         , &
      FichierResultat,FormatResu,FichierResultat2, FormatResu2 , &
      FichierListing                                           , &
      VarCalc, VarSto                                          , &
      OptionStockage, SectionStockage                          , &
      LoiHydrau, FichierLoiHydrau                              , &
      Barrage, Singularite, PCSing                             , &
      Apport, Deversoir                                        , &
      Confluent, Extremite, Algorithme, Abaque, FichierAbaque  , &
      Casier,             &  ! tableau des casiers
      Liaison,            &  ! tableau des liaisons
      ApportPluie,        &  ! tableau des apports de pluie
      FichierResultatCasier,& ! fichier des resultats des caracteristiques Casier
      FichierResultatLiaison,& ! fichier des resultats des caracteristiques Liaison
      FichierListingCasier ,&
      FichierListingLiaison,&
      FichierGeomCasier,&
!TAPENADE--
      Calage_Frott,Calage_Crues, nb_zone_frottement,max_mes,Option_calage, &
      calage_Constantes, FichierResultatCalage, FichierResultat1Calage, &
!--TAPENADE
      Erreur )

   !Donnees pour le calage automatique du frottement
   ! Nombre max de caractere de continuation : 39
   ! *********************************************************************
   !  FONCTION : LECTURE DU FICHIER CAS PAR APPEL DU LOGICIEL DAMOCLES.
   !---------------------------------------------------------------------
   !
   ! SOUS-POGRAMME APPELANT : - SUPERVISEUR
   !
   !**********************************************************************

   !=========================== Declarations ================================
   use M_PRECISION
   use M_APPORT_T            ! Type APPORT_T
   use M_BARRAGE_T           ! Type BARRAGE_T
   use M_CONFLUENT_T         ! Type CONFLUENT_T
   use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
   use M_DEVERSOIR_T         ! Type DEVERSOIR_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_EXTREMITE_T         ! Type EXTREMITE_T
   use M_FICHIER_T           ! Type FICHIER_T
   use M_LOI_T               ! Types LOI_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_SINGULARITE_T       ! Type SINGULARITE_T
   use M_ZONE_SECHE_T        ! Type ZONE_SECHE_T
   use M_ZONE_FROT_T         ! Type ZONE_SECHE_T
!TAPENADE--
   use M_DONNEES_CRUES_CALAGE_T ! type pour les donnees de crues
   use M_DONNEES_CALAGE_T       ! Donnees pour le calage automatique du Strickler
   use M_CONSTANTES_CALAGE_T
!--TAPENADE
   use M_INDEX_VARIABLE_C    ! Index des variables
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_MESSAGE_C           ! Messages d'erreur
   use M_PARAMETRE_C         ! EPS2
   use M_ALGOP_I
   use M_CALC_CONNECT_I
   use M_CALC_MAILLAGE_I
   use M_LEC_APPORT_I
   use M_LEC_BARRAGE_I
   use M_LEC_CONFLUENT_I
   use M_LEC_DEVER_I
   use M_LEC_FROTTEMENT_I
   use M_LEC_GEOM_I          ! Interface de sous-programme
   use M_LEC_LIGNE_I         ! Interface de sous-programme
   use M_LEC_LOI_I
   use M_LEC_PCSING_I
   use M_LEC_PLANIM_I
   use M_LEC_RESEAU_I
   use M_LEC_SING_I
   use M_LEC_SORTIES_I
   use M_LEC_STOCKAGE_I
   use M_LEC_ZONE_SECHE_I
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_DATE_S              ! Calcul de la date et de l'heure
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_CASIER_T       ! type Casier
   use M_LIAISON_T      ! type Liaison
   use M_APPORT_PLUIE_T ! type Apport de Pluie
   use M_LEC_CASIER_I          ! interface du sous-programme Lec_Casier
   use M_LEC_LIAISON_I         ! interface du sous-programme Lec_Liaison
   use M_LEC_APPORT_PLUIE_I    ! interface du sous-programme Lec_Apport_Pluie
   use M_CONSTANTES_CASIER_C   ! constantes de calcul propres a CASIER
   use M_XCAS_S


   !.. Implicit Declarations ..
   implicit none

   ! Parametres generaux
   integer        , intent(  out) :: VersionCode
   integer        , intent(  out) :: Noyau
   type(FICHIER_T), intent(inout) :: FichierModele
   type(FICHIER_T), intent(inout) :: FichierMotCle
   logical        , intent(  out) :: OptionCasier
   logical        , intent(  out) :: OptionCourlis
   type(FICHIER_T), intent(inout) :: FichierMotCleCourlis
   type(FICHIER_T), intent(inout) :: FichierDicoCourlis
   logical        , intent(  out) :: OndeSubm
   logical        , intent(  out) :: decentrement
   logical        , intent(  out) :: CalculValidation
   logical        , intent(  out) :: PerteChargeConfluent
   integer        , intent(  out) :: TypeValidation
   character(LEN=132)             :: nom_fortran
   character(LEN=132)             :: nom_bibli
   ! Modelisation physique
   integer     , intent(  out) :: Regime
   integer     , intent(  out) :: ModeleLit
   logical     , intent(  out) :: FrottParoiVerticale
   logical     , intent(  out) :: DebProgressifLM
   logical     , intent(  out) :: DebProgressifZS
   real(DOUBLE), intent(  out) :: FroudeLim
   Logical     , intent(  out) :: FrottementImplicite
   Logical     , intent(  out) :: PerteElargissementTrans
   Logical     , intent(  out) :: Boussinesq
   Logical     , intent(  out) :: NoConvection
   Logical     , intent(  out) :: Impli_Trans,Opt
   real(DOUBLE), intent(  out) :: HEPS
   real(DOUBLE), intent(  out) :: DZArriveeFront
   logical     , intent(  out) :: InterpLinCoeffFrott
   ! Parametres temporels
   real(DOUBLE), intent(  out) :: DT
   real(DOUBLE), intent(  out) :: TempsInitial
   integer     , intent(  out) :: CritereArret
   integer     , intent(  out) :: NbPasTemps
   real(DOUBLE), intent(  out) :: TempsMaximum
   logical     , intent(  out) :: PasTempsVariable
   real(DOUBLE), intent(  out) :: CourantObj
   real(DOUBLE), intent(  out) :: Cote_max_controle
   integer     , intent(  out) :: Section_Controle
   ! Geometrie
   type(FICHIER_T), intent(inout) :: FichierGeom
   integer        , intent(  out) :: FormatGeom
   type(PROFIL_T), dimension(:) , pointer       :: Profil      ! Profils geometriques
   integer       , dimension(:) , pointer       :: ProfDebBief ! Premiers profils des biefs
   integer       , dimension(:) , pointer       :: ProfFinBief ! Derniers profils des biefs
   integer                                      :: nb_bief_geom! Nombre de biefs du fichier geometrie
   logical                      , intent(  out) :: PresenceZoneStockage
   logical                      , intent(  out) :: Prof_Abs   ! profils abscisse  Absolue
   ! Maillage et planimetrage
   real(DOUBLE)    , dimension(:), pointer     :: X
   integer         , dimension(:), pointer     :: IDT
   real(DOUBLE)    , dimension(:), pointer     :: XDT
   type(FICHIER_T), intent(inout)              :: FichierMaillage
   type(FICHIER_T), intent(inout)              :: FichierSauveMaillage
   integer                                     :: TypeMaillage
   integer                                     :: NbBief
   real(DOUBLE)      , dimension(:)  , pointer :: absc_rel_ext_deb_bief
   real(DOUBLE)      , dimension(:)  , pointer :: absc_rel_ext_fin_bief
   real(DOUBLE)      , dimension(:)  , pointer :: absc_abs_ext_deb_bief
   real(DOUBLE)      , dimension(:)  , pointer :: absc_abs_ext_fin_bief
   integer                                     :: NbNoeud
   integer           , dimension(:)  , pointer :: NbExtNoeud
   integer           , dimension(:)  , pointer :: ExtDebBief
   integer           , dimension(:)  , pointer :: ExtFinBief
   integer           , dimension(:,:), pointer :: ExtNoeud
   integer                                     :: NbExtLibre
   integer           , dimension(:)  , pointer :: NumExtLibre
   ! Reseau
   Type(EXTREMITE_T), dimension(:), pointer       :: Extremite
   type(CONNECT_T)                , intent(  out) :: Connect
   integer, dimension(:)          , pointer       :: Algorithme
   ! Variables principales
   real(DOUBLE)    , dimension(:), pointer        :: Z
   real(DOUBLE)    , dimension(:), pointer        :: Q
   real(DOUBLE)    , dimension(:), pointer        :: CF1
   real(DOUBLE)    , dimension(:), pointer        :: CF2
   integer                       , intent(  out)  :: LoiFrottement
   ! Conditions initiales
   logical                       , intent(  out)  :: RepriseCalcul
   type(FICHIER_T)               , intent(inout)  :: FichierRepriseEcr
   type(FICHIER_T)               , intent(inout)  :: FichierRepriseLec
   logical                                        :: presence_ligne_deau
   integer                                        :: type_entree_ligne
   type(FICHIER_T)               , intent(inout)  :: FichierLigne
   integer                                        :: format_ligne
   type(ZONE_SECHE_T), dimension(:), pointer      :: ZoneSeche
   type(ZONE_FROT_T) , dimension(:), pointer      :: ZoneFrot
   ! Impressions - resultats
   character(LEN=255), intent(  out) :: TitreCas
   logical                                       :: impression_geo
   logical                       , intent(  out) :: ImpressionPlani
   logical                       , intent(  out) :: ImpressionCalcul
   logical                                       :: impression_reseau
   logical                                       :: impression_hydrau
   logical                                       :: impression_ligne
   integer                       , intent(  out) :: PasStockage
   integer                       , intent(  out) :: PasImpression
   integer                       , intent(  out) :: PremierPasStocke
   type(FICHIER_T)               , intent(inout) :: FichierResultat
   type(FICHIER_T)               , intent(inout) :: FichierResultat2
   integer                                       :: post_processeur
   integer                       , parameter     :: POST_RUBENS  = 1
   integer                       , parameter     :: POST_OPTHYCA = 2
   integer                       , parameter     :: POST_OPTRU   = 3
   integer                       , intent(  out) :: FormatResu
   integer                       , intent(  out) :: FormatResu2
   type(FICHIER_T)               , intent(inout) :: FichierListing
   integer                                       :: UniteListing
   real(DOUBLE)                                  :: ecart
   logical, dimension(NB_TOT_VAR), intent(  out) :: VarCalc
   logical, dimension(NB_TOT_VAR), intent(  out) :: VarSto
   integer                       , intent(  out) :: OptionStockage
   integer, dimension(:)         , pointer       :: SectionStockage
   ! Lois hydrauliques
   type(LOI_T)    , dimension(:) , pointer       :: LoiHydrau
   type(FICHIER_T)               , intent(inout) :: FichierLoiHydrau
   ! Barrage - singularites
   type(BARRAGE_T)                  , intent(  out) :: Barrage
   type(SINGULARITE_T), dimension(:), pointer       :: Singularite
   ! Pertes de charge singuliere
   real(DOUBLE)       , dimension(:), pointer       :: PCSing
   ! Apports et Deversoirs
   type(APPORT_T)     , dimension(:), pointer       :: Apport
   type(DEVERSOIR_T)  , dimension(:), pointer       :: Deversoir
   ! Confluents
   type(CONFLUENT_T)  , dimension(:), pointer       :: Confluent
   ! Abaques pour le calcul des pertes de charge automatique aux confluences
   real(DOUBLE)    , dimension(6,6,5) , intent(inout) :: Abaque
   type(FICHIER_T)                    , intent(inout) :: FichierAbaque
   type(CASIER_T),       dimension(:), pointer       :: Casier
   type(LIAISON_T),      dimension(:), pointer       :: Liaison
   type(APPORT_PLUIE_T), dimension(:), pointer       :: ApportPluie
   type(FICHIER_T),                    intent(inout) :: FichierResultatCasier
   type(FICHIER_T),                    intent(inout) :: FichierResultatLiaison
   type(FICHIER_T),                    intent(inout) :: FichierListingLiaison
   type(FICHIER_T),                    intent(inout) :: FichierListingCasier
   type(FICHIER_T),                    intent(inout) :: FichierGeomCasier
!TAPENADE--
   !
   ! Calage automatique des coefficients de Strickler
   ! ------------------------------------------------
   type(DONNEES_CALAGE_T) , dimension(:), pointer :: Calage_frott
   type(DONNEES_CRUES_CALAGE_T)                   :: Calage_crues
   type(CONSTANTES_CALAGE_T)                      :: Calage_Constantes
   logical                                        :: Option_Calage
   type(FICHIER_T)                                :: FichierResultatCalage,FichierResultat1Calage
   integer                         ,intent(  out) :: nb_zone_frottement,max_mes
!--TAPENADE
   integer                         ,intent(  out) :: CQMV
   !
   ! Traitement des erreurs
   !
   type(ERREUR_T)                   , intent(inout) :: Erreur

   end subroutine pretrait

   end interface

end module M_PRETRAIT_I
