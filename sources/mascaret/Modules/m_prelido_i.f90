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

module M_PRELIDO_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine  PRELIDO                         (             &
      Noyau                                                , &
     FichierMotCle                                         , &
     Regime, ModeleLit                                     , &
     FrottParoiVerticale                                   , &
     DebProgressifLM, DebProgressifZS                      , &
     DT, TempsInitial                                      , &
     CritereArret, NbPasTemps, TempsMaximum                , &
     FichierGeom, FormatGeom, Profil, PresenceZoneStockage , &
     X, IDT, XDT                                           , &
     FichierMaillage, FichierSauveMaillage                 , &
     TypeMaillage                                          , &
     Connect                                               , &
     Z, Q                                                  , &
     ST1, ST2, InterplinStrickler, LoiFrottement           , &
     RepriseCalcul                                         , &
     TitreCas                                              , &
     ImpressionPlani, ImpressionCalcul                     , &
     PasStockage, PasImpression                            , &
     PremierPasStocke                                      , &
     FichierResultat, FormatResu, FichierListing           , &
     VarCalc, VarSto                                       , &
     OptionStockage, SectionStockage                       , &
     LoiHydrau, FichierLoiHydrau                           , &
     Singularite, PCSing                                   , &
     Apport, Deversoir                                     , &
     Confluent                                             , &
     Extremite, Algorithme, Abaque, FichierAbaque          , &
       limite_mineur, limite_majeur, stmin, stmaj          , &
     cote_rive_d, cote_rive_g, debit, cote                 , &
     abscisse_debit, debit_apport, abscisse_perte          , &
     coeff_perte, abscisse_seuil, coeff_seuil, cote_seuil  , &
     Erreur                                                  &
                                             )

   ! **********************************************************************
   !  FONCTION : LECTURE DU FICHIER CAS PAR APPEL DU LOGICIEL DAMOCLES.
   !----------------------------------------------------------------------
   !
   ! SOUS-POGRAMME APPELANT : - PRETRAIT
   !
   ! SOUS-PROGRAMMES APPELES :
   !   - DAMOC      : lecture et interpretation du fichier cas
   !   - XINDIC_S   : calc de la sect corresp a une abscisse
   !   - MAILLER    : calcul du maillage
   !   - LEC_GEOM   : lecture des profils geometriques
   !   - LEC_LIGNE  : lecture de la ligne d'eau initiale
   !   - LEC_HYDRAU : lecture des lois hydrauliques
   !   - DATE_S     : Calcul de la date et de l'heure
   !
   !**********************************************************************

   !=========================== Declarations ================================
   use M_PRECISION
   use M_APPORT_T            ! Type APPORT_T
   use M_CONFLUENT_T         ! Type CONFLUENT_T
   use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
   use M_DEVERSOIR_T         ! Type DEVERSOIR_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_EXTREMITE_T         ! Type EXTREMITE_T
   use M_FICHIER_T           ! Type FICHIER_T
   use M_LOI_T               ! Types LOI_T
   use M_MAILLE_T            ! Types MAILLE_E_T et MAILLE_R_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_SINGULARITE_T       ! Type SINGULARITE_T
   use M_ZONE_SECHE_T        ! Type ZONE_SECHE_T
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_INDEX_VARIABLE_C    ! Indexe des variables
   use M_MESSAGE_C           ! Messages d'erreur
   use M_PARAMETRE_C         ! EPS2
   use M_LEC_GEOM_I          ! Interface de sous-programme
   use M_MAILLER_I           ! Interface de sous-programme
   use M_LEC_LIGNE_I         ! Interface de sous-programme
   use M_LEC_HYDRAU_I        ! Interface de sous-programme
   use M_ALGOP_I             ! Interface de sous-programme
   use M_TRAITER_ERREUR_I    ! Traitement de l'erreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_DATE_S              ! Calcul de la date et de l'heure
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue

   !.. Implicit Declarations ..
   implicit none

   type PERTE_DE_CHARGE_T
      sequence
      integer      :: num_branche    ! Numero de branche
      real(DOUBLE) :: abscisse_rel   ! Abscisse relative
      real(DOUBLE) :: coeff          ! Coefficient
   end type PERTE_DE_CHARGE_T

   !.. Parameters .. 
   integer, parameter :: NMAX = 300
   ! Parametres generaux
   integer        , intent(  out) :: Noyau
   type(FICHIER_T), intent(inout) :: FichierMotCle
   ! Modelisation physique
   integer     , intent(  out) :: Regime
   integer     , intent(  out) :: ModeleLit
   logical     , intent(  out) :: FrottParoiVerticale
   logical     , intent(  out) :: DebProgressifLM
   logical     , intent(  out) :: DebProgressifZS
   ! Parametres temporels
   real(DOUBLE), intent(  out) :: DT
   real(DOUBLE), intent(  out) :: TempsInitial
   integer     , intent(  out) :: CritereArret
   integer     , intent(  out) :: NbPasTemps
   real(DOUBLE), intent(  out) :: TempsMaximum
   integer                     :: unite_temps
   ! Geometrie
   type(FICHIER_T), intent(inout)               :: FichierGeom
   integer                                      :: FormatGeom
   integer                                      :: nb_point
   integer                                      :: nb_bief_geom ! Nombre de biefs lu dans
                                                                ! le fichier geometrie
   type(PROFIL_T), dimension(:) , pointer       :: Profil       ! Profils geometriques
   integer       , dimension(:) , pointer       :: ProfDebBief
   integer       , dimension(:) , pointer       :: ProfFinBief
   logical                      , intent(  out) :: PresenceZoneStockage
   ! Maillage et planimetrage
   integer                                      :: nb_pas_planim
   integer                                      :: nb_zone_planim
   real(DOUBLE)                                 :: pas_planim
   integer                                      :: profdeb_zone_planim
   integer                                      :: proffin_zone_planim
   real(DOUBLE)    , dimension(:), pointer      :: X
   integer         , dimension(:), pointer      :: IDT
   real(DOUBLE)    , dimension(:), pointer      :: XDT
   integer                       , intent(  out):: TypeMaillage
   integer                                      :: mode_saisie_maillage
   type(FICHIER_T), intent(inout)               :: FichierMaillage
   logical                                      :: sauvegarde_maillage
   type(FICHIER_T), intent(inout)               :: FichierSauveMaillage
   integer                                      :: nb_maille
   integer                                      :: nb_section
   type(MAILLE_R_T), dimension(:), allocatable  :: maille_r
   type(MAILLE_E_T), dimension(:), allocatable  :: maille_e
   real(DOUBLE)                                 :: abscisse_abs
   ! Calage
   integer                                      :: nb_zone_strickler
   real(DOUBLE)                                 :: valeur_strickler_min
   real(DOUBLE)                                 :: valeur_strickler_maj
   real(DOUBLE)                                 :: abscdeb_zone_strickler
   real(DOUBLE)                                 :: abscfin_zone_strickler
   integer                                      :: nb_profil_zone_sto
   integer                                      :: num_profil_sto
   real(DOUBLE)                                 :: limite_maj_gauche
   real(DOUBLE)                                 :: limite_maj_droite
   ! Reseau
   integer                                      :: nb_bief
   integer     , dimension(:), allocatable      :: ext_deb_bief
   integer     , dimension(:), allocatable      :: ext_fin_bief
   real(DOUBLE), dimension(:), allocatable      :: absc_ext_deb_bief
   real(DOUBLE), dimension(:), allocatable      :: absc_ext_fin_bief
   integer                                      :: indice
   integer                                      :: indice2
   integer                                      :: nb_noeud
   integer          , dimension(:), allocatable :: nb_ext_noeud
   integer        , dimension(:,:), allocatable :: ext_noeud
   integer                                      :: nb_ext_max
   integer                                      :: nb_ext_libre
   integer          , dimension(:), allocatable :: num_ext_libre
   Type(EXTREMITE_T), dimension(:), pointer     :: Extremite
   type(CONNECT_T)                , intent(out) :: Connect
   integer         , dimension(:), pointer      :: Algorithme
   ! Conditions initiales
   real(DOUBLE)    , dimension(:), pointer      :: Z
   real(DOUBLE)    , dimension(:), pointer      :: Q
   real(DOUBLE)    , dimension(:), pointer      :: ST1
   real(DOUBLE)    , dimension(:), pointer      :: ST2
   logical                       , intent(  out):: InterpLinStrickler
   integer                       , intent(  out):: LoiFrottement
   logical                       , intent(  out):: RepriseCalcul
   logical                                      :: presence_ligne_deau
   integer                                      :: type_entree_ligne
   integer                                      :: format_ligne
   integer                                      :: nb_zone_seche
   ! Impressions - resultats
   character(LEN=30), intent(out) :: TitreCas
   logical                      :: impression_geo
   logical      , intent(  out) :: ImpressionPlani
   logical                      :: ImpressionCalcul
   logical                      :: impression_reseau
   logical                      :: impression_hydrau
   logical                      :: impression_ligne
   integer       , intent(  out) :: PasStockage
   integer       , intent(  out) :: PasImpression
   integer       , intent(  out) :: PremierPasStocke
   integer       , intent(  out) :: OptionStockage
   integer, dimension(:), pointer:: SectionStockage
   type(FICHIER_T) , intent(inout) :: FichierResultat
   integer                         :: post_processeur
   integer         , parameter     :: POST_RUBENS  = 1
   integer         , parameter     :: POST_OPTHYCA = 2
   integer         , intent(  out) :: FormatResu
   type(FICHIER_T) , intent(inout) :: FichierListing
   real(DOUBLE)                    :: ecart
   logical, dimension(NB_TOT_VAR), intent(  out) :: VarCalc
   logical, dimension(NB_TOT_VAR), intent(  out) :: VarSto
   ! Lois hydrauliques
   type(LOI_T)    , dimension(:), pointer       :: LoiHydrau
   type(FICHIER_T)              , intent(inout) :: FichierLoiHydrau
   integer                                      :: type_entree_loi
   integer                                      :: nb_loi
   ! Barrage - singularites
   integer                                    :: nb_sing
   type(SINGULARITE_T), dimension(:), pointer :: Singularite
   integer                                    :: num_loi
   ! Pertes de charge singuliere
   real(DOUBLE), dimension(:), pointer :: PCSing
   integer                                   :: nb_pc_sing
   type(PERTE_DE_CHARGE_T), dimension(:), allocatable :: pc_sing
   real(DOUBLE)      :: abs_abs
   integer           :: indice_orig_branche
   ! Apports et Deversoirs
   integer                               :: nb_apport
   type(APPORT_T), dimension(:), pointer :: Apport 
   integer                               :: nb_deversoir
   type(DEVERSOIR_T), dimension(:), pointer :: Deversoir 
   ! Confluents
   integer                                  :: nb_confluent
   type(CONFLUENT_T), dimension(:), pointer :: Confluent 
   ! Abaques pour le calcul des pertes de charge automatique aux confluences
   real(DOUBLE)    , dimension(6,6,5) , intent(inout) :: Abaque
   type(FICHIER_T)                    , intent(inout) :: FichierAbaque
   ! Traitement des erreurs
   type(ERREUR_T), intent(inout) :: Erreur
   real(DOUBLE), dimension(:,:) , pointer   :: limite_mineur
   real(DOUBLE), dimension(:,:) , pointer   :: limite_majeur
   real(DOUBLE), dimension(:) , pointer     :: stmin
   real(DOUBLE), dimension(:) , pointer     :: stmaj
   real(DOUBLE), dimension(:) , pointer     :: cote_rive_g
   real(DOUBLE), dimension(:) , pointer     :: cote_rive_d
   real(DOUBLE), dimension(:) , pointer     :: debit
   real(DOUBLE), dimension(:) , pointer     :: cote
   real(DOUBLE), dimension(:) , pointer     :: abscisse_debit
   real(DOUBLE), dimension(:,:) , pointer   :: debit_apport
   real(DOUBLE), dimension(:) , pointer     :: abscisse_perte
   real(DOUBLE), dimension(:) , pointer     :: coeff_perte
   real(DOUBLE), dimension(:) , pointer     :: abscisse_seuil
   real(DOUBLE), dimension(:) , pointer     :: coeff_seuil
   real(DOUBLE), dimension(:) , pointer     :: cote_seuil

   ! Erreur
   !character(132)    :: !arbredappel_old
   ! CONSTANTES
   ! ----------
   integer, parameter :: NB_LANGUE = 2
   integer, parameter :: LANGUE_FRANCAISE = 1
   integer, parameter :: LANGUE_ANGLAISE  = 2

   end subroutine PRELIDO

   end interface

end module M_PRELIDO_I
