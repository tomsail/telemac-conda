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
  FroudeLim, FrottementImplicite, Impli_Trans ,Opt         , &
  PerteElargissementTrans , Boussinesq ,NoConvection,CQMV  , &
  Prof_Abs , HEPS                                          , &
  DT, TempsInitial, CritereArret, NbPasTemps, TempsMaximum , &
  Section_controle,Cote_max_controle                       , &
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
  FichierResultat, FormatResu, FichierResultat2, FormatResu2 ,&
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
  FichierGeomCasier, &
!TAPENADE--
  Calage_Frott,Calage_Crues, nb_zone_frottement,max_mes,Option_calage, & !Donnees pour le calage automatique du frottement
  calage_Constantes, FichierResultatCalage, FichierResultat1Calage, &
!--TAPENADE
  Erreur )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN - N. GOUTAL
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
!  FONCTION : LECTURE DU FICHIER CAS PAR APPEL DU LOGICIEL DAMOCLES.
!----------------------------------------------------------------------
!
! SOUS-POGRAMME APPELANT : - SUPERVISEUR
!
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
   use M_ZONE_FROT_T         ! Type ZONE_FROT_T
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
   type(FICHIER_T), intent(inout) :: FichierDicoCourlis
   type(FICHIER_T), intent(inout) :: FichierMotCleCourlis
   logical        , intent(  out) :: OndeSubm
   logical        , intent(  out) :: decentrement
   logical        , intent(  out) :: CalculValidation
   logical        , intent(  out) :: PerteChargeConfluent
   integer        , intent(  out) :: TypeValidation
   character(LEN=255)             :: nom_fortran
   character(LEN=255)             :: nom_bibli
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
   Integer     , intent(  out) :: CQMV
   Logical     , intent(  out) :: Impli_Trans,Opt
   real(DOUBLE), intent(  out) :: HEPS
   real(DOUBLE), intent(  out) :: DZArriveeFront
   logical     , intent(  out) :: InterpLinCoeffFrott
   ! Parametres temporels
   real(DOUBLE), intent(  out) :: DT
   real(DOUBLE), intent(  out) :: TempsInitial
   integer     , intent(  out) :: CritereArret
   integer     , intent(  out) :: NbPasTemps,Section_controle
   real(DOUBLE), intent(  out) :: TempsMaximum,Cote_max_controle
   logical     , intent(  out) :: PasTempsVariable
   real(DOUBLE), intent(  out) :: CourantObj
   ! Geometrie
   type(FICHIER_T), intent(inout) :: FichierGeom
   integer        , intent(  out) :: FormatGeom
   type(PROFIL_T), dimension(:) , pointer       :: Profil      ! Profils geometriques
   integer       , dimension(:) , pointer       :: ProfDebBief => null() ! Premiers profils des biefs
   integer       , dimension(:) , pointer       :: ProfFinBief => null() ! Derniers profils des biefs
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
   real(DOUBLE)      , dimension(:)  , pointer :: absc_rel_ext_deb_bief => null()
   real(DOUBLE)      , dimension(:)  , pointer :: absc_rel_ext_fin_bief => null()
   real(DOUBLE)      , dimension(:)  , pointer :: absc_abs_ext_deb_bief => null()
   real(DOUBLE)      , dimension(:)  , pointer :: absc_abs_ext_fin_bief => null()
   integer                                     :: NbNoeud
   integer           , dimension(:)  , pointer :: NbExtNoeud => null()
   integer           , dimension(:)  , pointer :: ExtDebBief => null()
   integer           , dimension(:)  , pointer :: ExtFinBief => null()
   integer           , dimension(:,:), pointer :: ExtNoeud => null()
   integer                                     :: NbExtLibre
   integer           , dimension(:)  , pointer :: NumExtLibre => null()
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
   !
   ! Traitement des erreurs
   !
   type(ERREUR_T)                   , intent(inout) :: Erreur
   ! SCALAIRES LOCAUX
   ! ----------------
   integer           :: iext            ! compteur sur les extremites libres
   integer           :: iprof           ! Compteur sur les profils
   integer           :: iprof_inf       ! borne inf de boucle
   integer           :: isect           ! Compteur sur les sections
   integer           :: retour          ! code de retour des fonctions intrinseques
   character(LEN=33) :: chaine_date     ! Chaine renvoyee par DATE_S
   integer           :: nb_site         ! Nombre de sites ou stocker
   integer           :: num_branche     ! Numero de branche de site ou stocker
   real(DOUBLE)      :: abscisse_rel    ! Abscisse relative du site ou stocker
   real(DOUBLE)      :: absc_abs        ! abscisse absolue correspondante
   integer           :: numero_max_loi  ! Numero de loi hydrau lu le + grand
   logical           :: sauvegarde_modele ! flag de sauvegarde du modele
   integer           :: ul              ! Numero d'unite d'un fichier
   real(DOUBLE) :: Abs_rel_controle
   real(DOUBLE) :: Abs_abs_controle
   integer      :: Bief_controle
   integer        ::  iliaison, icasier, ull, ulc
   integer        :: num_casier_origine, num_casier_fin, ilcc, nb_liaisonCC, nb_liaisonRC, jcasier
   integer, dimension(:,:), allocatable :: connect_casier
   integer        :: i,j,Nbcrue,max_nba
   integer, allocatable      :: itab(:)
   real(double), allocatable :: rtab(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line
   character(len=256)  :: xcasFile
   integer             :: unitNum
   ! Erreur
   !character(132)    :: !arbredappel_old

   !========================== Instructions =============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>PRETRAIT'

   ! DEFINITION DES POINTEURS
   !
   nullify(CF1)

   ! Open .xcas file
   unitNum = FichierMotCle%Unite
   xcasFile = FichierMotCle%Nom
   open(unit=unitNum, file=xcasFile, status="old", action="read", iostat=retour)
   if(retour.ne.0) then
    erreur%numero = 3
      erreur%ft     = err_3
      erreur%ft_c   = err_3c
      call traiter_erreur(erreur, xcasFile)
      return
   end if

   ! Read the listing file name
   pathNode = 'parametresImpressionResultats/listing/fichListing'
   line = xcasReader(unitNum, pathNode)
   FichierListing%Nom = line
   !========================================================================
   !                       LECTURE DES PARAMETRES GENERAUX
   !========================================================================
   ! Ouverture du fichier listing
   !-----------------------------
   UniteListing = FichierListing%Unite

   open(unit=UniteListing, file=FichierListing%Nom, access='SEQUENTIAL', &
        action='WRITE'           , form='FORMATTED'       , iostat=RETOUR      , &
        position='rewind'        , status='REPLACE'     )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR( Erreur , FichierListing%Nom )
      return
   end if

   ! Version du code
   !----------------
   pathNode = 'parametresGeneraux/versionCode'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) VersionCode

   call DATE_S(chaine_date)

   if( VersionCode == 3 ) then
      write(UniteListing,10000) ' V8P4R0 ', chaine_date
   endif

   if( VersionCode == 2 ) then
      write(UniteListing,10000) ' 6.1 ', chaine_date
   endif

   if( VersionCode == 1 ) then
      write(UniteListing,10000) ' 5.1 ', chaine_date
   endif

   write(UniteListing,10005)

   ! Noyau SARAP/REZO/MASCARET
   !--------------------------
   pathNode = 'parametresGeneraux/code'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) Noyau

   if( Noyau == NOYAU_SARAP ) then
      Regime = REGIME_PERMANENT
   else if( Noyau == NOYAU_MASCARET .or. Noyau == NOYAU_REZODT ) then
      Regime = REGIME_NON_PERMANENT
   else
      Erreur%Numero = 305
      Erreur%ft     = err_305
      Erreur%ft_c   = err_305c
      call TRAITER_ERREUR( Erreur , 'Noyau de calcul' , '1, 2 et 3' )
      return
   end if

   write(UniteListing,10010) NOM_NOYAU(Noyau)

   ! Fichier des mots cle
   !---------------------
   ! affectation en tete de sous programme
   write(UniteListing,10020) trim(FichierMotCle%Nom)

   ! Fichier du dictionnaire
   !------------------------
   ! affectation en tete de sous programme

   ! Fichier Fortran
   !----------------
   pathNode = 'parametresGeneraux/progPrincipal'
   nom_fortran = xcasReader(unitNum, pathNode)

   write(UniteListing,10030) trim(nom_fortran)

   ! Nom du fichier des bibliotheques
   !---------------------------------
   pathNode = 'parametresGeneraux/bibliotheques'
   line = xcasReader(unitNum, pathNode)
   if(len(trim(line)).ne.0) then
     pathNode = 'parametresGeneraux/bibliotheques/bibliotheque'
     nom_bibli = xcasReader(unitNum, pathNode)
     write(UniteListing,10035) trim(nom_bibli)
   endif

   ! Sauvegarde du modele
   !---------------------
   pathNode = 'parametresGeneraux/sauveModele'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) sauvegarde_modele

   if( sauvegarde_modele ) then
      write(UniteListing,10037) 'OUI'
      pathNode = 'parametresGeneraux/fichSauvModele'
      FichierModele%Nom = xcasReader(unitNum, pathNode)
      write(UniteListing,10040) trim(FichierModele%Nom)
   else
      write(UniteListing,10037) 'NON'
   endif

   ! Presence de Casiers (implique un couplage LIDO-CASIER)
   ! ------------------------------------------------------
   pathNode = 'parametresGeneraux/presenceCasiers'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) OptionCasier
   if( OptionCasier ) then
      write(UniteListing, 10044) 'OUI'
   else
      write(UniteListing, 10044) 'NON'
   endif

   ! Courlis Coupling (implies a MASCARET-COURLIS coupling)
   ! ------------------------------------------------------
   pathNode = 'parametresGeneraux/optionCourlis'
   line = xcasReader(unitNum, pathNode)

   if(len(trim(line)).eq.0) then  ! avoid direct comparison
     optionCourlis = .false.
   else
     read(unit=line, fmt=*) OptionCourlis
     if( OptionCourlis ) then
       write(UniteListing, 10047) 'OUI'
     else
       write(UniteListing, 10047) 'NON'
     endif
     pathNode = 'parametresGeneraux/fichierMotCleCourlis'
     FichierMotCleCourlis%Nom = xcasReader(unitNum, pathNode)
     pathNode = 'parametresGeneraux/dictionaireCourlis'
     FichierDicoCourlis%Nom = xcasReader(unitNum, pathNode)
   endif

   ! Perte de charge automatique due aux confluents
   ! ----------------------------------------------
   pathNode = 'parametresModelePhysique/perteChargeConf'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) PerteChargeConfluent
   if( PerteChargeConfluent ) then
      write(UniteListing,10045) 'OUI'
   else
      write(UniteListing,10045) 'NON'
   endif

   ! Perte de charge automatique due aux elargissements
   ! --------------------------------------------------
   pathNode = 'parametresNumeriques/perteChargeAutoElargissement'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) PerteElargissementTrans
   if( PerteElargissementTrans ) then
      write(UniteListing,10046) 'OUI'
   else
      write(UniteListing,10046) 'NON'
   endif

   ! Calcul d'une onde de submersion
   !--------------------------------
   pathNode = 'parametresNumeriques/calcOndeSubmersion'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) OndeSubm
   if( OndeSubm .and. Noyau == NOYAU_SARAP ) then
      Erreur%Numero = 301
      Erreur%ft     = err_301
      Erreur%ft_c   = err_301c
      call TRAITER_ERREUR( Erreur , 'Dam break flood wave' , 'SARAP' )
      return
   end if
   if( OndeSubm ) then
      write(UniteListing,10050)
   endif

   ! option de decentrement SARAP
   if (Noyau == NOYAU_SARAP) then
     pathNode = 'parametresNumeriques/decentrement'
     line = xcasReader(unitNum, pathNode)
     read(unit=line, fmt=*) decentrement
   endif

   !
   ! Modelisation de type Boussinesq
   ! -------------------------------
   pathNode = 'parametresNumeriques/termesNonHydrostatiques'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) Boussinesq
   !
   ! Empechement du torrentiel pour REZO
   ! -----------------------------------
   pathNode = 'parametresNumeriques/attenuationConvection'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) NoConvection

   !
   ! Apport de debit dans la quantite de mvt
   ! -------------------------------
   pathNode = 'parametresNumeriques/apportDebit'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) cqmv
   ! Calcul de validation
   !---------------------
   pathNode = 'parametresGeneraux/validationCode'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) CalculValidation
   if( CalculValidation ) then
      write(UniteListing,10060)
   endif

   ! Type de validation
   !-------------------
   pathNode = 'parametresGeneraux/typeValidation'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) typeValidation
   if( typeValidation.ne.0 ) then
      write(UniteListing,10070) TypeValidation
   endif

   !=======================
   ! MODELISATION PHYSIQUE
   !=======================
   write(UniteListing,10075)

   ! Modelisation du lit (Debord/Fond-Berge)
   !----------------------------------------
   pathNode = 'parametresModelePhysique/compositionLits'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) ModeleLit

   if( ModeleLit.lt.0.or.ModeleLit.gt.2 ) then
      Erreur%Numero = 305
      Erreur%ft     = err_305
      Erreur%ft_c   = err_305c
      call TRAITER_ERREUR( Erreur , 'Cross section layout' , '0, 1 or 2' )
      return
   end if

   if( ModeleLit == 0 ) then
      write(UniteListing,10080) 'Aucun'
   else if( ModeleLit == 1 ) then
      write(UniteListing,10080) 'Debord'
   else if(ModeleLit == 2) then
      write(UniteListing,10080) 'Fond-Berge'
   endif

   ! Conservation du frottement sur les parois verticales
   !-----------------------------------------------------
   pathNode = 'parametresModelePhysique/conservFrotVertical'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) FrottParoiVerticale
   if( FrottParoiVerticale ) then
      write(UniteListing,10090) 'OUI'
   else
      write(UniteListing,10090) 'NON'
   endif

   ! Debordement progressif dans le lit majeur
   !------------------------------------------
   pathNode = 'parametresModelePhysique/debordement/litMajeur'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) DebProgressifLM
   if( DebProgressifLM ) then
      write(UniteListing,10100) 'OUI'
   else
      write(UniteListing,10100) 'NON'
   endif

   ! Debordement progressif dans les zones de stockage
   !--------------------------------------------------
   pathNode = 'parametresModelePhysique/debordement/zoneStock'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) DebProgressifZS
   if( DebProgressifZS ) then
      write(UniteListing,10110) 'OUI'
   else
      write(UniteListing,10110) 'NON'
   endif

   ! Elevation de la cote signalant l'arrivee du front
   !--------------------------------------------------
   pathNode = 'parametresModelePhysique/elevCoteArrivFront'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) DZArriveeFront
   write(UniteListing,10115) DZArriveeFront

   ! Froude Limite pour les conditions aux limites
   !----------------------------------------------
   pathNode = 'parametresNumeriques/froudeLimCondLim'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) FroudeLim
   write(UniteListing,10120) FroudeLim

   ! Traitement du frottement
   !-------------------------
   pathNode = 'parametresNumeriques/traitImplicitFrot'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) FrottementImplicite
   if( FrottementImplicite ) then
      write(UniteListing,10130) 'IMPLICITATION DU FROTTEMENT'
   else
      write(UniteListing,10130) 'EXPLICITATION DU FROTTEMENT'
   endif

   ! Implicitation du noyau transcritique
   !-------------------------------------
   pathNode = 'parametresNumeriques/implicitNoyauTrans'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) Impli_Trans
   if( Impli_Trans )  then
      write(UniteListing,10136) 'IMPLICITATION DU SOLVEUR '
   else
      write(UniteListing,10136) 'EXPLICITATION DU SOLVEUR'
   endif

   ! Optimisation du temps calcul (flux figes)
   !------------------------------------------
   pathNode = 'parametresNumeriques/optimisNoyauTrans'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) Opt
   if( opt ) then
      write(UniteListing,10136) 'OPTIMISATION DU TEMPS CALCUL (FLUX FIGES)'
   endif

   ! Hauteur d'eau minimale
   ! ----------------------
   pathNode = 'parametresNumeriques/hauteurEauMini'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) HEPS

   ! Interpolation lineaire du coefficient de Frottement
   ! par rapport aux profils
   !---------------------------------------------------
   pathNode = 'parametresModelePhysique/interpolLinStrickler'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) InterpLinCoeffFrott
   if( InterpLinCoeffFrott ) then
      write(UniteListing,10135) 'OUI'
   else
      write(UniteListing,10135) 'NON'
   endif

   ! Les profils sont definies en abscisse absolue
   ! sur le reseau
   !---------------------------------------------------
   pathNode = 'parametresGeometrieReseau/geometrie/profilsAbscAbsolu'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) Prof_Abs

!TAPENADE--
   !=============================================================
   !      DONNEES POUR LE CALAGE AUTOMATIQUE DU STRICKLER
   !=============================================================
   pathNode = 'parametresCalageAuto'
   line = xcasReader(unitNum, pathNode)
   if(len(trim(line)).eq.0) then
       Option_Calage = .false.
   else
      pathNode = 'parametresCalageAuto/parametres/modeCalageAuto'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) Option_Calage
   endif

   if( Option_Calage ) then
      ! Parametres generaux
      pathNode = 'parametresCalageAuto/parametres/nomFichResult'
      FichierResultatCalage%nom = xcasReader(unitNum, pathNode)

      pathNode = 'parametresCalageAuto/parametres/nomFichListing'
      FichierResultat1Calage%nom = xcasReader(unitNum, pathNode)

      pathNode = 'parametresCalageAuto/parametres/typeLit'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) Calage_constantes%IESTIM

      pathNode = 'parametresCalageAuto/parametres/nbMaxIterations'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) Calage_constantes%NITER

      pathNode = 'parametresCalageAuto/parametres/precision'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) Calage_constantes%Precis

      ! Zone de frottements
      pathNode = 'parametresCalageAuto/zones/nbZones'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) nb_zone_frottement

      allocate( Calage_frott(nb_zone_frottement) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_frott' )
         return
      end if

      allocate( rtab(nb_zone_frottement) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab' )
          return
      end if

      pathNode = 'parametresCalageAuto/zones/absDebZone'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab

      do i = 1 , nb_zone_frottement
         Calage_frott(i)%Abscdeb_zone_frott = rtab(i)
      enddo

      pathNode = 'parametresCalageAuto/zones/absFinZone'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab

      do i = 1 , nb_zone_frottement
         Calage_frott(i)%Abscfin_zone_frott = rtab(i)
      enddo

      pathNode = 'parametresCalageAuto/zones/coefLitMin'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab

      do i = 1 , nb_zone_frottement
         Calage_frott(i)%Valeur_coeff_min = rtab(i)
      enddo

      pathNode = 'parametresCalageAuto/zones/coefLitMinBinf'
      line = xcasReader(unitNum, pathNode)
      if(len(trim(line)).eq.0) then
         ! Valeurs par defaut (Kmin=1)
         rtab(:) = 25.D0
      else
         read(unit=line, fmt=*) rtab
      endif
      do i = 1 , nb_zone_frottement
         if(rtab(i).ge.1.d-1) then
             Calage_frott(i)%Valeur_coeff_min_binf = rtab(i)
         else
             Calage_frott(i)%Valeur_coeff_min_binf = 25.D0
         endif
      enddo

      pathNode = 'parametresCalageAuto/zones/coefLitMinBsup'
      line = xcasReader(unitNum, pathNode)
      if(len(trim(line)).eq.0) then
         ! Valeurs par defaut (Kmax=100)
         rtab(:) = 100.D0
      else
         read(unit=line, fmt=*) rtab
      endif
      do i = 1 , nb_zone_frottement
         if(rtab(i).ge.1.d-1) then
            Calage_frott(i)%Valeur_coeff_min_bsup = rtab(i)
         else
            Calage_frott(i)%Valeur_coeff_min_bsup = 100.D0
         endif
      enddo

      pathNode = 'parametresCalageAuto/zones/coefLitMaj'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab

      do i = 1 , nb_zone_frottement
         Calage_frott(i)%Valeur_coeff_maj = rtab(i)
      enddo

      pathNode = 'parametresCalageAuto/zones/coefLitMajBinf'
      line = xcasReader(unitNum, pathNode)
      if(len(trim(line)).eq.0) then
         ! Valeurs par defaut (Kmin=1)
         rtab(:) = 1.D0
      else
         read(unit=line, fmt=*) rtab
      endif
      do i = 1 , nb_zone_frottement
         if(rtab(i).ge.1.d-1) then
            Calage_frott(i)%Valeur_coeff_maj_binf = rtab(i)
         else
            Calage_frott(i)%Valeur_coeff_maj_binf = 1.D0
         endif
      enddo

      pathNode = 'parametresCalageAuto/zones/coefLitMajBsup'
      line = xcasReader(unitNum, pathNode)
      if(len(trim(line)).eq.0) then
         ! Valeurs par defaut (Kmax=100)
         rtab(:) = 25.D0
      else
         read(unit=line, fmt=*) rtab
      endif
      do i = 1 , nb_zone_frottement
         if(rtab(i).ge.1.d-1) then
            Calage_frott(i)%Valeur_coeff_maj_bsup = rtab(i)
         else
            Calage_frott(i)%Valeur_coeff_maj_bsup = 25.D0
         endif
      enddo

      deallocate(rtab)

      ! Crues
      pathNode = 'parametresCalageAuto/listeCrues/nbCrues'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) Calage_crues%Nb_crue
      Nbcrue               = Calage_crues%Nb_crue

      allocate( Calage_crues%Nmes(Nbcrue) , STAT = retour)
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_crues.Nmes' )
         return
      end if
      allocate( Calage_crues%NbApports(Nbcrue) , STAT = retour)
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_crues.NbApports' )
         return
      end if

      pathNode = 'parametresCalageAuto/listeCrues/crues'
      line = xcasReader(unitNum, pathNode)
      if(len(trim(line)).eq.0) then
          print*,"Parse error => crues"
          call xerror(Erreur)
          return
      endif

      Do i = 1 , Calage_crues%Nb_crue
         pathNode = 'structureParametresCrueCalageAutomatique/nbMesures'
         if(i.eq.1) then
           line = xcasReader(unitNum, pathNode, 0)
         else
            line = xcasReader(unitNum, pathNode, 1)
         endif
         read(unit=line, fmt=*) Calage_crues%Nmes(i)

         pathNode = 'nbApports'
         line = xcasReader(unitNum, pathNode, 0)
         read(unit=line, fmt=*) Calage_crues%NbApports(i)
      enddo
      max_mes = maxval( Calage_crues%nmes )
      max_nba = maxval( Calage_crues%NbApports )

      allocate( Calage_crues%Xmesu(nbcrue,max_mes) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_crues.Xmesu' )
         return
      end if
      allocate( Calage_crues%Zmesu(nbcrue,max_mes) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_crues.Zmesu' )
         return
      end if
      allocate( Calage_crues%Pond(nbcrue,max_mes) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_crues.Pond' )
         return
      end if
      allocate( Calage_crues%Debit(nbcrue) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_crues.Debit' )
         return
      end if
      allocate( Calage_crues%ZAVAL(nbcrue) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_crues.ZAVAL' )
         return
      end if
      allocate( Calage_crues%Abscisse(nbcrue,max_nba) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_crues.Abscisse' )
         return
      end if
      allocate( Calage_crues%Apport_X(nbcrue,max_nba) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Calage_crues.Apport_X' )
         return
      end if

      pathNode = 'parametresCalageAuto/listeCrues/crues'
      line = xcasReader(unitNum, pathNode)

      Do I = 1 , Nbcrue
         pathNode = 'structureParametresCrueCalageAutomatique/debitAmont'
         if(i.eq.1) then
           line = xcasReader(unitNum, pathNode, 0)
         else
           line = xcasReader(unitNum, pathNode, 1)
         endif
         read(unit=line, fmt=*) Calage_crues%DEBIT(i)

         pathNode = 'coteAval'
         line = xcasReader(unitNum, pathNode, 0)
         read(unit=line, fmt=*) Calage_crues%ZAVAL(i)

         allocate( rtab(Calage_crues%Nmes(i)) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'rtab' )
            return
         end if

         pathNode = 'absMesures'
         line = xcasReader(unitNum, pathNode, 0)
         read(unit=line, fmt=*) rtab
         do j = 1 , Calage_crues%Nmes(i)
            Calage_crues%Xmesu(i,j) = rtab(j)
         enddo

         pathNode = 'coteMesures'
         line = xcasReader(unitNum, pathNode, 0)
         read(unit=line, fmt=*) rtab
         do j = 1 , Calage_crues%Nmes(i)
            Calage_crues%Zmesu(i,j) = rtab(j)
         enddo

         pathNode = 'pondMesures'
         line = xcasReader(unitNum, pathNode, 0)
         read(unit=line, fmt=*) rtab
         do j = 1 , Calage_crues%Nmes(i)
            Calage_crues%Pond(i,j) = rtab(j)
         enddo
         deallocate(rtab)

         if(Calage_crues%NbApports(i).gt.0) then
             allocate( rtab(Calage_crues%NbApports(i)) , STAT = retour )
             if( retour /= 0 ) then
                Erreur%Numero = 5
                Erreur%ft     = err_5
                Erreur%ft_c   = err_5c
                call TRAITER_ERREUR( Erreur , 'rtab' )
                return
             end if

             pathNode = 'absApports'
             line = xcasReader(unitNum, pathNode, 0)
             read(unit=line, fmt=*) rtab
             do j = 1 , Calage_crues%NbApports(i)
                Calage_Crues%abscisse(i,j) = rtab(j)
             enddo

             pathNode = 'debitApports'
             line = xcasReader(unitNum, pathNode, 0)
             read(unit=line, fmt=*) rtab
             do j = 1 , Calage_crues%NbApports(i)
                Calage_Crues%apport_X(i,j) = rtab(j)
             enddo

             deallocate(rtab)
         endif

      enddo

   endif
!--TAPENADE
   !=============================================================
   !                   PARAMETRES TEMPORELLES
   !=============================================================
   write(UniteListing,10140)

   ! Pas de temps
   !-------------
   pathNode = 'parametresTemporels/pasTemps'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) DT
   if( DT <= 0._DOUBLE ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Pas de temps' )
      return
   end if

   write(UniteListing,10150) DT

   ! Temps initial
   !--------------
   pathNode = 'parametresTemporels/tempsInit'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) TempsInitial
   write(UniteListing,10160) TempsInitial

   ! Critere d'arret du calcul
   !--------------------------
   pathNode = 'parametresTemporels/critereArret'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) CritereArret
   if( CritereArret /= TEMPS_MAXIMUM .and. &
       CritereArret /= NOMBRE_DE_PAS_TEMPS_MAXIMUM .and. &
       CritereArret /= COTE_MAXIMALE_AU_POINT_DE_CONTROLE) then
      Erreur%Numero = 305
      Erreur%ft     = err_305
      Erreur%ft_c   = err_305c
      call TRAITER_ERREUR( Erreur , 'Critere d''arret' , '1, 2 et 3' )
      return
   end if

   if( CritereArret == TEMPS_MAXIMUM ) then
      write(UniteListing,10170) 'TEMPS MAXIMUM'
   elseif( CritereArret == NOMBRE_DE_PAS_TEMPS_MAXIMUM ) then
      write(UniteListing,10170) 'NOMBRE DE PAS TEMPS MAXIMUM'
   elseif( CritereArret == COTE_MAXIMALE_AU_POINT_DE_CONTROLE ) then
      write(UniteListing,10170) 'COTE MAXIMALE AU POINT DE CONTROLE'
   endif

   ! Nombre de pas de temps du calcul
   !---------------------------------
   pathNode = 'parametresTemporels/nbPasTemps'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) NbPasTemps
   if( CritereArret == NOMBRE_DE_PAS_TEMPS_MAXIMUM .and. NbPasTemps <= 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de pas de temps' )
      return
   end if

   ! Point de controle de la cote maximale pour l'arret du calcul
   !-------------------------------------------------------------
   pathNode = 'parametresTemporels/abscisseControle'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) Abs_rel_controle

   pathNode = 'parametresTemporels/biefControle'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) Bief_controle

   pathNode = 'parametresTemporels/coteMax'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) Cote_max_controle
   if( CritereArret == COTE_MAXIMALE_AU_POINT_DE_CONTROLE .and. Bief_controle <= 0.) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Bief associe au point de controle' )
      return
   end if

   ! nb pas de temps non nul pour calculer POURC dans superviseur
   if( CritereArret == COTE_MAXIMALE_AU_POINT_DE_CONTROLE .and. NbPasTemps <= 0. ) then
      Erreur%Numero = 306
      Erreur%ft   = err_306
      Erreur%ft_c = err_306c
      call TRAITER_ERREUR  (Erreur, 'Nombre de pas de temps')
      return
   end if

   ! Temps maximum du calcul
   !------------------------
   pathNode = 'parametresTemporels/tempsMax'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) TempsMaximum
   if( CritereArret == TEMPS_MAXIMUM .and. TempsMaximum <= 0. ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Temps maximum de la simulation' )
      return
   end if

   if( CritereArret == TEMPS_MAXIMUM ) then
      write(UniteListing,10190) TempsMaximum
   elseif( CritereArret == NOMBRE_DE_PAS_TEMPS_MAXIMUM ) then
      write(UniteListing,10180) NbPasTemps
   endif

   ! Pas de temps variable
   !----------------------
   pathNode = 'parametresTemporels/pasTempsVar'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) PasTempsVariable
   if( PasTempsVariable .and. Noyau /= NOYAU_MASCARET ) then
      Erreur%Numero = 302
      Erreur%ft   = err_302
      Erreur%ft_c = err_302c
      call TRAITER_ERREUR  (Erreur, 'Pas de temps variable', 'MASCARET')
      return
   end if

   if( PasTempsVariable ) then
      write(UniteListing,10200) 'OUI'
   else
      write(UniteListing,10200) 'NON'
   endif

   ! Nombre de Courant souhaite
   !---------------------------
   pathNode = 'parametresTemporels/nbCourant'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) CourantObj
   if( PasTempsVariable .and. CourantObj <= 0. ) then
      Erreur%Numero = 306
      Erreur%ft   = err_306
      Erreur%ft_c = err_306c
      call TRAITER_ERREUR  (Erreur, 'Nombre de Courant souhaite')
      return
   end if

   write(UniteListing,10210) CourantObj

   !==============================================================
   !                       IMPRESSIONS - RESULTATS
   !==============================================================
   write(UniteListing,10300)

   ! Titre du calcul
   !----------------
   pathNode = 'parametresImpressionResultats/titreCalcul'
   TitreCas = xcasReader(unitNum, pathNode)
   write(UniteListing,10310) TitreCas

   ! Impression de la geometrie
   !---------------------------
   pathNode = 'parametresImpressionResultats/impression/impressionGeometrie'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) impression_geo
   if( impression_geo ) then
      write(UniteListing,10320) 'OUI'
   else
      write(UniteListing,10320) 'NON'
   endif

   ! Impression du planimetrage
   !---------------------------
   pathNode = 'parametresImpressionResultats/impression/impressionPlanimetrage'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) ImpressionPlani
   if( ImpressionPlani ) then
      write(UniteListing,10330) 'OUI'
   else
      write(UniteListing,10330) 'NON'
   endif

   ! Impression du reseau
   !---------------------
   pathNode = 'parametresImpressionResultats/impression/impressionReseau'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) impression_reseau
   if( impression_reseau ) then
      write(UniteListing,10340) 'OUI'
   else
      write(UniteListing,10340) 'NON'
   endif

   ! Impression des lois hydrauliques
   !---------------------------------
   pathNode = 'parametresImpressionResultats/impression/impressionLoiHydraulique'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) impression_hydrau
   if( impression_hydrau ) then
      write(UniteListing,10350) 'OUI'
   else
      write(UniteListing,10350) 'NON'
   endif

   ! Impression de la ligne d'eau initiale
   !--------------------------------------
   pathNode = 'parametresImpressionResultats/impression/impressionligneEauInitiale'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) impression_ligne
   if( impression_ligne ) then
      write(UniteListing,10360) 'OUI'
   else
      write(UniteListing,10360) 'NON'
   endif

   ! Impression en phase calcul
   !---------------------------
   pathNode = 'parametresImpressionResultats/impression/impressionCalcul'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) ImpressionCalcul
   if( ImpressionCalcul ) then
      write(UniteListing,10365) 'OUI'
   else
      write(UniteListing,10365) 'NON'
   endif

   ! Premier pas de temps a stocker
   !-------------------------------
   pathNode = 'parametresImpressionResultats/pasStockage/premPasTpsStock'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) PremierPasStocke
   if( PremierPasStocke <= 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Premier pas de temps a stocker' )
      return
   end if

   write(UniteListing,10370) PremierPasStocke

   ! Pas de stockage
   !----------------
   pathNode = 'parametresImpressionResultats/pasStockage/pasStock'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) PasStockage
   if( PasStockage <= 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Pas de stockage' )
      return
   end if

   write(UniteListing,10380) PasStockage

   ! Pas d'impression
   !-----------------
   pathNode = 'parametresImpressionResultats/pasStockage/pasImpression'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) PasImpression
   if( PasImpression <= 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Pas d''impression' )
      return
   end if

   write(UniteListing,10390) PasImpression

   ! fichier des resultats
   !----------------------
   pathNode = 'parametresImpressionResultats/resultats/fichResultat'
   FichierResultat%Nom = xcasReader(unitNum, pathNode)
   !!!! ATTENTION !!!!!
   FichierResultat2%Nom = ""

   write(UniteListing,10400) FichierResultat%Nom

   ! format du fichier des resultats
   !--------------------------------
   pathNode = 'parametresImpressionResultats/resultats/postProcesseur'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) post_processeur
   if( post_processeur /= POST_RUBENS .and. post_processeur /= POST_OPTHYCA ) then
      if( post_processeur /= POST_OPTRU ) then
         Erreur%Numero = 312
         Erreur%ft     = err_312
         Erreur%ft_c   = err_312c
         call TRAITER_ERREUR( Erreur , post_processeur )
         return
      endif
   endif

   if( post_processeur == POST_RUBENS ) then
      if( Noyau == NOYAU_SARAP ) then
         FormatResu = FORMAT_STO_PERMANENT
         write(UniteListing,10405) 'RUBENS', 'LIDOP'
      else if( Noyau == NOYAU_REZODT .or. Noyau == NOYAU_MASCARET ) then
         FormatResu = FORMAT_STO_NONPERMANENT
         write(UniteListing,10405) 'RUBENS', 'LIDONP'
      endif
   else if( post_processeur == POST_OPTHYCA ) then
      FormatResu = FORMAT_STO_OPTHYCA
      write(UniteListing,10405) 'OPTHYCA', 'OPTHYCA'
   endif

   if( post_processeur == POST_OPTRU ) then
      if( Noyau == NOYAU_SARAP ) then
         FormatResu = FORMAT_STO_PERMANENT
         write(UniteListing,10405) 'RUBENS', 'LIDOP'
      else if( Noyau == NOYAU_REZODT .or. Noyau == NOYAU_MASCARET ) then
         FormatResu = FORMAT_STO_NONPERMANENT
         write(UniteListing,10405) 'RUBENS', 'LIDONP'
         FormatResu2 = FORMAT_STO_OPTHYCA
        write(UniteListing,10405) 'OPTHYCA', 'OPTHYCA'
      endif
   else
      FormatResu2 = 0
   endif

   ! fichier listing
   !----------------
   ! Affectation en tete de sous programme
   write(UniteListing,10410) FichierListing%Nom

   ! Ficher de reprise en ecriture
   !------------------------------
   pathNode = 'parametresImpressionResultats/fichReprise/fichRepriseEcr'
   FichierRepriseEcr%Nom = xcasReader(unitNum, pathNode)
   write(UniteListing,10420) FichierRepriseEcr%Nom

   ! Ecart entre branches
   !---------------------
   pathNode = 'parametresImpressionResultats/rubens/ecartInterBranch'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) ecart
   if( ecart <= 0._DOUBLE ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Ecart entre branches' )
      return
   end if
   write(UniteListing,10430) ecart

   !============================================================
   !                   LECTURE DE LA GEOMETRIE
   !============================================================
   write(UniteListing,10440)

   ! Nom du fichier geometrie
   !-------------------------
   pathNode = 'parametresGeometrieReseau/fichier'
   FichierGeom%Nom = xcasReader(unitNum, pathNode)
   write(UniteListing,10450) FichierGeom%Nom

   ! Format du fichier geometrie
   !----------------------------
   pathNode = 'parametresGeometrieReseau/format'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) FormatGeom
   if( FormatGeom /= FORMAT_GEOM_LIDOV2P0 .and. FormatGeom /= FORMAT_GEOM_LIDOV3P0 ) then
      Erreur%Numero = 305
      Erreur%ft     = err_305
      Erreur%ft_c   = err_305c
      call TRAITER_ERREUR( Erreur , 'Format du fichier geometrie' , '1 et 2' )
      return
   end if

   if( FormatGeom == FORMAT_GEOM_LIDOV2P0 ) then
      write(UniteListing,10460) 'FORMAT LIDO 2.0'
   else if( FormatGeom == FORMAT_GEOM_LIDOV3P0 ) then
      write(UniteListing,10460) 'FORMAT LIDO 3.0'
   endif

   ! Lecture de la geometrie
   !------------------------
   call LEC_GEOM                    ( &
         Profil                     , & ! Resultats
         nb_bief_geom               , &
         ProfDebBief                , &
         ProfFinBief                , &
         Ecart                      , & ! Donnes non modifiees
         FrottParoiVerticale        , &
         Prof_Abs                   , &
         Noyau                      , &
         impression_geo             , &
         UniteListing               , &
         FichierGeom                , &
         FormatGeom                 , &
         Erreur                       )
   if( Erreur%numero /= 0 ) then
      return
   endif

   !========================================================
   !              LECTURE DES LOIS HYDRAULIQUES
   !========================================================
   call LEC_LOI        ( &
      LoiHydrau        , & ! Tableau des lois hydrauliques
      FichierLoiHydrau , & ! Fichier des lois hydrauliques
      impression_hydrau, & ! Flag d'impression des lois hydrauliques
      UniteListing     , & ! Unite logique fichier listing
      CritereArret     , & ! Criter d'arret du calcul
      TempsMaximum     , & ! Temps maximum du calcul
      unitNum          , & ! Unite logique du fichier .xcas
      Erreur             & ! Erreur
                       )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !========================================================
   !       LECTURE DU RESEAU ET DES EXTREMITES LIBRES
   !========================================================
   call LEC_RESEAU         ( &
     NbBief                , & ! Nombre de biefs
     absc_rel_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     absc_abs_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_abs_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     NbNoeud               , & ! Nombre de noeuds
     NbExtNoeud            , & ! Nombre d'extremite relie a chaque noeud
     ExtDebBief            , & ! Numero de l'extremite debut de chaque bief
     ExtFinBief            , & ! Numero de l'extremite fin de chaque bief
     ExtNoeud              , & ! Numero d'extremite lie a un noeud
     NbExtLibre            , & ! Nombre d'extremites libres
     NumExtLibre           , & ! Numero d'extremite libre
     Extremite             , & ! Extremites libres
     LoiHydrau             , & ! Lois hydrauliques
     impression_reseau     , & ! Flag d'impression du reseau
     UniteListing          , &
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     Noyau                 , & ! Noyau de calcul
     unitNum               , & ! Unite logique .xcas
     Erreur                  & ! Erreur
                           )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   numero_max_loi = 0

   do iext = 1 , NbExtLibre
      numero_max_loi = max( numero_max_loi,Extremite(iext)%NumeroLoi )
   end do

   if( numero_max_loi > size(LoiHydrau(:)) ) then
      Erreur%Numero = 384
      Erreur%ft     = err_384
      Erreur%ft_c   = err_384c
      call TRAITER_ERREUR( Erreur , numero_max_loi, size(LoiHydrau(:)) )
      return
   end if

   !==============================================================
   !                      CALCUL DU MAILLAGE
   !==============================================================
   call CALC_MAILLAGE      ( &
     X                     , & ! Tableau des abscisses
     TypeMaillage          , & ! Type de calcul du maillage
     FichierMaillage       , & ! Fichier du maillage
     FichierSauveMaillage  , & ! Fichier de sauvegarde du maillage
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premier profil d'un bief
     ProfFinBief           , & ! Dernier profil d'un bief
     absc_rel_ext_deb_bief , & ! Abscisse rel de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse rel de l'extremite debut du bief
     impression_geo        , & ! Flag d'impression de la geometrie
     FichierListing%Unite  , & ! Unite logique fichier listing
     unitNum               , & ! Unite logique .xcas
     Erreur                  & ! Erreur
                           )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !==============================================================
   !                      LECTURE DU PLANIMETRAGE
   !==============================================================
   call LEC_PLANIM       ( &
     Profil              , & ! Profils geometriques
     FichierListing%Unite, &
     unitNum             , & ! Unite logique .xcas
     Erreur                & ! Erreur
                         )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !====================================================================
   !                 LECTURE DES CONDITIONS INITIALES
   !====================================================================
   write(UniteListing,10560)

   ! Reprise de calcul
   !------------------
   pathNode = 'parametresConditionsInitiales/repriseEtude/repriseCalcul'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) RepriseCalcul
   if( RepriseCalcul .and. Noyau == NOYAU_SARAP ) then
      Erreur%Numero = 301
      Erreur%ft     = err_301
      Erreur%ft_c   = err_301c
      call TRAITER_ERREUR( Erreur , 'Reprise de calcul' )
      return
   end if

   if( RepriseCalcul ) then
      write(UniteListing,10570) 'OUI'
   else
      write(UniteListing,10570) 'NON'
   endif

   ! Ficher de reprise en lecture
   !-----------------------------
   pathNode = 'parametresImpressionResultats/fichReprise/fichRepriseEcr'
   FichierRepriseLec%Nom = xcasReader(unitNum, pathNode)

   if( RepriseCalcul ) then
      write(UniteListing,10580) FichierRepriseLec%Nom
   endif

   ! LIGNE D'EAU INITIALE
   !---------------------
   pathNode = 'parametresConditionsInitiales/ligneEau/LigEauInit'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) presence_ligne_deau
   if( ( Noyau == NOYAU_REZODT .or. Noyau == NOYAU_MASCARET) .and. &
       .not.RepriseCalcul .and. .not.presence_ligne_deau) then
      Erreur%Numero = 349
      Erreur%ft     = err_349
      Erreur%ft_c   = err_349c
      call TRAITER_ERREUR( Erreur )
      return
   end if

   if( presence_ligne_deau ) then
      write(UniteListing,10590) 'OUI'
   else
      write(UniteListing,10590) 'NON'
   endif

   if( presence_ligne_deau ) then
      ! Type d'entree de la ligne d'eau initiale
      pathNode = 'parametresConditionsInitiales/ligneEau/modeEntree'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) type_entree_ligne
      if( type_entree_ligne == SAISIE_PAR_CLAVIER .and.  &
          TypeMaillage == TYPE_MAILLAGE_PRECEDENT) then
         Erreur%Numero = 350
         Erreur%ft     = err_350
         Erreur%ft_c   = err_350c
         call TRAITER_ERREUR( Erreur )
         return
      end if

      if( type_entree_ligne == SAISIE_PAR_FICHIER ) then
         write(UniteListing,10600) 'PAR FICHIER'

         ! Nom du fichier
         !---------------
         pathNode = 'parametresConditionsInitiales/ligneEau/fichLigEau'
         FichierLigne%Nom = xcasReader(unitNum, pathNode)
         write(UniteListing,10610) FichierLigne%Nom

         ! Format du fichier
         !------------------
         pathNode = 'parametresConditionsInitiales/ligneEau/formatFichLig'
         line = xcasReader(unitNum, pathNode)
         read(unit=line, fmt=*) format_ligne
         if( format_ligne /= FORMAT_STO_OPTHYCA .and.  &
             format_ligne /= FORMAT_STO_PERMANENT) then
            Erreur%Numero = 305
            Erreur%ft     = err_305
            Erreur%ft_c   = err_305c
            call TRAITER_ERREUR( Erreur , 'Format du fichier geometrie' , '1 et 2' )
            return
         end if

         select case(format_ligne)

            case( FORMAT_STO_OPTHYCA )
               write(UniteListing,10620) 'OPTHYCA'
            case( FORMAT_STO_PERMANENT )
               write(UniteListing,10620) 'LIDO PERMANENT'
            case( FORMAT_STO_NONPERMANENT )
               write(UniteListing,10620) 'LIDO NON PERMANENT'

         end select

      else
         write(UniteListing,10600) 'PAR_CLAVIER'
      endif

      call LEC_LIGNE        ( &
         Z                  , & ! Resultats
         Q                  , &
         CF1                , &
         CF2                , &
         X                  , & ! Donnees/Resultats
         type_entree_ligne  , & ! Donnees
         FichierLigne       , &
         format_ligne       , &
         TypeMaillage       , &
         impression_ligne   , &
         UniteListing       , &
         Profil             , &
         Prof_Abs           , &
         ProfDebBief        , &
         ProfFinBief        , &
         absc_rel_ext_deb_bief, &
         absc_rel_ext_fin_bief, &
         Erreur             ) ! Erreur
      if( Erreur%Numero /= 0 ) then
         return
      endif

   endif ! presence_ligne_deau

   !==============================================================
   ! A PARTIR DE MAINTENANT, LE MAILLAGE EST CONNU,
   ! Z ET Q SONT EVENTUELLEMENT ALLOUE ET SONT CONNUS SI L.E. INI,
   ! SI ON N'A PAS DE LE INI (CAS PERMANENTS) => IL FAUT ALLOUER
   ! Z, Q ET LES INITIALISER
   !==============================================================
   if( .not. associated(Z) ) then
      allocate( Z(size(X)) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Z' )
         return
      end if
      Z(:) = W0    ! W0 = 0._DOUBLE

      allocate( Q(size(X)) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Q' )
         return
      end if
      Q(:) = W0    ! W0 = 0._DOUBLE
  endif

   !========================================================
   !                     CALCUL DE IDT et XDT
   !========================================================
   allocate( IDT(size(X)) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'IDT' )
      return
   end if

   allocate( XDT(size(X)) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'XDT' )
      return
   end if

   iprof_inf = 1

   do isect = 1 , size(X(:))
      iprof = iprof_inf
      do while( ( X(isect) <  (Profil(iprof)%AbsAbs   - EPS4) ) .or.    &
                ( X(isect) >= (Profil(iprof+1)%AbsAbs - EPS4) ) )
         if( ( dabs(X(isect)- Profil(size(Profil(:)))%AbsAbs)) <= 0.0001_DOUBLE ) then
             iprof = size(Profil(:)) - 1
             exit
         endif
         iprof = iprof + 1
      end do
      XDT(isect) = ( X(isect) - Profil(iprof)%AbsAbs ) / ( Profil(iprof + 1)%AbsAbs - Profil(iprof)%AbsAbs )
      IDT(isect) = iprof
      iprof_inf  = iprof
   end do

   !
   !==============================================================
   !               IMPRESSIONS - RESULTATS  annexe
   !==============================================================
   ! Option de stockage
   !-------------------
   pathNode = 'parametresImpressionResultats/stockage/option'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) OptionStockage
   if( OptionStockage /= STOCKAGE_TOUTES_SECTION .and. &
       OptionStockage /= STOCKAGE_LISTE_SECTION) then
      Erreur%Numero = 305
      Erreur%ft     = err_305
      Erreur%ft_c   = err_305c
      call TRAITER_ERREUR( Erreur , 'Option de stockage' , '1 ou 2' )
      return
   end if

   if( OptionStockage == STOCKAGE_TOUTES_SECTION ) then
      write(UniteListing,10432) 'A toutes les sections'
   elseif( OptionStockage == STOCKAGE_LISTE_SECTION ) then
      write(UniteListing,10432) 'A certaines sections'
   endif

   pathNode = 'parametresImpressionResultats/stockage/nbSite'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nb_site
   if( OptionStockage == STOCKAGE_LISTE_SECTION .and. nb_site <= 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de sites' )
      return
   end if

   if( OptionStockage == STOCKAGE_LISTE_SECTION ) then

      write(UniteListing,10434) nb_site

      allocate( SectionStockage(nb_site) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'SectionStockage' )
         return
      end if

      allocate( itab(nb_site) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab' )
          return
      end if
      allocate( rtab(nb_site) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab' )
          return
      end if

      pathNode = 'parametresImpressionResultats/stockage/branche'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab

      pathNode = 'parametresImpressionResultats/stockage/abscisse'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab

      do isect = 1 , nb_site
         num_branche = itab(isect)
         if( num_branche < 1 ) then
            Erreur%Numero = 372
            Erreur%ft     = err_372
            Erreur%ft_c   = err_372c
            call TRAITER_ERREUR( Erreur , isect )
            return
         end if

         abscisse_rel = rtab(isect)

         write(UniteListing,10436) isect , num_branche , abscisse_rel

         absc_abs = ABS_ABS_S    ( &
                    num_branche  , &
                    abscisse_rel , &
                    Profil       , &
                    ProfDebBief  , &
                    ProfFinBief  , &
                    Erreur         &
                                 )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         call XINDIC_S( SectionStockage(isect), &
                        absc_abs              , &
                        X                     , &
                        Erreur                )
         if( Erreur%Numero /= 0 ) then
            return
         endif

      end do

      deallocate(itab)
      deallocate(rtab)

   endif

   !
   !=============================================================
   !                      CAS D'UN ARRET DU CALCUL
   !           AVEC COTE MAXIMALE ATTEINTE A UN POINT DE CONTROLE
   !=============================================================
   ! initialisation des variables
   Section_controle = 1
   Abs_abs_controle = 0

   if( CritereArret == COTE_MAXIMALE_AU_POINT_DE_CONTROLE ) then
      !erreur sur l'abs rel ou le no du bief
      if( Bief_controle <= 0 .or. &
          Bief_controle > size(ProfDebBief) ) then
         Erreur%Numero = 332
         Erreur%ft     = err_332
         Erreur%ft_c   = err_332c
         call TRAITER_ERREUR( Erreur , 'point de controle pour arret du calcul' , Bief_controle , 1 )
         return
      end if

      if( Abs_rel_controle < (Profil(ProfDebBief(Bief_controle))%AbsRel-EPS4) &
           .or. &
          Abs_rel_controle > (Profil(ProfFinBief(Bief_controle))%AbsRel+EPS4) ) then
         Erreur%Numero = 387
         Erreur%ft     = err_387
         Erreur%ft_c   = err_387c
         call TRAITER_ERREUR( Erreur , 'point de controle pour arret du calcul' , Bief_controle )
         return
      end if

      ! calcul de l'abscisse abs
      Abs_abs_controle = ABS_ABS_S        ( &
        Bief_controle                     , &
        Abs_rel_controle                  , &
        Profil                            , &
        ProfDebBief                       , &
        ProfFinBief                       , &
        Erreur                              &
                                             )
      if( Erreur%Numero /= 0 ) then
         return
      end if

      ! Calcul de la section de calcul correspondante
      call XINDIC_S                          (&
           Section_controle,                  &
           Abs_abs_controle,                  &
           X,                                 &
           Erreur                              )
      if( Erreur%Numero /= 0 ) then
         return
      endif
   endif

   !==============================================================
   !                      LECTURE DES FROTTEMENTS
   !==============================================================
   ! Lois de frottement
   !-------------------
   pathNode = 'parametresCalage/frottement/loi'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) LoiFrottement
   if( LoiFrottement < LOI_FROTTEMENT_STRICKLER .or. &
       LoiFrottement > LOI_FROTTEMENT_NB_MAX ) then
      Erreur%Numero = 366
      Erreur%ft     = err_366
      Erreur%ft_c   = err_366c
      call TRAITER_ERREUR( Erreur , LoiFrottement )
      return
   end if

   ! Si on a lu un fichier geometrie au format 3.0 (c.a.d. qu'on n'a
   ! pas lu les coeff de frottement), on lit les coeff maintenant.
   !---------------------------------------------------------------
   if( FormatGeom == FORMAT_GEOM_LIDOV3P0 ) then
      call  LEC_FROTTEMENT  ( &
      CF1                   , &
      CF2                   , &
      X                     , &
      ZoneFrot              , &
      XDT                   , &
      Profil                , &
      ProfDebBief           , &
      ProfFinBief           , &
      absc_rel_ext_deb_bief , &
      absc_rel_ext_fin_bief , &
      InterpLinCoeffFrott   , &
      UniteListing          , &
      unitNum               , & ! Unite logique .xcas
      Erreur                  & ! Erreur
                            )
      if( Erreur%Numero /= 0 ) then
         return
      endif
   endif

   !========================================================
   !       CALCUL DE LA TABLE DE CONNECTIVITE
   !========================================================
   call CALC_CONNECT       ( &
     Connect               , & ! Table de connectivite
     NbBief                , & ! Nombre de biefs
     absc_abs_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_abs_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     X                     , & ! Abscisses de sections de calcul
     NbNoeud               , & ! Nombre de noeuds
     NbExtNoeud            , & ! Nombre d'extremite relie a chaque noeud
     ExtDebBief            , & ! Numero de l'extremite debut de chaque bief
     ExtFinBief            , & ! Numero de l'extremite fin de chaque bief
     ExtNoeud              , & ! Numero d'extremite lie a un noeud
     NbExtLibre            , & ! Nombre d'extremites libres
     NumExtLibre           , & ! Numero d'extremite libre
     Erreur                  & ! Erreur
                           )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !==============================================================
   !                  LECTURE DES ZONES DE STOCKAGE
   !==============================================================
   if( FormatGeom == FORMAT_GEOM_LIDOV3P0 ) then
      call LEC_STOCKAGE         ( &
            Profil              , & ! Profils geometriques
            PresenceZoneStockage, & ! Flag d'existence de zones de stockage
            FichierListing%Unite, & ! Unite logique fichier listing
            unitNum             , & ! Unite logique .xcas
            Erreur                & ! Erreur
                                )
      if( Erreur%Numero /= 0 ) then
         return
      endif
   endif

   !==============================================================
   !                    LECTURE DES ZONES SECHES
   !==============================================================
   call LEC_ZONE_SECHE     ( &
     ZoneSeche             , & ! Zones seches
     Connect               , & ! Connectivite du reseau
     X                     , & ! Maillage
     Profil                , &
     ProfDebBief           , &
     ProfFinBief           , &
     absc_rel_ext_deb_bief , &
     absc_rel_ext_fin_bief , &
     FichierListing%Unite  , & ! Unite logique fichier listing
     unitNum               , & ! Unite logique .xcas
     Erreur                  & ! Erreur
                           )
   if( Erreur%Numero /= 0 ) then
      return
   endif


   !==============================================================
   !      CALCUL DE L'ALGORITHME DE PARCOURS DES BIEFS
   !==============================================================
   if( Noyau == NOYAU_SARAP ) then
      call ALGOP                  (  &
               Algorithme          , &
               size(X)             , &
               NbExtNoeud          , &
               Connect             , &
               impression_reseau   , &
               FichierListing%Unite, &
               Erreur             )
      if( Erreur%Numero /= 0 ) then
         return
      endif
   endif

   !========================================================
   !                      LECTURE DU BARRAGE
   !========================================================
   if( OndeSubm ) then
      call LEC_BARRAGE      ( &
      Barrage               , & ! Barrage
      Connect               , & ! Connectivite du reseau
      X                     , & ! Tableau du maillage
      Profil                , & ! Profils geometriques
      ProfDebBief           , & ! Premiers profils des biefs
      ProfFinBief           , & ! Derniers profils des biefs
      absc_rel_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
      absc_rel_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
      FichierListing%Unite  , & ! Unite logique fichier listing
      unitNum               , & ! Unite logique .xcas
      Erreur                  & ! Erreur
                            )
      if( Erreur%Numero /= 0 ) then
         return
      endif
   endif

   !=========================================================
   !                   LECTURE DES SINGULARITES
   !=========================================================
   call LEC_SING           ( &
     Singularite           , & ! Pertes de charges singulieres
     LoiHydrau             , & ! Loi hydraulique
     Connect               , & ! Connectivite du reseau
     X                     , & ! Maillage
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     absc_rel_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     Noyau                 , & ! Noyau de calcul utilise
     UniteListing          , &
     unitNum               , & ! Unite logique .xcas
     Erreur                  & ! Erreur
                           )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !==============================================================
   !            LECTURE DES PERTES DE CHARGE SINGULIERES
   !==============================================================
   call LEC_PCSING         ( &
     PCSing                , & ! Pertes de charges singulieres
     X                     , & ! Tableau du maillage
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     absc_rel_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     FichierListing%Unite  , &
     unitNum               , & ! Unite logique .xcas
     Erreur                  & ! Erreur
                           )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !==============================================================
   !                      LECTURE DES DEBITS D'APPORTS
   !==============================================================
   call LEC_APPORT         ( &
     Apport                , & ! Tableau des lois hydrauliques
     Connect               , & ! Table de connectivite du reseau
     X                     , & ! Maillage
     LoiHydrau             , & ! Lois hydrauliques
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     absc_rel_ext_deb_bief , & ! Abscisse rel de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse rel de l'extremite debut du bief
     FichierListing%Unite  , & !
     unitNum               , & ! Unite logique .xcas
     Erreur                  & ! Erreur
                           )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !==============================================================
   !                      LECTURE DES DEVERSOIRS
   !==============================================================
   call LEC_DEVER          ( &
     Deversoir             , & ! Tableau des lois hydrauliques
     Connect               , & ! Table de connectivite du reseau
     X                     , & ! Maillage
     LoiHydrau             , & ! Lois hydrauliques
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     absc_rel_ext_deb_bief , & ! Abscisse rel de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse rel de l'extremite debut du bief
     FichierListing%Unite  , &
     unitNum               , & ! Unite logique .xcas
     Erreur                  & ! Erreur
                           )

   if( Erreur%Numero /= 0 ) then
      return
   endif

   !==============================================================
   !                      LECTURE DES CONFLUENTS
   !==============================================================
   call LEC_CONFLUENT    ( &
     Confluent           , &
     Connect             , &
     FichierListing%Unite, &
     Noyau               , &
     unitNum             , & ! Unite logique .xcas
     Erreur                & ! Erreur
                         )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !==============================================================
   !    LECTURE DES ABAQUES DES PERTES DE CHARGE AUX CONFLUENTS
   !==============================================================
   call LEC_ABAQUE    ( &
     Abaque           , & ! Barrage
     FichierAbaque    , & ! Fichier abaque
     Erreur             & ! Erreur
                      )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !=============================================================
   !               LECTURE DES VARIABLES A SORTIR
   !=============================================================
   call LEC_SORTIES      ( &
     VarSto              , &
     VarCalc             , &
     unitNum             , & ! Unite logique .xcas
     Erreur                & ! Erreur
                         )

   if( Erreur%numero /= 0 ) then
      return
   endif

   !==============================================================
   !                DE ALLOCATION DES TABLEAUX LOCAUX
   !==============================================================
   deallocate( absc_rel_ext_deb_bief , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'absc_rel_ext_deb_bief' )
      return
   end if

   deallocate( absc_rel_ext_fin_bief , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'absc_rel_ext_fin_bief' )
      return
   end if

   !==============================================================
   !                ECRITURE DU MODELE SUR FICHIER
   !==============================================================
   if( sauvegarde_modele ) then

      ul = FichierModele%Unite
      open(unit=ul, file=FichierModele%Nom, access='SEQUENTIAL'  , &
           action='WRITE'    , form='UNFORMATTED', iostat=RETOUR , &
           position='rewind' , status='REPLACE'  )

      if( RETOUR /= 0 ) then
         Erreur%Numero = 3
         Erreur%ft     = err_3
         Erreur%ft_c   = err_3c
         call TRAITER_ERREUR( Erreur , FichierModele%Nom )
         return
      end if

      write(ul,*)Noyau
      write(ul,*)OndeSubm
      write(ul,*)CalculValidation
      write(ul,*)TypeValidation
      write(ul,*)Regime
      write(ul,*)ModeleLit
      write(ul,*)LoiFrottement
      write(ul,*)FrottParoiVerticale
      write(ul,*)DebProgressifLM
      write(ul,*)DebProgressifZS
      write(ul,*)DZArriveeFront
      write(ul,*)FroudeLim
      write(ul,*)FrottementImplicite
      write(ul,*)DT
      write(ul,*)TempsInitial
      write(ul,*)CritereArret
      write(ul,*)NbPasTemps
      write(ul,*)TempsMaximum
      write(ul,*)PasTempsVariable
      write(ul,*)CourantObj
      write(ul,*)PresenceZoneStockage
      write(ul,*)X
      write(ul,*)IDT
      write(ul,*)XDT
      write(ul,*)CF1
      write(ul,*)CF2
      write(ul,*)ZoneSeche
      write(ul,*)ImpressionCalcul
      write(ul,*)Barrage
      write(ul,*)PCSing

      if( Noyau == NOYAU_SARAP ) write(ul,*) Algorithme

      write(ul,*)TypeMaillage           ! pour intersect
      write(ul,*)Z                      ! conditions initiale
      write(ul,*)Q                      ! ""
      write(ul,*)FormatGeom             ! pour intersect
      write(ul,*)InterpLinCoeffFrott     ! pour intersect
      write(ul,*)TitreCas               ! pour stock
      write(ul,*)ImpressionPlani        ! pour planim et intersect
      write(ul,*)PasStockage            ! pour stock
      write(ul,*)PasImpression          ! pour post_imp
      write(ul,*)PremierPasStocke       ! pour stock
      write(ul,*)FormatResu             ! pour stock
      write(ul,*)VarCalc                ! pour Post
      write(ul,*)VarSto                 ! pour post_imp et stock
      write(ul,*)OptionStockage         ! pour stock
      write(ul,*)SectionStockage        ! pour stock

      close (ul)

   endif ! de if sauvegarde maillage

   if( OptionCasier ) then

      !=======================================================================
      !                       LECTURE DES FICHIERS CASIERS
      !=======================================================================
      ulc                      = FichierListingCasier%Unite
      pathNode = 'parametresImpressionResultats/casier/listingCasier'
      FichierListingCasier%Nom = xcasReader(unitNum, pathNode)

      open(unit=ulc, file=FichierListingCasier%Nom, access='SEQUENTIAL', &
           action='WRITE'           , form='FORMATTED'       , iostat=RETOUR      , &
           position='rewind'        , status='REPLACE'     )
      if( RETOUR /= 0 ) then
         Erreur%Numero = 4
         Erreur%ft     = err_4
         Erreur%ft_c   = err_4c
         call TRAITER_ERREUR( Erreur , FichierListingCasier%Nom )
         return
      end if

      pathNode = 'parametresImpressionResultats/casier/listingLiaison'
      FichierListingLiaison%Nom = xcasReader(unitNum, pathNode)

      ull                       = FichierListingLiaison%Unite

      open(unit=ull, file=FichierListingLiaison%Nom, access='SEQUENTIAL', &
           action='WRITE'           , form='FORMATTED'       , iostat=RETOUR      , &
           position='rewind'        , status='REPLACE'     )
      if( RETOUR /= 0 ) then
         Erreur%Numero = 4
         Erreur%ft     = err_4
         Erreur%ft_c   = err_4c
         call TRAITER_ERREUR( Erreur , FichierListingLiaison%Nom )
         return
      end if

      pathNode = 'parametresImpressionResultats/casier/resultatCasier'
      FichierResultatCasier%Nom = xcasReader(unitNum, pathNode)

      pathNode = 'parametresImpressionResultats/casier/resultatLiaison'
      FichierResultatLiaison%Nom = xcasReader(unitNum, pathNode)

      pathNode = 'parametresCasier/fichierGeomCasiers'
      FichierGeomCasier%Nom = xcasReader(unitNum, pathNode)

      !========================================================================
      !                       LECTURE DE LA VARIABLE CASIER
      !========================================================================
      call LEC_CASIER                (&
                Casier,               &
                FichierGeomCasier,    &
                unitNum,              &
                Erreur    )
      if( Erreur%Numero /= 0 ) then
         return
      end if

      allocate( connect_casier(size(Casier),size(Casier)) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'de la matrice de connection casier-casier' )
          return
      end if
      connect_casier(:,:) = 0

      !========================================================================
      !                       LECTURE DE LA VARIABLE LIAISON
      !========================================================================
      call LEC_LIAISON(           &
                Liaison,          &
                connect_casier,   &
                X,                &
                Profil,           &
                ProfDebBief,      &
                ProfFinBief,      &
                unitNum,         & ! Pointeur vers document XML
                Erreur       )
      if( Erreur%Numero /= 0 ) then
         return
      end if

      !==========================================================================
      !           CALCUL DE LA MATRICE DE CONNECTION CASIER - CASIER
      !==========================================================================
      do icasier = 1 , size(Casier)
         ! on compte le nombre de liaisons casier - casier relie a icasier
         nb_liaisonCC = 0
         do jcasier = 1 , size(Casier)
            if( connect_casier(icasier,jcasier) == 1 ) then
               do iliaison = 1 , size(Liaison)
                  select case( Liaison(iliaison)%NatureLiaison )
                     case( LIAISON_TYPE_CASIER_CASIER )
                        num_casier_origine = Liaison(iliaison)%CaracCC%CasierOrigine
                        num_casier_fin     = Liaison(iliaison)%CaracCC%CasierFin
                        if( (num_casier_origine == icasier) .or. (num_casier_fin == icasier) ) then
                           if( (num_casier_origine == jcasier) .or. (num_casier_fin == jcasier) ) then
                              nb_liaisonCC = nb_liaisonCC + 1
                           end if
                        end if
                  end select
               end do
            end if
         end do

         allocate( Casier(icasier)%LiaisonCC(nb_liaisonCC,2) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'de definition des liaisons casier-casier' )
            return
         end if

         ! on affecte les numeros de casiers et liaisons associes
         ilcc = 1
         do jcasier = 1 , size(Casier)
            if( connect_casier(icasier,jcasier) == 1 ) then
               ! le casier "jcasier" est relie au casier "icasier"
               ! on cherche les numeros des liaisons qui relient ces deux casiers
               do iliaison = 1 , size(Liaison)
                  select case( Liaison(iliaison)%NatureLiaison )
                     case( LIAISON_TYPE_CASIER_CASIER )
                        num_casier_origine = Liaison(iliaison)%CaracCC%CasierOrigine
                        num_casier_fin     = Liaison(iliaison)%CaracCC%CasierFin
                        if( ( num_casier_origine == icasier ) .or. ( num_casier_fin == icasier ) ) then
                           if( ( num_casier_origine == jcasier ) .or. ( num_casier_fin == jcasier ) ) then
                              Casier(icasier)%LiaisonCC(ilcc,1) = iliaison
                              Casier(icasier)%LiaisonCC(ilcc,2) = jcasier
                              ilcc = ilcc+1
                           end if
                        end if
                  end select
               end do
            end if
         end do
      end do

      !==========================================================================
      !           CALCUL DE LA MATRICE DE CONNECTION RIVIERE - CASIER
      !==========================================================================
      do icasier = 1 , size(Casier)
         nb_liaisonRC = 0

         do iliaison = 1, size(Liaison)
            if ( Liaison(iliaison)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
                if ( Liaison(iliaison)%CaracRC%NumCasier == icasier ) then
                    nb_liaisonRC =  nb_liaisonRC + 1
                endif
            endif
         enddo

         allocate( Casier(icasier)%LiaisonRC(nb_liaisonRC,2) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'de definition des liaisons riviere-casier' )
            return
         end if

         ! Affectation des numeros de sections et liaisons associes
         ilcc = 1
         do iliaison = 1 , size(Liaison)
            if( Liaison(iliaison)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
                if ( Liaison(iliaison)%CaracRC%NumCasier == icasier ) then
                    ! la liaison "iliaison" est reliee au casier "icasier"
                    Casier(icasier)%LiaisonRC(ilcc,1) = iliaison
                    Casier(icasier)%LiaisonRC(ilcc,2) = Liaison(iliaison)%CaracRC%Section
                    ilcc = ilcc + 1
                endif
            endif
         enddo
      end do

      !========================================================================
      !                       LECTURE DE LA VARIABLE APPORTPLUIE
      !========================================================================
      call LEC_APPORT_PLUIE        (&
                 ApportPluie,       &
                 size(Casier),      &
                 LoiHydrau,         &
                 unitNum,           & ! Unite logique .xcas
                 ulc,&
                 Erreur)
      if( Erreur%Numero /= 0 ) then
         return
      end if
   else
     ! Fausse allocation pour pouvoir passer les pointeurs en argument
     allocate(Casier(1))
     allocate(Liaison(1))
     allocate(ApportPluie(1))
   end if

   close(unitNum)
   !Erreur%arbredappel = !arbredappel_old

   return

   ! ... Format Declarations ...
   10000 format (/18X,'*************************************'  &
                 /18X,'*        SYSTEME MASCARET           *'  &
                 /18X,'*         VERSION  ',A,'          *'     &
                 /18X,'* ',A33,' *'                          &
                 /18X,'*************************************'///)

   10005 format (/,'PARAMETRES GENERAUX',/, &
                &  '-------------------',/)
   10010 format ('Noyau de calcul utilise                         : ',A8)
   10020 format ('Nom du fichier des mots-cles                    : ',A)
   10030 format ('Nom du fichier du programme principal           : ',A)
   10035 format ('Nom du fichier des bibliotheques                : ',A)
   10037 format ('Sauvegarde du modele                            : ',A)
   10040 format ('Nom du fichier de sauvegarde du modele          : ',A)
   10044 format ('Presence de casier                              : ',A)
   10045 format ('Calcul auto des pertes de charge aux confluents : ',A)
   10046 format ('Perte automatique aux elargissements transcritique : ',A)
   10047 format ('Option courlis                                  : ',A)
   10050 format ('Calcul d''onde de submersion')
   10060 format ('Calcul de validation')
   10070 format ('Type de validation : ',i3)
   10075 format (/,'MODELISATION PHYSIQUE',/, &
                &  '---------------------',/)
   10080 format ('Modelisation du lit                               : ',A)
   10090 format ('Frottement sur les parois verticales              : ',A3)
   10100 format ('Debordement progressif dans le lit majeur         : ',A3)
   10110 format ('Debordement progressif dans les zones de stockage : ',A3)
   10115 format ('Elevation de cote signalant l''arrivee du front    : ',f12.3)
   10120 format ('Froude limite pour les conditions aux limites     : ',f12.3)
   10130 format ('Type de frottement                                : ',A)
   10135 format ('Interpolation lineaire des Coeff de frottement    : ',A)
   10136 format ('Implicitation du noyau transcritique              : ',A)
   10140 format (/,'PARAMETRES TEMPORELS',/, &
                &  '--------------------',/)
   10150 format ('Pas de temps                           : ',f12.3)
   10160 format ('Temps initial                          : ',f12.3)
   10170 format ('Critere d''arret du calcul              : ',A)
   10180 format ('Nombre de pas de temps du calcul       : ',i10)
   10190 format ('Duree maximale du calcul               : ',f12.1)
   10200 format ('Pas de temps variable                  : ',A3)
   10210 format ('Nombre de Courant objectif             : ',f12.3)
   10300 format (/,'IMPRESSION-RESULTATS',/, &
                &  '--------------------',/)
   10310 format ('Titre du calcul                        : ',A)
   10320 format ('Impression de la geometrie             : ',A3)
   10330 format ('Impression du planimetrage             : ',A3)
   10340 format ('Impression du reseau                   : ',A3)
   10350 format ('Impression des lois hydrauliques       : ',A3)
   10360 format ('Impression de la ligne d''eau initiale  : ',A3)
   10365 format ('Impression en phase calcul             : ',A3)
   10370 format (/,'Premier pas de temps a stocker         : ',i5)
   10380 format ('Pas de stockage                        : ',i5)
   10390 format ('Pas d''impression                       : ',i5)
   10400 format (/,'Fichier resultats                      : ',A)
   10405 format ('Post-processeur utilise                : ',A,/, &
                 'Format du fichier Resultats            : ',A)
   10410 format ('Fichier listing                        : ',A)
   10420 format ('Fichier de reprise en ecriture         : ',A)
   10430 format (/,'Ecart entre branches                   : ',f12.3)
   10432 format (/,'Option de stockage                   : ',A)
   10434 format ('Nombre de sites                      : ',i3,/)
   10436 format ('Site n0 : ',i3,' Branche n0 : ',i3,' Abscisse : ',f12.3)
   10440 format (/,'GEOMETRIE',/, &
                &  '---------',/)
   10450 format ('Nom du fichier geometrie    : ',A)
   10460 format ('Format du fichier geometrie : ',A)
   10560 format (/,'CONDITIONS INITIALES',/, &
                &  '--------------------',/)
   10570 format ('Reprise de calcul                        : ',A3)
   10580 format ('Fichier de reprise en lecture            : ',A)
   10590 format (/,'Presence d''une ligne d''eau initiale     : ',A3)
   10600 format ('Mode d''entree de la ligne d''eau         : ',A)
   10610 format ('Nom du fichier de la ligne d''eau        : ',A)
   10620 format ('Format du fichier de la ligne d''eau     : ',A)

   contains
   !========================================================
   !                 SOUS-PROGRAMMES UTILITAIRES
   !========================================================

   subroutine xerror(Erreur)

       use M_MESSAGE_C
       use M_ERREUR_T            ! Type ERREUR_T

       type(ERREUR_T)                   , intent(inout) :: Erreur

       Erreur%Numero = 704
       Erreur%ft     = err_704
       Erreur%ft_c   = err_704c
       call TRAITER_ERREUR( Erreur )

       return

   end subroutine xerror

end subroutine PRETRAIT
