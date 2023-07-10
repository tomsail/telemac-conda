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

! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
   !..................................................................................................................................
   ! Importation d'un modele mascaret a partir des fichiers natifs de Mascaret
   ! .................................................................................................................................
    subroutine IMPORT_MODELE_MASCARET(RetourErreur, Identifiant, TabNomFichier, TypeNomFichier, Taille, Impress, FichierMascaret)
      use M_APIMASCARET_STATIC
      use M_CONSTANTES_CALCUL_C
      use M_PRETRAIT_INTERFACE_I
      use M_PRETRAIT_TRACER_INTERFACE_I
      use M_INTERSECT_I
      use M_PLANIM_I
      use M_PLANMA_I
      use M_MODELE_TRACER_T
      implicit none

      integer, intent(out)                         :: RetourErreur   ! different de 0 si erreur
      integer, intent(in )                         :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
      character(LEN=255), dimension(*), intent(in) :: TabNomFichier  ! Tableau des noms des fichiers natifs Mascaret a importer
      character(LEN=40),  dimension(*), intent(in) :: TypeNomFichier ! Tableau des type des fichiers natifs Mascaret a importer:
                                                                     ! "casier", "geo", "loi", "cas","listing",
                                                                     ! "res", "listing_casier", "listing_liaison", "res_casier", "res_liaison"
      integer, intent(in )                         :: Taille         ! Taille des 2 tableaux TabNomFichier et TypeNomFichier
      integer, intent(in )                         :: Impress        ! impression sur les fichiers listing (1-> Vrai 0-> Faux)
      character(LEN=255), optional,     intent(in) :: FichierMascaret  ! Fichier listant les noms des fichiers natifs Mascaret a importer

      !------------------------------------------------------------------------------------------------------------
      ! variables locales
      !------------------------------------------------------------------------------------------------------------
      Type(MODELE_MASCARET_T)  :: Modele

      ! fichiers en entree d'un modele hydraulique
      Type(FICHIER_T)                        :: FichierMotCle ! type "cas"  exemple Model.cas (obligatoire)
      Type(FICHIER_T)                        :: FichierGeom   ! type "geo"  exemple Model.geo (obligatoire)
      Type(FICHIER_T)                        :: FichierLoiHydrau
      Type(FICHIER_T), dimension(:), pointer :: FichiersLois => null() ! type "loi"  exemple Model.loi (pas obligatoire)
      Type(FICHIER_T)                        :: FichierLigne  ! type "lig"  exemple Model.lig (pas obligatoire)

      ! fichiers en entree d'un modele sedimentaire
      Type(FICHIER_T)                        :: FichierMotCleCourlis ! type "cas"  exemple Model.cas (obligatoire)
      Type(FICHIER_T)                        :: FichierDicoCourlis ! type "dico"  exemple Model.cas (obligatoire)

      ! fichiers sorties d'un modele hydraulique
      Type(FICHIER_T)          :: FichierListing   ! type "listing"  exemple Model.listing (obligatoire)
      Type(FICHIER_T)          :: FichierResultat  ! type "res"      exemple Model.res     (obligatoire)
      Type(FICHIER_T)          :: FichierControle  ! type "?"        exemple controle.txt   (obligatoire)

      ! fichier en entre pour modele avec casiers (pas obligatoire)
      Type(FICHIER_T)          :: FichierGeomCasier      ! type "casier"           exemple Model.casier

      ! fichiers en sortie pour modele avec casiers (pas obligatoire)
      Type(FICHIER_T)          :: FichierListingLiaison  ! type "listing_liaison"  exemple Model.listing_liaison
      Type(FICHIER_T)          :: FichierListingCasier   ! type "listing_casier"   exemple Model.listing_casier
      Type(FICHIER_T)          :: FichierResultatLiaison ! type "res_liaison"      exemple Model.res_liaison
      Type(FICHIER_T)          :: FichierResultatCasier  ! type "res_casier"       exemple Model.res_casier

      ! fichiers en entree pour un modele avec Tracer (pas obligatoire)
      Type(Fichier_T) :: FichierConcInit   ! type 'tracer_conc'
      Type(Fichier_T), dimension(:), pointer :: FichiersLoisTracer => null() ! type "tracer_loi"
      Type(Fichier_T) :: FichierParPhy     ! type 'tracer_parphy'
      Type(Fichier_T) :: FichierMeteo      ! type 'tracer_meteo'

      ! fichiers en sortie pour modele avec Tracer (pas obligatoire)
      Type(Fichier_T) :: FichierListingTracer ! type 'tracer_listing'
      Type(Fichier_T) :: FichierResuTracer    ! type 'tracer_res'

      ! fichiers jamais utilises dans le cadre d'une etude importe par l'interface
      Type(FICHIER_T)          :: FichierModele   ! sauvegarde du modele
      Type(FICHIER_T)          :: FichierMaillage ! fichier definissant un maillage
      Type(FICHIER_T)          :: FichierSauveMaillage ! fichier de sauvegarde du maillage
      Type(FICHIER_T)          :: FichierRepriseLec    ! fichier de reprise en lecture
      Type(FICHIER_T)          :: FichierRepriseEcr    ! fichier de reprise en ecriture
      Type(FICHIER_T)          :: FichierResultat2     ! deuxieme fichier resultat


      ! Impressions - resultats
      !------------------------

      logical                                 :: Impression
      logical                                 :: ImpressionPlani

      integer :: phase_intersect
      INTEGER :: i, compteurLoi, compteurLoiTrac, retour, unitnum
      integer :: nb_pas

      character(LEN=255)          :: nomFic
      character(LEN=40)           :: typeFic

      Type(ERREUR_T)              :: Erreur
      character(LEN=256)          :: MessageErreurType

      ! Courlis
      ! MS2019 : Ajout a verifier si les API sont ok avec ces modifs

      real(DOUBLE)    , dimension (:)  , pointer :: varsed ! Courlis : profil evolution
      real(DOUBLE)                :: TempsInitial

      FichierModele%Nom        = ''
      FichierMaillage%Nom      = ''
      FichierSauveMaillage%Nom = ''
      FichierRepriseLec%Nom    = ''
      FichierRepriseEcr%Nom    = ''
      FichierResultat2%Nom     = ''

      FichierListingLiaison%Nom  = ''
      FichierListingCasier%Nom   = ''
      FichierResultatLiaison%Nom = ''
      FichierResultatCasier%Nom  = ''
      FichierGeomCasier%Nom      = ''

      FichierConcInit%Nom      = ''
      FichierParPhy%Nom        = ''
      FichierMeteo%Nom         = ''
      FichierListingTracer%Nom = ''
      FichierResuTracer%Nom    = ''

      RetourErreur = 0

      RetourErreur = TEST_INIT_AND_ID(Identifiant, 'IMPORT_MODELE_MASCARET')
      if (RetourErreur > 0 ) then
         RETURN
      end if


      if (Impress == 1) then
        Impression = .true.
      else
        Impression = .false.
      endif

      if (Impression) then
        FichierModele%Unite        = 11
        FichierSauveMaillage%Unite = 16
        FichierRepriseEcr%Unite    = 18
        FichierResultat%Unite      = 20
        FichierResultat2%Unite     = 25
        FichierListing%Unite       = 22
        FichierControle%Unite      = 24
        FichierListingCasier%Unite   = 33
        FichierListingLiaison%Unite  = 34
        FichierResultatCasier%Unite  = 35
        FichierResultatLiaison%Unite = 36
        FichierListingTracer%Unite   = 43
        FichierResuTracer%Unite      = 44
      else
        FichierModele%Unite        = -1
        FichierSauveMaillage%Unite = -1
        FichierRepriseEcr%Unite    = -1
        FichierResultat%Unite      = -1
        FichierResultat2%Unite     = -1
        FichierListing%Unite       = -1
        FichierControle%Unite      = -1
        FichierListingCasier%Unite   = -1
        FichierListingLiaison%Unite  = -1
        FichierResultatCasier%Unite  = -1
        FichierResultatLiaison%Unite = -1
        FichierListingTracer%Unite   = -1
        FichierResuTracer%Unite      = -1
      end if

      FichierMotCle%Unite        = 12
      FichierGeom%Unite          = 14
      FichierMaillage%Unite      = 15
      FichierRepriseLec%Unite    = 17
      FichierLigne%Unite         = 19
      FichierLoiHydrau%Nom       = ''
      FichierLoiHydrau%Unite     = 21
      FichierGeomCasier%Unite    = 37
      FichierConcInit%Unite      = 45
      FichierParPhy%Unite        = 47
      FichierMeteo%Unite         = 48

      ! Temporary for courlis initialising variable
      FichierMotCleCourlis%Unite = -1
      FichierMotCleCourlis%Nom = ''
      FichierDicoCourlis%Unite = -1
      FichierDicoCourlis%Nom = ''
      nullify(varsed)
      TempsInitial = 0.0

      Modele%FichierGeomCasier%Unite = FichierGeomCasier%Unite
      Modele%FichierGeomCasier%Nom   = FichierGeomCasier%Nom
      Modele%FichierListing%Unite  = FichierListing%Unite
      Modele%FichierListing%Nom    = FichierListing%Nom
      Modele%FichierListingCasier%Unite = FichierListingCasier%Unite
      Modele%FichierListingCasier%Nom   = FichierListingCasier%Nom
      Modele%FichierListingLiaison%Unite = FichierListingLiaison%Unite
      Modele%FichierListingLiaison%Nom   = FichierListingLiaison%Nom
      Modele%FichierResuCasier%Unite = FichierResultatCasier%Unite
      Modele%FichierResuCasier%Nom   = FichierResultatCasier%Nom
      Modele%FichierResuLiaison%Unite = FichierResultatLiaison%Unite
      Modele%FichierResuLiaison%Nom   = FichierResultatLiaison%Nom
      Modele%FichierResultat%Unite = FichierResultat%Unite
      Modele%FichierResultat%Nom   = FichierResultat%Nom
      Modele%FichierResultat2%Unite = FichierResultat2%Unite
      Modele%FichierResultat2%Nom   = FichierResultat2%Nom

      Modele%Tracer%FichierConcInit%Unite      = FichierConcInit%Unite
      Modele%Tracer%FichierConcInit%Nom        = FichierConcInit%Nom
      Modele%Tracer%FichierListingTracer%Unite = FichierListingTracer%Unite
      Modele%Tracer%FichierListingTracer%Nom   = FichierListingTracer%Nom
      Modele%Tracer%FichierResuTracer%Unite    = FichierResuTracer%Unite
      Modele%Tracer%FichierResuTracer%Nom      = FichierResuTracer%Nom

      ! common du canal listing
      ul_lst = FichierListing%Unite
      UL_LST_CAS = FichierListingCasier%Unite

      ! comptage de nombre de fichier de type loi/tracer_loi
      compteurLoi = 0
      compteurLoiTrac = 0

      if(present(FichierMascaret)) then
          unitnum = 123
          open(unit=unitnum, file=FichierMascaret, status="old", action="read", iostat=RetourErreur)
          if(RetourErreur.ne.0) then
              ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - Unable to read from the Mascaret file'
              return
          endif
          do
            read(unitnum, *, iostat=RetourErreur) nomFic
            if(RetourErreur > 0) then
                ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - Unable to read from the Mascaret file'
                return
            elseif(RetourErreur < 0) then
                exit
            else
                if (index(nomFic, '.tracer_loi').ne.0) then
                   compteurLoiTrac = compteurLoiTrac +1
                elseif(index(nomFic, '.loi').ne.0) then
                   compteurLoi = compteurLoi +1
                endif
            endif
         end do
      else
          do i= 1, Taille
            typeFic = TRIM(TypeNomFichier(i))
            if ( index(typeFic, 'tracer_loi') > 0) then
               compteurLoiTrac = compteurLoiTrac +1
            else if ( index(typeFic, 'loi') > 0) then
               compteurLoi = compteurLoi +1
            endif
          end do
      endif

      ! allocation de FichiersLois
      if(.not.associated(FichiersLois)) then
          allocate(FichiersLois(compteurLoi), STAT = retour)
          if (retour /= 0) then
            ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - Unable to allocate les fichiers des lois'
            RetourErreur = 2
            RETURN
          end if
      endif
      if(.not.associated(FichiersLoisTracer).and.compteurLoiTrac > 0) then
          allocate(FichiersLoisTracer(compteurLoiTrac), STAT = retour)
          if (retour /= 0) then
            ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - Unable to allocate les fichiers des lois tracer'
            RetourErreur = 2
            RETURN
          end if
      endif

      ! affection des noms des fichiers imposes par l'API
      compteurLoi = 0
      compteurLoiTrac = 0

      if(present(FichierMascaret)) then
        rewind(unitnum)
        do
            read(unitnum, *, iostat=RetourErreur) nomFic
            if(RetourErreur > 0) then
                ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - Unable to read from the Mascaret file'
                return
            elseif(RetourErreur < 0) then
                exit
            else
                if ( index(nomFic,'.xcas').ne.0) then
                   FichierMotCle%Nom = nomFic
                elseif ( index(nomFic,'.geo').ne.0) then
                   FichierGeom%Nom   = nomFic
                elseif ( index(nomFic , '.listing_casier').ne.0) then
                   FichierListingCasier%Nom = nomFic
                   Modele%FichierListingCasier%Nom = nomFic
                elseif ( index(nomFic , '.listing_liaison').ne.0) then
                   FichierListingLiaison%Nom = nomFic
                   Modele%FichierListingLiaison%Nom = nomFic
                elseif ( index(nomFic , '.listing').ne.0) then
                      FichierListing%Nom = nomFic
                      Modele%FichierListing%Nom = nomFic
                elseif ( index(nomFic,'.res_casier').ne.0) then
                   FichierResultatCasier%Nom = nomFic
                   Modele%FichierResuCasier%Nom = nomFic
                elseif ( index(nomFic, '.casier').ne.0) then
                   FichierGeomCasier%Nom = nomFic
                   Modele%FichierGeomCasier%Nom = nomFic
                elseif (index(nomFic,'.res_liaison').ne.0) then
                   FichierResultatLiaison%Nom = nomFic
                   Modele%FichierResuLiaison%Nom = nomFic
                elseif( index(nomFic, '.res').ne.0) then
                   FichierResultat%Nom = nomFic
                   Modele%FichierResultat%Nom = nomFic
                elseif ( index(nomFic,'.lig').ne.0) then
                   FichierLigne%Nom = nomFic
                elseif ( index(nomFic,'.tracer_conc').ne.0) then
                   FichierConcInit%Nom = nomFic
                   Modele%Tracer%FichierConcInit%Nom = nomFic
                elseif ( index(nomFic,'.tracer_loi').ne.0) then
                   compteurLoiTrac = compteurLoiTrac + 1
                   FichiersLoisTracer(compteurLoiTrac)%Nom = nomFic
                   FichiersLoisTracer(compteurLoiTrac)%Unite = 46
                elseif (index(nomFic,'loi').ne.0) then
                    compteurLoi = compteurLoi + 1
                    FichiersLois(compteurLoi)%Nom = nomFic
                    FichiersLois(compteurLoi)%Unite = 21
                elseif ( index(nomFic,'tracer_parphy').ne.0) then
                   FichierParPhy%Nom = nomFic
                elseif ( index(nomFic,'tracer_meteo').ne.0) then
                   FichierMeteo%Nom = nomFic
                elseif ( index(nomFic,'tracer_listing').ne.0) then
                   FichierListingTracer%Nom = nomFic
                   Modele%Tracer%FichierListingTracer%Nom = nomFic
                elseif ( index(nomFic ,'tracer_res').ne.0) then
                   FichierResuTracer%Nom = nomFic
                   Modele%Tracer%FichierResuTracer%Nom = nomFic
                endif
            endif
          end do
          close(123)
      else
        do i = 1, Taille
            nomFic  = TRIM(TabNomFichier(i))
            typeFic = TRIM(TypeNomFichier(i))
            if ( index(typeFic, 'xcas') > 0) then
               FichierMotCle%Nom = nomFic
            else if ( index(typeFic, 'geo') > 0) THEN
               FichierGeom%Nom   = nomFic
            else if ( index(typeFic, 'tracer_loi') > 0) THEN
               compteurLoiTrac = compteurLoiTrac + 1
               FichiersLoisTracer(compteurLoiTrac)%Nom = nomFic
               FichiersLoisTracer(compteurLoiTrac)%Unite = 46
            else if ( index(typeFic, 'loi') > 0) THEN
               compteurLoi = compteurLoi + 1
               FichiersLois(compteurLoi)%Nom = nomFic
               FichiersLois(compteurLoi)%Unite = 21
            else if ( index(typeFic, 'listing_casier') > 0) THEN
               FichierListingCasier%Nom = nomFic
               Modele%FichierListingCasier%Nom = nomFic
            else if ( index(typeFic, 'listing_liaison') > 0) THEN
               FichierListingLiaison%Nom = nomFic
               Modele%FichierListingLiaison%Nom = nomFic
            else if ( index(typeFic, 'res_casier') > 0) THEN
               FichierResultatCasier%Nom = nomFic
               Modele%FichierResuCasier%Nom = nomFic
            else if ( index(typeFic, 'casier') > 0) THEN
               FichierGeomCasier%Nom = nomFic
               Modele%FichierGeomCasier%Nom = nomFic
            else if ( index(typeFic, 'res_liaison') > 0) THEN
               FichierResultatLiaison%Nom = nomFic
               Modele%FichierResuLiaison%Nom = nomFic
            else if ( index(typeFic, 'lig') > 0) THEN
               FichierLigne%Nom = nomFic
            else if ( index(typeFic, 'tracer_conc') > 0) THEN
               FichierConcInit%Nom = nomFic
               Modele%Tracer%FichierConcInit%Nom = nomFic
            else if ( index(typeFic, 'tracer_parphy') > 0) THEN
               FichierParPhy%Nom = nomFic
            else if ( index(typeFic, 'tracer_meteo') > 0) THEN
               FichierMeteo%Nom = nomFic
            else if ( index(typeFic, 'tracer_listing') > 0) THEN
               FichierListingTracer%Nom = nomFic
               Modele%Tracer%FichierListingTracer%Nom = nomFic
            else if ( index(typeFic, 'tracer_res') > 0) THEN
               FichierResuTracer%Nom = nomFic
               Modele%Tracer%FichierResuTracer%Nom = nomFic
            else if ( index(typeFic, 'res') > 0) THEN
               FichierResultat%Nom = nomFic
               Modele%FichierResultat%Nom = nomFic  
            else if ( index(typeFic, 'listing') > 0) THEN
               FichierListing%Nom = nomFic
               Modele%FichierListing%Nom = nomFic          
            end if
         end do
      endif

      Erreur%Numero = 0
      Erreur%arbredappel = 'IMPORTATION_MODELE'

!      write(12,*)'Avant nullify'
      nullify(Modele%Connect%OrigineBief)
      nullify(Modele%Connect%FinBief)
      nullify(Modele%Connect%NbBiefConfluence)
      nullify(Modele%Connect%NumBiefConfluence)
      nullify(Modele%Connect%NumSectionConfluence)
      nullify(Modele%Connect%NumBiefExtLibre)
      nullify(Modele%Connect%NumSectionExtLibre)
      nullify(Modele%Profils)
      nullify(Modele%SectionPlan%S)
      nullify(Modele%SectionPlan%S1)
      nullify(Modele%SectionPlan%S2)
      nullify(Modele%SectionPlan%SS)
      nullify(Modele%SectionPlan%CELER)
      nullify(Modele%SectionPlan%B)
      nullify(Modele%SectionPlan%INV)
      nullify(Modele%SectionPlan%INTE)
      nullify(Modele%SectionPlan%DYDX)
      nullify(Modele%SectionPlan%PRESS)
      nullify(Modele%SectionPlan%DEB)
      nullify(Modele%SectionPlan%DEB1)
      nullify(Modele%SectionPlan%DEB2)
      nullify(Modele%SectionPlan%SD)
      nullify(Modele%SectionPlan%SD1)
      nullify(Modele%SectionPlan%SD2)
      nullify(Modele%SectionPlan%PRESSD)
      nullify(Modele%SectionPlan%BD)
      nullify(Modele%SectionPlan%DEBD)
      nullify(Modele%ZonesSeches)
      nullify(Modele%ZonesFrottement)
      nullify(Modele%LoisHydrau)
      nullify(Modele%Singularites)
      nullify(Modele%Deversoirs)
      nullify(Modele%Liaisons)
      nullify(Modele%Extremites)
      nullify(Modele%Casiers)
      nullify(Modele%Confluents)
      nullify(Modele%Apports)
      nullify(Modele%ApportsPluie)

      nullify(Modele%ProfilPlan%B1)
      nullify(Modele%ProfilPlan%B2)
      nullify(Modele%ProfilPlan%BS)
      nullify(Modele%ProfilPlan%P1)
      nullify(Modele%ProfilPlan%P2)
      nullify(Modele%ProfilPlan%S1)
      nullify(Modele%ProfilPlan%S2)
      nullify(Modele%ProfilPlan%S2G)
      nullify(Modele%ProfilPlan%SS)
      nullify(Modele%ProfilPlan%C)
      nullify(Modele%ProfilPlan%Deb1)
      nullify(Modele%ProfilPlan%Deb2)
      nullify(Modele%ProfilPlan%Pr)
      nullify(Modele%ProfilPlan%Inv)
      nullify(Modele%ProfilPlan%S1D)
      nullify(Modele%ProfilPlan%S2D)
      nullify(Modele%ProfilPlan%SSD)
      nullify(Modele%ProfilPlan%PrD)
      nullify(Modele%ProfilPlan%BD)
      nullify(Modele%ProfilPlan%DebD)

      nullify(Modele%ZREF)
      nullify(Modele%RDC)
      nullify(Modele%RGC)
      nullify(Modele%CF2)
      nullify(Modele%CF1)
      nullify(Modele%PCSing)
      nullify(Modele%XDT)
      nullify(Modele%X)
      nullify(Modele%SectionStockage)
      nullify(Modele%Algorithme)
      nullify(Modele%IDT)
      nullify(Modele%DZ)
      nullify(Modele%XD)
      nullify(Modele%DZD)
      nullify(Modele%ProfDebBief)
      nullify(Modele%ProfFinBief)
      nullify(Modele%absc_rel_ext_deb_bief)
      nullify(Modele%absc_rel_ext_fin_bief)
      nullify(Modele%F1)

      call  PRETRAIT_INTERFACE                                   ( &

        Modele%VersionCode, Modele%Noyau                         , &
        FichierModele, FichierMotCle                             , &
        Modele%OptionCasier                                      , &
        Modele%OptionCourlis, FichierMotCleCourlis, FichierDicoCourlis , &
        Modele%OndeSubm                                          , &
        Modele%CalculValidation, Modele%TypeValidation           , &
        Modele%Regime, Modele%ModeleLit                          , &
        Modele%FrottParoiVerticale, Modele%PerteChargeConfluent  , &
        Modele%DebProgressifLM, Modele%DebProgressifZS           , &
        Modele%DZArriveeFront                                    , &
        Modele%FroudeLim, Modele%FrottementImplicite             , &
        Modele%ImplicitTrans, Modele%Opt                         , &
        Modele%PerteElargissementTrans                           , &
        Modele%Boussinesq, Modele%NoConvection, Modele%CQMV      , &
        Modele%ProfAbs, Modele%HEPS                              , &
        Modele%DT, Modele%TempsInitial, Modele%CritereArret      , &
        Modele%NbPasTemps, Modele%TempsMaximum                   , &
        Modele%Section_controle,Modele%Cote_max_controle         , &
        Modele%PasTempsVariable, Modele%CourantObj               , &
        FichierGeom, Modele%FormatGeom, Modele%Profils           , &
        Modele%PresenceZoneStockage                              , &
        Modele%X, Modele%IDT, Modele%XDT                         , &
        FichierMaillage, FichierSauveMaillage                    , &
        Modele%TypeMaillage                                      , &
        Modele%Connect                                           , &
        Modele%CF1, Modele%CF2, Modele%InterpolLinStrickler      , &
        Modele%LoiFrottement                                     , &
        Modele%RepriseCalcul                                     , &
        FichierRepriseEcr, FichierRepriseLec                     , &
        FichierLigne                                             , &
        Modele%ZonesSeches                                       , &
        Modele%ZonesFrottement                                   , &
        Modele%TitreCas                                          , &
        ImpressionPlani, Modele%ImpressionCalcul                 , &
        Modele%PasStockage, Modele%PasImpression                 , &
        Modele%PremierPasStocke                                  , &
        FichierResultat, Modele%FormatResu, FichierResultat2     , &
        Modele%FormatResu2                                       , &
        FichierListing                                           , &
        Modele%VarCalc, Modele%VarSto                            , &
        Modele%OptionStockage, Modele%SectionStockage            , &
        Modele%LoisHydrau, FichierLoiHydrau                      , &
        Modele%Barrage, Modele%Singularites, Modele%PCSing       , &
        Modele%Apports, Modele%Deversoirs                        , &
        Modele%Confluents, Modele%Extremites, Modele%Algorithme  , &
        Modele%Abaque                                            , &
        Modele%Casiers,             &  ! tableau des casiers
        Modele%Liaisons,            &  ! tableau des liaisons
        Modele%ApportsPluie,        &  ! tableau des apports de pluie
        Modele%ProfDebBief, Modele%ProfFinBief, Modele%absc_rel_ext_deb_bief,  Modele%absc_rel_ext_fin_bief, &
        FichierResultatCasier,      &  ! fichier des resultats des caracteristiques Casier
        FichierResultatLiaison,     &  ! fichier des resultats des caracteristiques Liaison
        FichierListingCasier ,&
        FichierListingLiaison,&
        FichierGeomCasier,          &
        Modele%decentrement,         &
        Erreur, &
        FichiersLois, Impression)

      if (Erreur%Numero /= 0) then
        RetourErreur = Erreur%Numero
        ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - PRETRAIT_INTERFACE - '//Erreur%Message
        if (Impression) then
          rewind(FichierListing%Unite)
          close(FichierListing%Unite)
        endif
        return
      endif

      !
      !  Tracer
      !  ------
      RetourErreur = NULLIFIER_MODELE_TRACER(Modele%Tracer, MessageErreurType)
      if (RetourErreur /= 0) then
          ptrMsgsErreurs(Identifiant) = 'Unable to nullify MODEL_MASCARET_T.TRACER'
          return
      endif

      call  PRETRAIT_TRACER_INTERFACE                       ( &
         FichierMotCle                                       , & ! Fichier des mots-cles
         Modele%Connect                                      , & ! Table de connectivite
         Modele%Apports                                      , & ! Apports hydrauliques
         Modele%Profils                                      , & ! Profils geometriques
         Modele%X                                            , & ! Abscisses des sections de calcul
         Modele%Extremites                                   , & ! Extremites libres
         Modele%TempsMaximum                                 , & ! Temps maximum du calcul
         ! Lecture des parametres de Tracer
         Modele%OptionTracer                                 , & ! Choix d'un calcul avec TRACER
         Modele%Tracer%Nbtrac                                , & ! Nombre de traceurs
         Modele%Tracer%ConsTrac                              , & ! Constantes pour TRACER
         Modele%Tracer%FreqCouplage                          , & ! Frequence de couplage hydraulique/tracer
         ! Conc init, CL, sources, lois tracer
         Modele%Tracer%Presence_ConcIni                      , & ! Presence conc init traceurs
         Modele%Tracer%CondLimTrac                           , & ! Conditions aux limites
         Modele%Tracer%Sources_tracer                        , & ! Sources pour le traceur
         Modele%Tracer%LoiTracer                             , & ! Lois Tracer (CL ou sources)
         FichiersLoisTracer                                  , & ! Fichiers loi Tracer
         ! Lecture des parametres de QE
         Modele%Tracer%Modele_Qual_Eau                       , & ! Modele de QE
         Modele%Tracer%ParPhy                                , & ! Parametres de modele de QE
         Modele%Tracer%Meteo                                 , & ! Donnees meteo
         FichierParphy                                       , & ! Fichier des parametres de QE
         FichierMeteo                                        , & ! Fichier meteo
         ! Impression des parametres et resultats
         Modele%Tracer%FichierResuTracer                     , & ! Fichier resultats
         Modele%Tracer%FormatResuTracer                      , &
         Modele%Tracer%FichierListingTracer                  , & ! Fichier listing
         Modele%Tracer%ImpressionConcListing                 , & ! Logique pour les impressions
         Modele%Tracer%ImpressionConcIni                     , & ! Logique pour les impressions
         Modele%Tracer%ImpressionBilanTracer                 , & ! Logique pour les impressions
         Modele%PasStockage                                  , & ! Pas de stockage  (hydraulique)
         Modele%PasImpression                                , & ! Pas d'impression (hydraulique)
         Impression                                          , & ! Switch d'impression haut niv
         ! Traitement des erreurs
         Erreur)

      Modele%Tracer%DT_Trac = Modele%DT * dble(Modele%Tracer%FreqCouplage)
      Modele%Tracer%VarStoTracer(:) = .false.

      if( Erreur%Numero /= 0 ) then
        RetourErreur = Erreur%Numero
        ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - PRETRAIT_TRACER - '//Erreur%Message
        if (Impression) then
           rewind(FichierListing%Unite)
           close(FichierListing%Unite)
        endif
        return
      endif


      ! PLANIMETRAGE
      !=============
      call  PLANIM                      ( &

                Modele%ProfilPlan       , & ! Profils planimetres
                Modele%Profils          , & ! Caracteristiques des profils
                Modele%F1               , & ! Fonction impulsion
                Modele%DebProgressifLM  , & ! Debordement progressif lit majeur
                Modele%DebProgressifZS  , & ! Debordement progressif zones de stockaage
                ImpressionPlani         , & ! Impression du planimetrage
                FichierListing%Unite    , & ! Unite logique listing
                Modele%FrottParoiVerticale , & ! Conservation du frottement sur les parois verticales
                Modele%OptionCourlis           , & ! Activation de Courlis
                varsed                  , & ! Courlis : profil evolution
                TempsInitial            , & ! Courlis
                Erreur                  & ! Erreur
                                    )
      if (Erreur%Numero /= 0) then
        RetourErreur = Erreur%Numero
        ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - PLANIM - '//Erreur%Message
        if (Impression) then
          rewind(FichierListing%Unite)
          close(FichierListing%Unite)
        endif
        return
      endif


     ! INTERPOLATIONS AUX SECTIONS DE CALCUL
     !======================================
     phase_intersect = PHASE_INITIALISATION

     call Intersect      ( &

          Modele%ZREF              , & ! Tableau des cotes de ref aux sections
          Modele%RGC               , & ! Cotes de la rive gauche         ''
          Modele%RDC               , & ! Cotes de la rive droite         ''
          Modele%CF1               , & ! Coefficient de frottement mineur
          Modele%CF2               , & ! Coefficient de frottement majeur
          Modele%Profils           , & ! Profils geometriques
          Modele%X                 , & ! Abscisses des sections de calcul
          Modele%IDT               , & ! Positionement des sections / profils
          Modele%XDT               , & ! Positionement des sections / profils
          Modele%Connect           , & ! Connectivite du reseau
          Modele%Extremites        , & ! Extremite libre
          Modele%TypeMaillage      , & ! Type de calcul du maillage
          ImpressionPlani   , & ! flag d'impression
          FichierListing%Unite, & !
          Modele%FormatGeom          , & ! Format du fichier geometrie utilise
          Modele%InterpolLinStrickler  , & ! Flag d'interpolation lineaire des Strickler
          phase_intersect     , & ! Phase de la simulation
          Erreur                & ! Erreur
                              )

       if (Erreur%Numero /= 0) then
         RetourErreur = Erreur%Numero
         ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - Intersect - '//Erreur%Message
         if (Impression) then
           rewind(FichierListing%Unite)
           close(FichierListing%Unite)
         endif
         return
     endif

     if (Modele%Noyau == NOYAU_MASCARET) then
       nb_pas = Modele%Profils(1)%NbPas

       call PLANMA          ( &
            Modele%SectionPlan          , & ! Section planimetrees
            Modele%Profils              , & ! Caracteritiques des profils
            Modele%ProfilPlan           , & ! Profils planimetrees
            nb_pas                      , & ! Nombre de pas de planimetrage
            Modele%X                    , & ! Abscisse des sections de calcul
            Modele%DZ                   , & ! Caracteristiques des sections
            Modele%XD                   , & ! Abscisse des interfaces
            Modele%DZD                  , & ! Pas de planimetrage des interfaces
            Modele%XDT                  , & ! Position relative de la section/Profil
            Modele%IDT                  , & ! Profil de donnees amont de la section
            Modele%Connect              , & ! Connectivite du reseau
            Modele%CF1                  , & ! Strickler mineur
            Modele%CF2                  , & ! Strickler majeur
            Modele%PresenceZoneStockage , & ! Presence de zone de stockage
            Modele%LoiFrottement        , & ! Loi de frottement utilisee
            Modele%OptionCourlis               , & ! Activation de Courlis
            varsed                      , & ! Courlis : profil evolution
            TempsInitial                , & ! Courlis
            Erreur               )

       if (Erreur%Numero /= 0) then
         RetourErreur = Erreur%Numero
         ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - PLANMA - '//Erreur%Message
         if (Impression) then
           rewind(FichierListing%Unite)
           close(FichierListing%Unite)
         endif
         return
       endif
     endif

     ptrTabMascaret(Identifiant)%ModeleMascaret = Modele

     if (Impression) then
       rewind(FichierListing%Unite)
       close(FichierListing%Unite)
       if (Modele%OptionCasier) then
           close(FichierListingCasier%Unite)
           close(FichierListingLiaison%Unite)
       endif
     endif

     if(associated(FichiersLois)) deallocate(FichiersLois)
     if(associated(FichiersLoisTracer)) deallocate(FichiersLoisTracer)

     return

end subroutine IMPORT_MODELE_MASCARET
