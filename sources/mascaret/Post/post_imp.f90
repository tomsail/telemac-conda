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

subroutine POST_IMP( &
                     ! Donnees
              X , ZREF , & ! Maillage et cotes de reference
             RGC , RDC , & ! Rives gauche et droite
             CF1 , CF2 , & ! Strickler mineur et majeur
      Z , QT , Q1 , Q2 , & ! Cote debits mineur et majeur
             DebitFlux , & ! Debit Flux
               S1 , S2 , & ! Sections mineur et majeur
          B1 , B2 , BS , & ! Largeurs au miroir mineur, majeur et de stockage
               P1 , P2 , & ! Perimetres mouillees mineur et majeur
             RH1 , RH2 , & ! Rayons hydrauliques mineur et majeur
             FR , BETA , & ! Froude et BETA
                  TAUF , & ! Contrainte au fond
              Y , HMOY , & ! Hauteur d'eau et hauteur d'eau moyenne
             Q2G , Q2D , & ! Debits majeur droit et gauche
            VOL , VOLS , & ! Volumes lit actif et zone de stockage
            CHARG , SS , & ! Charge et Section de stockage
               V1 , V2 , & ! Vitesse mineur et majeur
          ZMAX , TZMAX , & ! Cote max et temps associe
                 VZMAX , & ! Vitesse a la cote max
          ZMIN , TZMIN , & ! Cote min et temps associe
         V1MIN , V1MAX , & ! Vitesse mineur min et max
                  BMAX , & ! Largeur au miroir max
                  TOND , & ! Temps d'arrivee de l'onde
          QMAX , TQMAX , & ! Debit max et temps associe
                  EMAX , & ! Energie maximale
         YVRAI , QVRAI , & ! Solutions analytiques
              Qdeverse , & ! Debit deverse
                 Temps , & ! Temps courant
                Apport , & ! Debits d'Apports
       PasTempsOptimal , & ! Pas de temps optimal
                     ! Modele
               Connect , & ! Connectivite du reseau
             ModeleLit , & ! Modele du lit (Debord/Crugos)
                     ! Parametres
             Iteration , & ! Numero du pas de temps
                 Noyau , & ! Noyau de calcul utilise
       PhaseSimulation , & ! Phase Initialisation/Calcul
      ImpressionCalcul , & ! impression complete du calcul
                Regime , & ! Regime Permanent / Non Permanent
                VarImp , & ! Variables a imprimer
          UniteListing , & ! Unite logique ficheier listing
                     ! Etats
        TempsPrecedent , & ! Temps precedent
          VolBiefActif , VolBiefStockage , & ! Volumes actifs et de stockage
            QAmontPrec , QAvalPrec       , & ! Debits amont et aval des biefs
                Erreur   & ! Erreur
                       )
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION :   IMPRESSION DES VALEURS DES VARIABLES SUR LISTING
!  --------
!----------------------------------------------------------------------
!
!  FICHIERS ENTREE/SORTIE : Listing (unite logique : UniteListing)
!  ----------------------
!
!
!  SOUS-PROGRAMME(S) APPELANT(S) : - SUPERVISEUR
!  -----------------------------
!  SOUS-PROGRAMME(S) APPELE(S)   : - CONVERSION_TEMPS  (interne)
!  ---------------------------     - INIT_VAR_SORTIE_S (module)
!
!  COMMENTAIRES :
!  ------------
!
!  DOCUMENTATION EXTERNE :
!  ---------------------
!
!***********************************************************************

!============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C ! Constantes
   use M_INDEX_VARIABLE_C    ! Constantes servant a reperer les variables
   use M_PARAMETRE_C         ! GPES, TETA
   use M_MESSAGE_C           ! Messages d'erreur
   use M_APPORT_T            ! Definition du type APPORT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_INIT_VAR_SORTIE_S   ! Initialisation des structures des variables a sortir
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Declarations Explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------

   ! Donnees
   real(DOUBLE)   , dimension(:), pointer  :: X
   real(DOUBLE)   , dimension(:), pointer  :: ZREF
   real(DOUBLE)   , dimension(:), pointer  :: RGC
   real(DOUBLE)   , dimension(:), pointer  :: RDC
   real(DOUBLE)   , dimension(:), pointer  :: CF1
   real(DOUBLE)   , dimension(:), pointer  :: CF2
   real(DOUBLE)   , dimension(:), pointer  :: Z
   real(DOUBLE)   , dimension(:), pointer   :: QT
   real(DOUBLE)   , dimension(:), pointer  :: Q1
   real(DOUBLE)   , dimension(:), pointer  :: Q2
   real(DOUBLE)   , dimension(:), pointer  :: DebitFlux
   real(DOUBLE)   , dimension(:), pointer  :: S1
   real(DOUBLE)   , dimension(:), pointer  :: S2
   real(DOUBLE)   , dimension(:), pointer  :: B1
   real(DOUBLE)   , dimension(:), pointer  :: B2
   real(DOUBLE)   , dimension(:), pointer  :: BS
   real(DOUBLE)   , dimension(:), pointer  :: P1
   real(DOUBLE)   , dimension(:), pointer  :: P2
   real(DOUBLE)   , dimension(:), pointer  :: RH1
   real(DOUBLE)   , dimension(:), pointer  :: RH2
   real(DOUBLE)   , dimension(:), pointer  :: FR
   real(DOUBLE)   , dimension(:), pointer  :: BETA
   real(DOUBLE)   , dimension(:), pointer  :: TAUF
   real(DOUBLE)   , dimension(:), pointer  :: Y
   real(DOUBLE)   , dimension(:), pointer  :: HMOY
   real(DOUBLE)   , dimension(:), pointer  :: Q2G
   real(DOUBLE)   , dimension(:), pointer  :: Q2D
   real(DOUBLE)   , dimension(:), pointer  :: VOL
   real(DOUBLE)   , dimension(:), pointer  :: VOLS
   real(DOUBLE)   , dimension(:), pointer  :: CHARG
   real(DOUBLE)   , dimension(:), pointer  :: SS
   real(DOUBLE)   , dimension(:), pointer  :: V1
   real(DOUBLE)   , dimension(:), pointer  :: V2
   real(DOUBLE)   , dimension(:), pointer  :: ZMAX
   real(DOUBLE)   , dimension(:), pointer  :: TZMAX
   real(DOUBLE)   , dimension(:), pointer  :: VZMAX
   real(DOUBLE)   , dimension(:), pointer  :: ZMIN
   real(DOUBLE)   , dimension(:), pointer  :: TZMIN
   real(DOUBLE)   , dimension(:), pointer  :: V1MIN
   real(DOUBLE)   , dimension(:), pointer  :: V1MAX
   real(DOUBLE)   , dimension(:), pointer  :: BMAX
   real(DOUBLE)   , dimension(:), pointer  :: TOND
   real(DOUBLE)   , dimension(:), pointer  :: QMAX
   real(DOUBLE)   , dimension(:), pointer  :: TQMAX
   real(DOUBLE)   , dimension(:), pointer  :: EMAX
   real(DOUBLE)   , dimension(:), pointer  :: YVRAI
   real(DOUBLE)   , dimension(:), pointer  :: QVRAI
   real(DOUBLE)   , dimension(:), pointer  :: Qdeverse
   real(DOUBLE)                 , intent(in   ) :: Temps
   type(APPORT_T) , dimension(:), intent(in   ) :: Apport
   real(DOUBLE)                 , intent(in   ) :: PasTempsOptimal
   ! Modele
   type(CONNECT_T)              , intent(in   ) :: Connect
   integer                      , intent(in   ) :: ModeleLit
   ! Parametres
   integer                      , intent(in   ) :: Iteration
   integer                      , intent(in   ) :: Noyau
   integer                      , intent(in   ) :: PhaseSimulation
   integer                      , intent(in   ) :: Regime
   logical        , dimension(:), intent(in   ) :: VarImp
   integer                      , intent(in   ) :: UniteListing
   Logical                      , intent(in   ) :: ImpressionCalcul
   ! Etats
   real(DOUBLE)                 , intent(inout) :: TempsPrecedent
   real(DOUBLE)   , dimension(:), pointer       :: VolBiefActif
   real(DOUBLE)   , dimension(:), pointer       :: VolBiefStockage
   real(DOUBLE)   , dimension(:), pointer       :: QAmontPrec
   real(DOUBLE)   , dimension(:), pointer       :: QAvalPrec
   type(ERREUR_T)               , intent(inout) :: Erreur
   !.. Constantes ..
   !----------------
   ! Nombre de colonnes de valeurs pour le listing
   integer     , parameter :: NB_COLONNES = 14
   !.. Variables locales ..
   !-----------------------
   real(DOUBLE), dimension(size(X))                   :: Q ! Debit total
   real(DOUBLE), dimension(size(Connect%OrigineBief)) :: delta_q_amont
                                                      ! DQ ext amont du bief
   real(DOUBLE), dimension(size(Connect%OrigineBief)) :: delta_q_aval
                                                      ! DQ ext aval du bief
   integer      :: retour      ! Code de retour des fonctions intrinseques
   integer      :: jour
   integer      :: heure
   integer      :: minute
   integer      :: seconde
   integer      :: nb_var_lst  ! Nombre de variables a sortir sur listing
   integer      :: n_sect_deb  ! Numero de la premiere section du bief
   integer      :: n_sect_fin  ! Numero de la derniere section du bief
   integer      :: nb_var_obl  ! Nombre de variables a sortir obligatoirement
   integer      :: nb_tab      ! Nombre de tableaux d'impression
   integer      :: nb_col      ! Nombre de colonnes par tableau
   integer      :: decal       ! Decalage d'indice pour le tableau de variable
   real(DOUBLE) :: vol_apporte        ! Volume apporte dans un bief pendant un pas de temps
   real(DOUBLE) :: vol_bief_precedent ! Volume du bief au temps precedent
   real(DOUBLE) :: vol_bief_total     ! Volume total du bief
   real(DOUBLE) :: vol_controle       ! Volume de controle
   real(DOUBLE) :: erreur_volume      ! Erreur sur le volume entre 2 pas de temps
   integer      :: sec_deb            ! Section debut de bief
   integer      :: sec_fin            ! Section fin de bief
   integer      :: dim                ! dimensionnement de pointeurs
   real(DOUBLE) :: DT                 ! Pas de temps
   ! Variables locales contenant les formats d'impression
   !-----------------------------------------------------
   character(9+5*NB_TOT_VAR) :: fmt_var_obl ! Formats des variables obligatoires
   character(9+5*NB_TOT_VAR) :: fmt_ligne   ! Format final d'une ligne a imprimer
   character(8), dimension(NB_TOT_VAR) :: fmt_var ! Format pour chaque variable
   ! Stockage local des noms et valeurs des variables a sortir
   !----------------------------------------------------------
   character(LEN=5)                                     :: nom_var_lst_int
   character(LEN=5), dimension(NB_TOT_VAR)              :: nom_var_lst
   integer         , dimension(:)         , allocatable :: val_var_lst_int
   real(DOUBLE)    , dimension(:,:)       , allocatable :: val_var_lst
   ! Declaration des 2 structures qui permettront de realiser des boucles
   ! sur les variables a stocker :
   !---------------------------------------------------------------------
   ! 1. Declaration de la structure contenant toutes les informations constantes
   ! sur les variables a stocker.
   !........................     declaration du tableau de structure var_nom ...
   !                                                                           !
   type(VAR_NOM_T), dimension(NB_TOT_VAR), save :: var_nom                     !
   !                                                                           !
   !........................ fin declaration du tableau de structure var_nom ..!
   ! 2. Declaration de la structure contenant toutes les informations dependantes
   ! de la simulation sur les variables a stocker.
   !........................     declaration du tableau de structure gdr .......
   !                                                                           !
   type(GDR_STO_T), dimension(NB_TOT_VAR) :: gdr                               !
   !                                                                           !
   !........................ fin declaration du tableau de structure gdr ......!
   ! Compteurs
   !----------
   integer        :: ivar  ! Compteur sur les variables
   integer        :: itab  ! Compteur sur les tableaux a imprimer
   integer        :: ibief ! Compteur sur les biefs
   integer        :: isec  ! Compteur sur les sections d'un bief
   integer        :: iapp  ! Compteur sur les apports
   !character(132) :: !arbredappel_old

   !============================== Instructions ===============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>POST_IMP'

   DT = Temps - TempsPrecedent

   !=============================================================================
   !                        Debut des impressions
   !=============================================================================
   write(UniteListing, &
       '("1"/" RESULTATS DU MODELE ",A6," EN NP = ",I6, &
       & " T = ", F12.1/1X,55("-")/)') &
       NOM_NOYAU(Noyau), Iteration, TEMPS

   if( Regime == REGIME_NON_PERMANENT ) then
      call CONVERSION_TEMPS( jour , heure , minute , seconde , Temps )
      write(UniteListing, &
          '(" DT = ",F10.2  ,/, &
          & " JOUR = ",I4," HEURE = ",I3," MINUTE = ",I3," SECONDE = ",I3,/)') &
          DT, jour, heure, minute, seconde
   endif
   If( ImpressionCalcul ) then
      ! Appel au sous-programme modulaire declarant les types et
      ! definissant les structures pour les variables a sortir
      !----------------------------------------------------------
      call INIT_VAR_SORTIE_S( &
           var_nom          , &
           gdr              , &
           X                , &
           ZREF             , &
           RGC  , RDC       , &
           CF1  , CF2       , &
           Z                , &
           QT               , &
           Q1   , Q2        , &
           DebitFlux        , &
           S1   , S2        , &
           B1   , B2        , &
           BS               , &
           P1   , P2        , &
           RH1  , RH2       , &
           FR   , BETA      , &
           TAUF             , &
           Y    , HMOY      , &
           Q2G  , Q2D       , &
           VOL  , VOLS      , &
           CHARG            , &
           SS               , &
           V1   , V2        , &
           ZMAX , TZMAX     , &
           VZMAX            , &
           ZMIN , TZMIN     , &
           V1MIN, V1MAX     , &
           BMAX             , &
           TOND             , &
           QMAX , TQMAX     , &
           EMAX             , &
           YVRAI, QVRAI     , &
           Qdeverse         , &
           VarImp           , &
           PhaseSimulation    &
                         )

      do isec = 1 , size(X)
         Q(isec) = Q1(isec) + Q2(isec)
      end do

!      if( PhaseSimulation == PHASE_INITIALISATION ) then
         ! Allocation de tableaux "Etats" de la fonctionnalite
         !----------------------------------------------------
         dim =  size(Connect%OrigineBief)
         if(.not.associated(QAmontPrec)) then
            allocate( QAmontPrec(dim) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'QAmontPrec' )
               return
            end if
         endif

         if(.not.associated(QAvalPrec)) then
            allocate( QAvalPrec(dim) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'QAvalPrec' )
               return
            end if
         endif

         if(.not.associated(VolBiefActif)) then
            allocate( VolBiefActif(dim) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft   = err_5
               Erreur%ft_c = err_5c
               call TRAITER_ERREUR( Erreur , 'VolBiefActif' )
              return
            end if
         endif

         if(.not.associated(VolBiefStockage)) then
            allocate( VolBiefStockage(dim) , STAT = retour )
            if( retour /= 0 ) then
              Erreur%Numero = 5
              Erreur%ft     = err_5
              Erreur%ft_c   = err_5c
              call TRAITER_ERREUR( Erreur , 'VolBiefStockage' )
              return
            end if
         endif

      if( PhaseSimulation == PHASE_INITIALISATION ) then
         if( ModeleLit == MODELE_LIT_FOND_BERGE ) then
            ! Redefinition de quelques noms de variables
            var_nom(VAR_Q1)%NomA4  = "QF  "
            var_nom(VAR_Q2)%NomA4  = "QB  "
            var_nom(VAR_S1)%NomA4  = "SF  "
            var_nom(VAR_S2)%NomA4  = "SB  "
            var_nom(VAR_B1)%NomA4  = "BF  "
            var_nom(VAR_B2)%NomA4  = "BB  "
            var_nom(VAR_RH1)%NomA4 = "RF  "
            var_nom(VAR_RH2)%NomA4 = "RB  "
            var_nom(VAR_V1)%NomA4  = "VF  "
            var_nom(VAR_V2)%NomA4  = "VB  "
         end if
      end if

      !=============================================================================
      !                      Impressions des valeurs numeriques
      !=============================================================================
      ! La premiere colonne d'impression contient les numeros des sections
      !--------------------------------------------------------------------
      nom_var_lst_int = "  I  "
      fmt_var_obl     = "(1X,I4,"
      ! On determine le nombre de variables a imprimer obligatoirement
      ! et on construit les formats pour les variables obligatoires
      !----------------------------------------------------------------
      ! Initialisation des compteurs de variables
      nb_var_obl = 0
      nb_var_lst = 0
      do ivar = 1 , NB_TOT_VAR
         if( var_nom(ivar)%Obligatoire ) then
            ! On comptabilise le nombre de variables a imprimer obligatoirement
            nb_var_obl = nb_var_obl + 1
            if( ivar.eq.1 ) then
               ! On construit le format d'impression des abscisses
               write(fmt_var(ivar), '("F",I2,".",I1,",")') &
                    merge(var_nom(ivar)%Precision + 9, 11, &
                       var_nom(ivar)%Precision + 9 < 12) , &
                       var_nom(ivar)%Precision
               fmt_var_obl = trim(fmt_var_obl)//fmt_var(ivar)
            else
               ! On construit le format d'impression des variables obligatoires
               write(fmt_var(ivar), '("F",I1,".",I1,",")') &
                     merge(var_nom(ivar)%Precision + 7, 9, &
                       var_nom(ivar)%Precision + 7 < 10) , &
                       var_nom(ivar)%Precision
               fmt_var_obl = trim(fmt_var_obl)//fmt_var(ivar)
            endif
            ! On comptabilise le nombre de variables a imprimer
            nb_var_lst = nb_var_lst + 1
            ! On place les variables obligatoires au debut de la liste
            !  des variables a imprimer
            nom_var_lst(nb_var_lst) = var_nom(ivar)%NomA4
         end if
      end do

      ! Calcul du nombre de tableaux
      ! sachant qu'on dispose de NB_COLONNES colonnes pour imprimer les valeurs
      !------------------------------------------------------------------------
      nb_tab = int(((nb_var_lst-nb_var_obl) / ( NB_COLONNES-nb_var_obl - 1 ))) + 1

      ! On imprime les valeurs des variables successivement pour chaque bief
      !----------------------------------------------------------------------
      label_bief: do ibief = 1 , size(Connect%OrigineBief)
         ! On repere les numeros de debut et de fin de section du bief
         n_sect_deb = Connect%OrigineBief(ibief)
         n_sect_fin = Connect%FinBief(ibief)

         write(UniteListing, &
              '(/" BIEF NUMERO ",I2,5X," I1 = ",I4," I2 = ",I4,/1X,39("=")/)') &
               ibief, n_sect_deb, n_sect_fin
         ! On alloue sur le nombre de sections que contient le bief
         ! les tableaux qui contiendront les noms et les valeurs
         ! des variables a imprimer
         if(.not.allocated(val_var_lst_int)) then
            allocate( val_var_lst_int( n_sect_fin - n_sect_deb + 1 ) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft   = err_5
               Erreur%ft_c = err_5c
               call TRAITER_ERREUR( Erreur , 'val_var_lst_int' )
               return
            end if
         endif

         if(.not.allocated(val_var_lst)) then
            allocate( val_var_lst( NB_TOT_VAR , n_sect_fin - n_sect_deb + 1 ) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'val_var_lst' )
               return
            end if
         endif

         ! On stocke les numeros des sections qui apparaitront en 1ere colonne
         do isec = n_sect_deb , n_sect_fin
            val_var_lst_int( isec - n_sect_deb + 1 ) = isec
         end do

         ! On stocke pour toutes les variables a imprimer
         ! leurs valeurs continument dans le meme tableau
         ! En premier, les variables obligatoires, puis les autres
         nb_var_lst = 0
         do ivar = 1 , NB_TOT_VAR
            if( var_nom(ivar)%Obligatoire ) then
               nb_var_lst = nb_var_lst + 1
               val_var_lst(nb_var_lst,:) = gdr(ivar)%Valeur(n_sect_deb:n_sect_fin)
            end if
         end do

         ! On imprime les valeurs en tableaux de NB_COLONNES sur le listing
        label_tab: do itab = 1 , nb_tab
            ! Calcul du nombre de colonnes pour les variables non obligatoires
            if( itab.eq.nb_tab ) then
               nb_col = mod( nb_var_lst - nb_var_obl , NB_COLONNES - nb_var_obl - 1 )
               if( nb_col .eq. 0 ) then
                  nb_col = NB_COLONNES - nb_var_obl - 1
               end if
            else
               nb_col = NB_COLONNES - nb_var_obl - 1
            end if

            ! Declage d'indice pour la recuperation des valeurs des variables
            ! non obligatoires dans le tableau contigue des variables
            decal = ( itab - 1 ) * ( NB_COLONNES - nb_var_obl - 1 )

            ! Impressions de l'entete donnant le nom abrege des variables
            nb_col = 0
            write(UniteListing, *)
            write(UniteListing, *)
            write(fmt_ligne, '("(A4,",I2,"A9)")') nb_col + nb_var_obl
            write(UniteListing, fmt_ligne) nom_var_lst_int, &
                 (nom_var_lst(ivar), ivar = 1, nb_var_obl), &
                 (nom_var_lst(ivar + nb_var_obl + decal), ivar = 1, nb_col)

            ! Impressions des valeurs numeriques pour chaque section du bief
            fmt_ligne = trim(fmt_var_obl)//")"
            do isec = n_sect_deb , n_sect_fin
               write(UniteListing, fmt_ligne) val_var_lst_int(isec - n_sect_deb + 1), &
                    (val_var_lst(ivar, isec - n_sect_deb + 1), ivar = 1, nb_var_obl)
            end do
            write(UniteListing, *)
         end do label_tab

         if(allocated(val_var_lst_int)) then
            deallocate( val_var_lst_int , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 6
               Erreur%ft     = err_6
               Erreur%ft_c   = err_6c
               call TRAITER_ERREUR( Erreur , 'val_var_lst_int' )
               return
            end if
         endif

         if(allocated(val_var_lst)) then
            deallocate(val_var_lst, STAT = retour)
            if( retour /= 0 ) then
               Erreur%Numero = 6
               Erreur%ft     = err_6
               Erreur%ft_c   = err_6c
               call TRAITER_ERREUR( Erreur , 'val_var_lst' )
               return
            end if
         endif

         !=============================================================================
         !   Impressions de la conservation du volume (dans la boucle sur les biefs)
         !=============================================================================
         if( ( noyau  == NOYAU_REZODT ).or.( Noyau == NOYAU_SARAP ) ) then
            vol_apporte = 0._DOUBLE
            sec_deb     = Connect%OrigineBief(ibief)
            sec_fin     = Connect%FinBief(ibief)
            if( PhaseSimulation == PHASE_INITIALISATION ) then
               QAmontPrec(ibief) = Q(Connect%OrigineBief(ibief))
               QAvalPrec(ibief)  = Q(Connect%FinBief    (ibief))

               ! Calcul du volume du bief
               !-------------------------
               VolBiefActif   (ibief) = 0._DOUBLE
               VolBiefStockage(ibief) = 0._DOUBLE

               do isec = sec_deb + 1 , sec_fin
                 VolBiefActif   (ibief) = VolBiefActif   (ibief) + vol (isec)
                  VolBiefStockage(ibief) = VolBiefStockage(ibief) + vols(isec)
               enddo
            endif

            ! Calcul des delta Q
            !-------------------
            delta_q_amont(ibief) = Q(sec_deb) - QAmontPrec(ibief)
            delta_q_aval (ibief) = Q(sec_fin) - QAvalPrec (ibief)

            ! Mise en memoire des debits
            !---------------------------
            QAmontPrec(ibief) = Q(Connect%OrigineBief(ibief))
            QAvalPrec (ibief) = Q(Connect%FinBief(ibief))

            if( Regime == REGIME_NON_PERMANENT ) then
               vol_bief_precedent = VolBiefActif(ibief) + VolBiefStockage(ibief)
               ! Calcul du volume apporte
               !-------------------------
               vol_controle      = Q(Connect%OrigineBief(ibief)) -     &
                                Q(Connect%FinBief(ibief)) -            &
                                (1. - TETA) *                          &
                                (delta_q_amont(ibief) -                &
                                delta_q_aval(ibief)                   )

               ! Ajout du volume des apports sur ce bief
               !----------------------------------------
               do iapp = 1 , size(Apport)
                  if( Apport(iapp)%SectionAm >= Connect%OrigineBief(ibief) .and. &
                     Apport(iapp)%SectionAv <= Connect%FinBief(ibief) ) then
                     vol_controle = vol_controle + Apport(iapp)%Debit
                  endif
               end do
               vol_apporte = vol_apporte + vol_controle * DT
            endif

            ! Calcul du volume du bief
            !-------------------------
            VolBiefActif   (ibief) = 0._DOUBLE
            VolBiefStockage(ibief) = 0._DOUBLE

            do isec = sec_deb + 1, sec_fin
               VolBiefActif   (ibief) = VolBiefActif   (ibief) + vol (isec)
               VolBiefStockage(ibief) = VolBiefStockage(ibief) + vols(isec)
            end do

            ! Impressions
            !------------
            vol_bief_total = VolBiefActif(ibief) + VolBiefStockage(ibief)

            if( Regime == REGIME_PERMANENT ) then
               write(UniteListing, 10000) VolBiefActif(ibief) , VolBiefStockage(ibief) , vol_bief_total
               10000   FORMAT(/' BILAN VOLUMIQUE DU BIEF :'                 &
                              /'------------------------  '                 &
                              /' VOLUME DANS LE LIT ACTIF   = ',F12.0,' M3' &
                              /' VOLUME STOCKE              = ',F12.0,' M3' &
                              /' VOLUME TOTAL               = ',F12.0,' M3' &
                              /)
            else if( Regime == REGIME_NON_PERMANENT ) then
               vol_apporte   = vol_controle * DT
               erreur_volume = vol_bief_total - vol_bief_precedent - vol_apporte
               write(UniteListing, 10010) VolBiefActif(ibief), VolBiefStockage(ibief), &
                            vol_bief_total, vol_apporte, erreur_volume
               10010   FORMAT(/' BILAN VOLUMIQUE DU BIEF :'                           &
                              /'------------------------  '                           &
                              /' VOLUME DANS LE LIT ACTIF             = ',F12.0,' M3' &
                              /' VOLUME DANS LES ZONES DE STOCKAGE    = ',F12.0,' M3' &
                              /' VOLUME TOTAL                         = ',F12.0,' M3' &
                              /' VOLUME APPORTE       ENTRE T-DT ET T = ',F12.0,' M3' &
                              /' ERREUR SUR LE VOLUME ENTRE T-DT ET T = ',F12.0,' M3' &
                              /)
            endif
         endif
      end do label_bief

      TempsPrecedent = Temps

     endif

   ! Fin des traitements
   !--------------------
   !Erreur%arbredappel = !arbredappel_old
   return

contains

   !***********************************************************************
   !
   !  SOUS-PROGRAMME INTERNE
   !
   !***********************************************************************

   subroutine CONVERSION_TEMPS ( &
                        jour   , &
                        heure  , &
                        minute , &
                        seconde, &
                        Temps    &
                               )
   !***********************************************************************
   !
   !  FONCTION :   CONVERSION DU TEMPS EN JOURS,HEURES,MINUTES ET SECONDES
   !  --------
   !
   !  SOUS PROGRAMMES APPELANT(S) : POST_IMP
   !  ---------------------------
   !  SOUS PROGRAMMES APPELE(S) :   Neant
   !  -------------------------
   !***********************************************************************

   !============================= Declarations ===========================

   use M_FICHIER_T

   !.. Arguments ..
   integer      , intent(out) :: jour
   integer      , intent(out) :: heure
   integer      , intent(out) :: minute
   integer      , intent(out) :: seconde
   real(DOUBLE) , intent(in ) :: Temps
   !.. Constantes ..
   real(DOUBLE) , parameter   :: NB_HEURE_PAR_JOUR     = 24._DOUBLE
   real(DOUBLE) , parameter   :: NB_SECONDE_PAR_MINUTE = 60._DOUBLE
   real(DOUBLE) , parameter   :: NB_SECONDE_PAR_HEURE  = 60._DOUBLE * NB_SECONDE_PAR_MINUTE
   !.. Variables locales ..
   real(DOUBLE) :: reste

   !============================ Instructions ==============================
   jour    = int(Temps / NB_SECONDE_PAR_HEURE / NB_HEURE_PAR_JOUR)
   reste   = Temps - jour * NB_SECONDE_PAR_HEURE * NB_HEURE_PAR_JOUR
   heure   = int(reste / NB_SECONDE_PAR_HEURE)
   reste   = reste - heure * NB_SECONDE_PAR_HEURE
   minute  = int(reste / NB_SECONDE_PAR_MINUTE)
   seconde = int(reste - minute * NB_SECONDE_PAR_MINUTE)

   return

   end subroutine CONVERSION_TEMPS

end subroutine POST_IMP
