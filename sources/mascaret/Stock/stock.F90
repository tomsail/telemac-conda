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

subroutine STOCK ( X               , &
                   ZREF            , &
                   RGC  , RDC      , &
                   CF1  , CF2      , &
                   Z               , &
                   Q               , &
                   Q1   , Q2       , &
                   DebitFlux       , &
                   S1   , S2       , &
                   B1   , B2       , &
                   BS              , &
                   P1   , P2       , &
                   RH1  , RH2      , &
                   FR   , BETA     , &
                   TAUF            , &
                   Y    , HMOY     , &
                   Q2G  , Q2D      , &
                   VOL  , VOLS     , &
                   CHARG           , &
                   SS              , &
                   V1   , V2       , &
                   ZMAX , TZMAX    , &
                   VZMAX           , &
                   ZMIN , TZMIN    , &
                   V1MIN, V1MAX    , &
                   BMAX            , &
                   TOND            , &
                   QMAX , TQMAX    , &
                   EMAX            , &
                   YVRAI , QVRAI   , &
                   Qdeverse        , &
                   TEMPS           , &
! Modele
                   Connect         , &
                   Casier          , &
                   Liaison         , &
! Parametre
                   FichierResultat , &
                   FichierResultat2 ,&
                   FichierResultatCasier, &
                   FichierResultatLiaison,&
                   OptionSto       , &
                   FormatResu      , &
                   FormatResu2     , &
                   PhaseSimulation , &
                   PhaseStockCasier, &
                   NumeroPasTemps  , &
                   VarSto          , &
                   SectionSto      , &
                   FichierMotCle   , &
                   Erreur          )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P.CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!                             D. POIZAT
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!
!             STOCKAGE DES RESULTATS SUR FICHIER
!
!-----------------------------------------------------------------------
!                             VARIABLES LOCALES
! .______________________.____._______________________________________________
! !    NOM          !TYPE!MODE!                   ROLE
! !_________________!____!____!_______________________________________________
! ! retour          ! R  !    ! Variable contenant le code de retour de fonctions
! ! NbSectEff       ! R  !    ! Dimension de la liste des sections a stocker
! ! isec            ! R  !    ! Compteur sur les secteurs
! ! SectionStoEff   ! R  !    ! Numeros des sections effectives a sortir (suivant option)
! !_________________!____!____!_______________________________________________
!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :       FichierResultat
!   ----------------------
!
!   SOUS-PROGRAMME(S) APPELANT(S) :  Neant
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S) :    STO_OPTHYCA, STO_NONPER, STO_PER
!   ---------------------------      (sous-programmes internes)
!                                    INIT_VAR_SORTIE_S (sous-programme de module)
!   COMMENTAIRES :
!   ------------
!                     Comment rajouter une variable a stocker ?
!                     -----------------------------------------
!
!   Soit VARNEW le nom de la nouvelle variable a stocker.
!   A. Il faut modifier 1 ligne :
!      I. Dans le module M_INDEX_VARIABLE_C :
!         1. Rajouter 1 unite a la constante NB_TOT_VAR
!            qui represente le nombre total de variables stockables
!            `  integer , parameter :: NB_TOT_VAR = ...'
!   B. Il faut introduire 3 lignes :
!      I.  Dans le module M_INDEX_VARIABLE_C :
!          2. Introduire la constante definissant l'indice de la
!             nouvelle variable dans les tableaux de variables
!             `  integer , parameter :: VAR_VARNEW = ...'
!      II. Dans le sous-programme STOCK :
!          3. Definir l'initialisation des informations constantes
!             de cette nouvelle variable
!             a. Si son stokage est obligatoire
!             b. Son nom long
!             c. Son nom sur 4 caracteres
!             d. Le nom de son unite physique
!             e. La precision souhaitee pour sa valeur numerique
!             f. Si elle est dependante du temps
!             Par exemple :
!             `var_nom(VAR_VARNEW )=VAR_NOM_T(.false.,"Variable Nouvelle              ","VNEW","m/s   ",3,.true. )'
!          4. Definir ses valeurs dans la structure contenant les
!             informations dependantes de la simulation
!             `gdr(VAR_VARNEW)%Valeur     => ...'
!             ou l'objet pointe est le tableau des valeurs par section
!             de cette nouvelle variables.
!             (Bien entendu le tableau des valeurs par section devra
!              etre defini, par exemple en le passant en argument
!              de la routine STOCK)
!
!
!***********************************************************************

   !============================= Declarations ============================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_FICHIER_T           ! Definition du type FICHIER_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_CASIER_T            ! Definition du type CASIER_T
   use M_LIAISON_T           ! Definition du type LIAISON_T
   use M_CONSTANTES_CALCUL_C ! Constantes servant a reperer la phase de calcul
   use M_INDEX_VARIABLE_C    ! Constantes servant a reperer les variables
   use M_PARAMETRE_C         ! Epsilon pour la difference de 2 temps
   use M_INIT_VAR_SORTIE_S   ! Initialisation des structures des variables a sortir
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_TRAITER_ERREUR_CASIER_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur
   !.. Declarations explicites ..
   !-----------------------------
   implicit none
   !.. Arguments ..
   !---------------
   real(DOUBLE)   , dimension(:), pointer :: X
   real(DOUBLE)   , dimension(:), pointer :: ZREF
   real(DOUBLE)   , dimension(:), pointer :: RGC
   real(DOUBLE)   , dimension(:), pointer :: RDC
   real(DOUBLE)   , dimension(:), pointer :: CF1
   real(DOUBLE)   , dimension(:), pointer :: CF2
   real(DOUBLE)   , dimension(:), pointer :: Z
   real(DOUBLE)   , dimension(:), pointer :: Q
   real(DOUBLE)   , dimension(:), pointer :: Q1
   real(DOUBLE)   , dimension(:), pointer :: Q2
   real(DOUBLE)   , dimension(:), pointer :: S1
   real(DOUBLE)   , dimension(:), pointer :: S2
   real(DOUBLE)   , dimension(:), pointer :: B1
   real(DOUBLE)   , dimension(:), pointer :: B2
   real(DOUBLE)   , dimension(:), pointer :: BS
   real(DOUBLE)   , dimension(:), pointer :: P1
   real(DOUBLE)   , dimension(:), pointer :: P2
   real(DOUBLE)   , dimension(:), pointer :: RH1
   real(DOUBLE)   , dimension(:), pointer :: RH2
   real(DOUBLE)   , dimension(:), pointer :: FR
   real(DOUBLE)   , dimension(:), pointer :: BETA
   real(DOUBLE)   , dimension(:), pointer :: TAUF
   real(DOUBLE)   , dimension(:), pointer :: Y
   real(DOUBLE)   , dimension(:), pointer :: HMOY
   real(DOUBLE)   , dimension(:), pointer :: Q2G
   real(DOUBLE)   , dimension(:), pointer :: Q2D
   real(DOUBLE)   , dimension(:), pointer :: VOL
   real(DOUBLE)   , dimension(:), pointer :: VOLS
   real(DOUBLE)   , dimension(:), pointer :: CHARG
   real(DOUBLE)   , dimension(:), pointer :: SS
   real(DOUBLE)   , dimension(:), pointer :: V1
   real(DOUBLE)   , dimension(:), pointer :: V2
   real(DOUBLE)   , dimension(:), pointer :: ZMAX
   real(DOUBLE)   , dimension(:), pointer :: TZMAX
   real(DOUBLE)   , dimension(:), pointer :: VZMAX
   real(DOUBLE)   , dimension(:), pointer :: ZMIN
   real(DOUBLE)   , dimension(:), pointer :: TZMIN
   real(DOUBLE)   , dimension(:), pointer :: V1MIN
   real(DOUBLE)   , dimension(:), pointer :: V1MAX
   real(DOUBLE)   , dimension(:), pointer :: BMAX
   real(DOUBLE)   , dimension(:), pointer :: TOND
   real(DOUBLE)   , dimension(:), pointer :: QMAX
   real(DOUBLE)   , dimension(:), pointer :: TQMAX
   real(DOUBLE)   , dimension(:), pointer :: EMAX
   real (DOUBLE)  , dimension (:), pointer :: YVRAI
   real (DOUBLE)  , dimension (:), pointer :: QVRAI
   real (DOUBLE)  , dimension (:),pointer  :: Qdeverse
   real (DOUBLE)  , dimension (:),pointer  :: DebitFlux
   real(DOUBLE)            , intent(in   ) :: TEMPS
   ! Modele
   type(CONNECT_T),                       intent(in   ) :: Connect
   type(CASIER_T),  dimension(:), pointer :: Casier
   type(LIAISON_T), dimension(:), pointer :: Liaison
   ! Parametres
   type(FICHIER_T)                      , intent(in   ) :: FichierResultat
   type(FICHIER_T)                      , intent(in   ) :: FichierResultat2
   type(FICHIER_T)                      , intent(in   ) :: FichierResultatCasier
   type(FICHIER_T)                      , intent(in   ) :: FichierResultatLiaison
   type(FICHIER_T)                      , intent(in   ) :: FichierMotCle
   integer                              , intent(in   ) :: OptionSto
   integer                              , intent(in   ) :: FormatResu
   integer                              , intent(in   ) :: FormatResu2
   integer                              , intent(in   ) :: PhaseSimulation, NumeroPasTemps ,&
                                                          PhaseStockCasier
   logical        , dimension(:)        , intent(in   ) :: VarSto
   integer        , dimension(:)        , pointer       :: SectionSto
   type(ERREUR_T)                       , intent(inout) :: Erreur
   !.. Variables locales ..
   !-----------------------
   integer, dimension(:), pointer :: SectionStoEff => null() ! Numeros des sections a sortir
   integer                        :: retour
   integer                        :: NbSectEff
   integer                        :: isec
   !character(132)                 :: !arbredappel_old
   ! Declaration des 2 structures qui permettront de realiser des boucles
   ! sur les variables a stocker :
   ! 1. Declaration de la structure contenant toutes les informations constantes
   ! sur les variables a stocker.
   type(VAR_NOM_T), dimension(NB_TOT_VAR), save :: var_nom
   ! 2. Declaration de la structure contenant toutes les informations dependantes
   ! de la simulation sur les variables a stocker.
   type(GDR_STO_T), dimension(NB_TOT_VAR) :: gdr

   !============================== Instructions ==============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STOCK'

   call INIT_VAR_SORTIE_S( &
       var_nom     , &
       gdr         , &
       X           , &
       ZREF        , &
       RGC  , RDC  , &
       CF1  , CF2  , &
       Z           , &
       Q           , &
       Q1   , Q2   , &
       DebitFlux   , &
       S1   , S2   , &
       B1   , B2   , &
       BS          , &
       P1   , P2   , &
       RH1  , RH2  , &
       FR   , BETA , &
       TAUF        , &
       Y    , HMOY , &
       Q2G  , Q2D  , &
       VOL  , VOLS , &
       CHARG       , &
       SS          , &
       V1   , V2   , &
       ZMAX , TZMAX, &
       VZMAX       , &
       ZMIN , TZMIN, &
       V1MIN, V1MAX, &
       BMAX        , &
       TOND        , &
       QMAX , TQMAX, &
       EMAX        , &
       YVRAI , QVRAI , &
       Qdeverse      , &
       VarSto      , &
       PhaseSimulation &
       )

   ! Traitement du probleme spatial
   ! ------------------------------
   if( OptionSto == STOCKAGE_LISTE_SECTION ) then
      ! les sections a considerer sont celles du tableau donne en entree
      NbSectEff     = size(SectionSto)
      SectionStoEff => SectionSto
   else
      ! toutes les sections sont a considerer
      NbSectEff = size(X)

      if(.not.associated(SectionStoEff)) allocate( SectionStoEff(NbSectEff) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'SectionStoEff' )
         return
      end if

      do isec = 1 , NbSectEff
         SectionStoEff(isec) = isec
      end do

   end if

   ! APPEL AU SOUS PROGRAMME INTERNE DE STOCKAGE SUIVANT LE FORMAT CHOISI
   ! --------------------------------------------------------------------
   select case( FormatResu )

      case( FORMAT_STO_NONPERMANENT )

         call STO_NONPER(        &
              Connect          , &
              NbSectEff        , &
              var_nom          , &
              gdr              , &
              FichierResultat  , &
              PhaseSimulation  , &
              TEMPS            , &
              NumeroPasTemps   , &
              SectionStoEff    , &
              Erreur             &
              )

         if( Erreur%Numero /= 0 ) then
            return
         endif

         if( PhaseStockCasier == PHASE_CALCUL ) then

            call STO_CASIER(            &
                 Casier               , &
                 FichierResultatCasier, &
                 PhaseSimulation,       &
                 TEMPS,                 &
                 NumeroPasTemps,        &
                 Erreur                 &
                     )

            if( Erreur%Numero /= 0 ) then
               return
            endif

            call STO_LIAISON            (&
                 Liaison                ,&
                 FichierResultatLiaison ,&
                 PhaseSimulation        ,&
                 TEMPS                  ,&
                 NumeroPasTemps         ,&
                 Erreur                 &
             )

            if( Erreur%Numero /= 0 ) then
               return
            endif

         end if

      case( FORMAT_STO_OPTHYCA )

         call STO_OPTHYCA(       &
              Connect          , &
              NbSectEff        , &
              var_nom          , &
              gdr              , &
              FichierResultat  , &
              PhaseSimulation  , &
              TEMPS            , &
              SectionStoEff    , &
              Erreur             &
              )

         if( Erreur%Numero /= 0 ) then
            return
         endif

         if( PhaseStockCasier == PHASE_CALCUL ) then

            call STO_OPTHYCA_CASIER     (&
                   Casier               ,&
                   FichierResultatCasier,&
                   PhaseSimulation      ,&
                   TEMPS                ,&
                   Erreur               )

            if( Erreur%Numero /= 0 ) then
               return
            end if

            call STO_OPTHYCA_LIAISON    (&
                   Liaison              ,&
                  FichierResultatLiaison,&
                  PhaseSimulation       ,&
                  TEMPS                 ,&
                  Erreur                )

            if( Erreur%Numero /= 0 ) then
               return
            end if

         end if

      case( FORMAT_STO_PERMANENT )

         call STO_PER (          &
              Connect          , &
              NbSectEff        , &
              var_nom          , &
              gdr              , &
              FichierResultat  , &
              PhaseSimulation  , &
              SectionStoEff    , &
              Erreur             &
              )

         if( Erreur%Numero /= 0 ) then
            return
         endif

      case( FORMAT_STO_LIDOP )

         call STO_LIDOP  (       &
              NbSectEff        , &
              var_nom          , &
              gdr              , &
              FichierResultat  , &
              FichierMotCle    , &
              PhaseSimulation  , &
              TEMPS            , &
              SectionStoEff    , &
              Erreur             &
              )

      case DEFAULT

         if( Erreur%Numero /= 0 ) then
            return
         endif

   end select

   if( FormatResu2 /=0 ) then

      call STO_OPTHYCA(       &
           Connect          , &
           NbSectEff        , &
           var_nom          , &
           gdr              , &
           FichierResultat2 , &
           PhaseSimulation  , &
           TEMPS            , &
           SectionStoEff    , &
           Erreur            &
           )

   endif

   ! liberation des allocations internes
   if( OptionSto /= STOCKAGE_LISTE_SECTION ) then

      if(associated(SectionStoEff)) deallocate( SectionStoEff , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'SectionStoEff' )
         return
      end if

   endif

   !Erreur%arbredappel = !arbredappel_old

   return

contains

!***********************************************************************
!
!  SOUS-PROGRAMME INTERNE
!
!***********************************************************************

!          ===========
subroutine STO_OPTHYCA( &
!          ===========
       Connect          , & ! Structure contenant la table de connectivite
       NbSectEff        , & ! Nombre des sections pour le stockage
       var_nom          , & ! Infos     constantes sur les variables a stocker
       gdr              , & ! Infos non constantes sur les variables a stocker
       FichierResultat  , & ! Structure pour le fichier de stockage
       PhaseSimulation  , & ! Variable indiquant la phase de la simulation
       TEMPS            , & ! Temps
       SectionStoEff    , & ! Tableau contenant la liste des sections a sortir
       Erreur             & ! Erreur
       )
!***********************************************************************
!
!  FONCTION :   STOCKAGE SUR FICHIER AU FORMAT OPTHYCA
!  --------
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S) :   Neant
!  -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_NUM_BIEF_S          ! Calcul du bief relatif a une section
   !.. Declarations implicites ..
   implicit none
   !.. Arguments ..
   type(CONNECT_T),                       intent(in) :: Connect
   integer                              , intent(in) :: NbSectEff
   type(VAR_NOM_T), dimension(:)        , intent(in) :: var_nom
   type(GDR_STO_T), dimension(:), target, intent(in) :: gdr
   type(FICHIER_T)                      , intent(in) :: FichierResultat
   integer                              , intent(in) :: PhaseSimulation
   real(DOUBLE)                         , intent(in) :: TEMPS
   integer        , dimension(:)        , intent(in) :: SectionStoEff
   type(ERREUR_T)                       , intent(inout) :: Erreur
   !.. Constantes ..
   integer     , parameter :: LEN_PRECISION= 10 ! Precision sur 7 chiffres maxi
   !.. Variables locales ..
   character(LEN_PRECISION) :: fmt_precision    ! Variable contenant le format
   integer :: ul                             ! Unite logique du fichier de stockage
   integer :: retour                         ! Variable contenant le code de retour
                                             ! des fonctions d'E/S
   integer :: ivar                           ! Compteur sur les variables a sortir
   integer :: isec                           ! Compteur sur les sections
   integer :: num_sect                       ! Indice sur la liste des sections a stocker
   !character(132) :: !arbredappel_old         ! Arbre d'appel d'avant l'entree dans la procedure
   real(DOUBLE)   :: temps_precedent
   save temps_precedent

   !============================ Instructions ==============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STO_OPTHYCA'
   ul = FichierResultat%Unite

   if( PhaseSimulation == PHASE_INITIALISATION ) then
      open(unit=ul          , file=FichierResultat%Nom, access='SEQUENTIAL', &
           action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
           position='append', status='REPLACE'    )

   else
      open(unit=ul          , file=FichierResultat%Nom, access='SEQUENTIAL', &
           action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
           position='append', status='OLD'    )
   end if

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR( Erreur , FichierResultat%Nom )
      return
   end if

   ! ECRITURE DES DONNEES INITIALES
   ! ==============================
   if( PhaseSimulation == PHASE_INITIALISATION ) then

      write(ul,'(A)') "[variables]"

      do ivar = 2 , NB_TOT_VAR

         ! En commencant la boucle a 2 on exclut la definition de
         ! la variable X dont les valeurs numeriques seront toujours stockees

         if( gdr(ivar)%ASortir ) then

            write(ul,"('""',A,'""',';','""',A,'""',';','""',A,'""',';',I1)") &
                 trim(var_nom(ivar)%NomLong), &
                 trim(var_nom(ivar)%NomA4)  , &
                 trim(var_nom(ivar)%Unite)  , &
                 var_nom(ivar)%Precision

         end if

      end do

      write(ul,'(A)') "[resultats]"
      ! ecriture des donnees au temps initial
      do isec = 1 , NbSectEff

         num_sect = SectionStoEff(isec)

         write(ul,"(F12.1,';','""',i2,'""',';','""',i5,'""')",advance='NO') &
            TEMPS , NUM_BIEF_S(Connect, num_sect, Erreur) , num_sect

         do ivar = 1 , NB_TOT_VAR

            if( gdr(ivar)%ASortir ) then

               write(fmt_precision,'("("";""f",I2,".",I1,")")') &
                  var_nom(ivar)%Precision + 9 , &
                  var_nom(ivar)%Precision

               if(ivar==1) then
                  write(ul, fmt_precision,advance='NO') &
                     gdr(ivar)%Valeur(num_sect)
               else
                  write(ul, fmt_precision,advance='NO') &
                     REAL(gdr(ivar)%Valeur(num_sect), kind=SIMPLE)
               endif

            end if

         end do

         write(ul, *) ! pour le retour a la ligne

      end do

      temps_precedent = TEMPS

   end if

   ! ECRITURE DES DONNEES AU COURS DU TEMPS
   ! ======================================
   if( TEMPS >= temps_precedent + 0.01_DOUBLE ) then ! pour eviter les pas de temps trop rapproches

      do isec = 1 , NbSectEff

         num_sect = SectionStoEff(isec)

         write(ul,"(F12.1,';','""',i2,'""',';','""',i5,'""')",advance='NO') &
            TEMPS, NUM_BIEF_S(Connect, num_sect, Erreur), num_sect

         do ivar = 1 , NB_TOT_VAR

            if( gdr(ivar)%ASortir ) then

               write(fmt_precision,'("("";""f",I2,".",I1,")")') &
                  var_nom(ivar)%Precision + 9 , &
                  var_nom(ivar)%Precision

               if(ivar==1) then
                  write(ul, fmt_precision,advance='NO') &
                     gdr(ivar)%Valeur(num_sect)
               else
                  write(ul, fmt_precision,advance='NO') &
                     REAL(gdr(ivar)%Valeur(num_sect), kind=SIMPLE)
               endif

            end if

         end do

         write(ul,*) ! pour le retour a la ligne

      end do

   endif

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STO_OPTHYCA

!          =========
subroutine STO_LIDOP  ( &
!          =========

       NbSectEff        , & ! Nombre des sections pour le stockage
       var_nom          , & ! Infos     constantes sur les variables a stocker
       gdr              , & ! Infos non constantes sur les variables a stocker
       FichierResultat  , & ! Structure pour le fichier de stockage
       FichierMotCle    , &
       PhaseSimulation  , & ! Variable indiquant la phase de la simulation
       TEMPS            , &
       SectionStoEff    , & ! Tableau contenant la liste des sections a sortir
       Erreur             & ! Erreur
       )
!***********************************************************************
!
!  FONCTION :   STOCKAGE SUR FICHIER AU FORMAT OPTHYCA
!  --------
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S) :   DATE_S
!  -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_NUM_BIEF_S          ! Calcul du bief relatif a une section
   use M_DATE_S
   !.. Declarations implicites ..
   implicit none
   !.. Arguments ..
   integer                              , intent(in) :: NbSectEff
   type(VAR_NOM_T), dimension(:)        , intent(in) :: var_nom
   type(GDR_STO_T), dimension(:), target, intent(in) :: gdr
   type(FICHIER_T)                      , intent(in) :: FichierResultat
   type(FICHIER_T)                      , intent(in) :: FichierMotCle
   integer                              , intent(in) :: PhaseSimulation
   real(DOUBLE)                         , intent(in) :: TEMPS
   integer        , dimension(:)        , intent(in) :: SectionStoEff
   type(ERREUR_T)                       , intent(inout) :: Erreur
   !.. Constantes ..
   integer     , parameter :: LEN_PRECISION = 10 ! Precision sur 7 chiffres maxi
   !.. Variables locales ..
   character(LEN_PRECISION) :: fmt_precision     ! Variable contenant le format
   integer :: ul                                 ! Unite logique du fichier de stockage
   integer :: retour                             ! Variable contenant le code de retour
                                                 ! des fonctions d'E/S
   integer :: ivar, i9                           ! Compteur sur les variables a sortir
   integer :: isec                               ! Compteur sur les sections
   integer :: num_sect                           ! Indice sur la liste des sections a stocker
   !character(132) :: !arbredappel_old           ! Arbre d'appel d'avant l'entree dans la procedure
   character(33)  :: chaine_date                 ! Chaine contenant la date
   character :: tab

   !============================ Instructions ==============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STO_LIDOP'
   ul = FichierResultat%Unite

   tab = transfer(i9, tab)
   i9 = 9

   if( PhaseSimulation == PHASE_INITIALISATION ) then

       open(unit=ul          , file=FichierResultat%Nom, access='SEQUENTIAL', &
            action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
            position='append', status='REPLACE'    )

   else

       open(unit=ul          , file=FichierResultat%Nom, access='SEQUENTIAL', &
            action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
            position='append', status='OLD'    )

   end if

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR( Erreur , FichierResultat%Nom )
      return
   end if

   ! ECRITURE DES DONNEES INITIALES
   ! ==============================
   if( PhaseSimulation == PHASE_INITIALISATION ) then

      call DATE_S( chaine_date )

      write (ul, '(A)') trim(chaine_date)
      write (ul, '(2A)') "le fichier cas est :  ", FichierMotCle%nom
      write (ul,*)
      write(ul,'(A)') "[variables]"
      write(ul,'(A)') "Numero de la section de calcul : Section"
      write(ul,'(A)') "Numero de l'abscisse : Abscisse (m)"

      do ivar = 2 , NB_TOT_VAR

         ! En commencant la boucle a 2 on exclut la definition de
         ! la variable X dont les valeurs numeriques seront toujours stockees
         if( gdr(ivar)%ASortir ) then

             write(ul,"(A,' : ',A,' (',A,')')") &
                  trim(var_nom(ivar)%NomLong), &
                  trim(var_nom(ivar)%NomA4)  , &
                  trim(var_nom(ivar)%Unite)
         end if

      end do

   end if

   write(ul, *) ! pour le retour a la ligne

   ! ECRITURE DES DONNEES AU COURS DU TEMPS (et au pas de temps 0)
   ! ======================================
   if( PhaseSimulation /= PHASE_INITIALISATION ) then

      write(ul,'(A, i4, A)') "[ligne d'eau numero ", int(TEMPS), " ]"

      write(ul,'(A)', advance='NO') "Section", tab, "Abscisse", tab

      do ivar = 2 , NB_TOT_VAR

         if( gdr(ivar)%ASortir ) then

            write(ul,'(2A)', advance='NO') trim(var_nom(ivar)%NomA4) , tab

         end if

      end do

      write(ul,*)
      write(ul,*)

      do isec = 1 , NbSectEff

         num_sect = SectionStoEff(isec)

         write(ul,"(i4)", advance='NO') num_sect

         do ivar = 1 , NB_TOT_VAR

            if( gdr(ivar)%ASortir ) then

               write(fmt_precision,'("(f",I2,".",I1,")")') &
                     var_nom(ivar)%Precision + 6 , &
                     var_nom(ivar)%Precision
               write(ul,"(A)", advance='NO') tab
               write(ul, fmt_precision, advance='NO') &
                  REAL(gdr(ivar)%Valeur(num_sect), kind=SIMPLE)

            end if

         end do

         write(ul, *) ! pour le retour a la ligne

      end do

   end if

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STO_LIDOP

!***********************************************************************
!
!  SOUS-PROGRAMME INTERNE
!
!***********************************************************************
!          =======
subroutine STO_PER (    &
!          =======
       Connect          , & ! Structure contenant la table de connectivite
       NbSectEff        , & ! Nombre des sections pour le stockage
       var_nom          , & ! Infos constantes sur les variables a stocker
       gdr              , & ! Infos non constantes sur les variables a stocker
       FichierResultat  , & ! Structure pour le fichier de stockage
       PhaseSimulation  , & ! Variable indiquant la phase de la simulation
       SectionStoEff    , & ! Tableau contenant la liste des sections a sortir
       Erreur             & ! Erreur
       )
!***********************************************************************
!
!  FONCTION :  STOCKAGE SUR FICHIER AU FORMAT `LIDO PERMANENT'
!  --------
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S)   : DATE_S (module)
!  -------------------------
!
!  DOCUMENTATION EXTERNE :
!  ---------------------
!   NOTE D'UTILISATION   :
!   NOTE DE PRINCIPE     :
!
!***********************************************************************

   !============================= Declarations ===========================
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_DATE_S              ! Calcul et ecriture de la date
   !.. Declarations explicites ..
   implicit none
   !.. Arguments ..
   type(CONNECT_T),                       intent(in) :: Connect
   integer                              , intent(in) :: NbSectEff
   type(VAR_NOM_T), dimension(:)        , intent(in) :: var_nom
   type(GDR_STO_T), dimension(:)        , intent(in) :: gdr
   type(FICHIER_T)                      , intent(in) :: FichierResultat
   integer                              , intent(in) :: PhaseSimulation
   integer        , dimension(:)        , intent(in) :: SectionStoEff
   type(ERREUR_T)                       , intent(inout) :: Erreur
   !.. Constantes ..
   character(LEN=3), parameter :: NOM_FIN       = "FIN"
   integer         , parameter :: LEN_PRECISION = 8  ! Precision sur 7 chiffres maxi
   !.. Variables locales ..
   character(LEN_PRECISION) :: fmt_precision ! Variable contenant le format
   character(LEN=72), dimension(3) :: TITRE
   integer :: ul                             ! Unite logique du fichier de stockage
   integer :: retour                         ! Variable contenant le code de retour
   integer :: nb_bief                        ! Nombre de biefs
                                             ! des fonctions d'E/S
   integer :: ivar                           ! Compteur sur les variables a sortir
   integer :: isec                           ! Compteur sur les val des variables a sortir
   integer :: ibief                          ! Compteur sur les biefs
   !character(132) :: !arbredappel_old       ! Arbre d'appel d'avant l'entree dans la procedure
   character(33)  :: chaine_date             ! Chaine contenant la date

   !============================ Instructions ==============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STO_PER'

   TITRE(1) = 'RESULTATS CALCUL, '
   TITRE(2) = 'FICHIER RESULTAT MASCARET'
   TITRE(3) = '-----------------------------------------------------------------------'

   ul = FichierResultat%Unite

   nb_bief = size(Connect%OrigineBief)

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      open(unit=ul          , file=FichierResultat%Nom  , access='SEQUENTIAL', &
           action='WRITE'   , form='FORMATTED'  , iostat=RETOUR      , &
           position='append', status='REPLACE'      )

      if( RETOUR /= 0 ) then
         Erreur%Numero = 4
         Erreur%ft     = err_4
         Erreur%ft_c   = err_4c
         call TRAITER_ERREUR( Erreur , FichierResultat%Nom )
         return
      end if

   else

      open(unit=ul           , file=FichierResultat%Nom  , access='SEQUENTIAL', &
           action='WRITE'    , form='FORMATTED'  , iostat=RETOUR      , &
           position='append', status='OLD'      )


      if( RETOUR /= 0 ) then
         Erreur%Numero = 4
         Erreur%ft     = err_4
         Erreur%ft_c   = err_4c
         call TRAITER_ERREUR( Erreur , FichierResultat%Nom )
         return
      end if

      ! ECRITURE DES VARIABLES
      ! ======================

      ! TITRE
      call DATE_S( chaine_date )

      write(ul, '(2A)') trim(TITRE(1)) , trim(chaine_date)
      write(ul, '(A72)') TITRE(2)
      write(ul, '(A72)') TITRE(3)

      ! VARIABLES BIEF
      write(ul,'(" IMAX  =", I5, " NBBIEF=", I5)') NbSectEff , nb_bief
      write(ul,'(" I1,I2 =",10I5)') ( Connect%OrigineBief(ibief) , Connect%FinBief(ibief) , ibief = 1 , nb_bief )

      ! NOMS ET VALEURS DES VARIABLES
      do ivar = 1 , NB_TOT_VAR

         if( gdr(ivar)%ASortir ) then

            write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
            write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
            write(ul, fmt_precision) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)

         end if

      end do

      write(ul,'(1X,A)') NOM_FIN

   end if

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STO_PER

!***********************************************************************
!
!  SOUS-PROGRAMME INTERNE
!
!***********************************************************************
!          ==========
subroutine STO_NONPER ( &
!          ==========
       Connect          , & ! Indices des sections des biefs
       NbSectEff        , & ! Nombre des sections pour le stockage
       var_nom          , & ! Infos constantes     sur les variables a stocker
       gdr              , & ! Infos non constantes sur les variables a stocker
       FichierResultat  , & ! Structure pour le fichier de stockage
       PhaseSimulation  , & ! Variable indiquant la phase de la simulation
       TEMPS            , & ! Temps
       NumeroPasTemps   , & ! Numero du pas de temps en cours
       SectionStoEff    , & ! Tableau contenant la liste des sections a sortir
       Erreur             & ! Erreur
                          )

!***********************************************************************
!
!  FONCTION :  STOCKAGE SUR FICHIER AU FORMAT `LIDO NON PERMANENT'
!  --------
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S)   : Neant
!  -------------------------
!
!***********************************************************************

   !============================= Declarations ===========================
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur
   !.. Declarations explicites ..
   implicit none
   !.. Arguments ..
   type(CONNECT_T),                       intent(in) :: Connect
   integer                              , intent(in) :: NbSectEff
   type(VAR_NOM_T), dimension(:)        , intent(in) :: var_nom
   type(GDR_STO_T), dimension(:)        , intent(in) :: gdr
   type(FICHIER_T)                      , intent(in) :: FichierResultat
   integer                              , intent(in) :: PhaseSimulation, NumeroPasTemps
   real(DOUBLE)                         , intent(in) :: TEMPS
   integer        , dimension(:)        , intent(in) :: SectionStoEff
   type(ERREUR_T)                       , intent(inout) :: Erreur
   !.. Constantes ..
   character(LEN=4), parameter :: NOM_FIN       = "FIN "
   character(LEN=4), parameter :: NOM_IDEB_BIEF = "I1  "
   character(LEN=4), parameter :: NOM_IFIN_BIEF = "I2  "
   !.. Variables locales ..
   character(LEN=72), dimension(3) :: TITRE
   integer :: ul                           ! Unite logique du fichier de stockage
   integer :: retour                       ! Variable contenant le code de retour
                                           ! des fonctions d'E/S
   integer :: nb_bief                      ! Nombre de bief
   integer :: ivar                         ! Compteur sur les variables a sortir
   integer :: isec                         ! Compteur sur les valeurs des var a sortir
   integer :: ibief                        ! Compteur sur les biefs
   !character(132) :: !arbredappel_old     ! Arbre d'appel d'avant l'entree dans la procedure

   !============================ Instructions ==============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STO_NONPER'

   TITRE(1) = ' '
   TITRE(2) = ' '
   TITRE(3) = ' '

   ul = FichierResultat%Unite

   nb_bief = size(Connect%OrigineBief)

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      open(unit=ul          , file=FichierResultat%Nom  , access='SEQUENTIAL', &
           action='WRITE'   , form='UNFORMATTED', iostat=RETOUR      , &
           position='append', status='REPLACE'      )

   else            ! PhaseSimulation == PHASE_CALCUL

      open(unit=ul           , file=FichierResultat%Nom  , access='SEQUENTIAL', &
           action='WRITE'    , form='UNFORMATTED', iostat=RETOUR      , &
           position='append', status='OLD'      )

   end if

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR( Erreur , FichierResultat%Nom )
      return
   end if

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      ! ECRITURE DES DONNEES INITIALES
      ! ------------------------------
      ! TITRE

      write (ul) TITRE(1)
      write (ul) TITRE(2)
      write (ul) TITRE(3)

      ! VARIABLES INITIALES : indices origine et fin de bief

      write (ul) NOM_IDEB_BIEF
      write (ul) NOM_IFIN_BIEF
      write (ul) NOM_FIN
      write (ul) nb_bief, nb_bief
      write (ul) (Connect%OrigineBief(ibief), ibief = 1, nb_bief)
      write (ul) (Connect%FinBief(ibief), ibief = 1, nb_bief)

      ! DONNEES INDEPENDANTES DU TEMPS
      ! ------------------------------

      ! NOMS DES VARIABLES

      do ivar = 1 , NB_TOT_VAR

         if( gdr(ivar)%ASortir .and. .not. var_nom(ivar)%DependantTemps ) then

            write(ul) var_nom(ivar)%NomA4

         end if

      end do

      write(ul) NOM_FIN

      ! VALEURS DES VARIABLES INDEPENDANTES DU TEMPS

      write (ul) NbSectEff, NbSectEff

      do ivar = 1 , NB_TOT_VAR

         if( gdr(ivar)%ASortir .and. .not. var_nom(ivar)%DependantTemps ) then

            write(ul) (REAL(gdr(ivar)%Valeur(SectionStoEff(isec)),kind=SIMPLE), &
                  isec = 1, NbSectEff)

         end if

      end do

      ! NOMS DES VARIABLES DEPENDANTES DU TEMPS

      do ivar = 1 , NB_TOT_VAR

         if( gdr(ivar)%ASortir .and. var_nom(ivar)%DependantTemps ) then

            write(ul) var_nom(ivar)%NomA4

         end if

      end do

      write(ul) NOM_FIN

   end if            ! de IF Phase_Simul

   ! ECRITURE DES DONNEES DEPENDANTES DU TEMPS
   ! -----------------------------------------
   write(ul) NumeroPasTemps, NumeroPasTemps
   write(ul) real(TEMPS,kind=SIMPLE), real(TEMPS,kind=SIMPLE)
   write(ul) NbSectEff       , NbSectEff

   do ivar = 1 , NB_TOT_VAR

      if( gdr(ivar)%ASortir .and. var_nom(ivar)%DependantTemps ) then

         write(ul) (real(gdr(ivar)%Valeur(SectionStoEff(isec)),kind=SIMPLE), &
               isec = 1, NbSectEff)

      end if

   end do

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STO_NONPER

! ======================================================================
subroutine STO_CASIER   (&
                    Casier          ,&
                    FichierResultat ,&
                    PhaseSimulation ,&
                    TEMPS           ,&
                    NumeroPasTemps  ,&
                    Erreur            )

!***********************************************************************
!
!  FONCTION :  STOCKAGE SUR FICHIER AU FORMAT `LIDO NON PERMANENT'
!  --------    DES RESULTATS PROPRES A CASIER
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S)   : Neant
!  -------------------------
!
!***********************************************************************

   !============================= Declarations ===========================
   use M_CASIER_T
   use M_ERREUR_T
   use M_TRAITER_ERREUR_CASIER_I
   use M_MESSAGE_CASIER_C
   use M_FICHIER_T
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C

   implicit none
   !.. Arguments ..
   type(CASIER_T), dimension(:), pointer :: Casier
   type(ERREUR_T),               intent(inout) :: Erreur
   real(DOUBLE),                 intent(in   ) :: TEMPS
   integer,                      intent(in   ) :: PhaseSimulation, NumeroPasTemps
   type(FICHIER_T),              intent(in   ) :: FichierResultat
   !.. Constantes ..
   character(LEN=4), parameter :: NOM_FIN       = "FIN "
   character(LEN=4), parameter :: NOM_IDEB_BIEF = "I1  "
   character(LEN=4), parameter :: NOM_IFIN_BIEF = "I2  "
   !.. Variables locales ..
   character(LEN=72), dimension(3) :: TITRE
   !character(132) :: !arbredappel_old
   integer :: ul, nb_casier, retour, icasier

   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STO_CASIER'

   TITRE(1) = ' '
   TITRE(2) = ' '
   TITRE(3) = ' '

   ul = FichierResultat%Unite

   nb_casier = size(Casier)

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      open(unit=ul          , file=FichierResultat%Nom  , access='SEQUENTIAL', &
           action='WRITE'   , form='UNFORMATTED', iostat=RETOUR      , &
           position='append', status='REPLACE'      )

   else            ! PhaseSimulation == PHASE_CALCUL

      open(unit=ul           , file=FichierResultat%Nom  , access='SEQUENTIAL', &
           action='WRITE'    , form='UNFORMATTED', iostat=RETOUR      , &
           position='append', status='OLD'      )

   end if

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR_CASIER( Erreur , FichierResultat%Nom )
      return
   end if

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      ! ECRITURE DES DONNEES INITIALES
      ! ------------------------------

      ! TITRE

      write (ul) TITRE(1)
      write (ul) TITRE(2)
      write (ul) TITRE(3)

      ! pas de variable initiale

      write (ul) NOM_IDEB_BIEF
      write (ul) NOM_IFIN_BIEF
      write (ul) NOM_FIN

      write(ul) 1, 1

      write(ul) 1
      write(ul) nb_casier

      ! DONNEES INDEPENDANTES DU PAS DE TEMPS
      ! -------------------------------------

      write (ul) 'X   '
      write (ul) 'ZFON'
      write (ul) NOM_FIN

      write (ul) nb_casier, nb_casier
      write(ul) (REAL(icasier, kind = SIMPLE), icasier = 1, nb_casier)
      write(ul) (REAL(Casier(icasier)%CoteFond, kind = SIMPLE), &
                 icasier = 1, nb_casier)

      ! NOMS DES VARIABLES DEPENDANTES DU TEMPS

      write (ul) 'SURF'
      write (ul) 'VOLU'
      write (ul) 'COTE'
      write (ul) NOM_FIN

   end if

   ! ECRITURE DES DONNEES DEPENDANTES DU TEMPS
   ! -----------------------------------------

   write(ul) NumeroPasTemps, NumeroPasTemps
   write(ul) real(TEMPS,kind=SIMPLE), real(TEMPS,kind=SIMPLE)

   write(ul) nb_casier, nb_casier

   write(ul) (REAL(Casier(icasier)%Surface, kind = SIMPLE), &
                 icasier = 1, nb_casier)
   write(ul) (REAL(Casier(icasier)%Volume, kind = SIMPLE), &
                 icasier = 1, nb_casier)
   write(ul) (REAL(Casier(icasier)%Cote, kind = SIMPLE), &
                 icasier = 1, nb_casier)

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STO_CASIER

!=============================================================================
subroutine STO_LIAISON   (           &
                    Liaison         ,&
                    FichierResultat ,&
                    PhaseSimulation ,&
                    TEMPS           ,&
                    NumeroPasTemps , &
                    Erreur            )

!***********************************************************************
!
!  FONCTION :  STOCKAGE SUR FICHIER AU FORMAT `LIDO NON PERMANENT'
!  --------    DES RESULTATS PROPRES A CASIER
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S)   : Neant
!  -------------------------
!
!***********************************************************************

   !============================= Declarations ===========================
   use M_LIAISON_T
   use M_ERREUR_T
   use M_TRAITER_ERREUR_CASIER_I
   use M_MESSAGE_CASIER_C
   use M_FICHIER_T
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C

   implicit none
   !.. Arguments ..
   type(LIAISON_T), dimension(:), pointer :: Liaison
   type(ERREUR_T),               intent(inout) :: Erreur
   real(DOUBLE),                 intent(in   ) :: TEMPS
   integer,                      intent(in   ) :: PhaseSimulation, NumeroPasTemps
   type(FICHIER_T),              intent(in   ) :: FichierResultat
   !.. Variables locales ..
   !character(132) :: !arbredappel_old
   character(LEN=72), dimension(3) :: TITRE
   integer :: ul, nb_liaison, retour, iliaison

   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STO_CASIER'

   TITRE(1) = ' '
   TITRE(2) = ' '
   TITRE(3) = ' '

   ul         = FichierResultat%Unite

   nb_liaison = size(Liaison)

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      open(unit=ul          , file=FichierResultat%Nom  , access='SEQUENTIAL', &
           action='WRITE'   , form='UNFORMATTED', iostat=RETOUR      , &
           position='append', status='REPLACE'      )

   else            ! PhaseSimulation == PHASE_CALCUL

      open(unit=ul           , file=FichierResultat%Nom  , access='SEQUENTIAL', &
           action='WRITE'    , form='UNFORMATTED', iostat=RETOUR      , &
           position='append', status='OLD'      )

   end if

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft   = err_4
      Erreur%ft_c = err_4c
      call TRAITER_ERREUR_CASIER (Erreur, FichierResultat%Nom)
      return
   end if

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      ! ECRITURE DES DONNEES INITIALES
      ! ------------------------------

      ! TITRE

      write (ul) TITRE(1)
      write (ul) TITRE(2)
      write (ul) TITRE(3)

      ! pas de variable initiale

      write (ul) 'I1  '
      write (ul) 'I2  '
      write (ul) 'FIN '

      write(ul) 1, 1

      write(ul) 1
      write(ul) nb_liaison

      ! DONNEES INDEPENDANTES DU PAS DE TEMPS
      ! -------------------------------------

      write (ul) 'X   '
      !     write (ul) 'TYPE'
      !     write (ul) 'NATURE'
      !     write (ul) 'LARGEUR'
      !     write (ul) 'LONGUEUR'
      !     write (ul) 'COTE'
      !     write (ul) 'SECTION'
      !     write (ul) 'RUGOSITE'
      !     write (ul) 'COEFFICIENT DE DEBIT'
      !     write (ul) 'COEFFICIENT PERTE DE CHARGE'
      !     write (ul) 'COEFFICIENT NOYE/DENOYE'
      write (ul) 'FIN '

      write (ul) nb_liaison, nb_liaison
      write (ul) (REAL(iliaison, kind = SIMPLE), iliaison = 1 , nb_liaison )
      !    write(ul) (Liaison(iliaison)%TypeLiaison, iliaison = 1, nb_liaison)
      !    write(ul) (Liaison(iliaison)%NatureLiaison, iliaison = 1, nb_liaison)
      !    write(ul) (Liaison(iliaison)%Largeur, iliaison = 1, nb_liaison)
      !    write(ul) (Liaison(iliaison)%Longueur, iliaison = 1, nb_liaison)
      !    write(ul) (Liaison(iliaison)%Cote, iliaison = 1, nb_liaison)
      !    write(ul) (Liaison(iliaison)%Section, iliaison = 1, nb_liaison)
      !    write(ul) (Liaison(iliaison)%Rugosite, iliaison = 1, nb_liaison)
      !    write(ul) (Liaison(iliaison)%CoefDebit, iliaison = 1, nb_liaison)
      !    write(ul) (Liaison(iliaison)%CoefPerteCharge, iliaison = 1, nb_liaison)
      !    write(ul) (Liaison(iliaison)%CoefNoye, iliaison = 1, nb_liaison)

      ! NOMS DES VARIABLES DEPENDANTES DU TEMPS

      write (ul) 'QECH'
      write (ul) 'VECH'
      write (ul) 'FIN '

   end if

   ! ECRITURE DES DONNEES DEPENDANTES DU TEMPS
   ! -----------------------------------------
   write(ul) NumeroPasTemps, NumeroPasTemps
   write(ul) real(TEMPS,kind=SIMPLE), real(TEMPS,kind=SIMPLE)

   write(ul) nb_liaison, nb_liaison

   write(ul) (REAL(Liaison(iliaison)%DebitEchange, kind = SIMPLE), &
                 iliaison = 1, nb_liaison)
   write(ul) (REAL(Liaison(iliaison)%VitesseEchange, kind = SIMPLE), &
                 iliaison = 1, nb_liaison)

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STO_LIAISON

!===========================================================================
subroutine STO_OPTHYCA_CASIER          (&
                        Casier         ,&
                        FichierResultat,&
                        PhaseSimulation,&
                        TEMPS          ,&
                        Erreur         )

!***********************************************************************
!
!  FONCTION :   STOCKAGE SUR FICHIER AU FORMAT OPTHYCA DES CARACTERISTIQUES
!  --------     DES CASIERS
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S) :   Neant
!  -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION
   use M_CASIER_T
   use M_ERREUR_T
   use M_FICHIER_T
   use M_CONSTANTES_CALCUL_C
   use M_MESSAGE_CASIER_C
   use M_TRAITER_ERREUR_CASIER_I

   implicit none
   !.. Arguments ..
   type(CASIER_T), dimension(:), pointer       :: Casier
   type(ERREUR_T),               intent(inout) :: Erreur
   type(FICHIER_T),              intent(in   ) :: FichierResultat
   integer,                      intent(in   ) :: PhaseSimulation
   real(DOUBLE),                 intent(in   ) :: TEMPS
   !.. Variables locales
   !character(132) :: !arbredappel_old
   integer :: ul, retour
   integer :: icasier, nb_casier

   !============================ Instructions ==============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STO_OPTHYCA_CASIER'

   ul = FichierResultat%Unite

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      open(unit=ul          , file=FichierResultat%Nom, access='SEQUENTIAL', &
           action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
           position='append', status='REPLACE'    )

   else

      open(unit=ul          , file=FichierResultat%Nom, access='SEQUENTIAL', &
           action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
           position='append', status='OLD'    )

   end if

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR_CASIER( Erreur , FichierResultat%Nom )
      return
   end if

   ! ECRITURE DES DONNEES INITIALES
   ! ==============================
   if( PhaseSimulation == PHASE_INITIALISATION ) then

        write(ul,'(A)') "/* resultats casiers"
        write(ul,'(A)') "[variables]"

        write(ul,'(A)') '"cote surface libre casier";"ZCAS";"m";2'
        write(ul,'(A)') '"surface casier";"SURCAS";"m2";2'
        write(ul,'(A)') '"volume casier";"VOLCAS";"m3";2'

        write(ul,'(A)') "[resultats]"

   end if

   ! ECRITURE DES DONNEES AU COURS DU TEMPS (et au pas de temps 0)
   ! ======================================

   nb_casier = size(Casier)

   do icasier = 1 , nb_casier

      write (ul,"(f11.0,';','""CAS',i3,'""',';',f8.2,';',f12.2,';',f12.2)") &
        TEMPS, icasier, Casier(icasier)%Cote, Casier(icasier)%Surface, Casier(icasier)%Volume

   end do

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STO_OPTHYCA_CASIER

!=============================================================================================
subroutine STO_OPTHYCA_LIAISON          (&
                        Liaison         ,&
                        FichierResultat ,&
                        PhaseSimulation ,&
                        TEMPS           ,&
                        Erreur         )

!***********************************************************************
!
!  FONCTION :   STOCKAGE SUR FICHIER AU FORMAT OPTHYCA DES CARACTERISTIQUES
!  --------     DES LIAISONS
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S) :   Neant
!  -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION
   use M_LIAISON_T
   use M_ERREUR_T
   use M_FICHIER_T
   use M_CONSTANTES_CALCUL_C
   use M_CONSTANTES_CASIER_C
   use M_MESSAGE_CASIER_C
   use M_TRAITER_ERREUR_CASIER_I

   implicit none
   !.. Arguments ..
   type(LIAISON_T), dimension(:), pointer      :: Liaison
   type(ERREUR_T),               intent(inout) :: Erreur
   type(FICHIER_T),              intent(in   ) :: FichierResultat
   integer,                      intent(in   ) :: PhaseSimulation
   real(DOUBLE),                 intent(in   ) :: TEMPS
   !.. Variables locales
   !character(132) :: !arbredappel_old
   integer :: ul, retour
   integer :: iliaison, nb_liaison

   !============================ Instructions ==============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STO_OPTHYCA_CASIER'

   ul = FichierResultat%Unite

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      open(unit=ul          , file=FichierResultat%Nom, access='SEQUENTIAL', &
           action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
           position='append', status='REPLACE'    )

   else

      open(unit=ul          , file=FichierResultat%Nom, access='SEQUENTIAL', &
           action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
           position='append', status='OLD'    )

   end if

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR_CASIER( Erreur , FichierResultat%Nom )
      return
   end if

   ! ECRITURE DES DONNEES INITIALES
   ! ==============================
   if( PhaseSimulation == PHASE_INITIALISATION ) then
      write(ul,'(A)') "/* resultats liaisons"
      write(ul,'(A)') "[variables]"

      write(ul,'(A)') '"debit echange";"QECH";"m3/s";3'
      write(ul,'(A)') '"vitesse echange";"VECH";"m/s";3'

      write(ul,'(A)') "[resultats]"

   end if

   ! ECRITURE DES DONNEES AU COURS DU TEMPS (et au pas de temps 0)
   ! ======================================
   nb_liaison = size(Liaison)

   do iliaison = 1 , nb_liaison

      if( Liaison(iliaison)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then

         write (ul,"(f11.0,';','""NUM',i4,'/RIV-CAS""',';',f10.3,';',f10.3)") &
         TEMPS, iliaison, Liaison(iliaison)%DebitEchange, Liaison(iliaison)%VitesseEchange

      else

         write (ul,"(f11.0,';','""NUM',i4,'/CAS-CAS""',';',f10.3,';',f10.3)") &
         TEMPS, iliaison, Liaison(iliaison)%DebitEchange, Liaison(iliaison)%VitesseEchange

      end if

   end do

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STO_OPTHYCA_LIAISON

end subroutine STOCK
