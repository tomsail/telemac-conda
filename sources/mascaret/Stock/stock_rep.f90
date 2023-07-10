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

subroutine STOCK_REP ( X               , &
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
                       ZVRAI , QVRAI   , &
                       XFRON           , &
                       Qdev            , &
                       TEMPS           , &
! Modele
                       Connect         , &
! Parametre
                       FichierResultat , &
                       VarSto          , &
                       SectionSto      , &
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
!   DOCUMENTATION EXTERNE :
!***********************************************************************

   !============================= Declarations ============================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_FICHIER_T           ! Definition du type FICHIER_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_CONSTANTES_CALCUL_C ! Constantes servant a reperer la phase de calcul
   use M_INDEX_VARIABLE_C    ! Constantes servant a reperer les variables
   use M_PARAMETRE_C         ! Epsilon pour la difference de 2 temps
   use M_INIT_VAR_SORTIE_S   ! Initialisation des structures des variables a sortir
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
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
   real(DOUBLE)   , dimension(:), pointer :: ZVRAI
   real(DOUBLE)   , dimension(:), pointer :: QVRAI
   real(DOUBLE)   , dimension(:), pointer :: XFRON
   real(DOUBLE)   , dimension(:), pointer :: Qdev
   real(DOUBLE)   , dimension(:), pointer :: DebitFlux
   real(DOUBLE)           , intent(in   ) :: TEMPS
   ! Modele
   type(CONNECT_T),                       intent(in   ) :: Connect
   ! Parametres
   type(FICHIER_T)                      , intent(in   ) :: FichierResultat
   logical        , dimension(:)        , intent(in   ) :: VarSto
   integer        , dimension(:)        , pointer       :: SectionSto
   type(ERREUR_T)                       , intent(inout) :: Erreur
   !.. Variables locales ..
   !-----------------------
   integer, dimension(:), pointer :: SectionStoEff => null() ! Numeros des sections a sortir
   integer                        :: retour
   integer                        :: NbSectEff
   integer                        :: isec
   integer                        :: PhaseSimulation
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

   PhaseSimulation = 1

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
       ZVRAI , QVRAI , &
       Qdev          , &
       VarSto      , &
       PhaseSimulation &
       )

   ! Traitement du probleme spatial
   ! ------------------------------

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

   ! APPEL AU SOUS PROGRAMME INTERNE DE STOCKAGE SUIVANT LE FORMAT CHOISI
   ! --------------------------------------------------------------------
   call STO_PER_REP (          &
            Connect          , &
            NbSectEff        , &
            var_nom          , &
            gdr              , &
            FichierResultat  , &
            PhaseSimulation  , &
            SectionStoEff    , &
            Temps            , &
            XFRON            , &
            Erreur             &
             )

   if( Erreur%Numero /= 0 ) then
      return
   endif

   if(associated(SectionStoEff)) deallocate(SectionStoEff)
   !Erreur%arbredappel = !arbredappel_old

   return

contains

!***********************************************************************
!
!  SOUS-PROGRAMME INTERNE
!
!***********************************************************************
!          ===========
subroutine STO_PER_REP (  &
!          ===========
       Connect          , & ! Structure contenant la table de connectivite
       NbSectEff        , & ! Nombre des sections pour le stockage
       var_nom          , & ! Infos     constantes sur les variables a stocker
       gdr              , & ! Infos non constantes sur les variables a stocker
       FichierResultat  , & ! Structure pour le fichier de stockage
       PhaseSimulation  , & ! Variable indiquant la phase de la simulation
       SectionStoEff    , & ! Tableau contenant la liste des sections a sortir
       Temps            , &
       XFRON            , &
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
   real(DOUBLE)                         , intent(in   ) :: TEMPS
   real(DOUBLE)   , dimension(:), pointer :: XFRON
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
   !character(132) :: !arbredappel_old         ! Arbre d'appel d'avant l'entree dans la procedure
   character(33)  :: chaine_date             ! Chaine contenant la date

   !============================ Instructions ==============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   retour        = 0
   isec          = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STO_PER'

   TITRE(1) = 'RESULTATS CALCUL, '
   TITRE(2) = 'FICHIER RESULTAT MASCARET '
   TITRE(3) = '-----------------------------------------------------------------------'

   ul = FichierResultat%Unite

   nb_bief = size(Connect%OrigineBief)

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

   ! ECRITURE DES VARIABLES
   ! ======================

   !   TITRE

   call DATE_S( chaine_date )

   write (ul, '(2A)') trim(TITRE(1)), trim(chaine_date)
   write (ul, '(A72)') TITRE(2)
   write (ul, '(A72)') TITRE(3)

   !   VARIABLES BIEF

   write (ul,'(" IMAX  =", I5, " NBBIEF=", I5)') NbSectEff, nb_bief
   write (ul,'(" I1,I2 =",10I5)') ( Connect%OrigineBief(ibief) , Connect%FinBief(ibief) , ibief = 1 , nb_bief )

   ! NOMS ET VALEURS DES VARIABLES
   !  Stockage des variables pour la reprise de calcul
   !  X 1 , Z7, Q1 8, Q2 9
   !

   ivar = 1
   write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
   write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
   write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)
   ivar = 5
   write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
   write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
   write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)
   ivar = 6
   write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
   write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
   write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)
   ivar = 7
   write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
   write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
   write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)
   write(ul,'(1X,A)') NOM_FIN

   !
   !  STOCKAGE DES VARIABLES MAX POUR LA REPRISE DE CALCUL
   !
   write (ul,*) temps
   write (ul,*) (XFRON(ibief),ibief=1,nb_bief)

   ivar = 32
   if( gdr(ivar)%ASortir .eqv. .TRUE. ) then
      write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
      write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
      write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
                isec = 1 , NbSectEff)
   end if

   ivar = 33
   if( gdr(ivar)%ASortir .eqv. .TRUE. ) then
      write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
      write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
      write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)
   end if

   ivar = 34
   if( gdr(ivar)%ASortir .eqv. .TRUE. ) then
      write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
      write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
      write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)
   end if

   ivar = 40
   if( gdr(ivar)%ASortir .eqv. .TRUE. ) then
      write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
      write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
      write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)
   end if

   ivar = 41
   if( gdr(ivar)%ASortir .eqv. .TRUE. ) then
      write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
      write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
      write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)
   end if

   ivar = 38
   if( gdr(ivar)%ASortir .eqv. .TRUE. ) then
      write(ul,'(1X,A)') trim(var_nom(ivar)%NomA4)
      write(fmt_precision,'("(5F10.",I1,")")') var_nom(ivar)%Precision
      write(ul, *) &
               (real(gdr(ivar)%Valeur(SectionStoEff(isec)), kind=SIMPLE), &
               isec = 1, NbSectEff)
   end if

   write(ul,'(1X,A)') NOM_FIN

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STO_PER_REP

end subroutine STOCK_REP
