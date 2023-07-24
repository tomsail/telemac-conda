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

subroutine PERSAR (   &
     Z              , & !/RESULTATS/
     Q              , &
     X              , & !/DONNEES NON MODIFIEES/
     ZREF           , &
     CF1            , &
     CF2            , &
     PCSing         , &
     IDT            , &
     XDT            , &
     Profil         , &
     ProfilPlan     , &
     F1             , &
     QInjec         , &
     Connect        , &
     Singularite    , &
     Extremite      , &
     ModeleLit      , &
     Confluent      , & ! Caracteristiques des confluences
     Abaque         , & ! Abaques des pertes de  charges aux confluences
     Impression     , &
     UniteListing   , & ! Unite logique du fichier listing
     Temps          , &
     Algorithme     , &
     LoiFrottement  , & ! Loi de frottement
     PerteChargeConfluent, & ! Perte de charge automatique aux confluents
     CQMV                , &
     decentrement   , &
     Erreur           & !/Erreur/
     )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P.CHERUBINI
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!   FONCTION :
!   ----------
!
!   CALCUL DE LA LIGNE D'EAU
!
!-----------------------------------------------------------------------
! ARGUMENTS
! .________________.____.____._______________________________________________
! !    NOM         !TYPE!MODE!                   ROLE
! !________________!____!____!_______________________________________________
! ! Z              ! R  !<-- ! COTE DE LA SURFACE LIBRE
! ! Q              ! R  !<-- ! DEBITS
! ! X              ! R  ! -->! ABSCISSE DES SECTIONS DE CALCUL(SUR LE BIEF)
! ! ZREF           ! R  ! -->! COTE DU FOND
! ! CF1,CF2        ! R  ! -->! COEF. DE STRICKLER ,  1= MINEUR   2= MAJEUR
! ! PCSing         ! R  ! -->! PERTE DE CHARGE SINGULIERE
! ! IDT            ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT            ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil         ! R  ! -->! Caracteristiques du planimetrage d'un profil
! ! ProfilPlan     ! R  ! -->! SECTION MOUILLEE  ZONE DE STOCKAGE
! ! QInjec         ! R  ! -->! DEBIT APPORTE OU SOUTIRE
! ! Connect        ! T  ! -->! Structure contenant la table de connectivite
! ! Singularite    ! T  ! -->! Structure decrivant la singularite
! ! Extremite      ! T  ! -->! Structure decrivant les extremites
! ! ModeleLit      ! I  ! -->! Modele du lit
! ! Impression     ! L  ! -->! Flag d'impression
! ! Temps          ! R  ! -->! Temps
! ! Algorithme     ! I  ! -->! TABLEAUX DES APPELS AUX S.P. RESOLVANT LE
! !                !    !    ! CALCUL DE LIGNES D'EAU :
! !                !    !    !  Algorithme = ISUB + I
! !                !    !    !   AVEC  ISUB = 100  ==>       QBIEF
! !                !    !    !         ISUB = 200  ==>  CALL PERMAT
! !                !    !    !         ISUB = 300  ==>  CALL QNODE (1ER PAS)
! !                !    !    !         ISUB = 400  ==>  CALL QNODE (2EM PAS)
! !                !    !    !         ISUB = 500  ==>  CALL QREPAR(1ER PAS)
! !                !    !    !         ISUB = 600  ==>  CALL QREPAR(2EM PAS)
! !                !    !    !     ET   I = LE NUMERO DU NOEUD OU DU BIEF
! ! Erreur         ! T  !<-->! Erreur
! !________________!____!____!______________________________________________
!
! VARIABLES LOCALES
! .______________________________________________________________________
! !   IA        ! I  ! -- ! COMPTEUR DU TABLEAU DES APPELS
! !   ICOMPT    ! I  ! -- ! COMPTEUR LIMITANT LE NOMBRE D'ITERATION
! !   NumPassage! I  ! -- ! INDICATEUR POUR LES APPELS A QNODE ET QREPAR
! !   noeud_bief! I  ! -- ! NUMERO DU NOEUD OU DU BIEF CONSIDERE
! !SommeDebitance R  ! -->! SOMME DES DEBITANCES DES BRANCHES AVAL
! !   ZAval     ! R  ! -->! TABLEAU DE COTE (INTERNE A QREPAR)
! !_____________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ------------------------
!   SOUS PROGRAMME APPELANT :  SARAP
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   - PERMAT :  SOUS-PROGRAMME DU CALCUL DE LA LIGNE D'EAU
!               DANS UN BIEF
!   - QNODE  :  REPARTITION DES DEBITS A UN NOEUD AVEC
!               EGALITE DES COTES ( UNE SEULE BRANCHE AVAL)
!   - QREPAR :  REPARTITION DES DEBITS A UN NOEUD AVEC
!               EGALITE DES COTES (PLUSIEURS BRANCHES AVAL)
!
! **********************************************************************

   !============================= Declarations ===========================
   !
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C         ! Messages d'erreur
   ! Types derives
   use M_CONFLUENT_T       ! Type confluent
   use M_CONNECT_T         ! Definition du Type CONNECT_T
   use M_EXTREMITE_T       ! Definition du Type EXTREMITE_T
   use M_PROFIL_T          ! Definition du Type PROFIL_T
   use M_PROFIL_PLAN_T     ! Definition du Type PROFIL_PLAN_T
   use M_ERREUR_T          ! Definition du type ERREUR_T
   use M_SINGULARITE_T     ! Definition du Type SINGULARITE_T
   ! Interfaces
   use M_CALC_PC_CONFLU_I    ! Calcul des pertes de charge auto aux confluents
   use M_PERMAT_I
   use M_QNODE_I
   use M_QREPAR_I
   use M_TRAITER_ERREUR_I  ! Traitement de l'erreur
   !.. Declarations explicites ..
   !-----------------------------
   implicit none
   !.. Arguments ..
   ! --------------
   real(DOUBLE)       , dimension(:)  , intent(out)   :: Z
   real(DOUBLE)       , dimension(:)  , intent(out)   :: Q
   real(DOUBLE)       , dimension(:)  , intent(in)    :: X
   real(DOUBLE)       , dimension(:)  , intent(in)    :: ZREF
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF1
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF2
   real(DOUBLE)       , dimension(:)  , intent(inout) :: PCSing
   integer            , dimension(:)  , intent(in)    :: IDT
   real(DOUBLE)       , dimension(:)  , intent(in)    :: XDT
   type(PROFIL_T)     , dimension(:)  , intent(in)    :: Profil
   type(PROFIL_PLAN_T)                , intent(in)    :: ProfilPlan
   real(Double)       ,dimension (:,:), intent(in)    :: F1
   real(DOUBLE)       , dimension(:)  , intent(in)    :: QInjec
   type(CONNECT_T)                    , intent(in)    :: Connect
   type(SINGULARITE_T), dimension(:)  , intent(in   ) :: Singularite
   type(EXTREMITE_T)  , dimension(:)  , intent(in)    :: Extremite
   integer                            , intent(in)    :: ModeleLit
   type(CONFLUENT_T)   , dimension(:) , intent(in   ) :: Confluent
   real(DOUBLE)    , dimension(:,:,:) , intent(in   ) :: Abaque
   logical                            , intent(in)    :: Impression
   integer            ,                 intent(in)    :: UniteListing
   real(DOUBLE)                       , intent(in)    :: Temps
   integer            , dimension(:)  , intent(in)    :: Algorithme
   integer                            , intent(in)    :: LoiFrottement
   logical                            , intent(in)    :: PerteChargeConfluent
   integer                            , intent(in)    :: CQMV
   logical                            , intent(in)    :: decentrement
   type(ERREUR_T)                     , intent(inout) :: Erreur
   !.. Constantes ..
   !----------------
   integer, parameter :: NPASS                 = 5000
   integer, parameter :: APPEL_QBIEF           = 1
   integer, parameter :: APPEL_PERMAT          = 2
   integer, parameter :: APPEL_QNODE_1ER_PASS  = 3
   integer, parameter :: APPEL_QNODE_2ND_PASS  = 4
   integer, parameter :: APPEL_QREPAR_1ER_PASS = 5
   integer, parameter :: APPEL_QREPAR_2ND_PASS = 6
   !.. Variables locales ..
   !-----------------------
   real(DOUBLE), dimension(:), allocatable :: SommeDebitance
   real(DOUBLE), dimension(:), allocatable :: ZAval
   integer      :: IA
   integer      :: ICOMPT
   integer      :: NumPassage
   integer      :: IAP
   integer      :: ISUB
   integer      :: noeud_bief
   logical      :: limite_libre
   real(DOUBLE) :: epsil
   real(DOUBLE), dimension(size(Connect%NbBiefConfluence)) :: DZPREV
   real(DOUBLE) :: ZINIT
   integer      :: isec  , Nbsect! Compteur sur les sections
   integer      :: iext  ! Compteur sur les extremites
   integer      :: iprof ! Compteur sur les profils
   integer      :: nb_pas_profil_max ! Nombre maximal de pas
                                     ! pour tous les profils
   integer        :: nappel
   !character(132) :: !arbredappel_old
   integer        :: retour ! code de retour des fonction d'e/s
   !
   ! Ajout pour courbe de tarage en condition aval
   integer      :: iinf, NBP
   real(DOUBLE) :: Q1, Q2, Qobj
   real(DOUBLE) :: Z1, Z2
   !============================= Instructions ===========================
   ! INITIALISATIONS
   !----------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>PERSAR'
   nappel = size(Algorithme)
   Nbsect = size(X)
   nb_pas_profil_max = Profil(1)%NbPas

   do iprof = 2 , size(Profil)
      nb_pas_profil_max = MAX( nb_pas_profil_max , Profil(iprof)%NbPas )
   end do

   allocate( SommeDebitance(nb_pas_profil_max) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SommeDebitance' )
      return
   end if

   allocate( ZAval(nb_pas_profil_max) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Zaval' )
      return
   end if

   !------------------------------------------
   !    RESOLUTION DU CALCUL DE LA LIGNE D'EAU
   !    DANS L'ORDRE DU TABLEAU Algorithme
   !------------------------------------------
   IA     = 0
   ICOMPT = 0

   label_IA : do while( IA < nappel )

      IA         = IA + 1
      ICOMPT     = ICOMPT + 1
      ISUB       = int( Algorithme(IA) / 100 )
      noeud_bief = Algorithme(IA) - 100 * ISUB

      select case( ISUB )

         case( APPEL_QBIEF )

            ! Connect%OrigineBief(noeud_bief) : NUMERO DE LA SECTION ORIGINE DU BIEF
            ! On cherche l'extremite libre qui correspond
            ! a la section Connect%OrigineBief(noeud_bief)

            limite_libre = .false.

            do iext = 1 , size(Connect%NumSectionExtLibre(:))

               if( Connect%NumSectionExtLibre(iext) == Connect%OrigineBief(noeud_bief) ) then
                  ! iext : NUMERO DE LA LIMITE LIBRE CORRESPONDANTE
                  limite_libre = .true.
                  exit
               end if

            end do

            if( limite_libre ) then
               !             --  ON PART D'UNE LIMITE LIBRE --
               Q(Connect%OrigineBief(noeud_bief)) = Extremite(iext)%PtQ(1)
               ! else        --  ON PART D'UN NOEUD --
            endif

            do isec = Connect%OrigineBief(noeud_bief) + 1 , Connect%FinBief(noeud_bief)

               Q(isec) = Q(isec - 1) + QInjec(isec)

            end do


         case( APPEL_PERMAT )

            ! Calcul des pertes de charge automatique aux confluences

            if( PerteChargeConfluent ) then

               call CALC_PC_CONFLU    ( &
                PCSing                , &
                Z                     , &
                Q                     , &
                X                     , &
                ZREF                  , &
                Profil                , &
                ProfilPlan            , &
                Confluent             , &
                Abaque                , &
                IDT                   , &
                XDT                   , &
                Connect               , &
                UniteListing          , &
                Erreur                  &
                                )
               if (Erreur%Numero /= 0) then
                  return
               endif

            endif

            ! Connect%FinBief(noeud_bief) : NUMERO DE LA SECTION EXTREME DU BIEF
            ! On cherche l'extremite libre qui correspond
            ! a la section Connect%FinBief(noeud_bief)

            limite_libre = .false.

            do iext = 1 , size(Connect%NumSectionExtLibre(:))

               if( Connect%NumSectionExtLibre(iext) == Connect%FinBief(noeud_bief) ) then
                  ! iext : NUMERO DE LA LIMITE LIBRE CORRESPONDANTE
                  limite_libre = .true.
                  exit
               end if

            end do

            if( limite_libre ) then
               !             --  ON PART D'UNE LIMITE LIBRE --
               if( Extremite(iext)%Type == CONDITION_TYPE_COTE_IMPOSE ) then
                  ZINIT= Extremite(iext)%PtZ(1)
               else
                  NBP = size(Extremite(iext)%PtZ)
                  QOBJ = Q(Connect%FinBief(noeud_bief))
                  if( QOBJ < Extremite(iext)%PtQ(1) .or. &
                      QOBJ > Extremite(iext)%PtQ(NBP) ) then
                     Erreur%numero = 601
                     Erreur%ft     = err_601
                     Erreur%ft_c   = err_601c
                     call TRAITER_ERREUR( Erreur , iext , QOBJ , &
                      Extremite(iext)%PtQ(1) , Extremite(iext)%PtQ(NBP) )
                     return
                  end if
                  Iinf = 1
                  do while( QOBJ > Extremite(iext)%PtQ(Iinf))
                     Iinf = Iinf + 1
                  enddo
                  Q1 = Extremite(iext)%PtQ(Iinf-1)
                  Q2 = Extremite(iext)%PtQ(Iinf)
                  Z1 = Extremite(iext)%PtZ(Iinf-1)
                  Z2 = Extremite(iext)%PtZ(Iinf)
                  ZINIT = Z1 + (Z2 - Z1) * ((QOBJ - Q1) / (Q2 - Q1))
               endif
            else
              !             --  ON PART D'UN NOEUD --
               ZINIT= Z(Connect%FinBief(noeud_bief))
            endif

            call PERMAT (                     &
             Z                              , & !/RESULTATS/
             Q                              , &
             ZINIT                          , & !/DONNEES NON MODIFIEES/
             X                              , &
             ZREF                           , &
             CF1                            , &
             CF2                            , &
             PCSing                         , &
             IDT                            , &
             XDT                            , & !/DONNEES NON MODIFIEES
             Profil                         , &
             ProfilPlan                     , & ! (ARGUMENTS DE S.P) /
             F1                             , &
             Connect                        , &
             noeud_bief                     , &
             nbsect                         , &
             Singularite                    , &
             ModeleLit                      , &
             Impression                     , &
             UniteListing                   , &
             Temps                          , &
             LoiFrottement                  , &
             CQMV                           , &
             decentrement                   , &
             Erreur                   ) !Erreur


            if( Erreur%Numero /= 0 ) then
               return
            endif

         case( APPEL_QNODE_1ER_PASS )

            NumPassage = 1

            call QNODE (     &
             Q             , & !/DONNEES MODIFIEES/
             Z             , &
             noeud_bief    , & !/DONNEES NON MODIFIEES/
             NumPassage    , &
             Connect       , &
             Erreur          &
             )

            if( Erreur%Numero /= 0 ) then
               return
            endif

         case( APPEL_QNODE_2ND_PASS )

            NumPassage = 2

            call QNODE (     &
             Q             , & !/DONNEES MODIFIEES/
             Z             , &
             noeud_bief    , & !/DONNEES NON MODIFIEES/
             NumPassage    , &
             Connect       , &
             Erreur          &
             )
            if( Erreur%Numero /= 0 ) then
               return
            endif

         case( APPEL_QREPAR_1ER_PASS )

            NumPassage = 1

            call QREPAR (     &
             SommeDebitance , & !/RESULTATS/
             ZAval          , &
             NumPassage     , & !/DONNEES MODIFIEES/
             Q              , &
             Z              , &
             ZREF           , & !/DONNEES NON MODIFIEES/
             X              , &
             CF1            , &
             CF2            , &
             IDT            , &
             XDT            , &
             Profil         , &
             ProfilPlan     , &
             noeud_bief     , &
             Connect        , &
             ModeleLit      , &
             epsil          , &
             DZPREV         , &
             UniteListing   , &
             LoiFrottement  , &
             Erreur           &
             )
            if( Erreur%Numero /= 0 ) then
               return
            endif

         case( APPEL_QREPAR_2ND_PASS )

            NumPassage = 2

            call QREPAR (     &
             SommeDebitance , & !/RESULTATS/
             ZAval          , &
             NumPassage     , & !/DONNEES MODIFIEES/
             Q              , &
             Z              , &
             ZREF           , & !/DONNEES NON MODIFIEES/
             X              , &
             CF1            , &
             CF2            , &
             IDT            , &
             XDT            , &
             Profil         , &
             ProfilPlan     , &
             noeud_bief     , &
             Connect        , &
             ModeleLit      , &
             epsil          , &
             DZPREV         , &
             UniteListing   , & ! Unite logisue du fichier listing
             LoiFrottement  , &
             Erreur           &
             )
            if( Erreur%Numero /= 0 ) then
               return
            endif

            !  TEST DU RETOUR DE QREPAR :
            ! . SI NumPassage=999 ON A CONVERGE , LA REPARTITION DE DEBIT EST BONNE
            ! . SINON IL FAUT ITERER , ON FAIT UN RETOUR AU PREMIER APPEL A QREPAR
            !   LE NOMBRE D'ITERATIONS ETANT LIMITE PAR NPASS
            if( NumPassage /= 999 .and. ICOMPT < NPASS ) then
               IAP = nappel - IA + 1
               IA  = IAP
            else if( ICOMPT >= NPASS ) then
               Erreur%Numero = 32
               Erreur%ft     = err_32
               Erreur%ft_c   = err_32c
               call TRAITER_ERREUR( Erreur , noeud_bief )
               return
            endif

         case default

            Erreur%Numero = 33
            Erreur%ft     = err_33
            Erreur%ft_c   = err_33c
            call TRAITER_ERREUR( Erreur , IA )
            return

      end select

   end do label_IA

   !  Desallocation des tableaux locaux
   !  ---------------------------------
   deallocate( SommeDebitance , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'SommeDebitance' )
      return
   end if

   deallocate( ZAval , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'Zaval' )
      return
   end if

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine PERSAR
