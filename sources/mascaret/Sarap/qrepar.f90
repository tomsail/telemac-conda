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

subroutine QREPAR (   &
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
     Profil_plan    , &
     NumConfluence  , &
     Connect        , &
     ModeleLit      , &
     Epsil          , &
     DZPREV         , &
     UniteListing   , & ! Unite logisue du fichier listing
     LoiFrottement  , &
     Erreur           & !/Erreur/
     )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!   FONCTION :
!   --------
!
!   REPARTITION DES DEBITS AU NOEUD NumConfluence
!   CAS DE PLUSIEURS BRANCHES AVAL
!
! ----------------------------------------------------------------------
! ARGUMENTS
! ._______________.____.____._______________________________________________
! ! NOM           !TYPE!MODE! ROLE
! !_______________!____!____!______________________________________________
! ! SommeDebitance! R  !<-- ! SOMME DES DEBITANCES DES BRANCHES AVAL
! ! ZAval         ! R  !<-- ! TABLEAU DE COTE (INTERNE A QREPAR)
! ! NumPassage    ! I  !<-->! VARIABLE INDICATRICE :
! !               !    !    !  = 1  CALCUL DU DEBIT DANS LA BRANCHE AVAL
! !               !    !    !  = 2  CALCUL DES COTES DANS LES BRANCHES AMONT
! ! Q             ! R  !<-->! TABLEAU DES DEBITS
! ! Z             ! R  ! -->! TABLEAU DES COTES DE LA SURFACE LIBRE
! ! ZREF          ! R  ! -->! COTES DU FOND DU LIT
! ! X             ! R  ! -->! ABSCISSES DES SECTIONS DE CALCUL
! ! CF1           ! R  ! -->! COEF. DE STRICKLER , LIT MINEUR
! ! CF2           ! R  ! -->! COEF. DE STRICKLER , LIT MAJEUR
! ! IDT           ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT           ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil        ! T  ! -->! Caracteristiques du planimetrage d'un profil
! ! Profil_plan   ! R  ! -->! Variables de profil planimetrees
! ! NumConfluence ! I  ! -->! NUMERO DU NOEUD A TRAITER
! ! Connect       ! T  ! -->! Structure contenant la table de connectivite
! ! ModeleLit     ! I  ! -->! Modele du lit
! !_______________!____!____!______________________________________________
!
! VARIABLES LOCALES
! ._______________.____.____.______________________________________________
! !               !    !    !
! !_______________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMME APPELANT :  PERSAR
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   INTERPOLATION_S : INTERPOLATION DE LAGRANGE D'ODRE N
!
!   REPAR           : CALCUL DE LA REPARTITION DES DEBITS ENTRE
!                     LE LIT MINEUR ET LE LIT MAJEUR
!
!   RHSBP_S :  CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE
!              SECTION
!
! ----------------------------------------------------------------------

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C         ! Messages d'erreur
   ! Types derives
   use M_CONNECT_T         ! Type CONNECT_T
   use M_PROFIL_T          ! Type PROFIL_T
   use M_PROFIL_PLAN_T     ! Type PROFIL_PLAN_T
   use M_ERREUR_T          ! Type ERREUR_T
   ! Procedures-module
   use M_INTERPOLATION_S   ! Sous-programme INTERPOLATION_S
   use M_RHSBP_S           ! Sous-programme RHSBP_GENERIQUE_S
   use M_TRAITER_ERREUR_I  ! Traitement de l'erreur
   ! Interfaces
   use M_REPAR_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   ! --------------
   ! TABLEAUX DIMENSIONNES A NMPLAN
   real(DOUBLE)       , dimension(:)  , intent(  out) :: SommeDebitance
   real(DOUBLE)       , dimension(:)  , intent(  out) :: ZAval
   integer            ,                 intent(inout) :: NumPassage
   ! TABLEAU  DIMENSIONNE  A NbSect
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Q
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Z
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: ZREF
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: X
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: CF1
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: CF2
   integer            , dimension(:)  , intent(in   ) :: IDT
   real(DOUBLE)       , dimension(:)  , intent(in   ) :: XDT
   type(PROFIL_T)     , dimension(:)  , intent(in   ) :: Profil
   type(PROFIL_PLAN_T),                 intent(in   ) :: Profil_plan
   integer            ,                 intent(in   ) :: NumConfluence
   type(CONNECT_T)    ,                 intent(in)    :: Connect
   integer            ,                 intent(in   ) :: ModeleLit
   real(DOUBLE)       ,                 intent(inout) :: Epsil
   real(DOUBLE)       , dimension(:)  , intent(  out) :: DZPREV
   integer            ,                 intent(in   ) :: UniteListing
   integer                            , intent(in)    :: LoiFrottement
   type(ERREUR_T)     ,                 intent(inout) :: Erreur

   !.. Constantes ..
   !----------------
   ! PENTMI : VALEUR MINIMUM SIGNIFICATIVE DE LA PENTE
   ! EPS    : COEFFICIENT UTILISE POUR MESURER LES VARIATIONS
   !          SIGNIFICATIVES DE COTE
   real(DOUBLE), parameter :: PENTMI = 1.0E-6_DOUBLE
   real(DOUBLE), parameter :: EPS    = 0.5E-3_DOUBLE

   !.. Variables locales ..
   !-----------------------
   ! A DIMENSIONNER AU NOMBRE DE BIEFS A UN NOEUD
   ! En pratique a NBBIEF
   integer      :: I2AM(size(Connect%OrigineBief))
   integer      :: I1AV(size(Connect%OrigineBief))
   integer      :: I2AV(size(Connect%OrigineBief))
   real(DOUBLE) :: QDDZ(size(Connect%OrigineBief))
   real(DOUBLE) :: DELZ(size(Connect%OrigineBief))
   real(DOUBLE) :: B1 , B2
   real(DOUBLE) :: BETA
   real(DOUBLE) :: BST
   real(DOUBLE) :: DDZ
   real(DOUBLE) :: DEB
   real(DOUBLE) :: DEBM1
   real(DOUBLE) :: DEBP1
   real(DOUBLE) :: DELQ
   real(DOUBLE) :: DERIVD
   real(DOUBLE) :: P1 , P2
   real(DOUBLE) :: PENTE
   real(DOUBLE) :: QAMONT
   real(DOUBLE) :: QAMPEN        ! Debitance
   real(DOUBLE) :: Q2
   real(DOUBLE) :: Q1
   real(DOUBLE) :: RH1 , RH2
   real(DOUBLE) :: S1 , S2
   real(DOUBLE) :: SDELQ
   real(DOUBLE) :: SDELZ
   real(DOUBLE) :: SQ
   real(DOUBLE) :: SQDDZ
   real(DOUBLE) :: SQDDZI
   real(DOUBLE) :: VMOY
   real(DOUBLE) :: Z1
   real(DOUBLE) :: ZF
   real(DOUBLE) :: ZMAX
   real(DOUBLE) :: ZMAXI
   real(DOUBLE) :: ZNODE
   integer      :: I
   integer      :: J
   integer      :: JAVAL , JAMONT
   integer      :: K
   integer      :: K1
   integer      :: NBAVAL
   integer      :: NBAVQ
   integer      :: NBAMON
   integer      :: nb_pas_1
   integer      :: nb_pas_profil_max
   integer        :: ibief, isec, iprof
   integer        :: num_bief, num_sect
   logical        :: type_origine
   !character(132) :: !arbredappel_old

   !============================= Instructions ===========================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>QREPAR'

   ! REPERAGE DES BRANCHES AMONT, DES BRANCHES AVAL, CALCUL DU DEBIT AMONT
   ! ---------------------------------------------------------------------
   JAVAL  = 0
   JAMONT = 0
   QAMONT = 0._DOUBLE

   do ibief = 1 , Connect%NbBiefConfluence(NumConfluence)
      num_bief = Connect%NumBiefConfluence(NumConfluence,ibief)
      num_sect = Connect%NumSectionConfluence(NumConfluence,ibief)

      ! On cherche si la section num_sect est une origine ou une fin du bief
      type_origine = .false.
      do isec = 1 , size(Connect%OrigineBief)
         if (Connect%OrigineBief(isec) == num_sect) then
            type_origine = .true.
            exit
         end if
      end do

      if( type_origine ) then
         JAVAL       = JAVAL + 1
         I1AV(JAVAL) = Connect%OrigineBief(num_bief)
         I2AV(JAVAL) = Connect%FinBief(num_bief)
      else
         JAMONT       = JAMONT + 1
         I2AM(JAMONT) = Connect%FinBief(num_bief)
         QAMONT       = QAMONT + Q(I2AM(JAMONT))
      end if
   end do

   ! NBAVAL = NOMBRE DE BRANCHES AVAL
   ! NBAMON = NOMBRE DE BRANCHES AMONT
   NBAVAL = JAVAL
   NBAMON = JAMONT

   ! PREMIER PASSAGE
   ! ---------------
   ! CALCUL DES DEBITS DANS LES BRANCHES AVAL EN ECRIVANT L'EGALITE DES
   ! PENTES DE LIGNE DE CHARGE
   label_NumPassage_2 : if( NumPassage == 1 ) then
      ! RECHERCHE DES COTES DES FONDS ET DES VARIABLES DE PLANIMETRAGE
      ! POUR DEFINIR UNE PLAGE DE VARIATION
      DZPREV(NumConfluence) = 1000._DOUBLE
      I                     = I1AV(1)
      ZF                    = ZREF(I)
      ZMAX                  = ZREF(I) + real(Profil(IDT(I))%NbPas - 1, DOUBLE) * Profil(IDT(I))%Pas

      do J = 2 , NBAVAL
         I     = I1AV(J)
         ZF    = DMIN1(ZF, ZREF(I))
         ZMAXI = ZREF(I) + real(Profil(IDT(I))%NbPas - 1, DOUBLE) * Profil(IDT(I))%Pas
         ZMAX  = DMIN1(ZMAX, ZMAXI)
      end do

      ! On cherche le nombre maximal de pas de tous les profils
      nb_pas_profil_max = Profil(1)%NbPas
      do iprof = 2 , size(Profil)
         nb_pas_profil_max = MAX(nb_pas_profil_max, Profil(iprof)%NbPas)
      end do

      DDZ   = ( ZMAX - ZF ) / real(nb_pas_profil_max - 1, DOUBLE)
      Epsil = EPS * DDZ

      ! CALCUL DES TABLEAUX DE DEBITANCES
      PENTE = 0._DOUBLE
      do K = 1 , nb_pas_profil_max
         SommeDebitance(K) = 0._DOUBLE
      end do

      label_j : do J = 1 , NBAVAL

         I = I1AV(J)

         label_k : do K = 1, nb_pas_profil_max

            ZAval(K) = ZF + real(K - 1, DOUBLE) * DDZ
            if( K == 1 ) cycle label_k
            if( ZAval(K) > ZREF(I) + Epsil ) then
               call RHSBP_S                                            &
                    (B1 , B2 , BST , P1 , P2 , S1 , S2 , RH1 , RH2 ,   &
                    I , ZAval(K) , ZREF(I) , IDT , XDT ,               &
                    Profil , Profil_plan,                              &
                    UniteListing, Erreur)
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               call REPAR(       &
                   DEB         , & ! Resultats
                   VMOY        , &
                   BETA        , &
                   Q1          , &
                   Q2          , &
                   S1          , & ! Donnees modifiees
                   S2          , &
                   RH1         , &
                   RH2         , &
                   P1          , & ! Donnees non modifiees
                   P2          , &
                   Q(I)        , &
                   CF1(I)      , &
                   CF2(I)      , &
                   ModeleLit   , &
                   LoiFrottement, &
                   Profil(IDT(I))%Nom, &
                   Erreur        &
                   )

               if( Erreur%Numero /= 0 ) then
                  return
               endif
            else
               DEB = 0._DOUBLE
            endif

            SommeDebitance(K) = SommeDebitance(K) + DEB

         end do label_k

         ! EVALUATION DE LA PENTE DE LA LIGNE DE CHARGE
         PENTE = PENTE + ( ZREF(I1AV(J)) - ZREF(I2AV(J)) ) / ( X(I2AV(J)) - X(I1AV(J)) )

      end do label_j

      ! INTERPOLATION SUR LE TABLEAU DES DEBITANCES
      ! EN VERIFIANT QUE LES DEBITANCES CROISSENT BIEN AVEC LES COTES
      PENTE = PENTE / NBAVAL
      if( PENTE < PENTMI ) PENTE = PENTMI

      QAMPEN   = QAMONT / dsqrt(PENTE)
      if( QAMPEN > SommeDebitance(nb_pas_profil_max) ) QAMPEN = SommeDebitance(nb_pas_profil_max)
      nb_pas_1 = nb_pas_profil_max
      K1       = 2

      do while( K1 < nb_pas_1 )

         ! Si la debitance n'est pas monotone => suppression d'un element du tableau
         ! et decalage

         do while( SommeDebitance(K1) <= SommeDebitance(K1 - 1) .and. K1 < nb_pas_1 )

            do K = K1 , nb_pas_1 - 1
               ZAval   (K)       = ZAval(K + 1)
               SommeDebitance(K) = SommeDebitance(K + 1)
            end do
            nb_pas_1 = nb_pas_1 - 1
         end do
         if( SommeDebitance(K1) <= SommeDebitance(K1 - 1) .and. K1 >= nb_pas_1 ) then
            nb_pas_1 = nb_pas_1 - 1
         end if

         K1 = K1 + 1

      end do

      ! Calcul d'une cote de depart ZNODE
      call INTERPOLATION_S                                  &
           ( ZNODE  ,                                       &
           QAMPEN , 1 , SommeDebitance , ZAval , nb_pas_1 , &
           Erreur                                           )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      ! CALCUL DES DEBITS DES BRANCHES
      SQ = QAMONT                ! Somme de debits
      do J = 1 , NBAVAL
         I = I1AV(J)
         if( ZNODE > ZREF(I) + Epsil ) then
            call RHSBP_S                                          &
                 (B1 , B2 , BST , P1 , P2 , S1 , S2 , RH1 , RH2 , &
                 I , ZNODE , ZREF(I) , IDT , XDT ,                &
                 Profil , Profil_plan,                            &
                 UniteListing, Erreur                             &
                 )
            if( Erreur%Numero /= 0 ) then
               return
            endif

            call REPAR (       &
                 DEB         , & ! Resultats
                 VMOY        , &
                 BETA        , &
                 Q1          , &
                 Q2          , &
                 S1          , & ! Donnees modifiees
                 S2          , &
                 RH1         , &
                 RH2         , &
                 P1          , & ! Donnees non modifiees
                 P2          , &
                 Q(I)        , &
                 CF1(I)      , &
                 CF2(I)      , &
                 ModeleLit    , &
                 LoiFrottement, &
                 Profil(IDT(I))%Nom, &
                 Erreur        &
                  )
            if( Erreur%Numero /= 0 ) then
               return
            endif
         else
            ! --  PAS D'ECOULEMENT DANS CE BIEF --
            DEB = 0._DOUBLE
         endif

         Q(I) = DEB * dsqrt(PENTE)
         SQ   = SQ - Q(I)            ! SQ = reste de debit

      end do

      ! CORRECTION POUR AVOIR EXACTEMENT LA CONTINUITE DES DEBITS
      do J = 1 , NBAVAL
         I    = I1AV(J)
         Q(I) = Q(I) + SQ / NBAVAL
      end do

      ! SECOND PASSAGE
      ! --------------
      ! CORRECTION DES DEBITS EN FONCTION DES ECARTS CONSTATES SUR LES COTES

   else if( NumPassage == 2 ) then label_NumPassage_2

      SDELZ = 0._DOUBLE
      SDELQ = 0._DOUBLE
      SQDDZ = 0._DOUBLE
      SQDDZI= 0._DOUBLE
      ZNODE = 0._DOUBLE

      ! TEST SUR L'EGALITE DES COTES
      ! ( NBAVQ EST LE NOMBRE DE BIEFS AVAL OU IL EXISTE UN ECOULEMENT )

      NBAVQ = NBAVAL

      do J = 1 , NBAVAL
         I = I1AV(J)
         if( Z(I) > (ZREF(I) + Epsil) ) then
            ZNODE  = ZNODE + Z(I)
         else
            !        -- PAS D'ECOULEMENT DANS CETTE BRANCHE --
            NBAVQ = NBAVQ - 1
         endif
      end do

      ! DANS LE CAS OU IL Y A UNE BRANCHE SANS ECOULEMENT,
      ! STOP, SAUF SI LE NOEUD EST SIMPLE ( 2 BRANCHES AVAL )
      if( NBAVQ /= NBAVAL .and. NBAVAL > 2 ) then
         Erreur%numero = 47
         Erreur%ft     = err_47
         Erreur%ft_c   = err_47c
         call TRAITER_ERREUR( Erreur , NumConfluence , NBAVAL )
         return
      endif

      ZNODE  = ZNODE / real(NBAVQ, DOUBLE)

      do J = 1 , NBAVAL
         I = I1AV(J)
         if( Z(I) > (ZREF(I) + Epsil) ) then
            DELZ(J) = ZNODE - Z(I)
            SDELZ= SDELZ + abs(DELZ(J))
         else
            DELZ(J) = 0._DOUBLE
         endif
      end do

      ! SI ON A CONVERGE :
      ! .CORRECTION DES COTES AMONT
      ! .SORTIE AVEC NumPassage=999
      if( SDELZ < Epsil ) then
         do J = 1 , NBAMON
            I    = I2AM(J)
            Z(I) = ZNODE
         end do

         NumPassage = 999

         !Erreur%arbredappel = !arbredappel_old
         !------
         return
         !------
      endif

      ! STOP SI LE RESULTAT N'EST PAS AMELIORE
      if( SDELZ >= DZPREV(NumConfluence) ) then
         Erreur%Numero = 48
         Erreur%ft     = err_48
         Erreur%ft_c   = err_48c
         call TRAITER_ERREUR( Erreur , NumConfluence )
         return
      else
         DZPREV(NumConfluence) = SDELZ
      endif

      do J = 1 , NBAVAL

         I = I1AV(J)

         ! CALCUL DE LA DEBITANCE
         call RHSBP_S                                          &
              (B1 , B2 , BST , P1 , P2 , S1 , S2 , RH1 , RH2 , &
              I , Z(I) , ZREF(I) , IDT , XDT ,                 &
              Profil , Profil_plan,                            &
              UniteListing, Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         call REPAR (             &
                    DEB         , & ! Resultats
                    VMOY        , &
                    BETA        , &
                    Q1          , &
                    Q2          , &
                    S1          , & ! Donnees modifiees
                    S2          , &
                    RH1         , &
                    RH2         , &
                    P1          , & ! Donnees non modifiees
                    P2          , &
                    Q(I)        , &
                    CF1(I)      , &
                    CF2(I)      , &
                    ModeleLit    , &
                    LoiFrottement, &
                    Profil(IDT(I))%Nom, &
                    Erreur        &
                                 )
         if( Erreur%Numero /=0 ) then
            return
         endif

         ! CALCUL DE LA DERIVEE DE LA DEBITANCE PAR RAPPORT A Z
         Z1 = Z(I) + DELZ(J)

         call RHSBP_S                                               &
                   (B1 , B2 , BST , P1 , P2 , S1 , S2 , RH1 , RH2 , &
                   I , Z1 , ZREF(I) , IDT , XDT ,                   &
                   Profil , Profil_plan,                            &
                   UniteListing, Erreur )
         if( Erreur%Numero /=0 ) then
            return
         endif

         call REPAR (            &
                   DEBP1       , & ! Resultats
                   VMOY        , &
                   BETA        , &
                   Q1          , &
                   Q2          , &
                   S1          , & ! Donnees modifiees
                   S2          , &
                   RH1         , &
                   RH2         , &
                   P1          , & ! Donnees non modifiees
                   P2          , &
                   Q(I)        , &
                   CF1(I)      , &
                   CF2(I)      , &
                   ModeleLit    , &
                   LoiFrottement, &
                   Profil(IDT(I))%Nom, &
                   Erreur        &
                                 )

         if( Erreur%Numero /=0 ) then
            return
         endif

         Z1 = Z(I) - DELZ(J)

         call RHSBP_S                                               &
                   (B1 , B2 , BST , P1 , P2 , S1 , S2 , RH1 , RH2 , &
                   I , Z1 , ZREF(I) , IDT , XDT ,                   &
                   Profil , Profil_plan,                            &
                   UniteListing, Erreur                             &
                   )

         if( Erreur%Numero /=0 ) then
            return
         endif

         call REPAR (            &
                   DEBM1       , & ! Resultats
                   VMOY        , &
                   BETA        , &
                   Q1          , &
                   Q2          , &
                   S1          , & ! Donnees modifiees
                   S2          , &
                   RH1         , &
                   RH2         , &
                   P1          , & ! Donnees non modifiees
                   P2          , &
                   Q(I)        , &
                   CF1(I)      , &
                   CF2(I)      , &
                   ModeleLit    , &
                   LoiFrottement, &
                   Profil(IDT(I))%Nom, &
                   Erreur        &
                                 )
         if( Erreur%Numero /=0 ) then
            return
         endif

         DERIVD  = ( DEBP1 - DEBM1 ) / 2._DOUBLE / DELZ(J)
         QDDZ(J) = Q(I) * DERIVD / DEB
         SQDDZ   = SQDDZ  + QDDZ(J)
         SQDDZI  = SQDDZI + QDDZ(J) * Z(I)

      end do

      ! CALCUL DE LA NOUVELLE COTE A ATTEINDRE, PUIS CORRECTION DES DEBITS
      ZNODE = SQDDZI / SQDDZ

      do J = 1 , NBAVAL
         I       = I1AV(J)
         DELZ(J) = ZNODE - Z(I)
         DELQ    = QDDZ(J) * DELZ(J)
         SDELQ   = SDELQ + DELQ
      end do

      do J = 1 , NBAVAL
         I    = I1AV(J)
         DELQ = QDDZ(J) * DELZ(J) - SDELQ / NBAVAL
         Q(I) = Q(I) + DELQ
      end do

      ! ERREUR DANS L'ALGORITHME
      ! ------------------------

   else label_NumPassage_2

      Erreur%Numero = 49
      Erreur%ft   = err_49
      Erreur%ft_c = err_49c
      call TRAITER_ERREUR (Erreur, NumConfluence,NumPassage)
      return

   endif label_NumPassage_2

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine QREPAR
