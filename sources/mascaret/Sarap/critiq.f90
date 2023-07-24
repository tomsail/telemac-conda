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

subroutine CRITIQ ( &
     ZCRIT        , & ! /RESULTATS/
     Section      , & ! /DONNEES NON MODIFIEES/
     ZREF         , &
     Q            , & ! /DONNEES NON MODIFIEES
     CF1          , & !  (ARGUMENTS DE S.P APPELES)/
     CF2          , &
     IDT          , &
     XDT          , &
     Profil       , &
     Profil_plan  , &
     ModeleLit    , &
     LoiFrottement, & ! Loi de frottement
     UniteListing , & ! Unite logique fichier listing
     Erreur         & ! /Erreur/
     )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P.CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!   FONCTION :
!   --------
!
!   CALCUL DE LA COTE CRITIQUE DANS LA SECTION Section
!
! ----------------------------------------------------------------------
! ARGUMENTS
! .__________________.____._______________________________________________
! !    NOM      !TYPE!MODE!                   ROLE
! !_____________!____!____!______________________________________________
! ! ZCRIT       ! R  !<-- ! COTE CRITIQUE
! ! Section           ! I  ! -->! SECTION DU CALCUL DE LA COTE CRITIQUE
! ! ZREF        ! R  ! -->! COTE DU FOND
! ! Q           ! R  ! -->! DEBIT GLOBAL
! ! CF1         ! R  ! -->! COEF. DE FROTTEMENT , LIT MINEUR
! ! CF2         ! R  ! -->! COEF. DE FROTTEMENT , LIT MAJEUR
! ! IDT         ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT         ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil      ! T  ! -->! Caracteristiques du planimetrage d'un profil
! ! Profil_plan ! T  ! -->! Variables de profil planimetrees
! ! ModeleLit   ! I  ! -->! Modele du lit
! !_____________!____!____!______________________________________________
!
! VARIABLES LOCALES
! .___________.________________________________________________________
! !           !    !    !
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -----------------------
!
!   SOUS PROGRAMME APPELANT :  PERMAT
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   RHSBP_S : CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE SECTION
!
!   REPAR : CALCUL DE LA REPARTITION DES DEBITS ENTRE LE LIT MINEUR
!           ET LE LIT MAJEUR ACTIF
!
! ----------------------------------------------------------------------

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   ! Constantes nommees
   use M_PARAMETRE_C
   use M_MESSAGE_C         ! Messages d'erreur
   ! Types derives
   use M_PROFIL_T          ! Type PROFIL_T
   use M_PROFIL_PLAN_T     ! Type PROFIL_PLAN_T
   use M_ERREUR_T          ! Type ERREUR_T
   ! Procedures-module
   use M_RHSBP_S           ! Sous-programme RHSBP_S
   use M_TRAITER_ERREUR_I  ! Traitement des erreurs
   ! Interfaces
   use M_REPAR_I
   !.. Declarations explicites ..
   !-----------------------------
   implicit none
   !.. Arguments ..
   ! --------------
   real(DOUBLE)                       , intent(out)   :: ZCRIT
   integer            ,                 intent(in)    :: Section
   ! TABLEAUX DIMENSIONNES A NMSCAL
   real(DOUBLE)       , dimension(:)  , intent(in)    :: ZREF
   real(DOUBLE)       ,                 intent(in)    :: Q
   real(DOUBLE)       ,                 intent(in)    :: CF1
   real(DOUBLE)       ,                 intent(in)    :: CF2
   integer            , dimension(:)  , intent(in)    :: IDT
   real(DOUBLE)       , dimension(:)  , intent(in)    :: XDT
   type(PROFIL_T)     , dimension(:)  , intent(in)    :: Profil
   type(PROFIL_PLAN_T),                 intent(in)    :: Profil_plan
   integer                            , intent(in)    :: ModeleLit
   integer                            , intent(in)    :: LoiFrottement
   integer            ,                 intent(in   ) :: UniteListing
   type(ERREUR_T)     ,                 intent(inout) :: Erreur
   !.. Variables locales ..
   ! ----------------------
   real(DOUBLE) :: ZK, PAS, FCRIT1, FCRIT2, BETA
   real(DOUBLE) :: B1, B2, S1, S2, P2, P1, RH1, RH2
   real(DOUBLE) :: Q1, Q2, DEB, V, BST
   integer      :: num_profil, ipassage, icompt
   !character(132) :: !arbredappel_old

   !============================= Instructions ===========================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CRITIQ'
   ZK         = ZREF(Section)
   num_profil = IDT(Section)
   PAS        = Profil(num_profil)%Pas / 2._DOUBLE
   FCRIT1     = 100._DOUBLE

   ! CALCUL DE COTE CRITIQUE
   ! -----------------------
   label_boucle_n : do ipassage = 0 , 2

      FCRIT2 = 100._DOUBLE

      if( ipassage == 1 ) then

         ZK  = ZCRIT - PAS
         PAS = 0.2_DOUBLE * PAS

      else if( ipassage == 2 ) then

         ZK  = ZCRIT - PAS
         PAS = 0.01_DOUBLE

      endif

      icompt = 0

      do while( icompt < 3000 .and. FCRIT2 > 0._DOUBLE )

         icompt = icompt + 1

         ZCRIT  = ZK + icompt * PAS

         call RHSBP_S (                                       &
              B1 , B2 , BST , P1 , P2 , S1 , S2 , RH1 , RH2 , &
              Section , ZCRIT , ZREF(Section) , IDT , XDT ,   &
              Profil , Profil_plan,                           &
              UniteListing, Erreur )

         if( Erreur%Numero /= 0 ) then
            return
         endif

         call REPAR (       &
              DEB         , & ! Resultats
              V           , &
              BETA        , &
              Q1          , &
              Q2          , &
              S1          , & ! Donnees modifiees
              S2          , &
              RH1         , &
              RH2         , &
              P1          , & ! Donnees non modifiees
              P2          , &
              Q           , &
              CF1         , &
              CF2         , &
              ModeleLit    , &
              LoiFrottement, &
              Profil(IDT(Section))%Nom, &
              Erreur        &
             )

         if( Erreur%Numero /= 0 ) then
            return
         endif

         FCRIT2 = BETA * Q**2 * ( B1 + B2 ) / GPES / ( S1 + S2 )**3 - 1._DOUBLE

         if( FCRIT2 > 0._DOUBLE ) then
            FCRIT1 = FCRIT2
         endif

      end do

   end do label_boucle_n

   ZCRIT = ZCRIT + PAS * FCRIT2 / ( FCRIT1 - FCRIT2 )

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CRITIQ
