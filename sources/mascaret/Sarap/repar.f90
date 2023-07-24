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

subroutine REPAR  ( &
     DEB          , & ! Resultats
     VMOY         , &
     BETA         , &
     Q1           , &
     Q2           , &
     S1           , & ! Donnees modifiees
     S2           , &
     RH1          , &
     RH2          , &
     P1           , & ! Donnees non modifiees
     P2           , &
     Q            , &
     CF1          , &
     CF2          , &
     ModeleLit    , &
     LoiFrottement, & ! Loi de frottement
     NomProfil    , & ! Nom du profil pour affichage erreur
     Erreur         & ! Erreur
                  )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!   FONCTION :
!   --------
!   CALCUL DE LA REPARTITION DE DEBIT
!   MODELE DEBORD : REPARTITION LIT MINEUR/LIT MAJEUR , UTILISANT LE
!                   PARAMETRE DE MODIFICATION DES DEBITANCES A
!   MODELE CRUGOS : APPLICATION DE LA FORMULE DE COMPOSITION DES
!   ou Fond Berge   RUGOSITES D'EINSTEIN -  LE LIT EST ALORS CONSIDERE
!                   COMME UNIQUE, AVEC UN COEFFICIENT DE RUGOSITE
!                   FONCTION DES COEFFICIENTS INITIAUX, ET DES
!                   PARAMETRES GEOMETRIQUES
!   SI AUNCUN MODELE N'A ETE RETENU, LE MODELE DEBORD EST APPLIQUE EN
!   PRENANT LE PARAMETRE A EGAL A 1
!
! ----------------------------------------------------------------------
! ARGUMENTS
! .________________.____._______________________________________________
! !    NOM    !TYPE!MODE!                   ROLE
! !___________!____!____!______________________________________________
! !  DEB      ! R  !<-- ! DEBITANCE
! !  VMOY     ! R  !<-- ! VITESSE MOYENNE
! !  BETA     ! R  !<-- ! COEFFICIENT DE REPARTITION DE VITESSES MIN/MAJ
! !  Q1,Q2    ! R  !<-- ! DEBIT
! !  S1,S2    ! R  !<-->! SECTION MOUILLEE    ) INDICE 1 LIT MINEUR
! !  RH1,RH2  ! R  !<-->! RAYON HYDRAULIQUE   )        2 LIT MAJEUR
! !  Q        ! R  ! -->! DEBIT GLOBAL
! !  P1,P2    ! R  ! -->! PERIMETRE MOUILLE   )
! !  CF1,CF2  ! R  ! -->! COEF. DE FROTTEMENT MIN et MAJ
! !  ModeleLit! I  ! -->! Type du modele du lit
! !  Erreur   ! T  ! -->! Erreur
! !___________!____!____!______________________________________________
!  VARIABLES LOCALES
! .___________.____.____.______________________________________________
! !  RH       ! R  !<-- ! RAYON HYDRAULIQUE
! !  A        ! R  ! -- ! PARAMETRE DU MODELE DEBORD
! !  PUT      ! R  ! -- ! VALEUR SEUIL POUR LE CALCUL DE A
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
! ----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -------------------------
!   SOUS PROGRAMMES APPELANTS :  REZO, CRITIQ, PERMAT, QREPAR, SARAP
!   ---------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!   COMMENTAIRES :
!   --------------
!   POUR TRAITER UN LIT COMPOSE EN LIT UNIQUE, IL SUFFIT DE DEMANDER
!   LE MODELE CRUGOS, AVEC DES COEFFICIENTS DE RUGOSITE EGAUX
! ----------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! MODELE_LIT
   use M_ERREUR_T            ! type ERREUR_T
   use M_DEBITANCE_S         ! Calcul de la debitance
   use M_TRAITER_ERREUR_I    ! Traitement de l'erreur

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE)  , intent(out)   :: DEB
   real(DOUBLE)  , intent(out)   :: VMOY
   real(DOUBLE)  , intent(out)   :: BETA
   real(DOUBLE)  , intent(out)   :: Q1
   real(DOUBLE)  , intent(out)   :: Q2
   real(DOUBLE)  , intent(inout) :: S1
   real(DOUBLE)  , intent(inout) :: S2
   real(DOUBLE)  , intent(inout) :: RH1
   real(DOUBLE)  , intent(inout) :: RH2
   real(DOUBLE)  , intent(in)    :: Q
   real(DOUBLE)  , intent(in)    :: P1
   real(DOUBLE)  , intent(in)    :: P2
   real(DOUBLE)  , intent(in)    :: CF1
   real(DOUBLE)  , intent(in)    :: CF2
   integer       , intent(in)    :: ModeleLit
   integer       , intent(in)    :: LoiFrottement
   Character(30) , intent(in)    :: NomProfil
   type(ERREUR_T), intent(inout) :: Erreur
   !.. Local Scalars ..
   real(DOUBLE) :: st1_temp
   real(DOUBLE) :: RH
   real(DOUBLE) :: A,A0,DEB1,DEB2,ETA,FP1,FP2,FS1,FS2,R0,S,STEQUI,USETA,VALOP
   !character(132) :: !arbredappel_old
   ! Les Constantes sont declares dans le module M_PARAMETRES
   real(DOUBLE) :: PUT = 0.3_DOUBLE

   !.. Intrinsic Functions ..
   intrinsic DCOS, DSQRT

   !============================= Instructions =============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>REPAR'

   ! GRANDEURS INDEPENDANTES DU MODELE
   ! ---------------------------------
   ! Tests
   if( P1 <= EPS6 ) then
      Erreur%Numero = 40
      Erreur%ft     = err_40
      Erreur%ft_c   = err_40c
      call TRAITER_ERREUR_1C1R( Erreur , NomProfil, P1 )
      return
   endif

   if( S1 <= EPS6 ) then
      Erreur%Numero = 41
      Erreur%ft     = err_41
      Erreur%ft_c   = err_41c
      call TRAITER_ERREUR( Erreur , S1 )
      return
   endif

   RH   = ( S1 + S2 ) / ( P1 + P2 )
   VMOY = Q / ( S1 + S2 )

   ! MODELE DE COMPOSITION DES RUGOSITES EN LIT UNIQUE
   ! -------------------------------------------------
   label_FOND_BERGE : if( ModeleLit == MODELE_LIT_FOND_BERGE ) then
      ! MODELISATION FOND/BERGE
      ! CALCULS INTERNES DU MODELE
      S      = S1 + S2
      FP1    = P1 / ( CF1**W32 )
      FP2    = P2 / ( CF2**W32 )
      FS1    = S * ( FP1 / ( FP1 + FP2 ) )
      FS2    = S * ( FP2 / ( FP1 + FP2 ) )
      USETA  = FS2 / FS1
      STEQUI = ( ( P1 + P2 ) / ( FP1 + FP2 ) )**W23

      ! RESULTATS
      DEB  = STEQUI * S * RH**W23
      BETA = 1._DOUBLE
      Q1   = Q / ( 1._DOUBLE + USETA )
      Q2   = USETA * Q1

      ! MODIFICATION DES VARIABLES GEOMETRIQUES
      S1 = FS1
      S2 = FS2
      RH1 = S1 / P1
      if( abs(P2).GT.EPS6 ) then
         RH2 = S2 / P2
      else
         RH2 = 0._DOUBLE
      end if

   else label_FOND_BERGE
      ! VALEUR DE BASE DU PARAMETRE DU MODELE DEBORD
      ! --------------------------------------------
      ! COMPOSITION DES RUGOSITE DEBORD (OPTIO2) OU NON DEFINIE (OPTIO3)
      A = 1._DOUBLE

      ! MODELE DEBORD
      ! -------------
      if( ModeleLit == MODELE_LIT_DEBORD ) then
         A0 = W09 * ( CF2 / CF1 )**W16
         R0 = RH2 / RH1
         if( R0 >= PUT ) then
            A = A0
         else
            A = ( ( 1._DOUBLE - A0 ) * DCOS( PI * R0 / PUT ) + 1._DOUBLE + A0 ) / 2._DOUBLE
         end if
      end if

      call DEBITANCE_S( &
        DEB1          , &
        st1_temp      , &
        RH1           , &
        S1            , &
        LoiFrottement , &
        CF1           , &
        Erreur          &
                      )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      DEB1 = DEB1 * A
      VALOP = S2**2 + S1 * S2 * ( 1._DOUBLE - A * A )
      IF(VALOP.LT.0.D0) THEN
         Erreur%Numero = 705
         Erreur%ft     = err_705
         Erreur%ft_c   = err_705c
         call TRAITER_ERREUR( Erreur , VALOP )
         return
      ENDIF
      DEB2 = CF2 * DSQRT( VALOP ) * RH2**W23
      DEB  = DEB1 + DEB2

      if( S2 <= ( S1 * EPS4 ) ) then
         BETA = 1._DOUBLE
         Q2   = 0._DOUBLE
         Q1   = Q
      else
         ETA  = DEB1 / DEB2
         BETA = ( ETA**2 / S1 + 1._DOUBLE / S2 ) * ( S1 + S2 ) / ( 1._DOUBLE + ETA )**2
         Q2   = Q / ( 1._DOUBLE + ETA )
         Q1   = ETA * Q2
      end if

   end if label_FOND_BERGE

   !Erreur%arbredappel = !arbredappel_old

end subroutine REPAR
