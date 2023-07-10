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

subroutine FLUSRC( &
                IEL1 , &
                IEL2 , &
              ISEGIN , &
               VNOIN , &
                   W , &
              FLUSCE , &
                 BXY , &
                  ZF , &
                 EPS , &
              Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION  : . CALCUL DES FLUX DUS AUX TERMES SOURCES
!                 - DE TYPE  DECENTRE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! ! . IEL1         !<-- ! ELEMENT VOISIN                               !
! ! . IEL2         !<-- ! ELEMENT VOISIN                               !
! ! . ISEGIN       !<-- ! ARETE INTERNE                                ! 
! ! . VNOIN        ! -->! NORMALE DU SEGMENT INTERNE                   !
! !                !    ! (2 PREMIERES COMPOSANTES) ET                 !
! !                !    ! LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)    !
! ! . W            ! -->! VARIABLES CONSERVATIVES DU PB A L'INSTANT N  !
! ! . FLUSCE       !<-- ! TABLEAU DES TERMES SOURCES                   !
! ! . BXY          ! -->! LES COORDONNEES X,Y DU BARYCENTRE DE         !
! !   ZF           !    !                                              !
! ! . EPS          ! -->! PRECISION SUR LES HAUTEURS D'EAU             !
! !________________!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!        -- (TABLEAU DE TRAVAIL)
!-----------------------------------------------------------------------
!     - SOUS PROGRAMME(S) APPELANT : CALCON
!     - PORTABILITE:
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   integer     ,                   intent(in)    :: IEL1,IEL2,ISEGIN
   ! 1ere dimension NFON, 2nde dimension NSEGIN
   real(DOUBLE), dimension(:,:)  , intent(in)    :: VNOIN
   ! 1ere dimension NFON, 2nde dimension NELEM
   real(DOUBLE), dimension(:,:)  , intent(inout) :: W
   ! 1ere dimension 3, 2nde dimension 100
   real(DOUBLE), dimension(:,:)  , intent(  out) :: FLUSCE
   ! 1ere dimension NELEM, 2nde dimension 2
   real(DOUBLE), dimension(:,:)  , intent(in)    :: BXY
   ! 1ere dimension NELEM
   real(DOUBLE), dimension(:)    , intent(in)    :: ZF
   real(DOUBLE),                   intent(in)    :: EPS
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE) :: T11(3),T21(3),TS11(3),TS21(3)
   real(DOUBLE) :: T12(3),T22(3),TS12(3),TS22(3)
   real(DOUBLE) :: GE(3)
   real(DOUBLE) :: XGI, YGI, XGJ, YGJ, DIJ, A1, A2
   real(DOUBLE) :: HI,UI,VI,HJ,VJ,UJ,XN,YN
   real(DOUBLE) :: CT2,CT,RLAMB0,RLAMBM,ALPHA,CI2
   real(DOUBLE) :: CJ,CJ2,RLAMBJ,RLAMBI,RLAMBP
   real(DOUBLE) :: RI,RJ,UT,VT,CI
   real(DOUBLE) :: PSA1,PSA2,PSA
   integer      :: INDIC
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>FLUSRC'

   !------
   ! 1. INITIALISATIONS
   !------
   INDIC = 0
   RLAMBI = 0._DOUBLE
   RLAMBJ = 0._DOUBLE
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   !------
   ! 1. CALCUL DES TERMES SOURCES A l'INTERFACE IEL1 , IEL2
   !------
   !   --->    QUELQUES CALCULS INTERMEDIAIRES
   !           ------------------------------
   HI = W(1,IEL1)
   if( HI > EPS ) then
      UI = W(2,IEL1) / HI
      VI = W(3,IEL1) / HI
   else
      HI        = EPS
      UI        = 0._DOUBLE
      VI        = 0._DOUBLE
      W(2,IEL1) = 0._DOUBLE
      W(3,IEL1) = 0._DOUBLE
      INDIC     = INDIC + 1
   endif

   HJ = W(1,IEL2)
   if( HJ > EPS ) then
      UJ = W(2,IEL2) / HJ
      VJ = W(3,IEL2) / HJ
   else
      HJ        = EPS
      UJ        = 0._DOUBLE
      VJ        = 0._DOUBLE
      W(2,IEL2) = 0._DOUBLE
      W(3,IEL2) = 0._DOUBLE
      INDIC     = INDIC + 1
   endif

   if( INDIC == 2 ) then
      FLUSCE(1,IEL1) = 0._DOUBLE
      FLUSCE(2,IEL1) = 0._DOUBLE
      FLUSCE(3,IEL1) = 0._DOUBLE
      FLUSCE(1,IEL2) = 0._DOUBLE
      FLUSCE(2,IEL2) = 0._DOUBLE
      FLUSCE(3,IEL2) = 0._DOUBLE
   else
      XN = VNOIN (1,ISEGIN)
      YN = VNOIN (2,ISEGIN)
      !   --->    CALCUL DES MOYENNES DE ROE DE U,V,H,C**2 ET C
      !           ---------------------------------------------
      RI  = dsqrt( HI )
      RJ  = dsqrt( HJ )
      UT  = ( RI * UI + RJ * UJ ) /(RI + RJ)
      VT  = ( RI * VI + RJ * VJ ) /(RI + RJ)
      CT2 = GPES * ( HI + HJ ) / 2._DOUBLE
      CT  = dsqrt ( CT2 )

      !   --->  TEST SUR LE SIGNE DE LA VALEUR PROPRE LAMB0 = <UT,N>
      !           ----------------------------------------------------------
      RLAMB0 = UT * XN + VT * YN

      !     CALCUL DES MATRICES DES VALEURS PROPRES 
      !--------------------------------------------
      T11(1) = 1._DOUBLE
      T11(2) = UT - CT * XN
      T11(3) = VT - CT * YN
      T21(1) = 0._DOUBLE
      T21(2) = CT * YN
      T21(3) = -CT * XN
      T12(1) = 1._DOUBLE
      T12(2) = UT + CT * XN
      T12(3) = VT + CT * YN
      T22(1) = 0._DOUBLE
      T22(2) = -CT * YN
      T22(3) = +CT * XN
      TS11(1) = (UT * XN + VT * YN) * CT + CT2
      TS21(1) = (2 * VT * XN - 2 * UT * YN) * CT
      TS11(2) = -XN * CT
      TS21(2) = 2 * YN * CT
      TS11(3) = -YN * CT
      TS21(3) = -2 * XN * CT
      TS12(1) = -(UT * XN + VT * YN) * CT + CT2
      TS22(1) = -(2 * VT * XN - 2 * UT * YN) * CT
      TS12(2) = +XN * CT
      TS22(2) = -2 * YN * CT
      TS12(3) = +YN * CT
      TS22(3) = +2 * XN * CT

      !----------Calculs pour les termes sources--------------------
      XGI = BXY(IEL1,1)
      YGI = BXY(IEL1,2)
      XGJ = BXY(IEL2,1)
      YGJ = BXY(IEL2,2)
      DIJ = dsqrt( ( XGJ -XGI )**2 + ( YGJ - YGI )**2 )
      A1  = VNOIN(3,ISEGIN) * DIJ / 2._DOUBLE
      A2  = VNOIN(3,ISEGIN) * DIJ / 2._DOUBLE

      !  GRADIENTS DE FOND
      GE(1) = 0._DOUBLE
      GE(2) = GPES * ( ( W(1,IEL2) + W(1,IEL1) ) / 2._DOUBLE ) * ( ZF(IEL2) - ZF(IEL1) ) * XN / DIJ
      GE(3) = GPES * ( ( W(1,IEL2) + W(1,IEL1) ) / 2._DOUBLE ) * ( ZF(IEL2) - ZF(IEL1) ) * YN / DIJ

      if( RLAMB0 >= 0._DOUBLE ) then
         !        ---- SEGMENT SORTIE---------
         RLAMBM = RLAMB0 - CT
         ALPHA  = UI * XN + VI * YN
         !TBTB DEBUT : MODIFICATION DE RLAMBM SI RLAMBM < D1
         CI2    = GPES * HI
         CI     = dsqrt( CI2 )
         CJ2    =  GPES * HJ
         CJ     = dsqrt( CJ2 )
         RLAMBI = ALPHA - CI
         RLAMBJ = UJ * XN + VJ * YN - CJ

         !TBTB : MODIF UNIQUEMENT DANS LA DETENTE :
         if( RLAMBI < 0._DOUBLE .and. RLAMBJ > 0._DOUBLE ) then
            RLAMBM = dmin1( 0._DOUBLE , RLAMBM ) - dabs( RLAMBI - RLAMBJ ) / 4._DOUBLE
         endif
         !TBTB FIN

         !------------CALCUL DES TERMES SOURCES ------------------------
         FLUSCE(1,IEL1) = 0._DOUBLE
         FLUSCE(2,IEL1) = 0._DOUBLE
         FLUSCE(3,IEL1) = 0._DOUBLE
         FLUSCE(1,IEL2) = 0._DOUBLE
         FLUSCE(2,IEL2) = 0._DOUBLE
         FLUSCE(3,IEL2) = 0._DOUBLE

         !   --->    TEST SUR LE SIGNE DE LAMBDAM
         !           ----------------------------
         if ( RLAMBM < 0._DOUBLE ) then
            !----------CALCUL DES TERMES SOURCES --------------------------
            PSA            = TS11(1) * Ge(1) + TS11(2) * Ge(2) + TS11(3) * Ge(3)
            FLUSCE(1,IEL1) = PSA * T11(1)
            FLUSCE(2,IEL1) = PSA * T11(2)
            FLUSCE(3,IEL1) = PSA * T11(3)
            PSA1           = TS12(1) * Ge(1) + TS12(2) * Ge(2) + TS12(3) * Ge(3)
            PSA2           = TS22(1) * Ge(1) + TS22(2) * Ge(2) + TS22(3) * Ge(3)
            FLUSCE(1,IEL2) = ( PSA1 * T12(1) + PSA2 * T22(1) )
            FLUSCE(2,IEL2) = ( PSA1 * T12(2) + PSA2 * T22(2) )
            FLUSCE(3,IEL2) = ( PSA1 * T12(3) + PSA2 * T22(3) )
         else
            !           -----
            FLUSCE(1,IEL1) = 0._DOUBLE
            FLUSCE(2,IEL1) = 0._DOUBLE 
            FLUSCE(3,IEL1) = 0._DOUBLE
            FLUSCE(1,IEL2) = GE(1) * CT2 *2._DOUBLE
            FLUSCE(2,IEL2) = GE(2) * CT2 *2._DOUBLE
            FLUSCE(3,IEL2) = GE(3) * CT2 *2._DOUBLE
         endif
         !           -----
         !      testest
      else
         !      testest
         !   --->    PETITS CALCULS
         !           --------------
         RLAMBP = RLAMB0 + CT
         ALPHA  = UI * XN + VI * YN
         !TBTB DEBUT : MODIFICATION DE RLAMBP SI RLAMBM < D1
         CI2    = GPES*HI 
         CI     = dsqrt (CI2)
         CJ2    =  GPES*HJ
         CJ     = dsqrt (CJ2)
         RLAMBI = ALPHA - CI
         RLAMBJ = UJ * XN + VJ * YN - CJ
         !TBTB : MODIF UNIQUEMENT DANS LA DETENTE :
         if( RLAMBI < 0._DOUBLE .and. RLAMBJ >  0._DOUBLE ) then
            RLAMBP = dmax1( 0._DOUBLE , RLAMBP ) + dabs( RLAMBI - RLAMBJ ) / 4._DOUBLE
         endif
         !TBTB FIN

         !-----------CALCUL DES TERMES SOURCE --------------------------
         FLUSCE(1,IEL1) = GE(1) * CT2 * 2._DOUBLE
         FLUSCE(2,IEL1) = GE(2) * CT2 * 2._DOUBLE
         FLUSCE(3,IEL1) = GE(3) * CT2 * 2._DOUBLE
         FLUSCE(1,IEL2) = 0._DOUBLE
         FLUSCE(2,IEL2) = 0._DOUBLE
         FLUSCE(3,IEL2) = 0._DOUBLE
         !   --->    TEST SUR LE SIGNE DE LAMBDAP
         !           ----------------------------
         if( RLAMBP > 0._DOUBLE ) then
            !-----------CALCUL DES TERMES SOURCE ------------------
            PSA1           = TS11(1) * Ge(1) + TS11(2) * Ge(2) + TS11(3) * Ge(3)
            PSA2           = TS21(1) * Ge(1) + TS21(2) * Ge(2) + TS21(3) * Ge(3)
            FLUSCE(1,IEL1) = ( PSA1 * T11(1) + PSA2 * T21(1) )
            FLUSCE(2,IEL1) = ( PSA1 * T11(2) + PSA2 * T21(2) )
            FLUSCE(3,IEL1) = ( PSA1 * T11(3) + PSA2 * T21(3) )
            PSA            = TS12(1)*Ge(1)+TS12(2)*Ge(2)+TS12(3)*Ge(3)
            FLUSCE(1,IEL2) = PSA*T12(1)
            FLUSCE(2,IEL2) = PSA*T12(2)
            FLUSCE(3,IEL2) = PSA*T12(3)
         endif
      endif
      !       testest
      FLUSCE(1,IEL1) = FLUSCE(1,IEL1) * A1 / CT2
      FLUSCE(2,IEL1) = FLUSCE(2,IEL1) * A1 / CT2
      FLUSCE(3,IEL1) = FLUSCE(3,IEL1) * A1 / CT2
      FLUSCE(1,IEL2) = FLUSCE(1,IEL2) * A2 / CT2
      FLUSCE(2,IEL2) = FLUSCE(2,IEL2) * A2 / CT2
      FLUSCE(3,IEL2) = FLUSCE(3,IEL2) * A2 / CT2

   endif

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine FLUSRC
