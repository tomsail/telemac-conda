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

subroutine  SING10      ( &
            AS,BS,CS    , & ! Coeff de l'equ discretisee de la singularite
            BAM,VAM     , & ! largeur au miroir et vitesse section precedente
            ZAM,ZAV     , & ! Cotes precedente et suivante
            CoeffDebit  , & ! Coefficient de debit de la vanne
            LargeurVanne, & ! Largeur de la vanne
            ZINF        , & ! Cote basse du seuil de la vanne
            ZSUP        , & ! Cote haute du seuil de la vanne
            Erreur        & ! Erreur
                          )

! *********************************************************************
! PROGICIEL : MASCARET        S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!
!   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UNE VANNE
!   ( SINGULARITE DE TYPE 10 ).
!
!   Q = AS * DHAMONT + BS * DHAVAL + CS
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS-PROGRAMME APPELANT :  KSING
!   -----------------------
!   SOUS-PROGRAMMES APPELES :  ---
!   -----------------------
!
!   COMMENTAIRES :
!   ------------
! . CALCUL DE LA LOI Q(Z) SUR UNE SINGULARITE DE TYPE SEUIL OU ORIFICE
!   SELON L OUVERTURE DE LA VANNE ET LES COTE DE LIGNE D EAU
!
! . Q EST > 0 DE L'INDICE 1 VERS L'INDICE 2
!   DANS LE CAS DE L ECOULEMENT AU DESSUS D UN SEUIL :
!     EN REGIME DENOYE , LA LOI STANDARD EST APPLIQUEE
!     EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
!
! . RH=(ZAVAL-ZSING)/(ZAMONT-ZSING)
!      ---          RH < 0.8   C= +1
!      ---   0.8  < RH < 1.0   C=  C1*RH**3 + C2*RH**2 + C3 *RH + C4
!
!   DANS LE CAS DE L ECOULEMENT PAR UN ORIFICE :
!     ON DISTINGUE EGALEMENT LE REGIME NOYE ET LE REGIME DENOYE
!
! . AS,BS ET CS SONT LES COEFFICIENTS DE L'EQUATION DISCRETISEE :
!   Q = AS*DZAMONT + BS*DZAVAL + CS
!   DZAMONT REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DZAMONT REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!   DZAVAL  REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DZAVAL  REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!
!
!   LE CAS SEUIL EST TRAITE COMME DANS SING3.f DE LIDO
!
!***********************************************************************

   !============================ Declarations ==============================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE), intent(  out) :: AS, BS, CS
   real(DOUBLE), intent(in   ) :: BAM, VAM
   real(DOUBLE), intent(in   ) :: ZAM, ZAV
   real(DOUBLE), intent(in   ) :: CoeffDebit, LargeurVanne
   real(DOUBLE), intent(in   ) :: ZINF, ZSUP
   type(ERREUR_T), intent(inout) :: Erreur
   !.. Constantes
   real(DOUBLE) :: C1 =   0._DOUBLE ! Coefficients de la fonction
   real(DOUBLE) :: C2 = -25._DOUBLE ! permettant le passage
   real(DOUBLE) :: C3 =  40._DOUBLE ! du regime denoye
   real(DOUBLE) :: C4 = -15._DOUBLE ! au regime noye
   ! Variables locales
   logical :: orifice ! indique si on est dans le cas d'un orifice
                      ! au lieu d'un seuil
   real(DOUBLE) :: C,CH1,CT,DCDRH,DCDZAM,DCDZAV,DQ0DZAM
   real(DOUBLE) :: HAMONT1,Q0,Q1,RH,S,S1,SENS,VAM1
   real(DOUBLE) :: CH     ! Charge dans section precedent le seuil
   real(DOUBLE) :: H1     ! Hauteur au dessus du seuil
   real(DOUBLE) :: H2     ! dans sections precedente et suivante
   real(DOUBLE) :: HAMONT ! Hauteur au dessus du seuil amont
   real(DOUBLE) :: HAVAL  ! Hauteur au dessus du seuil aval
   real(DOUBLE)   :: ouverture       ! ouverture de l'orifice
   !character(132) :: arbredappel_old ! arbre d'appel d'avant l'entree dans procedure

   !.. Intrinsic Functions .. 
   intrinsic DMAX1, DMIN1, DSQRT

   !============================= Instructions =============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>SING10'

   CT = CoeffDebit * DSQRT( 2._DOUBLE * GPES )

   AS = 0._DOUBLE
   BS = 0._DOUBLE
   CS = 0._DOUBLE

   H1 = ZAM - ZINF
   H2 = ZAV - ZINF

   ouverture = ZSUP - ZINF

   HAMONT = DMAX1(H1,H2)
   HAVAL  = DMIN1(H1,H2)

   if( HAMONT <= 0._DOUBLE ) then
      !Erreur%arbredappel = arbredappel_old
      return
   end if

   if( H1 <= 0._DOUBLE ) then
      H1 = 0._DOUBLE
   end if

   if( H2 <= 0._DOUBLE ) then
      H2 = 0._DOUBLE
   end if

   Q0 = 0._DOUBLE
   Q1 = 0._DOUBLE

   ! CALCUL DU DEBIT
   ! ---------------
   if( HAMONT > 0._DOUBLE ) then
      if( ( HAVAL < ouverture ) .and. ( HAMONT < 1.5_DOUBLE * ouverture ) ) then
         !------------------------------------------
         !        ECOULEMENT AU DESSUS D UN SEUIL
         !------------------------------------------
         orifice = .false.
         CH      = HAMONT + ( VAM**2 ) / ( 2 * GPES )
         Q0      = CT * LargeurVanne * ( CH**1.5 )
      else
         !------------------------------------
         !        ECOULEMENT PAR UN ORIFICE
         !------------------------------------
         orifice = .true.
         if( HAVAL > ouverture ) then
            Q0 = CT * LargeurVanne * ouverture * ( HAMONT - HAVAL )**0.5
         else
            Q0 = CT * LargeurVanne * ouverture * (HAMONT)**0.5
         end if
      end if
   end if

   S       = Q0 / VAM
   S1      = S + BAM * EPS2
   VAM1    = Q0 / S1
   HAMONT1 = HAMONT + EPS2

   if( HAMONT1 > 0._DOUBLE ) then

      if( ( orifice.eqv..false. ) ) then
         !------------------------------------------
         !        ECOULEMENT AU DESSUS D UN SEUIL
         !------------------------------------------
         CH1 = HAMONT1 + (VAM1**2)/(2*GPES)
         Q1  = CT * LargeurVanne * (CH1**1.5)

      elseif( HAVAL > ouverture ) then
         !------------------------------------
         !        ECOULEMENT PAR UN ORIFICE
         !------------------------------------
         Q1 = CT * LargeurVanne * ouverture * ( HAMONT1 - HAVAL )**0.5
      else
         Q1 = CT * LargeurVanne * ouverture * HAMONT1**0.5
      end if
   end if

   DQ0DZAM = ( Q1 - Q0 ) / EPS2

   if( ZAM >= ZAV ) then
      !-------------------------------
      !     ECOULEMENT DE (1) VERS (2)
      !-------------------------------
      RH   = H2 / H1
      SENS = 1._DOUBLE
   else
      !-------------------------------
      !     ECOULEMENT DE (2) VERS (1)
      !-------------------------------
      RH   = H1 / H2
      SENS = -1._DOUBLE
   end if

   ! CALCUL DES COEFFICIENTS
   ! -----------------------
   if( ( orifice.eqv..false. ) ) then
      !---------------------------------------
      !     ECOULEMENT AU DESSUS D UN SEUIL
      !---------------------------------------
      if( RH < 0.8_DOUBLE ) then
         !--------------------------------------------
         !        ECOULEMENT DENOYE 
         !--------------------------------------------
         C     = SENS * 1
         DCDRH = 0._DOUBLE
      else
         !------------------------------------------
         !        ECOULEMENT NOYE
         !------------------------------------------
         C     = SENS * ( C1 * ( RH**3 ) + C2 * ( RH**2 ) + C3 * RH + C4 )
         DCDRH = SENS * ( 3._DOUBLE * C1 * ( RH**2 ) + 2._DOUBLE * C2 * RH + C3 )
      end if

      AS = C * DQ0DZAM + Q0 * DCDRH * ( -HAVAL / ( HAMONT**2 ) )
      BS = Q0 * DCDRH * ( 1._DOUBLE / HAMONT )
      CS = C * Q0

   elseif( HAVAL > ouverture ) then
      !---------------------------------
      !     ECOULEMENT PAR UN ORIFICE
      !---------------------------------
      if( HAMONT > 2._DOUBLE * ouverture ) then
         !------------------------------------------
         !        ECOULEMENT NOYE
         !------------------------------------------
         C      = SENS * 0.65_DOUBLE
         DCDZAM = 0._DOUBLE
         DCDZAV = 0._DOUBLE

      elseif( 1.5_DOUBLE * ouverture < HAMONT ) then
         C      = SENS * ( 2.05_DOUBLE - 0.7_DOUBLE * HAMONT / ouverture )
         DCDZAM = -0.7_DOUBLE / ouverture
         DCDZAV = 0._DOUBLE
      else
         C      = SENS * 1.0_DOUBLE
         DCDZAM = 0._DOUBLE
         DCDZAV = 0._DOUBLE
      end if !

      AS = C * DQ0DZAM + Q0 * DCDZAM
      BS = C * (-DQ0DZAM) + Q0 * DCDZAV
      CS = C * Q0

   else
      !--------------------------------------------
      !        ECOULEMENT DENOYE 
      !--------------------------------------------
      if( HAMONT > 2._DOUBLE * ouverture ) then
         C      = SENS * 0.65_DOUBLE
         DCDZAM = 0._DOUBLE
         DCDZAV = 0._DOUBLE
      else
         C      = SENS * (2.05_DOUBLE - 0.7_DOUBLE * HAMONT / ouverture )
         DCDZAM = -0.7_DOUBLE / ouverture
         DCDZAV = 0._DOUBLE
      end if

      AS = C * DQ0DZAM + Q0 * DCDZAM
      BS = Q0 * DCDZAV
      CS = C * Q0

   end if

   !Erreur%arbredappel = arbredappel_old

   return

end subroutine SING10
