!                       *****************
                        SUBROUTINE PRENL2
!                       *****************
!
     &( IANGNL, COEFNL, NDIRE , NF    , RAISF , XLAMD , XMU   )
!
!***********************************************************************
! TOMAWAC   V6P1                                   22/06/2011
!***********************************************************************
!
!brief   PREPARES THE COMPUTATION FOR THE NON-LINEAR INTERACTION
!+                SOURCE TERM BETWEEN QUADRUPLETS USING THE MDIA METHOD
!+                ("MULTIPLE DISCRETE INTERACTION APPROXIMATION")
!+                PROPOSED BY TOLMAN (2004)
!+
!+
!+            PROCEDURE SPECIFIC TO THE CASE WHERE THE FREQUENCIES
!+                FOLLOW A GEOMETRICAL PROGRESSION AND THE DIRECTIONS
!+                ARE EVENLY DISTRIBUTED OVER [0;2.PI].
!
!+note    THIS SUBROUTINE IS TO BE USED IN CONJONCT
!+          SUBROUTINE QNLIN2, WHICH IT OPTIMISES.
!
!reference   TOLMAN H.L. (2004):
!+             "INVERSE MODELING OF DISCRETE INTERACTION APPROXIMATIONS
!+             FOR NONLINEAR INTERACTIONS IN WIND WAVES". OCEAN
!+             MODELLING, 6, 405-422
!
!history  E. GAGNAIRE-RENOU
!+        04/2011
!+        V6P1
!+   CREATED
!
!history  G.MATTAROLO (EDF - LNHE)
!+        22/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COEFNL         |<--| COEFFICIENTS USED FOR DIA METHOD
!| IANGNL         |<--| ANGUAR INDICES TABLE
!| NF             |-->| NUNMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| RAISF          |-->| FREQUENTIAL RATIO
!| XLAMD          |-->| DIA STANDARD CONFIGURATION LAMBDA COEFFICIENT
!| XMU            |-->| COEFFICIENTS FOR MDIA METHOD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                                                     !
!  APPELS :    - PROGRAMME(S) APPELANT  :  WAC                        !
!  ********    - PROGRAMME(S) APPELE(S) :  ANGLES, INTANG             !
!                                                                     !
!
      USE INTERFACE_TOMAWAC, EX_PRENL2 => PRENL2
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER, INTENT(IN)             :: NDIRE , NF
      INTEGER, INTENT(INOUT)          :: IANGNL(NDIRE,16)
      DOUBLE PRECISION, INTENT(IN)    :: RAISF , XLAMD , XMU
      DOUBLE PRECISION, INTENT(INOUT) :: COEFNL(32)

!.....LOCAL VARIABLES
!     """"""""""""""""""
      INTEGER  JP
      DOUBLE PRECISION DELTA1, DELTA2, DTMOIN, DTPLUS, DTETAD, XXXX
      DOUBLE PRECISION APLUS , AMOIN , BPLUS , BMOIN , FPLUS , FMOIN
!
!
!
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     ++++      COMPUTATION FOR K1 AND K2 WITH THE MU VALUE     ++++
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XXXX=XMU
!
!=====!---------------------------------------------------!
!  1  ! COMPUTATIONS RELATED TO ANGULAR INTERPOLATION     !
!=====!---------------------------------------------------!
!
!.....1.1 DETERMINES RESONANT DIRECTIONS
!         (WITH THE CONVENTION  0 < DTPLUS < DTMOIN)
!     """""""""""""""""""""""""""""""""""""""""""""
      CALL  ANGLES( XXXX  , DTPLUS, DTMOIN)
!
!.....1.2 DETERMINES ANGULAR INDICES FOR THE 'STANDARD' CONFIGURATION
!         (CORRESPONDING TO (-DTPLUS,DTMOIN))
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (XXXX.GT.1.D-6) THEN
        DELTA1=-DTPLUS
        DELTA2= DTMOIN
        DO JP=1,NDIRE
          CALL INTANG(IANGNL(JP, 2), IANGNL(JP, 1), JP , NDIRE , DELTA1)
          CALL INTANG(IANGNL(JP, 3), IANGNL(JP, 4), JP , NDIRE , DELTA2)
        ENDDO
      ELSE
        DO JP=1,NDIRE
          IANGNL(JP, 1)=JP
          IANGNL(JP, 2)=JP-1
          IF (JP.EQ.1) IANGNL(JP, 2)=NDIRE
          IANGNL(JP, 3)=JP
          IANGNL(JP, 4)=JP+1
          IF (JP.EQ.NDIRE) IANGNL(JP, 4)=1
        ENDDO
      ENDIF
!
!.....1.3 DETERMINES ANGULAR INDICES FOR THE 'IMAGE' CONFIGURATION
!         (CORRESPONDING TO (DTPLUS,-DTMOIN))
!     """""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (XXXX.GT.1.D-6) THEN
        DELTA1= DTPLUS
        DELTA2=-DTMOIN
        DO JP=1,NDIRE
        CALL INTANG( IANGNL(JP, 5), IANGNL(JP, 6), JP , NDIRE , DELTA1)
        CALL INTANG( IANGNL(JP, 8), IANGNL(JP, 7), JP , NDIRE , DELTA2)
        ENDDO
      ELSE
        DO JP=1,NDIRE
          IANGNL(JP, 5)=JP
          IANGNL(JP, 6)=JP+1
          IF (JP.EQ.NDIRE) IANGNL(JP, 6)=1
          IANGNL(JP, 7)=JP
          IANGNL(JP, 8)=JP-1
          IF (JP.EQ.1) IANGNL(JP, 8)=NDIRE
        ENDDO
      ENDIF
!
!.....1.4 DETERMINES COEFFICIENTS OF ANGULAR INTERPOLATION
!     """""""""""""""""""""""""""""""""""""""""""
      DTETAD=360.D0/DBLE(NDIRE)
      APLUS=DTPLUS/DTETAD-DBLE(INT(DTPLUS/DTETAD))
      AMOIN=DTMOIN/DTETAD-DBLE(INT(DTMOIN/DTETAD))
!
!
!=====!---------------------------------------------------!
!  2  ! COMPUTATIONS RELATED TO FREQUENCY INTERPOLATION   !
!=====!---------------------------------------------------!
      FPLUS=LOG(1.D0+XXXX)/LOG(RAISF)
      FMOIN=LOG(1.D0-XXXX)/LOG(RAISF)
      BPLUS=(RAISF**(FPLUS-INT(FPLUS)     )-1.D0)/(RAISF-1.D0)
      BMOIN=(RAISF**(FMOIN-INT(FMOIN)+1.D0)-1.D0)/(RAISF-1.D0)
!
!
!=====!---------------------------------------------------!
!  3  ! ASSIGNS THE COEFFICIENTS FOR QNLIN2               !
!=====!---------------------------------------------------!
      COEFNL( 1)=(1.D0-APLUS) * (1.D0-BPLUS)
      COEFNL( 2)=      APLUS  * (1.D0-BPLUS)
      COEFNL( 3)=(1.D0-APLUS) *       BPLUS
      COEFNL( 4)=      APLUS  *       BPLUS
      COEFNL( 5)=(1.D0-AMOIN) * (1.D0-BMOIN)
      COEFNL( 6)=      AMOIN  * (1.D0-BMOIN)
      COEFNL( 7)=(1.D0-AMOIN) *       BMOIN
      COEFNL( 8)=      AMOIN  *       BMOIN
      COEFNL( 9)=FPLUS
      COEFNL(10)=FMOIN
      COEFNL(11)=1.D0/(1.D0+XXXX)**4
      COEFNL(12)=1.D0/(1.D0-XXXX)**4
      COEFNL(13)=DBLE(1)
      COEFNL(14)=DBLE(NF+INT(1.D0-FMOIN))
!
!
!
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     ++++    COMPUTATION FOR K1 AND K2 WITH THE LAMDA VALUE    ++++
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XXXX=XLAMD
!
!=====!---------------------------------------------------!
!  1  ! COMPUTATIONS RELATED TO ANGULAR INTERPOLATION     !
!=====!---------------------------------------------------!
!
!.....1.1 DETERMINES RESONANT DIRECTIONS
!         (WITH THE CONVENTION  0 < DTPLUS < DTMOIN)
!     """""""""""""""""""""""""""""""""""""""""""""
      CALL  ANGLES( XXXX  , DTPLUS, DTMOIN)
!
!.....1.2 DETERMINES ANGULAR INDICES FOR THE 'STANDARD' CONFIGURATION
!         (CORRESPONDING TO (-DTPLUS,DTMOIN))
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (XXXX.GT.1.D-6) THEN
        DELTA1=-DTPLUS
        DELTA2= DTMOIN
        DO JP=1,NDIRE
          CALL INTANG(IANGNL(JP,10), IANGNL(JP, 9), JP , NDIRE , DELTA1)
          CALL INTANG(IANGNL(JP,11), IANGNL(JP,12), JP , NDIRE , DELTA2)
        ENDDO
      ELSE
        DO JP=1,NDIRE
          IANGNL(JP, 9)=JP
          IANGNL(JP,10)=JP-1
          IF (JP.EQ.1) IANGNL(JP,10)=NDIRE
          IANGNL(JP,11)=JP
          IANGNL(JP,12)=JP+1
          IF (JP.EQ.NDIRE) IANGNL(JP,12)=1
        ENDDO
      ENDIF
!
!.....1.3 DETERMINES ANGULAR INDICES FOR THE 'IMAGE' CONFIGURATION
!         (CORRESPONDING TO (DTPLUS,-DTMOIN))
!     """""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (XXXX.GT.1.D-6) THEN
        DELTA1= DTPLUS
        DELTA2=-DTMOIN
        DO JP=1,NDIRE
          CALL INTANG(IANGNL(JP,13), IANGNL(JP,14), JP , NDIRE , DELTA1)
          CALL INTANG(IANGNL(JP,16), IANGNL(JP,15), JP , NDIRE , DELTA2)
        ENDDO
      ELSE
        DO JP=1,NDIRE
          IANGNL(JP,13)=JP
          IANGNL(JP,14)=JP+1
          IF (JP.EQ.NDIRE) IANGNL(JP,14)=1
          IANGNL(JP,15)=JP
          IANGNL(JP,16)=JP-1
          IF (JP.EQ.1) IANGNL(JP,16)=NDIRE
        ENDDO
      ENDIF
!
!.....1.4 DETERMINES COEFFICIENTS OF ANGULAR INTERPOLATION
!     """""""""""""""""""""""""""""""""""""""""""
      DTETAD=360.D0/DBLE(NDIRE)
      APLUS=DTPLUS/DTETAD-DBLE(INT(DTPLUS/DTETAD))
      AMOIN=DTMOIN/DTETAD-DBLE(INT(DTMOIN/DTETAD))
!
!
!=====!---------------------------------------------------!
!  2  ! COMPUTATIONS RELATED TO FREQUENCY INTERPOLATION   !
!=====!---------------------------------------------------!
      FPLUS=LOG(1.D0+XXXX)/LOG(RAISF)
      FMOIN=LOG(1.D0-XXXX)/LOG(RAISF)
      BPLUS=(RAISF**(FPLUS-INT(FPLUS)     )-1.D0)/(RAISF-1.D0)
      BMOIN=(RAISF**(FMOIN-INT(FMOIN)+1.D0)-1.D0)/(RAISF-1.D0)
!
!
!=====!---------------------------------------------------!
!  3  ! ASSIGNS THE COEFFICIENTS FOR QNLIN2               !
!=====!---------------------------------------------------!
      COEFNL(17)=(1.D0-APLUS) * (1.D0-BPLUS)
      COEFNL(18)=      APLUS  * (1.D0-BPLUS)
      COEFNL(19)=(1.D0-APLUS) *       BPLUS
      COEFNL(20)=      APLUS  *       BPLUS
      COEFNL(21)=(1.D0-AMOIN) * (1.D0-BMOIN)
      COEFNL(22)=      AMOIN  * (1.D0-BMOIN)
      COEFNL(23)=(1.D0-AMOIN) *       BMOIN
      COEFNL(24)=      AMOIN  *       BMOIN
      COEFNL(25)=FPLUS
      COEFNL(26)=FMOIN
      COEFNL(27)=1.D0/(1.D0+XXXX)**4
      COEFNL(28)=1.D0/(1.D0-XXXX)**4
      COEFNL(29)=DBLE(1)
      COEFNL(30)=DBLE(NF+INT(1.D0-FMOIN))
!
!
      RETURN
      END
