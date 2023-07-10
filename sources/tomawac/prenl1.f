!                   *****************
                    SUBROUTINE PRENL1
!                   *****************
!
     &( IANGNL, COEFNL, NDIRE , NF    , RAISF , XLAMD )
!
!***********************************************************************
! TOMAWAC   V6P1                                   22/06/2011
!***********************************************************************
!
!brief    PREPARES THE COMPUTATION FOR THE NON-LINEAR INTERACTION
!+                SOURCE TERM BETWEEN QUADRUPLETS USING THE DIA METHOD
!+               ("DISCRETE INTERACTION APPROXIMATION") PROPOSED BY
!+                HASSELMANN AND HASSELMANN (1985).
!+
!+
!+            PROCEDURE SPECIFIC TO THE CASE WHERE THE FREQUENCIES
!+                FOLLOW A GEOMETRICAL PROGRESSION AND THE DIRECTIONS
!+                ARE EVENLY DISTRIBUTED OVER [0;2.PI].
!
!note     THIS SUBROUTINE IS TO BE USED IN CONJONCTION WITH THE
!+          SUBROUTINE QNLIN1, WHICH IT OPTIMISES.
!
!reference  HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!+                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!+                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART1 :
!+                      A NEW METHOD FOR EFFICIENT COMPUTATION OF THE EXACT
!+                      NON-LINEAR TRANSFER INTEGRAL". JPO, VOL 15, PP 1369-1377.
!reference    HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!+                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!+                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART2 :
!+                      PARAMETERIZATIONS OF THE NONLINEAR ENERGY TRANSFER
!+                      FOR APPLICATION IN WAVE MODELS". JPO, VOL 15, PP 1378-1391.
!
!history  M. BENOIT
!+        26/06/96
!+        V1P2
!+   CREATED
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  G.MATTAROLO (EDF - LNHE)
!+        22/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COEFNL         |<--| COEFFICIENTS USED FOR DIA METHOD
!| IANGNL         |<--| ANGULAR INDICES TABLE
!| NF             |-->| NUNMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| RAISF          |-->| FREQUENTIAL RATIO
!| XLAMD          |-->| DIA STANDARD CONFIGURATION LAMBDA COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_PRENL1 => PRENL1
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER, INTENT(IN)             ::  NDIRE , NF
      INTEGER, INTENT(INOUT)          ::  IANGNL(NDIRE,8)
      DOUBLE PRECISION, INTENT(IN)    :: RAISF , XLAMD
      DOUBLE PRECISION, INTENT(INOUT) :: COEFNL(16)
!
!.....LOCAL VARIABLES
!     """"""""""""""""""
      INTEGER  JP
      DOUBLE PRECISION DELTA1, DELTA2, DTMOIN, DTPLUS, DTETAD
      DOUBLE PRECISION APLUS , AMOIN , BPLUS , BMOIN , FPLUS , FMOIN
!
!=====C---------------------------------------------------C
!  1  C COMPUTATIONS RELATED TO ANGULAR INTERPOLATION     C
!=====C---------------------------------------------------C
!
!.....1.1 DETERMINES RESONANT DIRECTIONS
!         (WITH THE CONVENTION  0
!     """""""""""""""""""""""""""""""""""""""""""""
      CALL  ANGLES( XLAMD , DTPLUS, DTMOIN)
!
!.....1.2 DETERMINES ANGULAR INDICES FOR THE 'STANDARD' CONFIGURATION
!         (CORRESPONDING TO (-DTPLUS,DTMOIN))
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""
      DELTA1=-DTPLUS
      DELTA2= DTMOIN
      DO JP=1,NDIRE
        CALL INTANG( IANGNL(JP,2) , IANGNL(JP,1) , JP , NDIRE , DELTA1)
        CALL INTANG( IANGNL(JP,3) , IANGNL(JP,4) , JP , NDIRE , DELTA2)
      ENDDO ! JP
!
!.....1.3 DETERMINES ANGULAR INDICES FOR THE 'IMAGE' CONFIGURATION
!         (CORRESPONDING TO (DTPLUS,-DTMOIN))
!     """""""""""""""""""""""""""""""""""""""""""""""""""""
      DELTA1= DTPLUS
      DELTA2=-DTMOIN
      DO JP=1,NDIRE
        CALL INTANG( IANGNL(JP,5) , IANGNL(JP,6) , JP , NDIRE , DELTA1)
        CALL INTANG( IANGNL(JP,8) , IANGNL(JP,7) , JP , NDIRE , DELTA2)
      ENDDO ! JP
!
!.....1.4 DETERMINES COEFFICIENTS OF ANGULAR INTERPOLATION
!     """""""""""""""""""""""""""""""""""""""""""
      DTETAD=360.D0/DBLE(NDIRE)
      APLUS=DTPLUS/DTETAD-DBLE(INT(DTPLUS/DTETAD))
      AMOIN=DTMOIN/DTETAD-DBLE(INT(DTMOIN/DTETAD))
!
!
!=====C---------------------------------------------------C
!  2  C COMPUTATIONS RELATED TO FREQUENCY INTERPOLATION   C
!=====C---------------------------------------------------C
      FPLUS=LOG(1.D0+XLAMD)/LOG(RAISF)
      FMOIN=LOG(1.D0-XLAMD)/LOG(RAISF)
      BPLUS=(RAISF**(FPLUS-INT(FPLUS)     )-1.D0)/(RAISF-1.D0)
      BMOIN=(RAISF**(FMOIN-INT(FMOIN)+1.D0)-1.D0)/(RAISF-1.D0)
!
!
!=====C---------------------------------------------------C
!  3 C ASSIGNS THE COEFFICIENTS FOR QNLIN1                C
!=====C---------------------------------------------------C
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
      COEFNL(11)=1.D0/(1.D0+XLAMD)**4
      COEFNL(12)=1.D0/(1.D0-XLAMD)**4
      COEFNL(13)=DBLE(1)
      COEFNL(14)=DBLE(NF+INT(1.D0-FMOIN))
!
      RETURN
      END
