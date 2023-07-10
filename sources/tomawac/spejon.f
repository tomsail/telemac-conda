!                   *****************
                    SUBROUTINE SPEJON
!                   *****************
!
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN )
!
!***********************************************************************
! TOMAWAC   V6P1                                   28/06/2011
!***********************************************************************
!
!brief    COMPUTES A JONSWAP FREQUENCY SPECTRUM BASED
!+                ON A SERIES OF FREQUENCIES.
!
!history  M. BENOIT (EDF/DER/LNH)
!+        15/11/95
!+        V1P0
!+
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
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AL             |-->| PHILLIPS CONSTANT (ALPHA)
!| FP             |-->| JONSWAP SPECTRUM PEAK FREQUENCY
!| FPMIN          |-->| MINIMUM PEAK FREQUENCY VALUE
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GAMMA          |-->| JONSWAP SPECTRUM PEAK FACTOR
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| NF             |-->| NUMBER OF FREQUENCIES
!| SIGMAA         |-->| VALUE OF SIGMA FOR JONSWAP SPECTRUM (F<FP)
!| SIGMAB         |-->| VALUE OF SIGMA FOR JONSWAP SPECTRUM (F>FP)
!| SPEC           |<--| JONSWAP VARIANCE DENSITY FREQUENCY SPECTRUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : GRAVIT, DEUPI, FREQ, E2FMIN
      USE INTERFACE_TOMAWAC, EX_SPEJON => SPEJON
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    :: NF
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAA, SIGMAB, GAMMA, FPMIN
      DOUBLE PRECISION, INTENT(IN)    :: FP    , AL
      DOUBLE PRECISION, INTENT(INOUT) :: SPEC(NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JF
      DOUBLE PRECISION COEF  , ARG1   , ARG2  , ARG3  , SIG   , FF
!
!
      IF(FP.GT.FPMIN) THEN
        COEF=AL*GRAVIT**2/DEUPI**4
        DO JF=1,NF
          FF=FREQ(JF)
          IF(FF.LT.FP) THEN
            SIG=SIGMAA
          ELSE
            SIG=SIGMAB
          ENDIF
          ARG1=0.5D0*((FF-FP)/(SIG*FP))**2
          IF (ARG1.LT.99.D0) THEN
            ARG1=GAMMA**EXP(-ARG1)
          ELSE
            ARG1=1.D0
          ENDIF
          ARG2=1.25D0*(FP/FF)**4
          IF (ARG2.LT.99.D0) THEN
            ARG2=EXP(-ARG2)
          ELSE
            ARG2=0.D0
          ENDIF
          ARG3=COEF/FF**5
          SPEC(JF)=ARG1*ARG2*ARG3
          IF (SPEC(JF).LT.E2FMIN) SPEC(JF)=0.D0
        ENDDO ! JF
      ELSE
        DO JF=1,NF
          SPEC(JF)=0.D0
        ENDDO ! JF
      ENDIF
!
      RETURN
      END
