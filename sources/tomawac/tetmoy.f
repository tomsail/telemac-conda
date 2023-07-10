!                   *****************
                    SUBROUTINE TETMOY
!                   *****************
!
     &( TETAM , F     , NDIRE , NF    , NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   28/06/2011
!***********************************************************************
!
!brief    COMPUTES THE MEAN DIRECTION OF A DIRECTIONAL SPECTRUM
!+               (BY COMPUTING THE ARC TANGENT OF THE MEAN SINE AND
!+                COSINE). THE RESULT IS IN RADIANS.
!
!history  P. THELIIER; M. BENOIT
!+        01/02/95
!+        V1P0
!+
!
!history  M. BENOIT
!+        05/07/96
!+        V1P2
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
!| COSMOY         |<->| WORK TABLE
!| COSTET         |-->| COSINE OF TETA ANGLE
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SINMOY         |<->| WORK TABLE
!| SINTET         |-->| SINE OF TETA ANGLE
!| TAILF          |-->| FACTEUR DE QUEUE DU SPECTRE
!| TAUXC          |<->| WORK TABLE
!| TAUXS          |<->| WORK TABLE
!| TETAM          |<--| DIRECTIONAL SPECTRUM MEAN DIRECTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, FREQ, DFREQ, TAILF,
     &                                 COSTET, SINTET
!
      USE INTERFACE_TOMAWAC, EX_TETMOY => TETMOY
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TETAM(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  IP    , JP    , JF
      DOUBLE PRECISION AUXC  , AUXS  ,SEUIL , COEFT, DFDTET
      DOUBLE PRECISION DTETAR, COSMOY,SINMOY, TAUXC, TAUXS
!
!-----------------------------------------------------------------------
!
      DTETAR=DEUPI/DBLE(NDIRE)
      SEUIL =1.D-10
!
      DO IP=1,NPOIN2
        COSMOY=0.D0
        SINMOY=0.D0
!
!-----C-------------------------------------------------------C
!-----C  SUMS UP THE DISCRETISED PART OF THE SPECTRUM         C
!-----C-------------------------------------------------------C
!
        DO JF=1,NF
!
          DFDTET=DFREQ(JF)*DTETAR
!
          TAUXC=0.D0
          TAUXS=0.D0
!
          DO JP=1,NDIRE
            AUXC=COSTET(JP)*DFDTET
            AUXS=SINTET(JP)*DFDTET
            TAUXC=TAUXC+F(IP,JP,JF)*AUXC
            TAUXS=TAUXS+F(IP,JP,JF)*AUXS
          ENDDO
!
          COSMOY=COSMOY+TAUXC
          SINMOY=SINMOY+TAUXS
!
        ENDDO                   ! JF
!
!-----C-------------------------------------------------------------C
!-----C  TAKES THE HIGH FREQUENCY PART INTO ACCOUNT (OPTIONAL)      C
!-----C-------------------------------------------------------------C
!
        IF(TAILF.GT.1.D0) THEN
          COEFT=FREQ(NF)/((TAILF-1.D0)*DFREQ(NF))
          COSMOY=COSMOY+TAUXC*COEFT
          SINMOY=SINMOY+TAUXS*COEFT
        ENDIF
!
!-----C-------------------------------------------------------------C
!-----C  COMPUTES THE MEAN DIRECTION                                C
!-----C  (IN RADIANS BETWEEN 0 AND 2.PI)                            C
!-----C-------------------------------------------------------------C
!
        IF(ABS(SINMOY).LT.SEUIL.AND.
     &       ABS(COSMOY).LT.SEUIL) THEN
          TETAM(IP) = 0.D0
        ELSE
          TETAM(IP)=ATAN2(SINMOY,COSMOY)
          IF(TETAM(IP).LT.0.D0) TETAM(IP)=TETAM(IP)+DEUPI
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
