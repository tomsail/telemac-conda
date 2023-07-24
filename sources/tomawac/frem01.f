!                   *****************
                    SUBROUTINE FREM01
!                   *****************
!
     &( FM01  , F     , NF    , NDIRE , NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE MEAN SPECTRAL FREQUENCY FM01 FOR ALL
!+                THE NODES IN THE 2D MESH. THIS FREQUENCY IS DEFINED
!+                FROM THE M0 AND M1 SPECTRAL MOMENTUM.
!code
!+                     SOMME(  F(FREQ,TETA)*FREQ DFREQ DTETA  )
!+             FM01 =  ----------------------------------------
!+                     SOMME(  F(FREQ,TETA) DFREQ DTETA  )
!
!note     THE HIGH-FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!+         IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 2.
!
!history  P. THELLIER; M. BENOIT
!+        09/02/95
!+        V1P0
!+   CREATED
!
!history  M. BENOIT
!+        05/07/96
!+        V1P2
!+   MODIFIED
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
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AUX1           |<->| WORK TABLE
!| AUX2           |<->| WORK TABLE
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FM01           |<--| MEAN FREQUENCIES F01
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, FREQ, DFREQ, TAILF
!
      USE INTERFACE_TOMAWAC, EX_FREM01 => FREM01
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER,          INTENT(IN) ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FM01(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION SEUIL , DTETAR, AUX3  , AUX4, AUX1, AUX2
!
!
      SEUIL = 1.D-20
      DTETAR= DEUPI/DBLE(NDIRE)
      DO IP = 1,NPOIN2
        AUX1 = 0.D0
        AUX2 = 0.D0
!
!-----C-------------------------------------------------------C
!-----C SUMS UP THE CONTRIBUTIONS FOR THE DISCRETISED PART OF THE SPECTRUM     C
!-----C-------------------------------------------------------C
        DO JF = 1,NF-1
          AUX3=DTETAR*DFREQ(JF)
          AUX4=AUX3*FREQ(JF)
          DO JP = 1,NDIRE
            AUX1 = AUX1 + F(IP,JP,JF)*AUX4
            AUX2 = AUX2 + F(IP,JP,JF)*AUX3
          ENDDO                ! JP
        ENDDO                   ! JF
!
!-----C-------------------------------------------------------------C
!-----C (OPTIONALLY) TAKES INTO ACCOUNT THE HIGH-FREQUENCY PART     C
!-----C-------------------------------------------------------------C
        IF (TAILF.GT.2.D0) THEN
          AUX3=DTETAR*(DFREQ(NF)+FREQ(NF)/(TAILF-1.D0))
          AUX4=DTETAR*(DFREQ(NF)+FREQ(NF)/(TAILF-2.D0))*FREQ(NF)
        ELSE
          AUX3=DTETAR*DFREQ(NF)
          AUX4=AUX3*FREQ(NF)
        ENDIF
        DO JP = 1,NDIRE
          AUX1 = AUX1 + F(IP,JP,NF)*AUX4
          AUX2 = AUX2 + F(IP,JP,NF)*AUX3
        ENDDO                   ! JP
!
!-----C-------------------------------------------------------------C
!-----C COMPUTES THE MEAN FREQUENCY                                 C
!-----C-------------------------------------------------------------C
        IF (AUX2.LT.SEUIL) THEN
          FM01(IP) = SEUIL
        ELSE
          FM01(IP) = AUX1/AUX2
        ENDIF
      ENDDO ! IP
!
      RETURN
      END
