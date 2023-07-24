!                   *****************
                    SUBROUTINE WPOWER
!                   *****************
!
     &( F     , CG    , NF    , NDIRE , NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   29/06/2011
!***********************************************************************
!
!brief    COMPUTES THE VARIANCE OF THE DIRECTIONAL SPECTRUM FOR
!+                ALL THE NODES IN THE 2D MESH. IT IS COMPUTED BY
!+                INTEGRATION OVER FREQUENCIES AND DIRECTIONS AND CAN
!+                TAKE THE HIGH FREQUENCY PART OF THE SPECTRUM INTO
!+                ACCOUNT.
!
!note     THE HIGH-FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!+         IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 1.
!
!history  M. BENOIT
!+        20/05/2003
!+        V5P3
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
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  G.MATTAROLO (EDF - LNHE)
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| POWER          |<--| WAVE POWER PER METER ALONG WAVE CREST
!| ROEAU          |-->| WATER DENSITY
!| TAILF          |-->| FACTEUR DE QUEUE DU SPECTRE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, GRAVIT, TAILF , ROEAU,
     &    DFREQ, POWER
!
      USE INTERFACE_TOMAWAC, EX_WPOWER => WPOWER
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    ::          NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF)
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER          IP    , JP    , JF
      DOUBLE PRECISION AUX1  , DTETAR, ROGER
!
!
      DTETAR=DEUPI/DBLE(NDIRE)
      ROGER=ROEAU*GRAVIT/1000.D0
      DO IP=1,NPOIN2
        POWER(IP)=0.D0
      ENDDO
!
!-----C-------------------------------------------------------C
!-----C  SUMS UP THE DISCRETISED PART OF THE SPECTRUM         C
!-----C-------------------------------------------------------C
      DO JF=1,NF
        AUX1=DFREQ(JF)*DTETAR
        DO JP=1,NDIRE
          DO IP=1,NPOIN2
            POWER(IP) = POWER(IP) + F(IP,JP,JF)*CG(IP,JF)*AUX1
          ENDDO
        ENDDO
      ENDDO
!
!-----C-------------------------------------------------------------C
!-----C  TAKES THE HIGH FREQUENCY PART INTO ACCOUNT (OPTIONAL)      C
!-----C-------------------------------------------------------------C
!
      IF(TAILF.GT.1.D0) THEN
        AUX1=DTETAR*GRAVIT/(2.D0*DEUPI*TAILF)
        DO JP=1,NDIRE
          DO IP=1,NPOIN2
            POWER(IP)=POWER(IP) + F(IP,JP,NF)*AUX1
          ENDDO
        ENDDO
      ENDIF
!
!-----C-------------------------------------------------------------C
!-----C  CONVERTS TO KW/M  (MULTIPLIES BY RO.G/1000)                C
!-----C-------------------------------------------------------------C
!
      DO IP=1,NPOIN2
        POWER(IP)=POWER(IP)*ROGER
      ENDDO
!
      RETURN
      END
