!                   *****************
                    SUBROUTINE QWIND2
!                   *****************
!
     &( TSTOT , TSDER , F     , XK    , USOLD , USNEW , TWOLD , TWNEW ,
     &  NF    , NDIRE , NPOIN2, USN   , USO   )
!
!***********************************************************************
! TOMAWAC   V6P3                                   27/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE WAVE GENERATION
!+               (BY WIND) SOURCE TERM BASED ON SNYDER ET AL. (1981),
!+                MODIFIED BY KOMEN ET AL. (1984) TO MAKE USE OF THE
!+                FRICTION VELOCITY U* INSTEAD OF THE U5 VELOCITY
!+               (MEASURED 5 METERS ABOVE) FOR THE WIND.
!+
!+            THIS GENERATION THEORY IS IDENTICAL TO THAT IN WAM-CYCLE 3.
!
!reference  SNYDER ET AL. (1981) :
!+                     "ARRAY MEASUREMENTS OF ATMOSPHERIC PRESSURE
!+                      FLUCTUATIONS ABOVE SURFACE GRAVITY WAVES".
!+                      JOURNAL OF FLUID MECH., VOL 102., PP 1-59.
!reference KOMEN ET AL.  (1984) :
!+                     "ON THE EXISTENCE OF A FULLY DEVELOPED
!+                      WINDSEA SPECTRUM". JPO, VOL 14, PP 1271-1285.
!
!history  M. BENOIT (EDF/DER/LNH)
!+        26/03/96
!+        V1P1
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
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF/LNHE)
!+        23/12/2012
!+        V6P3
!+   A first optimisation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TWNEW          |-->| WIND DIRECTION AT TIME N+1
!| TWOLD          |-->| WIND DIRECTION AT TIME N
!| USN            |<--| WORK TABLE
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USO            |<--| WORK TABLE
!| USOLD          |-->| FRICTION VELOCITY AT TIME N
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,USDPI,ROAIR,ROEAU,CIMPLI,
     &                TETA, FREQ
!FROM TOMAWAC MODULE
! CIMPLI          IMPLICITATION COEFFICIENT FOR SOURCE TERMS
      USE INTERFACE_TOMAWAC, EX_QWIND2 => QWIND2
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: TWOLD(NPOIN2),TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: USO(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT):: USN(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF),XK(NPOIN2,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JP,JF,IP
      DOUBLE PRECISION C1,DIREC,CONST,COEPHAS,SURDEUPIFREQ,BETAO,BETAN
!
!-----------------------------------------------------------------------
!
      C1 = 0.25D0 * (ROAIR/ROEAU) * DEUPI
!
!     ARRAYS DEPENDING ONLY ON POINTS AND DIRECTION
!     COULD BE OPTIMISED MORE BY DECOMPOSING THE COS...
!
      DO JP=1,NDIRE
        DIREC=TETA(JP)
        DO IP=1,NPOIN2
          USO(IP,JP)=28.D0*USOLD(IP)*COS(DIREC-TWOLD(IP))
          USN(IP,JP)=28.D0*USNEW(IP)*COS(DIREC-TWNEW(IP))
        ENDDO
      ENDDO
!
!     LOOP ON THE DISCRETISED DIRECTIONS
!
      DO JF=1,NF
        CONST=C1*FREQ(JF)
        SURDEUPIFREQ=USDPI/FREQ(JF)
        DO JP=1,NDIRE
!
!         COMPUTES THE PROPORTIONALITY FACTORS BETA
!         AND TAKES THE SOURCE TERM INTO ACCOUNT
!
          DO IP=1,NPOIN2
            COEPHAS = XK(IP,JF)*SURDEUPIFREQ
            BETAO=MAX(USO(IP,JP)*COEPHAS-1.D0,0.D0)*CONST
            BETAN=MAX(USN(IP,JP)*COEPHAS-1.D0,0.D0)*CONST
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)
     &                      + (BETAO+CIMPLI*(BETAN-BETAO))*F(IP,JP,JF)
            TSDER(IP,JP,JF)=TSDER(IP,JP,JF)+BETAN
          ENDDO
!
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
