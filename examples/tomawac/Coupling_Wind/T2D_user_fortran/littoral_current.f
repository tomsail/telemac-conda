!                   **************************
                    SUBROUTINE LITTORAL_CURRENT
!                   **************************
     &    (H,UBOR,VBOR,U,V,LIUBOR,XNEBOR,YNEBOR,NBOR,NPTFR,NPTFR2,
     &     KENT,KENTU)
!
!
!***********************************************************************
! TELEMAC2D   V6P2                                   28/10/2010
!***********************************************************************
!
!brief    FORCING OF THE LITTORAL CURRENT ALONG THE BOUNDARY
!+        ASSUMING : BOTTOM FRICTION = WAVE INDUCED FORCING
!
!history  C.VILLARET  (HRW)
!+        20/09/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KENT           |-->| FLAG: IMPOSED FLOW RATE
!| KENTU          |-->| FLAG: IMPOSED VELOCITY
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| H              |-->| WATER DEPTH
!| UBOR           |<->| X-COMPONENT OF PRESCRIBED VELOCITY
!| VBOR           |<->| Y-COMPONENT OF PRESCRIBED VELOCITY
!| XNEBOR         |-->| X-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY: FXWAVE, FYWAVE,CF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

      INTEGER, INTENT(IN) :: NPTFR,NPTFR2
      INTEGER, INTENT(IN) :: KENT,KENTU
      INTEGER, INTENT(IN) :: LIUBOR(NPTFR)
      INTEGER, INTENT(IN) :: NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR,2)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,U,V
!
      INTEGER K
      DOUBLE PRECISION FORCE, UMAG,FX,FY,EPS,CCF,HH,HBREAK
!
      EPS= 1.D-04
!
! BREAKING DEPTH : OUTSIDE THE BREAK ZONE ,FORCING IS SET TO ZERO
!
      HBREAK=2.5D0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      DO K=1,NPTFR
        IF(LIUBOR(K).EQ.KENTU.OR.LIUBOR(K).EQ.KENT) THEN
          FX=FXWAVE%R(NBOR(K))
          FY=FYWAVE%R(NBOR(K))
          FORCE = -(FX*XNEBOR(K)+FY*YNEBOR(K))
          CCF= MAX(CF%R(NBOR(K)),EPS)
          HH=MAX(H%R(NBOR(K)),0.D0)
          IF(HH.GE.HBREAK) FORCE=0.D0
          UMAG= SQRT(2*ABS(FORCE)*HH/CCF)
          IF (FORCE.GE.EPS) THEN
!           UBOR(K,1) = UBOR(K,1)- XNEBOR(K) * UMAG
!           VBOR(K,1) = VBOR(K,1)- YNEBOR(K) * UMAG
            UBOR(K,1) = - XNEBOR(K) * UMAG
            VBOR(K,1) = - YNEBOR(K) * UMAG

          ELSEIF (FORCE.LE.-EPS) THEN
!           UBOR(K,1) = UBOR(K,1)+XNEBOR(K) * UMAG
!           VBOR(K,1) = VBOR(K,1)+YNEBOR(K) * UMAG
            UBOR(K,1) = +XNEBOR(K) * UMAG
            VBOR(K,1) = +YNEBOR(K) * UMAG
          ELSE
!           UBOR(K,1)= UBOR(K,1)
!           VBOR(K,1)= UBOR(K,1)
            UBOR(K,1)= 0.D0
            VBOR(K,1)= 0.D0

          ENDIF
          U%R(NBOR(K)) = UBOR(K,1)
          V%R(NBOR(K)) = VBOR(K,1)
        ENDIF
      ENDDO
!-----------------------------------------------------------------------
      RETURN
      END

