!                   *****************
                    SUBROUTINE CONWAC
!                   *****************
!
     &( CX, CY, CT, XK, CG, NPOIN2, NDIRE , JF    , NF    )
!
!***********************************************************************
! TOMAWAC   V6P3                                   14/068/2011
!***********************************************************************
!
!brief    COMPUTES THE ADVECTION FIELD (3D WITHOUT CURRENT).
!
!warning  TETA IS THE DIRECTION WRT NORTH, CLOCKWISE
!
!history  M. BENOIT (EDF LNHE)
!+        19/01/2004
!+        V5P4
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments.
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources.
!
!history  G.MATTAROLO (EDF - LNHE)
!+        14/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument.
!
!history  J-M HERVOUET (EDF-LNHE)
!+        27/11/2012
!+        V6P3
!+   Optimisation (loops on NPOIN2 and NDIRE swapped to get smaller
!+   strides, work array TRA01 differently used, etc.).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COSF           |-->| COSINE OF THE LATITUDES OF THE POINTS 2D
!| CY             |<--| ADVECTION FIELD ALONG X(OR PHI)
!| CX             |<--| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| CT             |<--| ADVECTION FIELD ALONG TETA
!| DZY            |-->| SEA BOTTOM SLOPE ALONG X
!| DZX            |-->| SEA BOTTOM SLOPE ALONG Y
!| JF             |-->| INDEX OF THE FREQUENCX
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| PROMIN         |-->| MINIMUM VALUE OF WATER DEPTH
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| TGF            |-->| TANGENT OF THE LATITUDES OF THE POINTS 2D
!| TRA01          |<->| WORK TABLE
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : RADDEG,DEUPI,SR,GRAVIT, PROINF,
     &                    SPHE, PROMIN, DEPTH, COSTET, SINTET, FREQ,
     & COSF  , TGF   , DZX   , DZY, TRA01
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_CONWAC => CONWAC
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2,JF
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CY(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: CX(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: CT(NPOIN2,NDIRE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JP,IP
      DOUBLE PRECISION GSQP,SRCF,TFSR,DDDN,DEUKD,TR1,TR2
!
      GSQP=GRAVIT/(2.D0*DEUPI)
!
      IF(PROINF) THEN
!
!-----------------------------------------------------------------------
!     INFINITE WATER DEPTH ...
!-----------------------------------------------------------------------
!
        IF(.NOT.SPHE) THEN
!
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
          DO JP=1,NDIRE
            TR1=GSQP/FREQ(JF)*COSTET(JP)
            TR2=GSQP/FREQ(JF)*SINTET(JP)
            DO IP=1,NPOIN2
              CY(IP,JP)=TR1
              CX(IP,JP)=TR2
              CT(IP,JP)=0.D0
            ENDDO
          ENDDO
!
        ELSE
!
!       ----------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
          DO JP=1,NDIRE
            TR1=GSQP/FREQ(JF)*COSTET(JP)
            TR2=GSQP/FREQ(JF)*SINTET(JP)
            DO IP=1,NPOIN2
              SRCF=SR/COSF(IP)
              TFSR=TGF(IP)*SR
              CY(IP,JP)=TR1*SR*RADDEG
              CX(IP,JP)=TR2*SRCF*RADDEG
              CT(IP,JP)=TR2*TFSR
            ENDDO
          ENDDO
!
        ENDIF
!
      ELSE
!
!-----------------------------------------------------------------------
!     FINITE WATER DEPTH ....
!-----------------------------------------------------------------------
!
        DO IP=1,NPOIN2
          DEUKD=2.D0*XK(IP,JF)*DEPTH(IP)
          IF(DEUKD.GT.7.D2) THEN
            TRA01(IP)=0.D0
          ELSE
            TRA01(IP)=DEUPI*FREQ(JF)/SINH(DEUKD)
          ENDIF
        ENDDO
!
        IF(.NOT.SPHE) THEN
!
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
          DO JP=1,NDIRE
            DO IP=1,NPOIN2
              IF(DEPTH(IP).GT.PROMIN) THEN
                DDDN=-SINTET(JP)*DZY(IP)+COSTET(JP)*DZX(IP)
                CY(IP,JP)=CG(IP,JF)*COSTET(JP)
                CX(IP,JP)=CG(IP,JF)*SINTET(JP)
                CT(IP,JP)=-TRA01(IP)*DDDN
              ELSE
                CY(IP,JP)=0.D0
                CX(IP,JP)=0.D0
                CT(IP,JP)=0.D0
              ENDIF
            ENDDO
          ENDDO
!
        ELSE
!
!       ----------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
          DO JP=1,NDIRE
            DO IP=1,NPOIN2
              IF(DEPTH(IP).GT.PROMIN) THEN
                SRCF=SR/COSF(IP)
                TFSR=SR*TGF(IP)
                DDDN=-SINTET(JP)*DZY(IP)*SR+COSTET(JP)*DZX(IP)*SRCF
                CY(IP,JP)=(CG(IP,JF)*COSTET(JP))*SR*RADDEG
                CX(IP,JP)=(CG(IP,JF)*SINTET(JP))*SRCF*RADDEG
                CT(IP,JP)=CG(IP,JF)*SINTET(JP)*TFSR
     &                   -TRA01(IP)*DDDN*RADDEG
              ELSE
                CY(IP,JP)=0.0D0
                CX(IP,JP)=0.0D0
                CT(IP,JP)=0.0D0
              ENDIF
            ENDDO
          ENDDO
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
