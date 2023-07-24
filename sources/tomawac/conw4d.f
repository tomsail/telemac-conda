!                   *****************
                    SUBROUTINE CONW4D
!                   *****************
!
     &(CX,CY,CT,CF,XK,CG, NPOIN2,NDIRE,JF,NF)
!
!***********************************************************************
! TOMAWAC   V7P1
!***********************************************************************
!
!brief    COMPUTES THE ADVECTION FIELD.
!
!warning  IN THIS CASE THE X AXIS IS VERTICAL ORIENTED UPWARDS AND
!+            THE Y AXIS IS HORIZONTAL ORIENTED TOWARDS THE RIGHT;
!+            TETA IS THE DIRECTION WRT NORTH, CLOCKWISE
!
!history  F MARCOS (LNH)
!+        01/02/95
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
!+        14/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF-LNHE)
!+        27/11/2012
!+        V6P3
!+   Optimisation (loops on NPOIN2 and NDIRE swapped to get smaller
!+   strides, work array TRA01 differently used, etc.)
!
!history  J-M HERVOUET (EDF-LNHE)
!+        08/01/2014
!+        V7P0
!+   Was always called with COURAN=.TRUE., with heavy tests on this
!+   variable. COURAN now considered=.TRUE. and suppressed.
!
!history  J-M HERVOUET (EDF-LNHE)
!+        16/11/2015
!+        V7P1
!+   DZHDT always included in formulas, even if depth not varying. This
!+   assumes that DZHDT is duly set to 0 in such cases. Testing MAREE
!+   was a bug since it is true only with depth given in a file...
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COSF           |-->| COSINE OF THE LATITUDES OF THE POINTS 2D
!| COSTET         |-->| COSINE OF TETA ANGLE
!| CT             |<--| ADVECTION FIELD ALONG TETA
!| CX             |<--| ADVECTION FIELD ALONG X(OR PHI)
!| CY             |<--| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| CF             |<--| ADVECTION FIELD ALONG FREQUENCY
!| DEPTH          |-->| WATER DEPTH
!| DVY            |-->| DERIVATIVE OF CURRENT SPEED DU/DX
!| DVX            |-->| DERIVATIVE OF CURRENT SPEED DU/DY
!| DUY            |-->| DERIVATIVE OF CURRENT SPEED DV/DX
!| DUX            |-->| DERIVATIVE OF CURRENT SPEED DV/DY
!| DZHDT          |-->| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| DZY            |-->| SEA BOTTOM SLOPE ALONG X
!| DZX            |-->| SEA BOTTOM SLOPE ALONG Y
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| JF             |-->| INDEX OF THE FREQUENCY
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| SINTET         |-->| SINE OF TETA ANGLE
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| TGF            |-->| TANGENT OF THE LATITUDES OF THE POINTS 2D
!| U              |-->| CURRENT SPEED ALONG X
!| V              |-->| CURRENT SPEED ALONG Y
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,USDPI,SR,RADDEG,GRAVIT,
     &PROINF, SPHE, COSF,TGF,DEPTH,DZHDT,DZY,DZX,FREQ,COSTET,SINTET,
     &               UC,     VC,    DUX,  DUY,   DVX, DVY, TRA01
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_CONW4D => CONW4D
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2,JF
!
      DOUBLE PRECISION, INTENT(INOUT) :: CX(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CY(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CF(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF),XK(NPOIN2,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,IPOIN
      DOUBLE PRECISION GSQP,SRCF,TFSR,DDDN,LSDUDN,LSDUDS
      DOUBLE PRECISION USGD,DEUKD,TR1,TR2
!
!***********************************************************************
!
      GSQP=GRAVIT/(2.D0*DEUPI)
!
!-----------------------------------------------------------------------
!     INFINITE WATER DEPTH ...
!-----------------------------------------------------------------------
!
      IF(PROINF) THEN
!
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
        IF(.NOT.SPHE) THEN
!
          DO IP=1,NDIRE
            TR1=GSQP/FREQ(JF)*COSTET(IP)
            TR2=GSQP/FREQ(JF)*SINTET(IP)
            DO IPOIN=1,NPOIN2
              LSDUDN= SINTET(IP)*
     &              (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &              + COSTET(IP)*
     &              ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              LSDUDS= COSTET(IP)*
     &               (COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &              + SINTET(IP)*
     &               (COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              CX(IPOIN,IP,JF)=TR2+UC(IPOIN)
              CY(IPOIN,IP,JF)=TR1+VC(IPOIN)
              CT(IPOIN,IP,JF)=-LSDUDN
              CF(IPOIN,IP,JF)=-CG(IPOIN,JF)*XK(IPOIN,JF)*LSDUDS*USDPI
            ENDDO
          ENDDO
!
!       ----------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
        ELSE
!
          DO IP=1,NDIRE
            TR1=GSQP/FREQ(JF)*COSTET(IP)
            TR2=GSQP/FREQ(JF)*SINTET(IP)
            DO IPOIN=1,NPOIN2
              SRCF=SR/COSF(IPOIN)
              LSDUDN= SINTET(IP)*SR*
     &               (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &               + COSTET(IP)*SRCF*
     &               ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              LSDUDS= COSTET(IP)*SR*
     &               (COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &              + SINTET(IP)*SRCF*
     &               (COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              CX(IPOIN,IP,JF)=(TR2+UC(IPOIN))*RADDEG*SRCF
              CY(IPOIN,IP,JF)=(TR1+VC(IPOIN))*RADDEG*SR
              CT(IPOIN,IP,JF)=TR2*TGF(IPOIN)*SR - LSDUDN*RADDEG
              CF(IPOIN,IP,JF)= - LSDUDS*RADDEG*
     &                        CG(IPOIN,JF)*XK(IPOIN,JF)*USDPI
            ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!     FINITE WATER DEPTH ....
!-----------------------------------------------------------------------
!
      ELSE
!
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
        IF(.NOT.SPHE) THEN
!
          DO IPOIN=1,NPOIN2
            DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
            IF(DEUKD.GT.7.D2) THEN
              TRA01(IPOIN) = 0.D0
            ELSE
              TRA01(IPOIN) = DEUPI*FREQ(JF)/SINH(DEUKD)
            ENDIF
          ENDDO
!
          DO IP=1,NDIRE
            DO IPOIN=1,NPOIN2
              DDDN=-SINTET(IP)*DZY(IPOIN)+COSTET(IP)*DZX(IPOIN)
              CY(IPOIN,IP,JF)=CG(IPOIN,JF)*COSTET(IP)
              CX(IPOIN,IP,JF)=CG(IPOIN,JF)*SINTET(IP)
              CT(IPOIN,IP,JF)=-TRA01(IPOIN)*DDDN
            ENDDO
          ENDDO
!
          DO IPOIN=1,NPOIN2
            DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
            IF(DEUKD.GT.7.D2) THEN
              TRA01(IPOIN)=0.D0
            ELSE
              TRA01(IPOIN)=XK(IPOIN,JF)*DEUPI*FREQ(JF)/SINH(DEUKD)
            ENDIF
          ENDDO
!
          DO IP=1,NDIRE
            DO IPOIN=1,NPOIN2
              LSDUDN= SINTET(IP)*
     &             (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &            + COSTET(IP)*
     &             ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              LSDUDS= COSTET(IP)*
     &             (COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &            + SINTET(IP)*
     &             (COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              USGD=VC(IPOIN)*DZY(IPOIN)+UC(IPOIN)*DZX(IPOIN)
              CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF) + UC(IPOIN)
              CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF) + VC(IPOIN)
              CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF) - LSDUDN
              CF(IPOIN,IP,JF)= (TRA01(IPOIN)*(USGD+DZHDT(IPOIN))
     &               - LSDUDS*CG(IPOIN,JF)*XK(IPOIN,JF))*USDPI
            ENDDO
          ENDDO
!
!       --------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       --------------------------------------------------------------
!
        ELSE
!
          DO IPOIN=1,NPOIN2
            DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
            IF(DEUKD.GT.7.D2) THEN
              TRA01(IPOIN) = 0.D0
            ELSE
              TRA01(IPOIN) = DEUPI*FREQ(JF)/SINH(DEUKD)
            ENDIF
          ENDDO
!
          DO IP=1,NDIRE
            DO IPOIN=1,NPOIN2
              SRCF=SR/COSF(IPOIN)
              TFSR=TGF(IPOIN)*SR
              DDDN=-SINTET(IP)*DZY(IPOIN)*SR+COSTET(IP)*DZX(IPOIN)*SRCF
              CY(IPOIN,IP,JF)=(CG(IPOIN,JF)*COSTET(IP))*SR*RADDEG
              CX(IPOIN,IP,JF)=(CG(IPOIN,JF)*SINTET(IP))*SRCF*RADDEG
              CT(IPOIN,IP,JF)=CG(IPOIN,JF)*SINTET(IP)*TFSR
     &                                   -TRA01(IPOIN)*DDDN*RADDEG
            ENDDO
          ENDDO
!
          DO IPOIN=1,NPOIN2
            DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
            IF(DEUKD.GT.7.D2) THEN
              TRA01(IPOIN)=0.D0
            ELSE
              TRA01(IPOIN)=XK(IPOIN,JF)*DEUPI*FREQ(JF)/SINH(DEUKD)
            ENDIF
          ENDDO
!
          DO IP=1,NDIRE
            DO IPOIN=1,NPOIN2
              SRCF=SR/COSF(IPOIN)
              LSDUDN= SINTET(IP)*SR*
     &             (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &            + COSTET(IP)*SRCF*
     &             ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              LSDUDS= COSTET(IP)*SR*
     &             ( COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &            + SINTET(IP)*SRCF*
     &             ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              USGD=VC(IPOIN)*DZY(IPOIN)*SR
     &            +UC(IPOIN)*DZX(IPOIN)*SRCF
              CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF)+VC(IPOIN)*SR*RADDEG
              CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF)+UC(IPOIN)*SRCF*RADDEG
              CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF)-LSDUDN*RADDEG
              CF(IPOIN,IP,JF)=
     &         (TRA01(IPOIN)*(USGD*RADDEG+DZHDT(IPOIN))
     &        -LSDUDS*RADDEG*CG(IPOIN,JF)*XK(IPOIN,JF))*USDPI
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

