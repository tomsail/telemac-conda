!                   *******************
                    SUBROUTINE MAXSLOPE
!                   *******************
!
     &(SLOPE,ZF,ZR,XEL,YEL,NELEM,NELMAX,NPOIN,IKLE,EVOL,UNSV2D,MESH,
     & ZFCL_MS,AVAIL,NOMBLAY,NSICLA)
!
!***********************************************************************
! SISYPHE   V7P2                                   14/11/2016
!***********************************************************************
!
!brief    COLLAPSE OF SAND WITH A SLOPE GREATER THAN A
!+                STABILITY CRITERION.
!+        For more explanation see release notes 5.8
!
!history  J-M HERVOUET (LNH)
!+        16/11/2007
!+        V5P8
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        08/03/2013
!+        V6P3
!+   Now possible with several classes of sediment.
!
!history  MICHIEL KNAAPEN (HRW)
!+        08/12/2016
!+        V7P2
!+        Improvements of the implementation of the morphological factor
!+        in the subroutine. Solution of stability issues. For futher
!+        details see the documentation.
!+
!history R KOPMANN (BAW)
!+        18/05/2018
!+        V7P3
!+   Angle will reduced to avoid changes below active layer only in case
!+   of multi grain
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EVOL           |<->| WORK ARRAY, THEN EVOLUTION DUE TO SLIDE
!| IKLE           |-->| CONNECTIVITY TABLE
!| MESH           |-->| MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| SLOPE          |-->| MAXIMUM SLOPE IN DEGREES
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASES
!| XEL,YEL        |-->| MESH COORDINATES PER ELEMENT
!| ZF             |<->| BOTTOM THAT WILL BE MODIFIED
!| ZR             |-->| NON ERODABLE BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : MOFAC
!
      USE INTERFACE_SISYPHE, EX_MAXSLOPE => MAXSLOPE
!
      USE DECLARATIONS_SISYPHE, ONLY : ES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN,NOMBLAY,NSICLA
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3)
!
      DOUBLE PRECISION, INTENT(IN   ) :: SLOPE
      DOUBLE PRECISION, INTENT(INOUT) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: EVOL,ZFCL_MS
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D
      TYPE(BIEF_MESH) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I,IG1,IG2,IR1,IR2,J
      DOUBLE PRECISION X2,X3,Y2,Y3,Z2,Z3,A,B,L,ZC,DEUXSURF,TANSL
      DOUBLE PRECISION Q(3),QG1,QG2,QR1,QR2
      DOUBLE PRECISION PI
      DOUBLE PRECISION EZ1,EZ2,EZ3
!
      LOGICAL CASE2
!
      INTRINSIC SQRT,MIN,MAX,TAN
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
      TANSL = TAN( PI*SLOPE/180.D0 )
!
!     INITIALISES THE RIGHT-HAND SIDE EVOL TO ZERO
!
      CALL CPSTVC(UNSV2D,EVOL)
      CALL OS('X=0     ',X=EVOL)
!
!     ONE CLASS VERSION
!
      IF(NSICLA.EQ.1) THEN
!
!       LOOP ON ELEMENTS
!
        DO IELEM=1,NELEM
!
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
!
          X2=XEL(IELEM,2)
          X3=XEL(IELEM,3)
          Y2=YEL(IELEM,2)
          Y3=YEL(IELEM,3)
          Z2=ZF(I2)-ZF(I1)
          Z3=ZF(I3)-ZF(I1)
!
!         TWICE THE TRIANGLE AREA
!
          DEUXSURF=X2*Y3-X3*Y2
!
!         AVERAGE BOTTOM IN THE ELEMENT
!
          ZC=(ZF(I1)+ZF(I2)+ZF(I3))/3.D0
!
!         COMPONENTS OF BOTTOM GRADIENT
!
          A=(Z2*Y3-Z3*Y2)/DEUXSURF
          B=(Z3*X2-Z2*X3)/DEUXSURF
!
!         CORRECTING FACTOR ON SLOPE
!
          L=MIN(1.D0,TANSL/MAX(SQRT(A**2+B**2),1.D-8))
!
!         L LIMITED DUE TO NON-ERODABLE BEDS : ZF MUST NOT GO BELOW ZR
!
          IF(ZF(I1).GT.ZC) L=MAX(L,(ZR(I1)-ZC)/MAX(ZF(I1)-ZC,1.D-8))
          IF(ZF(I2).GT.ZC) L=MAX(L,(ZR(I2)-ZC)/MAX(ZF(I2)-ZC,1.D-8))
          IF(ZF(I3).GT.ZC) L=MAX(L,(ZR(I3)-ZC)/MAX(ZF(I3)-ZC,1.D-8))
!
!         BUILDS THE RIGHT-HAND SIDE
!
!         HERE THE EVOLUTIONS ARE MULTIPLIED BY SURFAC/3
!         BECAUSE THE REAL EVOLUTION TAKING INTO ACCOUNT OTHER ELEMENTS
!         WILL NEED A FACTOR (SURFAC/3)/(INTEGRAL OF BASIS)
!
          EVOL%R(I1)=EVOL%R(I1)+(1.D0-L)*(ZC-ZF(I1))*DEUXSURF/6.D0
          EVOL%R(I2)=EVOL%R(I2)+(1.D0-L)*(ZC-ZF(I2))*DEUXSURF/6.D0
          EVOL%R(I3)=EVOL%R(I3)+(1.D0-L)*(ZC-ZF(I3))*DEUXSURF/6.D0
!
        ENDDO
!
      ELSE
!
!       MULTI-CLASS VERSION
!
!       INITIALING TO 0. THE EVOLUTIONS DUE TO SLIDE FOR EACH CLASS
!
        DO I=1,NSICLA
          CALL OS('X=0     ',X=ZFCL_MS%ADR(I)%P)
        ENDDO
!
!       LOOP ON ELEMENTS
!
        DO IELEM=1,NELEM
!
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
!
          X2=XEL(IELEM,2)
          X3=XEL(IELEM,3)
          Y2=YEL(IELEM,2)
          Y3=YEL(IELEM,3)
          Z2=ZF(I2)-ZF(I1)
          Z3=ZF(I3)-ZF(I1)
!
!         TWICE THE TRIANGLE AREA
!
          DEUXSURF=X2*Y3-X3*Y2
!
!         AVERAGE BOTTOM IN THE ELEMENT
!
          ZC=(ZF(I1)+ZF(I2)+ZF(I3))/3.D0
!
!         COMPONENTS OF BOTTOM GRADIENT
!
          A=(Z2*Y3-Z3*Y2)/DEUXSURF
          B=(Z3*X2-Z2*X3)/DEUXSURF
!
!         CORRECTING FACTOR ON SLOPE
!
          L=MIN(1.D0,TANSL/MAX(SQRT(A**2+B**2),1.D-8))
!
!         L LIMITED DUE TO NON-ERODABLE BEDS AND ACTIVE LAYER THICKNESS:
!         ZF MUST NOT GO BELOW ZR OR ACTIVE LAYER
!
!          IF(ZF(I1).GT.ZC) L=MAX(L,(ZR(I1)-ZC)/MAX(ZF(I1)-ZC,1.D-8))
!          IF(ZF(I2).GT.ZC) L=MAX(L,(ZR(I2)-ZC)/MAX(ZF(I2)-ZC,1.D-8))
!          IF(ZF(I3).GT.ZC) L=MAX(L,(ZR(I3)-ZC)/MAX(ZF(I3)-ZC,1.D-8))
          EZ1 = MAX(ZR(I1),ZF(I1)-ES(I1,1))
          EZ2 = MAX(ZR(I2),ZF(I2)-ES(I2,1))
          EZ3 = MAX(ZR(I3),ZF(I3)-ES(I3,1))
          IF(ZF(I1).GT.ZC) L=MAX(L,(EZ1-ZC)/MAX(ZF(I1)-ZC,1.D-8))
          IF(ZF(I2).GT.ZC) L=MAX(L,(EZ2-ZC)/MAX(ZF(I2)-ZC,1.D-8))
          IF(ZF(I3).GT.ZC) L=MAX(L,(EZ3-ZC)/MAX(ZF(I3)-ZC,1.D-8))
!
!         BUILDS THE RIGHT-HAND SIDE
!
!         HERE THE EVOLUTIONS ARE MULTIPLIED BY SURFAC/3
!         BECAUSE THE REAL EVOLUTION TAKING INTO ACCOUNT OTHER ELEMENTS
!         WILL NEED A FACTOR (SURFAC/3)/(INTEGRAL OF BASIS)
!
!         FIRST IN TERMS OF QUANTITIES BROUGHT TO POINTS
!
          Q(1)=(1.D0-L)*(ZC-ZF(I1))*DEUXSURF/6.D0
          Q(2)=(1.D0-L)*(ZC-ZF(I2))*DEUXSURF/6.D0
          Q(3)=(1.D0-L)*(ZC-ZF(I3))*DEUXSURF/6.D0
!
          EVOL%R(I1)=EVOL%R(I1)+Q(1)
          EVOL%R(I2)=EVOL%R(I2)+Q(2)
          EVOL%R(I3)=EVOL%R(I3)+Q(3)
!
!         TAKING INTO ACCOUNT THE QUANTITIES TO UPDATE ZFCL_MS
!         IG1 AND IG2 : POINTS THAT GIVE
!         IR1 AND IR2 : POINTS THAT RECEIVE
!         CASE2: TWO POINTS GIVE TO THE THIRD ONE (THE OTHER CASE IS
!                ONE POINT GIVES TO THE TWO OTHERS)
          CASE2=.FALSE.
!
!         PARAMETERISING TO REDUCE THE 6 CASES TO 2
!
          IF(Q(1).GE.0.D0) THEN
            IF(Q(2).GE.0.D0) THEN
!             3 GIVES TO 1 AND 2
              IG1=I3
              QG1=Q(3)
              IR1=I1
              QR1=Q(1)
              IR2=I2
              QR2=Q(2)
            ELSE
              IF(Q(3).GE.0.D0) THEN
!               2 GIVES TO 1 AND 3
                IG1=I2
                QG1=Q(2)
                IR1=I1
                QR1=Q(1)
                IR2=I3
                QR2=Q(3)
              ELSE
!               2 AND 3 GIVE TO 1
                IG1=I2
                QG1=Q(2)
                IG2=I3
                QG2=Q(3)
                IR1=I1
                QR1=Q(1)
                CASE2=.TRUE.
              ENDIF
            ENDIF
          ELSE
            IF(Q(2).GT.0.D0) THEN
              IF(Q(3).GT.0.D0) THEN
!               1 GIVES TO 2 AND 3
                IG1=I1
                QG1=Q(1)
                IR1=I2
                QR1=Q(2)
                IR2=I3
                QR2=Q(3)
              ELSE
!               1 AND 3 GIVE TO 2
                IG1=I1
                QG1=Q(1)
                IG2=I3
                QG2=Q(3)
                IR1=I2
                QR1=Q(2)
                CASE2=.TRUE.
              ENDIF
            ELSE
!             1 AND 2 GIVE TO 3
              IG1=I1
              QG1=Q(1)
              IG2=I2
              QG2=Q(2)
              IR1=I3
              QR1=Q(3)
              CASE2=.TRUE.
            ENDIF
          ENDIF
!
          IF(CASE2) THEN
!
!           THE TWO DONNORS CASE : IG1 AND IG2 GIVE TO IR1
!           ZFCL_MS IS HERE VOLUMES
!
            DO I=1,NSICLA
              ZFCL_MS%ADR(I)%P%R(IG1)=ZFCL_MS%ADR(I)%P%R(IG1)
     &                               +QG1*AVAIL(IG1,1,I)
              ZFCL_MS%ADR(I)%P%R(IG2)=ZFCL_MS%ADR(I)%P%R(IG2)
     &                               +QG2*AVAIL(IG2,1,I)
              ZFCL_MS%ADR(I)%P%R(IR1)=ZFCL_MS%ADR(I)%P%R(IR1)
     &                               -QG1*AVAIL(IG1,1,I)
     &                               -QG2*AVAIL(IG2,1,I)
            ENDDO
!
          ELSE
!
!           THE ONE DONNOR CASE : IG1 GIVES TO IR1 AND IR2
!           ZFCL_MS IS HERE VOLUMES
!
            DO I=1,NSICLA
              ZFCL_MS%ADR(I)%P%R(IG1)=ZFCL_MS%ADR(I)%P%R(IG1)
     &                               +QG1*AVAIL(IG1,1,I)
              ZFCL_MS%ADR(I)%P%R(IR1)=ZFCL_MS%ADR(I)%P%R(IR1)
     &                               +QR1*AVAIL(IG1,1,I)
              ZFCL_MS%ADR(I)%P%R(IR2)=ZFCL_MS%ADR(I)%P%R(IR2)
     &                               +QR2*AVAIL(IG1,1,I)
            ENDDO
!
          ENDIF
!
        ENDDO
!
!       ADDING VOLUMES IN PARALLEL
!
        IF(NCSIZE.GT.1) THEN
          DO I=1,NSICLA
            CALL PARCOM(ZFCL_MS%ADR(I)%P,2,MESH)
          ENDDO
        ENDIF
!
!       FINAL DIVISION BY THE INTEGRAL OF BASES: VOLUMES CHANGED INTO
!       BED VARIATIONS (LIKE EVOL BELOW)
!
        DO I=1,NSICLA
          DO J=1,NPOIN
            ZFCL_MS%ADR(I)%P%R(J)=ZFCL_MS%ADR(I)%P%R(J)*
     &                UNSV2D%R(J)/MOFAC/10.D0
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FINAL RESOLUTION
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(EVOL,2,MESH)
      ENDIF
!
!     FINAL DIVISION BY THE INTEGRAL OF BASES: QUANTITIES CHANGED INTO
!     ELEVATIONS
!
      DO I=1,NPOIN
        EVOL%R(I)=EVOL%R(I)*UNSV2D%R(I)/MOFAC/10.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
