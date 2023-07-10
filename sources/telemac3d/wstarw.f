!                   *****************
                    SUBROUTINE WSTARW
!                   *****************
!
     & (WW,WSS,WUP,WDOWN,SOMMEW)
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    ENSURES THE TRANSFER OF THE AVERAGE PER LAYER
!+                FROM DZ/DZSTAR*WSTAR TO W
!+                BY SOLVING THE EQUATION:
!code
!+                 DZ            DZ      DZ      DZ
!+           W =  ---- WSTAR + (--- + U --- + V ---)
!+               DZSTAR          DT      DX      DY
!+
!+
!+      INTEGRATED ON EACH LAYER!!!
!+
!+      BEWARE: THIS EQUATION IS WRITTEN IN THE TRANSFORMED MESH!
!+
!+      EQUATION INTEGRATED ON A LAYER, MULTIPLIED BY A 2D TEST
!+      FUNCTION, THEN INTEGRATED ON THE 2D OMEGA DOMAIN!
!+
!+      FOR EACH PLANE IP WITHIN [2,NPLAN-2], SOLVES :
!+
!+      M2D * ( W(IP+1) + W(IP) ) =  2 * M2D * DZW*(IP+1/2)
!+
!+            1
!+         + --- * M2D * (Z(N+1,IP+1)+Z(N+1,IP)-Z(N,IP+1)-Z(N,IP))
!+           DT
!+
!+                /   /Z*(IP+1)       1           DZ      DZ
!+         + 2 * /   /           ----------- ( U --- + V --- ) PSIH
!+              /OM /Z*(IP)      DZ*(IP+1/2)     DX      DY
!+
!+
!+              BEWARE: PSIH IS A 2D LINEAR BASE
!+
!+
!+
!+ ALSO COMPUTES WUP STARTING FROM THE BOTTOM, AND WDOWN STARTING FROM
!+ THE TOP, THEN COMPUTES THE AVERAGE OF BOTH
!+
!+
!+ NOTE : ALTERNATE AND SIMPLER VERSION BY JMH :
!+
!+ 1. INITIALISATION
!+
!+     CALL OS('X=Y     ',X=WW,Y=WSS)
!+
!+ 2. DZ/DT TERM
!+
!+     CALL OS('X=X+CY  ',X=WW,Y=Z3   ,C= 1.D0/DT)
!+     CALL OS('X=X+CY  ',X=WW,Y=ZPROP,C=-1.D0/DT)
!+
!+ 3. HORIZONTAL GRADIENTS FOR Z TERM
!+
!+     DO IPLAN=1,NPLAN
!+       CALL GRAD2D(T2_01,T2_02,ZPROP,IPLAN,SVIDE,
!+    *              UNSV2D,T2_03,IELM2H,MESH2D,MSK,MASKEL)
!+       DO I=1,NPOIN2
!+         IAD=I+(IPLAN-1)*NPOIN2
!+         WW%R(IAD)=WW%R(IAD)+U%R(IAD)*T2_01%R(I)+V%R(IAD)*T2_02%R(I)
!+       ENDDO
!+     ENDDO
!
!note     ZPROP IS HERE USED LIKE ZN.
!
!history  A. DECOENE (INRIA-LNHE)
!+        10/12/04
!+        V5P5
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
!history  J-M HERVOUET (jubilado)
!+        10/09/2017
!+        V7P3
!+   Adaptation to cases where MESH3D%NELMAX is not equal to NELEM3.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SOMMEW         |<->|
!| WDOWN          |<->|
!| WSS            |-->| AVERAGE OF DZ*WSTAR PER LAYER AT N+1
!| WUP            |<->|
!| WW             |<->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN) :: WSS
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WW
      DOUBLE PRECISION,INTENT(INOUT) :: WUP(*),WDOWN(*),SOMMEW(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,IPLANINF,IPLANSUP,NELMAX,IAD,IADINF,IADSUP
      INTEGER IELEM2,IELEM3,I1,I2,I3
!
      DOUBLE PRECISION :: SURDT,SURNPLANMU
!
!=======================================================================
!
!     SOLVES THE LINEAR SYSTEM
!
!=======================================================================
!
      NELMAX=MESH3D%NELMAX
      SURDT=1.D0/DT
      SURNPLANMU=1.D0/(NPLAN-1)
!
! 1.BUILDS THE LUMPED 2D MASS MATRIX
!
      MAT2D%ADR(1)%P%TYPDIA='Q'
      MAT2D%ADR(1)%P%TYPEXT='0'
!
      CALL VECTOR
     & (MAT2D%ADR(1)%P%D, '=', 'MASBAS          ',IELMH,1.D0,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
!
      CALL OS('X=Y     ',X=T2_01,Y=MAT2D%ADR(1)%P%D)
!
      IF(NCSIZE.GT.1) CALL PARCOM(T2_01,2,MESH2D)
      IF(MSK) THEN
        CALL OS('X=1/Y   ',X=T2_01,Y=T2_01,
     &                IOPT=2,INFINI=0.D0,ZERO=1.D-6)
      ELSE
        CALL OS('X=1/Y   ',X=T2_01,Y=T2_01)
      ENDIF
!
!-----------------------------------------------------------------------
!
! 2. COMPUTES W AT THE BOTTOM: IMPOSES THE BOUNDARY CONDITION AT THE BOTTOM:
!
!                           DZF            DZF
!       W(PLAN1) = U(PLAN1) --- + V(PLAN1) ---
!                           DX             DY
!
!     DOES NOT CONSIDER DIRICHLET ON W FOR THE TIME BEING
!
      CALL OV('X=YZ    ', X=WW%R, Y=GRADZF%ADR(1)%P%R, Z=U%R,
     &        DIM1=NPOIN2)
      CALL OV('X=X+YZ  ', X=WW%R, Y=GRADZF%ADR(2)%P%R, Z=V%R,
     &        DIM1=NPOIN2)
      CALL OV('X=Y     ', X=WUP, Y=WW%R, DIM1=NPOIN2)
!
!-----------------------------------------------------------------------
!
! 3. COMPUTES W AT THE FREE SURFACE: IMPOSES THE KINEMATIC CONDITION:
!
!                  DZS            DZS            DZS
!       W(NPLAN) = --- + U(NPLAN) --- + V(NPLAN) ---
!                  DT             DX             DY
!
      IAD = NPLAN*NPOIN2  ! LAST NODE FROM NPLAN
!
      CALL OV('X=YZ    ', X=WW%R(IAD-NPOIN2+1:IAD), Y=GRADZS%ADR(1)%P%R,
     &        Z=U%R(IAD-NPOIN2+1:IAD), DIM1=NPOIN2)
      CALL OV('X=X+YZ  ', X=WW%R(IAD-NPOIN2+1:IAD), Y=GRADZS%ADR(2)%P%R,
     &        Z=V%R(IAD-NPOIN2+1:IAD), DIM1=NPOIN2)
      CALL OV('X=X+Y   ', X=WW%R(IAD-NPOIN2+1:IAD), Y=DSSUDT%R,
     &        DIM1=NPOIN2)
      CALL OV('X=Y     ',X=WDOWN(IAD-NPOIN2+1:IAD),
     &                   Y=WW%R(IAD-NPOIN2+1:IAD), DIM1=NPOIN2)
!
!=======================================================================
!
! 4. BUILDS THE NON-ASSEMBLED VECTOR PSIH * UCONV GRAD(Z)
!
      CALL VECTOR(T3_01, '=', 'VGRADF2         ',IELM3,1.D0,ZPROP,
     &            SVIDE,SVIDE,UCONV,VCONV,SVIDE,MESH3D,MSK,MASKEL)
!
!         THE NON-ASSEMBLED VECTOR IS IN MESH3D%W
!
! 5. ASSEMBLES THE VECTOR PSIH * U GRAD(Z)
!         ON EACH LAYER [IP,IP+1] IN T3_01
!
      DO IPLAN=1,NPLAN-1
        IAD = IPLAN*NPOIN2  ! LAST NODE FROM IPLAN
        CALL OV('X=C     ', X=T3_01%R(IAD-NPOIN2+1:IAD), C=0.D0,
     &          DIM1=NPOIN2)
        DO IELEM2 = 1, NELEM2
          IELEM3 = (IPLAN-1) * NELEM2 + IELEM2
          I1=MESH3D%IKLE%I(IELEM3)
          I2=MESH3D%IKLE%I(IELEM3+NELMAX)
          I3=MESH3D%IKLE%I(IELEM3+2*NELMAX)
          T3_01%R(I1)=T3_01%R(I1)+MESH3D%W%R(IELEM3)
          T3_01%R(I2)=T3_01%R(I2)+MESH3D%W%R(IELEM3+NELMAX)
          T3_01%R(I3)=T3_01%R(I3)+MESH3D%W%R(IELEM3+2*NELMAX)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
! 6. COMPUTES [(W)PLAN_INF + (W)PLAN_SUP] ON THE INTERMEDIATE PLANES
!
!                FOR PLAN_INF = 1 TO NPLAN-1
!
!        => USES T2_02 AND SEM2D%ADR(1) FOR THE SECOND MEMBER
!
      DO IPLAN = 1,NPLAN-1
!
        IPLANINF = IPLAN
        IPLANSUP = IPLAN+1
        IADINF = IPLANINF * NPOIN2
        IADSUP = IPLANSUP * NPOIN2
!
!       1. 2*AVERAGE [DZ W*] FOR LAYER [IPLAN_INF,IPLAN_SUP]
!          T2_02 SHOULD BE HERE OF TYPE 11 (LINEAR TRIANGLE)
        CALL CPSTVC(H,T2_02)
        CALL OV('X=CY    ', X=T2_02%R,
     &          Y=WSS%R(IADINF-NPOIN2+1:IADINF),
     &          C=2.D0, DIM1=NPOIN2)
!
!       2. (1/DT) * [Z(N+1)-Z(N)]PLAN_SUP + [Z(N+1)-Z(N)]PLAN_INF
!
!       2.1. PLAN_SUP
!
        CALL OV('X=X+CY  ', X=T2_02%R,
     &           Y=Z3%R(IADSUP-NPOIN2+1:IADSUP),
     &           C=SURDT, DIM1=NPOIN2)
!
        CALL OV('X=X+CY  ', X=T2_02%R,
     &           Y=ZPROP%R(IADSUP-NPOIN2+1:IADSUP),
     &           C=-SURDT, DIM1=NPOIN2)
!
!       2.2. PLAN_INF
!
        CALL OV('X=X+CY  ', X=T2_02%R,
     &           Y=Z3%R(IADINF-NPOIN2+1:IADINF),
     &           C=SURDT, DIM1=NPOIN2)
!
        CALL OV('X=X+CY  ', X=T2_02%R,
     &           Y=ZPROP%R(IADINF-NPOIN2+1:IADINF),
     &           C=-SURDT, DIM1=NPOIN2)
!
!       3. MULTIPLIES BY THE MASS
!
        CALL OS('X=YZ    ',X=SEM2D%ADR(1)%P,Y=MAT2D%ADR(1)%P%D,
     &                     Z=T2_02)
!
!       4. ADDS PSIH * UCONV GRAD(Z)
!
        CALL OV('X=X+CY  ', X=SEM2D%ADR(1)%P%R,
     &           Y=T3_01%R(IADINF-NPOIN2+1:IADINF),
     &           C=2.D0, DIM1=NPOIN2)
!
!       5. SOLVES THE SYSTEM
!
        IF(NCSIZE.GT.1) CALL PARCOM(SEM2D%ADR(1)%P,2,MESH2D)
!
!       SOLUTION IN (SOMMEW)PLAN_INF
!
        CALL OV('X=YZ    ',SOMMEW(IADINF-NPOIN2+1:IADINF),
     &          SEM2D%ADR(1)%P%R,T2_01%R,0.D0,NPOIN2)
!
      ENDDO
!
!    WUP COMPUTED FROM THE BOUNDARY CONDITION (BOTTOM, UP THE PLANES)
!
!     LOOP ON THE PLANES
!     NOTE FROM ASTRID: 2 TO NPLAN-1 WOULD BE ENOUGH
      DO IPLAN = 2,NPLAN
!
        IADSUP = IPLAN * NPOIN2         ! LAST NODE FROM IPLAN
        IADINF = (IPLAN-1) * NPOIN2     ! LAST NODE FROM (IPLAN-1)
!
!       [ WUP ] IPLAN = - [ WUP ] IPLAN-1 + [(W) IPLAN + (W) IPLAN-1]
!                     = - [ WUP ] IPLAN-1 + [ SOMMEW ] IPLAN-1
        CALL OV('X=Y+CZ  ', X=WUP(IADSUP-NPOIN2+1:IADSUP),
     &         Y=SOMMEW(IADINF-NPOIN2+1:IADINF),
     &         Z=WUP(IADINF-NPOIN2+1:IADINF), C=-1.D0, DIM1=NPOIN2)
!
      ENDDO
!
!    WDOWN COMPUTED FROM THE BOUNDARY CONDITION (FREE SURFACE,
!                                                    DOWN THE PLANES)
!
!     LOOP ON THE PLANES
!     NOTE FROM ASTRID : 1 TO NPLAN - 2 WOULD BE ENOUGH
      DO IPLAN = 1,NPLAN-1
!
!       PLAN_SUP = (NPLAN-IPLAN+1)
!       PLAN_INF = (NPLAN-IPLAN)
!
        IADSUP = (NPLAN-IPLAN+1) * NPOIN2   ! LAST NODE FROM PLAN_SUP
        IADINF = (NPLAN-IPLAN) * NPOIN2     ! LAST NODE FROM PLAN_INF
!
!       [ WDOWN ] PLAN_INF  = - [ WDOWN ] PLAN_SUP + [(W) PLAN_SUP + (W) PLAN_INF]
!                     = - [ WDOWN ] PLAN_SUP  + [ SOMMEW ] PLAN_INF
        CALL OV('X=Y+CZ  ', X=WDOWN(IADINF-NPOIN2+1:IADINF),
     &          Y=SOMMEW(IADINF-NPOIN2+1:IADINF),
     &          Z=WDOWN(IADSUP-NPOIN2+1:IADSUP), C=-1.D0,
     &          DIM1=NPOIN2)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     FINAL W = AVERAGE OF WUP AND WDOWN
!
      DO IPLAN=2,NPLAN-1
        IAD = IPLAN * NPOIN2  ! LAST NODE FROM IPLAN
        CALL OV('X=CY    ', X=WW%R(IAD-NPOIN2+1:IAD),
     &          Y=WDOWN(IAD-NPOIN2+1:IAD),
     &          C=(IPLAN-1.D0)*SURNPLANMU, DIM1=NPOIN2)
        CALL OV('X=X+CY  ', X=WW%R(IAD-NPOIN2+1:IAD),
     &          Y=WUP(IAD-NPOIN2+1:IAD),
     &          C=SURNPLANMU*(NPLAN-IPLAN), DIM1=NPOIN2)
      ENDDO
!
!=======================================================================
!
      RETURN
      END
