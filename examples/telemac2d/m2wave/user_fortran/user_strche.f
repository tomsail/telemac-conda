!                   **********************
                    SUBROUTINE USER_STRCHE
!                   **********************
!
!
!***********************************************************************
! BIEF
!***********************************************************************
!
!brief    COMPUTES THE BOTTOM FRICTION COEFFICIENT
!+                IF VARIABLE IN SPACE.
!
!note     IN PARAMETER ESTIMATION WITH A LIST OF TESTS,
!+         THESE VALUES ARE DISCARDED.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+  HERE A CONSTANT FRICTION VALUE IS GIVEN:
!+
!+  DO I=1,NPOIN
!+    CHESTR%R(I) = 60.D0
!+  ENDDO
!
!history  J-M HERVOUET (LNH)
!+        01/10/96
!+        V5P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
!     DECLARATIONS MUST BE ADAPTED TO EVERY CODE
!     THIS EXAMPLE APPLIES TO TELEMAC2D
!
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION X0(10),Y0(10),A1(10),A2(10),C(10),A3,R,PI
      INTEGER I, J
!
!-----------------------------------------------------------------------
!
      X0(1)=-5.30D0
      Y0(1)=50.18D0
      A1(1)=  30.D0
      A2(1)= -63.D0
      C(1) = 54.3D0
!
      X0(2)=-2.71D0
      Y0(2)=49.36D0
      A1(2)=  47.D0
      A2(2)= -30.D0
      C(2) = 47.2D0
!
      X0(3)=-2.71D0
      Y0(3)=49.36D0
      A1(3)= -30.D0
      A2(3)=-110.D0
      C(3) = 47.2D0
!
      X0(4)=-2.12D0
      Y0(4)=49.14D0
      A1(4)=  45.D0
      A2(4)= -83.D0
      C(4) = 47.2D0
!
      X0(5)=-2.00D0
      Y0(5)=49.75D0
      A1(5)= 110.D0
      A2(5)= -40.D0
      C(5) = 62.2D0
!
      X0(6)=-1.35D0
      Y0(6)=49.68D0
      A1(6)=   0.D0
      A2(6)= -90.D0
      C(6) = 62.2D0
!
      X0(7)= 0.57D0
      Y0(7)=50.85D0
      A1(7)=  90.D0
      A2(7)= -48.D0
      C(7) = 62.2D0
!
      X0(8)= 1.47D0
      Y0(8)=51.00D0
      A1(8)= 132.D0
      A2(8)= -48.D0
      C(8) = 76.5D0
!
      PI = 4.D0*ATAN(1.D0)
      R=6400000.D0
      DO I=1,8
        X0(I)=R*X0(I)*PI/180.D0
        Y0(I)=R*LOG(TAN((Y0(I)+90.D0)*PI/360.D0))
     &       -R*LOG(TAN(69.D0*PI/180.D0))
        A1(I)=A1(I)*PI/180.D0
        A2(I)=A2(I)*PI/180.D0
      ENDDO
!
      DO I=1,NPOIN
        CHESTR%R(I) = 95.3D0
      ENDDO
      DO I=1,NPOIN
        IF (Y(I).GT.100000.D0) THEN
          DO J=1,8
            A3=ATAN2(Y(I)-Y0(J),X(I)-X0(J))
            IF (A3.LT.A1(J).AND.A3.GT.A2(J)) CHESTR%R(I)=C(J)
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     COMMENTS HERE MAY BE CHANGED
!
      WRITE(LU,*) 'STRCHE (BIEF): MODIFICATION OF FRICTION'
      WRITE(LU,*)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
