!                    *************************
                     SUBROUTINE USER_CONDIN_UV
!                    *************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS U, V
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE TPXO
      USE OKADA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION XMIN,XMAX,YMIN,YMAX
      DOUBLE PRECISION U0,X0,Y0,R,THETA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!      CALL OS('X=C     ',X=U,C=0.01D0)
!
      U0 = 0.6D0
      XMIN = 0.0D0
      XMAX = 4.0D0
!
      DO I=1,NPOIN
        IF ((X(I).GE.XMIN).AND.(X(I).LE.XMAX)) THEN
          IF (Y(I).GT.0.D0) THEN
            U%R(I) =-1.0D0*U0
            V%R(I) = 0.0D0
          ELSE IF (Y(I).LT.0.D0) THEN
            U%R(I) = U0
            V%R(I) = 0.0D0
          ENDIF

        ELSE IF (X(I).LT.XMIN) THEN
          X0 = 0.D0
          Y0 = 0.D0
          R = SQRT((X(I)-X0)**2 + (Y(I)-Y0)**2) 
          THETA = ATAN((Y(I)-Y0)/(X(I)-X0))
          U%R(I) = U0*SIN(THETA)
          V%R(I) = -1.0D0*U0*COS(THETA)

        ELSE IF (X(I).GT.XMAX) THEN
          X0 = XMAX
          Y0 = 0.D0
          R = SQRT((X(I)-X0)**2 + (Y(I)-Y0)**2) 
          THETA = ATAN((Y(I)-Y0)/(X(I)-X0))
          U%R(I) = -1.0D0*U0*SIN(THETA)
          V%R(I) = U0*COS(THETA)

        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
