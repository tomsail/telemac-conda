!                   ********************************
                    SUBROUTINE USER_PRERES_TELEMAC2D
!                   ********************************
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    PREPARES THE USER VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION HLEFT, COEF, X0, XA, XB
!
!=======================================================================
! COMPUTE THE EXACT WATER DEPTH AND VELOCITY
!=======================================================================
!
      IF((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))) THEN
!
        HLEFT = 1.D0
        X0 = 8.D0
        COEF = SQRT(HLEFT*GRAV)
        XA = X0 - MAX(AT,DT)*COEF
        XB = X0 + 2.D0*MAX(AT,DT)*COEF
!
        DO I = 1,NPOIN
          PRIVE1(I) = COEF-(X(I)-X0)/2.D0/MAX(AT,DT)
          PRIVE1(I) = 4.D0*PRIVE1(I)**2/9.D0/GRAV
          PRIVE2(I) = 2.D0*((X(I)-X0)/MAX(AT,DT)+COEF)/3.D0
          PRIVE3(I) = 0.D0
!
          IF (X(I) <= XA) THEN
            PRIVE1(I) = HLEFT
            PRIVE2(I) = 0.D0
          ELSE IF (X(I) >= XB) THEN
            PRIVE1(I) = 0.D0
            PRIVE2(I) = 0.D0
          ENDIF

        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END
