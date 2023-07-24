!                 *****************
                  SUBROUTINE CALCUW_GAIA
!                 *****************
!
     & ( UW, H, HW, TW, GRAV ,NPOIN, TYPE_HOULE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the wave orbital velocity.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     GRAV  Acceleration of gravity
!>@param[in]     H     Water depth
!>@param[in]     HW    Wave depth
!>@param[in]     NPOIN Number of points
!>@param[in]     TW    Wave period
!>@param[in,out] UW    Orbital velocity (regular wave)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: UW(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: TW(NPOIN),H(NPOIN), HW(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: GRAV
      INTEGER, INTENT(IN) :: TYPE_HOULE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION   PI,DPI2
      DOUBLE PRECISION   POL, Y ,X
!
      DOUBLE PRECISION   TN,T,A,URMS
      INTEGER I
      INTRINSIC SQRT, SINH, ATAN
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
      DPI2 = 4.D0 * PI * PI
!
      IF(TYPE_HOULE==1)THEN
!        REGULAR (MONOCHROMATIC) WAVE CASE
!
!  SOLVES Y=X*TH(X) WITH Y=(2*PI/TW)**2*H/G AND X=(2*PI/L)*H
!  USING A POLYNOMIAL FUNCTION (HUNT METHOD - 9TH ORDER)
!
        DO I=1,NPOIN
          IF ( (TW(I) .GT. 0.D0).AND.(HW(I).GT.0.D0) ) THEN
            Y = DPI2 / GRAV * H(I) / (TW(I) * TW(I))
            POL = 1.D0 + Y * ( 0.66667D0 +
     &                  Y * ( 0.35550D0 +
     &                  Y * ( 0.16084D0 +
     &                  Y * ( 0.06320D0 +
     &                  Y * ( 0.02174D0 +
     &                  Y * ( 0.00654D0 +
     &                  Y * ( 0.00171D0 +
     &                  Y * ( 0.00039D0 +
     &                  Y * ( 0.00011D0 ) ))))))))
            X = SQRT( Y*Y + Y / POL )
!
            IF ( X .GT. 10.D0) THEN
              UW(I) = 0.D0
            ELSE
              UW(I) = PI / TW(I) * HW(I) / (SINH(X))
            ENDIF
          ELSE
            UW(I) = 0.D0
          ENDIF
        ENDDO
!
      ELSEIF(TYPE_HOULE==2)THEN
!     IRREGULAR (SPECTRAL)WAVE CASE
!
!      SOULSBY AND SMALLMAN METHOD (1986)
!
        DO I=1,NPOIN
          IF ( (TW(I) .GT. 0.D0).AND.(HW(I).GT.0.D0) ) THEN
!           for a Jonswap spectrum
!           Tp=1.28*Tz
!           (Tw is supposed be Tp from tomawac)
!           (Hw is supposed be Hm0 from tomawac)
            TN=(H(I)/GRAV)**0.5D0
!           T=TN/TZ
            T=TN/(TW(I)/1.28D0)
            A=(6500.D0+(0.56+15.54*T)**6)**(1.D0/6.D0)
            URMS=(0.25D0/(1+A*T**2)**3)*HW(I)/TN
            UW(I)= DSQRT(2.D0)*URMS
          ELSE
            UW(I)=0.D0
          ENDIF
        ENDDO
      ELSE
        WRITE(LU,*)'VALUE OF TYPE OF WAVES IS NOT OK'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      RETURN
      END SUBROUTINE CALCUW_GAIA
