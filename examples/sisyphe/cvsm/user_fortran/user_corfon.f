!                   **********************
                    SUBROUTINE USER_CORFON
!                   **********************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE
!
!history  Y AUDOUIN (LNHE)
!+        20/09/2018
!+        V8P0
!+
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
      DOUBLE PRECISION L, MPZ
      INTEGER I
!
!-----------------------------------------------------------------------
!
      MPZ = 9.964433629385642D0
      DO I=1,NPOIN
! part1: straight flume left
! at the end (11.5 m, z = 9.977)
        IF (X(I).LE. 101.1819D0.AND.Y(I).LE.42.6371D0) THEN
          ZF%R(I) = 10.D0 - ABS(Y(I)-31.1371D0)*0.002D0
        ENDIF
!
! part II: bend
!
! midpoint is at x 101.1819 and 104.6819 + 4.0
! the length of the circle part is l_circle_part = (pi*4.0)/2.0
! l_circle_part = 6.283185307179586 (1/4 of the circle)
! z at the start of the circle part = 9.977
! z at the middle of the circle part = 9.964433629385642
! z at the end of the circle part = 9.951867258771284
! radius is 4.0
!
        IF (Y(I).GT.42.6371D0) THEN
          L=3.141592654D0*4.0D0*
     &     (1.5707963D0-DACOS((ABS(X(I)-101.1819D0))/
     &     (SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2))))/
     &     3.141592654D0
!
          IF(X(I).LT.101.1819D0) THEN
              ZF%R(I) = L*(0.002D0) + MPZ
          ENDIF
!
          IF(X(I).GE.101.1819D0) THEN
            ZF%R(I) = L*(-0.002D0) + MPZ
          ENDIF
        ENDIF
!
! part III: straight part right
! starts with ~ 9.961192852465999
        IF (X(I).GE.101.1819D0.AND.Y(I).LE.42.6371D0) THEN
          ZF%R(I)= 9.951867258771284D0-(42.6371D0-Y(I))*(0.002D0)
        ENDIF
!
      END DO


! Change of  Bottom slope
!      DO I=1,NPOIN
!        IF (X(I).LE.101.1819D0.AND.Y(I).LE.42.6371) THEN
!
!        ENDIF
!        IF (Y(I).GT.42.6371D0) THEN
!!         IF (((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2).GE.12.0409D0
!!     *  .AND.((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2)
!!     *  .LE.20.5209D0) THEN
! Calculating the length of a circle section
!          IF (X(I).LT.101.1819D0) THEN
!!          R=SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2)
!            L=3.141592654D0*4.0D0*
!     &     (1.5707963D0-DACOS((ABS(X(I)-101.1819D0))/
!     &     (SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2))))/
!     &     3.141592654D0
!            ZF%R(I) = L*(0.0016D0) + 9.97072D0
!          ENDIF
!          IF (X(I).GT.101.1819D0) THEN
!!          R=SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2)
!            L=3.141592654D0*4.D0*
!     &     (1.5707963D0-DACOS((ABS(X(I)-101.1819D0))/
!     &     (SQRT((X(I)-101.1819D0)**2+(Y(I)-42.6371D0)**2))))/
!     &     3.141592654D0
!            ZF%R(I) = L*(-0.0016D0) + 9.97072D0
!          ENDIF
!!         ENDIF
!        ENDIF
!        IF (X(I).GE.101.1819D0.AND.Y(I).LE.42.6371D0) THEN
!          ZF%R(I) = (ABS(Y(I)-42.6371D0))*(-0.0016D0) + 9.95954D0
!        ENDIF
!        IF (X(I).LE.101.1819D0.AND.Y(I).LE.42.8D0.AND.Y(I).GE.42.5D0)
!     &  THEN
!          ZF%R(I) = 9.98145D0
!        ENDIF
!        IF (X(I).GE.101.1819D0.AND.Y(I).LE.42.6D0.AND.Y(I).GE.42.45D0)
!     &  THEN
!          ZF%R(I) = 9.960055D0
!        ENDIF
!      END DO
!
      RETURN
      END
