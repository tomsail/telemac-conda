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
!     DOUBLE PRECISION LO
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  Bosse a t=0
!

!      LO = 1.5D0
      DO I=1,NPOIN
        ZF%R(I) = 0.D0
        IF (Y(I).GE.8138.D0) THEN
!         <20
          ZF%R(I)=0.000904D0*(Y(I)-8138.D0)+10.26D0
          PRINT *,Y(I),ZF%R(I)
        ELSEIF((Y(I).LE.8138.D0).AND.(Y(I).GE.7474.D0)) THEN
!        20-40
          ZF%R(I)=0.000491D0*(Y(I)-7474.D0)+9.94D0
        ELSEIF((Y(I).LE.7474.D0).AND.(Y(I).GE.6922.D0)) THEN
!        40-60
          ZF%R(I)=0.000392D0*(Y(I)-6922.D0)+9.72D0
        ELSEIF((Y(I).LE.6922.D0).AND.(Y(I).GE.6378.D0)) THEN
!        60-80
          ZF%R(I)=0.000807D0*(Y(I)-6378.D0)+9.28D0
        ELSEIF((Y(I).LE.6378.D0).AND.(Y(I).GE.5766.D0)) THEN
!        80-100
          ZF%R(I)=0.00119D0*(Y(I)-5766.D0)+8.55D0
        ELSEIF((Y(I).LE.5766.D0).AND.(Y(I).GE.5256.D0)) THEN
!        100-120
          ZF%R(I)=0.00061D0*(Y(I)-5256.D0)+8.24D0
        ELSEIF((Y(I).LE.5256.D0).AND.(Y(I).GE.4708.D0)) THEN
!        120-140
          ZF%R(I)=0.00059D0*(Y(I)-4708.D0)+7.92D0
        ELSEIF((Y(I).LE.4708.D0).AND.(Y(I).GE.4064.D0)) THEN
!        140-160
          ZF%R(I)=0.000364D0*(Y(I)-4064.D0)+7.68D0
        ELSEIF(Y(I).LE.4064.D0) THEN
!        140-160
          ZF%R(I)=0.000716D0*(Y(I)-3042.D0)+6.95D0
        ENDIF
      ENDDO
      RETURN
      END
