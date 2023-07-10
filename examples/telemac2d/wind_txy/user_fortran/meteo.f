!                   ****************
                    SUBROUTINE METEO
!                   ****************
!
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,AT,LT,NPOIN,VENT,ATMOS,
     & ATMFILEA,ATMFILEB,FILES,LISTIN,
     & PATMOS_VALUE,AWATER_QUALITY,PLUIE,AOPTWIND,AWIND_SPD)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES ATMOSPHERIC PRESSURE AND WIND VELOCITY FIELDS
!+               (IN GENERAL FROM INPUT DATA FILES).
!
!warning  CAN BE ADAPTED BY USER
!
!history  J-M HERVOUET (LNHE)
!+        02/01/2004
!+        V5P4
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
!+        30/01/2013
!+        V6P3
!+   Now 2 options with an example for reading a file. Extra arguments.
!
!history  C.-T. PHAM (LNHE)
!+        09/07/2014
!+        V7P0
!+   Reading a file of meteo data for exchange with atmosphere
!+   Only the wind is used here
!
!history R.ATA (LNHE)
!+        09/11/2014
!+        V7P0
!+  introducion of water quality option + pluie is introduced as
!+   an optional parameter + remove of my_option which is replaced
!+   by a new keyword + value of patmos managed also with a new keyword
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        07/01/2015
!+        V7P0
!+  Adding optional arguments to remove USE DECLARATIONS_TELEMAC2D.
!
!history R.ATA (LNHE)
!+        16/11/2015
!+        V7P0
!+  Adding USE WAQTEL...
!
!history A. LEROY (LNHE)
!+        25/11/2015
!+        V7P1
!+  INTERPMETEO now writes directly in variables of WAQTEL which
!+  can be used by the other modules. This makes it possible to
!+  remove subsequent calls to INTERPMETEO in TELEMAC3D
!
!history  P. PRODANOVIC (RIGGS ENGINEERING LTD)
!+        15/06/2016
!+        V7P0
!+   Converts the wind data to cartesian form, then interpolates. This
!+   eliminates errors when interpolating direction between 359 and 1
!+   degrees azimuth.
!
!history J.-M. HERVOUET (RETIRED)
!+        01/07/2017
!+        V7P2
!+  Setting of UL moved outside the test IF(LT.EQ.0)... After a post by
!+  Qilong Bi (thanks Qilong...).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| ATMFILEA       |-->| LOGICAL UNIT OF THE ASCII ATMOSPHERIC FILE
!| ATMFILEB       |-->| LOGICAL UNIT OF THE BINARY ATMOSPHERIC FILE
!| ATMOS          |-->| YES IF PRESSURE TAKEN INTO ACCOUNT
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| FUAIR          |<->| VELOCITY OF WIND ALONG X, IF CONSTANT
!| FVAIR          |<->| VELOCITY OF WIND ALONG Y, IF CONSTANT
!| LISTIN         |-->| IF YES, PRINTS INFORMATION
!| LT             |-->| ITERATION NUMBER
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PATMOS         |<--| ATMOSPHERIC PRESSURE
!| PATMOS_VALUE   |-->| VALUE OF ATMOSPHERIC PRESSURE IS CONSTANT
!| VENT           |-->| YES IF WIND TAKEN INTO ACCOUNT
!| WINDX          |<--| FIRST COMPONENT OF WIND VELOCITY
!| WINDY          |<--| SECOND COMPONENT OF WIND VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY: TAIR_VALUE,ATMOSEXCH,RAYAED2
      USE DECLARATIONS_TELEMAC2D, ONLY : X,Y
      USE METEO_TELEMAC, ONLY: TAIR,SYNC_METEO,RAY3,RAINFALL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LT,NPOIN,ATMFILEA,ATMFILEB
      LOGICAL, INTENT(IN)             :: ATMOS,VENT,LISTIN
      DOUBLE PRECISION, INTENT(INOUT) :: WINDX(NPOIN),WINDY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: PATMOS(*)
      DOUBLE PRECISION, INTENT(IN)    :: AT,PATMOS_VALUE
      DOUBLE PRECISION, INTENT(INOUT) :: FUAIR,FVAIR
      TYPE(BIEF_FILE), INTENT(IN)     :: FILES(*)
!     OPTIONAL
      LOGICAL, INTENT(IN)          ,OPTIONAL :: AWATER_QUALITY
      TYPE(BIEF_OBJ), INTENT(INOUT),OPTIONAL :: PLUIE
      INTEGER, INTENT(IN)          ,OPTIONAL :: AOPTWIND
      DOUBLE PRECISION, INTENT(IN) ,OPTIONAL :: AWIND_SPD(2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL WATER_QUALITY
      INTEGER UL,OPTWIND
      DOUBLE PRECISION WIND_SPD(2)
!
      DOUBLE PRECISION, PARAMETER :: EPS = 1.D-3
!
!     ######################################################################
!     IDWM WIND INTERPOLATION CUSTOM VARIABLES
!     ######################################################################
!
      INTEGER NUMSTA, NUMPOINTS, A, B, K, I
      DOUBLE PRECISION THETA_RAD, TMPDIR, TMPSPD, PI, DTR
!
!     COORDINATES OF THE STATIONS UTM
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XX, YY, AT_WIND
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: OUT_WSPD
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: OUT_WDIRX
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: OUT_WDIRY
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: WIND, POINTS
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INPSTA_S
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INPSTA_D

!     ADDED ON 2016.05.26
!     THIS IS THE X AND Y COMPONENT OF THE WIND READ FROM FILE
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INPSTA_WINDX
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INPSTA_WINDY
!
!     ######################################################################
!
!-----------------------------------------------------------------------
!
!     DATA THAT YOU DECLARE AND READ HERE ONCE IN A FILE MAY HAVE TO BE
!     KEPT BECAUSE THIS SUBROUTINE IS CALLED AT EVERY TIME STEP.
!     WITHOUT THE SAVE COMMAND, ALL LOCAL DATA ARE FORGOTTEN IN THE NEXT
!     CALL.
!
      SAVE
!
!-----------------------------------------------------------------------
!
!     DEFAULT VALUES OF PARAMETERS WHEN THEY ARE NOT GIVEN
!
      WATER_QUALITY=.FALSE.
      IF(PRESENT(AWATER_QUALITY)) WATER_QUALITY=AWATER_QUALITY
      OPTWIND=1
      IF(PRESENT(AOPTWIND)) OPTWIND=AOPTWIND
      WIND_SPD(1)=0.D0
      WIND_SPD(2)=0.D0
      IF(PRESENT(AWIND_SPD)) THEN
        WIND_SPD(1)=AWIND_SPD(1)
        WIND_SPD(2)=AWIND_SPD(2)
      ENDIF
!
!-----------------------------------------------------------------------
!
      UL = FILES(ATMFILEA)%LU

!     AT FIRST TIMESTEP
!
      IF(LT.EQ.0) THEN
!
!       ATMOSPHERIC PRESSURE AND AIR TEMPERATURE
!
        IF(ATMOS.OR.WATER_QUALITY) THEN
          CALL OV('X=C     ', X=PATMOS, C=PATMOS_VALUE, DIM1=NPOIN)
        ENDIF
        IF(WATER_QUALITY) THEN
          CALL OV('X=C     ', X=TAIR%R, C=TAIR_VALUE, DIM1=NPOIN)
        ENDIF
!
!       WIND AND/OR OTHER METEO VARIABLES
!
        IF(VENT.OR.WATER_QUALITY) THEN
          IF(OPTWIND.EQ.1.AND.FILES(ATMFILEA)%NAME(1:1).EQ.' ') THEN
!           IN THIS CASE THE WIND IS CONSTANT, VALUE GIVEN IN STEERING FILE
            CALL OV( 'X=C     ' , X=WINDX, C=FUAIR, DIM1=NPOIN)
            CALL OV( 'X=C     ' , X=WINDY, C=FVAIR, DIM1=NPOIN)
          ELSEIF(FILES(ATMFILEA)%NAME(1:1).NE.' ') THEN
!           WATER QUALITY
            IF(WATER_QUALITY) THEN
!             TIME VARYING WATER QUALITY OTHER THAN THERMIC IN 3D
              IF(ATMOSEXCH.EQ.0) THEN
                CALL SYNC_METEO(AT)
!             TIME VARYING WATER QUALITY WITH HEAT EXCHANGE WITH ATMOSPHERE
              ELSEIF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
                CALL SYNC_METEO(AT)
                CALL OS('X=C     ', X=RAYAED2, C=RAY3%R(1))
              ENDIF
            ELSEIF(OPTWIND.EQ.2) THEN
              CALL SYNC_METEO(AT)
!
!         ######################################################################
!         IDWM WIND INTERPOLATION; THIS IS EXECUTED ONLY ONCE AT THE START
!         ######################################################################
!
            ELSEIF(OPTWIND.EQ.3) THEN
        ! READ BLANK LINES AT BEGINNING OF FILE
              READ(UL,*)
        ! READ NUMSTA AND NUMPOINTS
              READ(UL,*) NUMSTA, NUMPOINTS

          !ALLOCATE THE ARRAYS
              ALLOCATE(XX(NUMSTA), YY(NUMSTA), AT_WIND(NUMPOINTS))
              ALLOCATE(WIND(NUMPOINTS,NUMSTA*2+1), POINTS(NPOIN,2))
              ALLOCATE(INPSTA_S(NUMSTA,3), INPSTA_D(NUMSTA,3))
              ALLOCATE(INPSTA_WINDX(NUMSTA,3), INPSTA_WINDY(NUMSTA,3))
              ALLOCATE(OUT_WSPD(NPOIN))
              ALLOCATE(OUT_WDIRX(NPOIN),OUT_WDIRY(NPOIN))
!
          ! READ STATION COORDINATES
              DO B = 1,NUMSTA
                READ(UL,*) XX(B), YY(B)
              ENDDO
!
          ! READ THE WIND TIME SERIES FROM THE INPUT FILE
          ! FIRST COLUMN IS TIME IN SECONDS, REST OF COLUMNS ARE WSPD
          ! AND WDIR FOR EACH STATION READ
              DO A = 1,NUMPOINTS
                READ(UL,*) (WIND(A,B), B=1,NUMSTA*2+1)
              ENDDO
!
          ! EXTRACT AT_WIND FROM WIND(A,B); FIRST COLUMN IS TIME IN SECONDS
              DO A = 1,NUMPOINTS
                AT_WIND(A) = WIND(A,1)
              ENDDO
!
          ! ASSEMBLE THE POINTS ARRAY FOR IDWM FUNCTION
              DO I = 1,NPOIN
                POINTS(I,1) = X(I)
                POINTS(I,2) = Y(I)
              ENDDO
!
! #######################################################################
!
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FOR THE REMAINING TIME STEPS
!
      IF(VENT.OR.WATER_QUALITY) THEN
!       WATER QUALITY
        IF(FILES(ATMFILEA)%NAME(1:1).NE.' ') THEN
          IF(WATER_QUALITY) THEN
!           TIME VARYING WATER QUALITY
            IF(ATMOSEXCH.EQ.0) THEN
              CALL SYNC_METEO(AT)
              IF(PRESENT(PLUIE)) THEN
                CALL OS('X=Y     ',X=PLUIE, Y=RAINFALL) ! MM/S
              ENDIF
!           TIME VARYING WATER QUALITY WITH HEAT EXCHANGE WITH ATMOSPHERE
            ELSEIF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
              CALL SYNC_METEO(AT)
              CALL OS('X=C     ', X=RAYAED2, C=RAY3%R(1))
            ENDIF
          ELSEIF(VENT) THEN
!           WIND VARYING IN TIME CONSTANT IN SPACE
            IF(OPTWIND.EQ.2) THEN
              CALL SYNC_METEO(AT)
!           WIND VARYING IN TIME AND SPACE
            ELSEIF(OPTWIND.EQ.3) THEN
!
! #######################################################################
!         IDWM WIND INTERPOLATION CODE
! #######################################################################
!
!
!       ASSEMBLE THE ARRAYS OF X,Y,WNDSPD AND X,Y,WNDDIR FOR EACH ITERATION
            PI = 4.D0*ATAN(1.D0)
            DTR = PI/180.D0
            DO A = 1,NUMPOINTS
              IF(AT_WIND(A).EQ.AT) THEN
                WRITE(LU,*) 'METEO: WIND READ AT: ', AT
                DO B = 1,NUMSTA
                  ! ASSEMBLE THE ARRAYS FOR THIS TIME STEP
                  ! DIRECTIONS FROM INPUT FILE
                  INPSTA_D(B,1) = XX(B)
                  INPSTA_D(B,2) = YY(B)
                  INPSTA_D(B,3) = WIND(A,B*2+1)

                  ! SPEEDS FROM INPUT FILE
                  INPSTA_S(B,1) = XX(B)
                  INPSTA_S(B,2) = YY(B)
                  INPSTA_S(B,3) = WIND(A,B*2)

                  ! ADDED ON 2016.06.15
                  ! CHECK IF WIND SPEED IS +VE, AND IF WIND DIRECTION
                  ! IS BETWEEN 0 AND 360 DEG;
                  IF (INPSTA_S(B,3) < 1.D-6) THEN
                    INPSTA_S(B,3) = 1.D-6
                  END IF

                  IF (INPSTA_D(B,3) .LT. 1.D-6) THEN
                    INPSTA_D(B,3) = 1.D-6
                  END IF

                  IF (INPSTA_D(B,3) .GT. 360.D0) THEN
                    INPSTA_D(B,3) = 360.D0
                  END IF

                  ! ADDED ON 2016.05.26
                  ! RATHER THAN INTERPOLATING THE DIRECTION VARIABLE
                  ! CONVERT INPSTA_D TO ITS X AND Y COMPONENTS,
                  ! INTERPOLATE BOTH

                  ! THIS IS NEEDED BECAUSE INTERPOLATING A DIRECTION
                  ! VARIABLE THAT TAKES ON VALUES BETWEEN 0 AND 360 HAS
                  ! PROBLEMS WHEN INTERPOLATING NODES LOCATED CLOSE TO
                  ! STATIONS WITH  DIR~0'S AND DIR~350'S

                  ! ASSIGN X AND Y COORDINATES
                  INPSTA_WINDX(B,1) = XX(B)
                  INPSTA_WINDX(B,2) = YY(B)

                  INPSTA_WINDY(B,1) = XX(B)
                  INPSTA_WINDY(B,2) = YY(B)

                  ! THIS IS JUST TO KEEP VARIABLE NAMES SHORT
                  TMPSPD = INPSTA_S(B,3)
                  TMPDIR = INPSTA_D(B,3)

                  ! CONVERT INP_STA_D TO INPSTA_WINDX AND INPSTA_WINDY
                  ! THESE ARE CARTESIAN VECTORS OF THE WIND
                  IF (TMPDIR >= 0.D0 .AND. TMPSPD >= 0.D0) THEN
                    IF ((TMPDIR >= 0.D0) .AND. (TMPDIR <= 90.D0)) THEN
                      THETA_RAD = TMPDIR * DTR
                      INPSTA_WINDX(B,3) = -SIN(THETA_RAD)*TMPSPD
                      INPSTA_WINDY(B,3) = -COS(THETA_RAD)*TMPSPD
                    END IF
!
                    IF ((TMPDIR > 90.D0) .AND. (TMPDIR <= 180.D0)) THEN
                      THETA_RAD = (180.D0 - TMPDIR) * DTR
                        INPSTA_WINDX(B,3) = -SIN(THETA_RAD)*TMPSPD
                        INPSTA_WINDY(B,3) =  COS(THETA_RAD)*TMPSPD
                    END IF
!
                    IF ((TMPDIR > 180.D0) .AND. (TMPDIR <= 270.D0)) THEN
                      THETA_RAD = (TMPDIR-180.D0) * DTR
                        INPSTA_WINDX(B,3) = SIN(THETA_RAD)*TMPSPD
                        INPSTA_WINDY(B,3) = COS(THETA_RAD)*TMPSPD
                    END IF
!
                    IF ((TMPDIR > 270.D0) .AND. (TMPDIR <= 360.D0)) THEN
                      THETA_RAD = (360.D0-TMPDIR) * DTR
                        INPSTA_WINDX(B,3) =  SIN(THETA_RAD)*TMPSPD
                        INPSTA_WINDY(B,3) = -COS(THETA_RAD)*TMPSPD
                    END IF
                  ELSE
                    INPSTA_WINDX(B,3) = -999.D0
                    INPSTA_WINDY(B,3) = -999.D0
                  ENDIF
                ENDDO ! B

              CALL IDWM_T2D(INPSTA_WINDX,POINTS,OUT_WDIRX,NPOIN,NUMSTA)
              CALL IDWM_T2D(INPSTA_WINDY,POINTS,OUT_WDIRY,NPOIN,NUMSTA)

!               FINAL WINDX AND WINDY OUTPUT
                DO K = 1,NPOIN
                  WINDX(K) = OUT_WDIRX(K)
                  WINDY(K) = OUT_WDIRY(K)
                ENDDO

              ENDIF !IF(AT_WIND(A).EQ.AT)
            ENDDO ! A
!
! #######################################################################
!
            ENDIF
          ENDIF
        ENDIF
!
!       WIND AND/OR OTHER METEO VARIABLES
!       VARYING IN SPACE AND TIME, FROM A BINARY FILE
!
        IF(FILES(ATMFILEB)%NAME(1:1).NE.' ') THEN
          IF(FILES(ATMFILEA)%NAME(1:1).NE.' ') THEN
            WRITE(LU,*) 'METEO: THE DATA FROM THE ASCII METEO'
            WRITE(LU,*) 'FILE WILL BE OVERWRITTEN BY THE'
            WRITE(LU,*) 'CORRESPONDING BINARY FILE DATA'
          ENDIF
          CALL METEO_FROM_BINARY_FILE(PATMOS,WINDX,WINDY,AT,
     &                                NPOIN,
     &                                ATMOS,ATMFILEB,FILES,LISTIN,
     &                                OPTWIND,WIND_SPD)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
