!                   ********************
                    SUBROUTINE USER_TIDE
!                   ********************
!
     &(NMAR,FMTMAR)
!
!***********************************************************************
! TOMAWAC
!***********************************************************************
!
!brief    READS THE TIDES IN A USER-DEFINED FILE FORMAT.
!
!note     DURING THE FIRST PASS THE USER MUST IDENTIFY THE TIMES TV1 AND TV2
!+        WHICH SURROUND THE FIRST TIME STEP. NEXT, USING THE ARRAYS
!+        XRELV,YRELV,UR,VR OR DIRECTLY FROM THE TIDE FILE, THE USER
!+        MAY HAVE TO INTERPOLATE THE TIDES READ FROM THE FILE INTO THE
!+        ARRAYS U1,V1 U2,V2.
!+
!+    INTERPOLATION SUBROUTINE FASP :
!+
!+    CALL FASP(X,Y,Z1,NPOIN,XRELV,YRELV,ZR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!+
!+    THE CODE WILL INTERPOLATE THE TIDE AUTOMATICALLY BETWEEN THESE
!+        2 TIME STEPS.
!+
!+    THE OTHER PASSES OCCUR WHEN A NEW RECORD IS REQUIRED (AT>TV2).
!+        IN THIS CASE TV2,Z2 ONLY ARE TO BE COMPUTED.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FMTMAR         |-->| TIDAL WATER LEVEL FILE FORMAT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NMAR           |-->| LOGICAL UNIT NUMBER OF TIDAL WATER LEVEL FILE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
!     USE DECLARATIONS_TOMAWAC, ONLY :   NPOIN2, X, Y, DDC, AT,
!    &                ZM1, ZM2, TM1, TM2, NBOR, NPTFR
      USE INTERFACE_TOMAWAC, EX_USER_TIDE=> USER_TIDE
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NMAR
      CHARACTER(LEN=8), INTENT(IN)    :: FMTMAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      WRITE(LU,*) '*********************************************'
      WRITE(LU,*) '      YOU ARE CALLING SUBROUTINE USER_TIDE   '
      WRITE(LU,*) '            (TIDE FILE FORMAT = 4)           '
      WRITE(LU,*) '           BUT YOU DID NOT MODIFY IT         '
      WRITE(LU,*) '*********************************************'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
