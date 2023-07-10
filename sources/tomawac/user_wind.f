!                   ********************
                    SUBROUTINE USER_WIND
!                   ********************
!
     &(NVEN,FMTVEN)
!
!***********************************************************************
! TOMAWAC
!***********************************************************************
!
!brief    READS THE WINDS FROM A USED-DEFINED FILE FORMAT.
!
!note     DURING THE FIRST PASS THE USER MUST IDENTIFY THE TIMES TV1 AND TV2
!+        WHICH SURROUND THE FIRST TIME STEP. NEXT, USING THE ARRAYS
!+        XRELV,YRELV,UR,VR OR DIRECTLY FROM THE WIND FILE, THE USER
!+        MAY HAVE TO INTERPOLATE THE WINDS READ FROM THE FILE INTO THE
!+        ARRAYS U1,V1 U2,V2.
!+
!+    INTERPOLATION SUBROUTINE FASP :
!+
!+    CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!+
!+    CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!+
!+    THE CODE WILL INTERPOLATE THE WIND AUTOMATICALLY BETWEEN THESE
!+        2 TIME STEPS.
!+
!+    THE OTHER PASSES OCCUR WHEN A NEW RECORD IS REQUIRED (AT>TV2).
!+        IN THIS CASE TV2,U2,V2 ONLY ARE TO BE COMPUTED.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| FMTVEN         |-->| WIND FILE FORMAT
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVEN           |-->| LOGICAL UNIT NUMBER OF THE WIND DATA FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
!     USE DECLARATIONS_TOMAWAC, ONLY :  NPOIN2, X, Y, DDC, AT,
!    &    TV1,TV2,UV1,UV2,VV1,VV2,VV1,VV2, NBOR, NPTFR
      USE INTERFACE_TOMAWAC, EX_USER_WIND => USER_WIND
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NVEN
      CHARACTER(LEN=8), INTENT(IN)    :: FMTVEN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      WRITE(LU,*) '*********************************************'
      WRITE(LU,*) '      YOU ARE CALLING SUBROUTINE VENUTI      '
      WRITE(LU,*) '            (WINDS FILE FORMAT = 4)          '
      WRITE(LU,*) '           BUT YOU DID NOT MODIFY IT         '
      WRITE(LU,*) '*********************************************'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
