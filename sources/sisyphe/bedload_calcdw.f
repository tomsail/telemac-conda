!                   ***********************************
                    SUBROUTINE BEDLOAD_CALCDW ! (_IMP_)
!                   ***********************************
!
     &  (UCW, UW, TW, NPOIN, PI, UW1, UW2, TW1, TW2)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES QUADRATIC VELOCITIES AND PERIODS
!+               (CASE WITH WAVES).
!
!history  C. VILLARET
!+        **/10/2003
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF POINTS
!| PI             |-->| PI
!| TW             |-->| WAVE PERIOD
!| TW1            |<->| MID WAVE PERIOD, CURRENT IN THE WAVE DIRECTION
!| TW2            |<->| MID WAVE PERIOD, CURRENT IN THE OPPOSITE DIRECTION
!| UCW            |-->| CURRENT PROJECTED IN THE WAVE DIRECTION
!| UW             |-->| ORBITAL WAVE VELOCITY
!| UW1            |<->| WORK ARRAY
!| UW2            |<->| WORK ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_BEDLOAD_CALCDW => BEDLOAD_CALCDW
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!     2/ GLOBAL VARIABLES
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UCW, UW, TW
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: PI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UW1, UW2, TW1, TW2
!
!     3/ LOCAL VARIABLES
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: UCMOY, RAP
      DOUBLE PRECISION            :: ACOSMRAP, ACOSPRAP, SQRTRAP
      DOUBLE PRECISION, PARAMETER :: ZERO = 1.D-06
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      DO I = 1,NPOIN
        UCMOY = ABS(UCW%R(I))
        ! ****************** !
        !    I - WAVES ONLY  ! (_IMP_)
        ! ****************** !
        IF (UCMOY <= ZERO) THEN
          UW1%R(I) = UW%R(I)
          UW2%R(I) = UW%R(I)
          TW1%R(I) = TW%R(I) / 2.D0
          TW2%R(I) = TW%R(I) / 2.D0
        ELSE
          RAP = UW%R(I) / UCMOY
          ! ******************** !
          ! II - WAVES ARE PREDOMINANT ! (_IMP_)
          ! ******************** !
          IF (RAP > 1.D0) THEN
            ACOSMRAP = ACOS(-1.D0/RAP)
            ACOSPRAP = ACOS( 1.D0/RAP)
            SQRTRAP  = SQRT(1.D0-1.D0/RAP**2)
            TW1%R(I) = TW%R(I)*ACOSMRAP / PI
            TW2%R(I) = TW%R(I)*ACOSPRAP / PI
            UW1%R(I) = 2.D0*UCMOY**2 + UW%R(I)**2
     &               + 3.D0*UCMOY*UW%R(I)*SQRTRAP/ACOSMRAP
            UW1%R(I) = SQRT(UW1%R(I))
            UW2%R(I) = 2.D0*UCMOY**2 + UW%R(I)**2
     &               - 3.D0*UCMOY*UW%R(I)*SQRTRAP/ACOSPRAP
            UW2%R(I) = SQRT(UW2%R(I))
          ! ********************** !
          ! III - CURRENTS ARE PREDOMINANT ! (_IMP_)
          ! ********************** !
          ELSE
            UW1%R(I) = UCW%R(I)*SQRT(2.D0 + RAP**2)
            UW2%R(I) = ZERO
            TW1%R(I) = TW%R(I)
            TW2%R(I) = ZERO
          ENDIF
        ENDIF
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE BEDLOAD_CALCDW
