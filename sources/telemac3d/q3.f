!                   ****************************
                    DOUBLE PRECISION FUNCTION Q3
!                   ****************************
!
     &( I , TIME , ENTET )
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    PRESCRIBES THE DISCHARGE FOR FLOW IMPOSED
!+                LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        08/04/09
!+        V6P0
!+   Original version.
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
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification of FCT size due to modification of TRACER numbering
!
!history  J-M HERVOUET (LNHE)
!+        28/09/2015
!+        V7P1
!+   Removing hardcoded array OK.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ENTET          |-->| IF YES, LISTING PRINTOUTS ALLOWED
!| I              |-->| NUMBER OF THE LIQUID BOUNDARY
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC3D, EX_Q3 => Q3
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: I
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) :: FCT
!
!-----------------------------------------------------------------------
!
!     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OKQ3 REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKQ3 IS SET  TO .FALSE.
!
      IF(OKQ3(I).AND.T3D_FILES(T3DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE Q(1), Q(2), ETC, Q(99), DEPENDING ON I
        FCT='Q(       '
        IF(I.LT.10) THEN
          WRITE(FCT(3:3),FMT='(I1)') I
          FCT(4:4)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(3:4),FMT='(I2)') I
          FCT(5:5)=')'
        ELSE
          WRITE(LU,*) 'Q3 NOT PROGRAMMED FOR MORE
     &                 THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(Q3,FCT,TIME,T3D_FILES(T3DIMP)%LU,
     &                      ENTET,OKQ3(I))
!
      ENDIF
!
      IF(.NOT.OKQ3(I).OR.T3D_FILES(T3DIMP)%NAME(1:1).EQ.' ') THEN
!
!     Q IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
!
        Q3 = DEBIMP(I)

        ! USER UPDATE
        CALL USER_Q3(Q3, I, TIME, ENTET)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
