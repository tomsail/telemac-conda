!                   *****************************
                    DOUBLE PRECISION FUNCTION SL3
!                   *****************************
!
     &( I , TIME , N , ENTET )
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    PRESCRIBES THE FREE SURFACE ELEVATION FOR LEVEL
!+                IMPOSED LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNH)
!+        12/12/00
!+        V5P9
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
!+   Modification size FCT due to modification of TRACER numbering
!
!history  J-M HERVOUET (LNHE)
!+        28/09/2015
!+        V7P1
!+   Removing hardcoded array OK.
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Call of USER_SL3 User Fortran where the modifications are done
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ENTET          |-->| IF YES, LISTING PRINTOUTS ALLOWED
!| I              |-->| NUMBER OF LIQUID BOUNDARY
!| N              |-->| GLOBAL NUMBER OF POINT
!|                |   | IN PARALLEL NUMBER OF POINT IN ORIGINAL MESH
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: I,N
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
!     THE VALUE IN IT. IF YES, OKSL3 REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKSL3 IS SET  TO .FALSE.
!
      IF(OKSL3(I).AND.T3D_FILES(T3DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE SL(1), SL(2), ETC, SL(99), DEPENDING ON I
        FCT='SL(      '
        IF(I.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') I
          FCT(5:5)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') I
          FCT(6:6)=')'
        ELSE
          WRITE(LU,*) 'SL3 NOT PROGRAMMED FOR MORE
     &                 THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(SL3,FCT,TIME,T3D_FILES(T3DIMP)%LU,
     &                      ENTET,OKSL3(I))
!
      ENDIF
!
      IF(.NOT.OKSL3(I).OR.T3D_FILES(T3DIMP)%NAME(1:1).EQ.' ') THEN
!
!     SL IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
!
        SL3 = COTIMP(I)
!
        ! USER UPDATE OF VALUE
        CALL USER_SL3(I, TIME, N, ENTET, SL3)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
