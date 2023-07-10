!                   *****************
                    SUBROUTINE KEPICL
!                   *****************
!
     & (LIKBOF,LIEBOF,LIUBOF,LIKBOL,LIEBOL,LIUBOL,LIKBOS,LIEBOS,
     &  NPTFR,NPLAN,NPOIN2,KENT,KSORT,KENTU)
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION
!+                SOURCE TERM STEP OF THE K-EPSILON MODEL.
!+
!+            CASE.
!
!warning  LIKBOR AND LIEBOR ARE BUILT FROM LIUBOR
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED DISCHARGE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIEBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE BOTTOM
!| LIEBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON ON THE LATERAL WALLS
!| LIEBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE SURFACE
!| LIKBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON K AT THE BOTTOM
!| LIKBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON K ON THE LATERAL WALLS
!| LIKBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON K AT THE SURFACE
!| LIUBOF         |-->| TYPE OF BOUNDARY CONDITIONS ON U AT THE BOTTOM
!| LIUBOL         |-->| TYPE OF BOUNDARY CONDITIONS ON U ON THE LATERAL WALLS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC3D, ONLY : LIMKF,LIMEF,LIMKS,LIMES
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPTFR, NPLAN, NPOIN2
      INTEGER, INTENT(IN)    :: KENT, KSORT, KENTU
      INTEGER, INTENT(IN)    :: LIUBOF(NPOIN2)
      INTEGER, INTENT(IN)    :: LIUBOL(NPTFR*NPLAN*2)
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2), LIKBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR*NPLAN*2)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR*NPLAN*2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPTFR, IPOIN2,NPTFR3
!
!-----------------------------------------------------------------------
!
!     LATERAL BOUNDARIES :
!
!-----------------------------------------------------------------------
!
      NPTFR3=NPLAN*NPTFR
!
      DO IPTFR=1,NPTFR3
        IF(LIUBOL(IPTFR).EQ.KENT.OR.LIUBOL(IPTFR).EQ.KENTU) THEN
          LIKBOL(IPTFR) = KENT
          LIEBOL(IPTFR) = KENT
        ELSE
          LIKBOL(IPTFR) = KSORT
          LIEBOL(IPTFR) = KSORT
        ENDIF
!       SAVING VALUES IN THE SECOND DIMENSION (SEE POINT_TELEMAC3D)
        LIKBOL(IPTFR+NPTFR3) = KSORT
        LIEBOL(IPTFR+NPTFR3) = KSORT
      ENDDO
!
!-----------------------------------------------------------------------
!
!     BOTTOM
!
!-----------------------------------------------------------------------
!
      IF(LIMKF.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LIKBOF(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          IF(LIUBOF(IPOIN2).EQ.KSORT) THEN
            LIKBOF(IPOIN2) = KSORT
          ELSE
            LIKBOF(IPOIN2) = KENT
          ENDIF
        ENDDO
      ENDIF
!
      IF(LIMEF.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LIEBOF(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          IF(LIUBOF(IPOIN2).EQ.KSORT) THEN
            LIEBOF(IPOIN2) = KSORT
          ELSE
! BEGIN OF PART SPECIFIC TO THIS CASE
!           LIEBOF(IPOIN2) = KENT
            LIEBOF(IPOIN2) = KSORT
! END OF PART SPECIFIC TO THIS CASE
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FREE SURFACE
!
!-----------------------------------------------------------------------
!
      IF(LIMKS.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LIKBOS(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          LIKBOS(IPOIN2) = KSORT
        ENDDO
      ENDIF
!
      IF(LIMES.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LIEBOS(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          LIEBOS(IPOIN2) = KSORT
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

