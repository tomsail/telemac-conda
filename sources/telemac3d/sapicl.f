!                   *****************
                    SUBROUTINE SAPICL
!                   *****************
!
     & (LINUBOF,LIUBOF,LINUBOL,LIUBOL,LINUBOS,
     &  NPTFR,NPLAN,NPOIN2,KENT,KSORT)
!
!***********************************************************************
! TELEMAC3D   V8P0                                   21/08/2018
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION
!+                SOURCE TERM STEP OF THE SPALART MODEL.
!+
!
!history A. Bourgoin
!+        21/08/2018
!+        V6P0
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
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
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D, ONLY : LIMNUF,LIMNUS
!
      USE BIEF
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPTFR, NPLAN, NPOIN2
      INTEGER, INTENT(IN)    :: KENT, KSORT
      INTEGER, INTENT(IN)    :: LIUBOF(NPOIN2)
      INTEGER, INTENT(IN)    :: LIUBOL(NPTFR*NPLAN*2)
      INTEGER, INTENT(INOUT) :: LINUBOF(NPOIN2), LINUBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LINUBOL(NPTFR*NPLAN*2)

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPTFR, IPOIN2,NPTFR3
!
!-----------------------------------------------------------SAPICL------------
!
!     LATERAL BOUNDARIES :
!
!-----------------------------------------------------------------------
!
      NPTFR3=NPLAN*NPTFR
!
      DO IPTFR=1,NPTFR3
        IF(LIUBOL(IPTFR).EQ.KENT) THEN
          LINUBOL(IPTFR) = KENT
        ELSE
          LINUBOL(IPTFR) = KSORT
        ENDIF
!       SAVING VALUES IN THE SECOND DIMENSION (SEE POINT_TELEMAC3D)
        LINUBOL(IPTFR+NPTFR3) = KSORT
      ENDDO
!
!-----------------------------------------------------------------------
!
!     BOTTOM
!
!-----------------------------------------------------------------------
!
      IF(LIMNUF.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LINUBOF(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          IF(LIUBOF(IPOIN2).EQ.KSORT) THEN
            LINUBOF(IPOIN2) = KSORT
          ELSE
            LINUBOF(IPOIN2) = KENT
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
      IF(LIMNUS.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LINUBOS(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          LINUBOS(IPOIN2) = KSORT
        ENDDO
      ENDIF

!
!-----------------------------------------------------------------------
!
      RETURN
      END
